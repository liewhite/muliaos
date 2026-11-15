package muliao.triggers

import muliao.contract.*
import muliao.notifications.{Notice, Notifications, Target}
import muliao.platform.AppError
import muliao.platform.db.{Database, Tx}
import muliao.platform.events.ChangeBus
import muliao.resources.ResourcePaths
import muliao.contract.AgentState
import muliao.resources.agents.Agents
import muliao.sessions.*
import muliao.triggers.store.{FaultRow, Repo, TriggerRow}
import muliao.workspace.AgentExec
import zio.*
import zio.json.*

import java.time.{Instant, ZoneId}

/** 为什么触发。 */
enum Occasion:
  /** 定时、一次性到期 */
  case Due
  /** 常驻流的事件（每项一行） */
  case Events(lines: Chunk[String])
  /** 立即运行（界面或 trigger_run） */
  case Manual

/** 一次触发的结局。 */
enum Fired:
  case Started(session: SessionId)
  /** 上一次开的会话还没做完 */
  case Blocked(session: SessionId)
  /** 正常地不触发（前置检查说没事、只记了基线、事件丢弃） */
  case Quiet(why: String)
  /** 该做没做成（agent 已归档、前置检查失败、投不进）：一次性的要告诉用户 */
  case NotDone(why: String)

  def result: FireResult = this match
    case Started(s) => FireResult(Some(s), s"已触发，开了会话 ${s.value}")
    case Blocked(s) => FireResult(None, s"上一次开的会话 ${s.value} 还没做完，这次没触发")
    case Quiet(w)   => FireResult(None, w)
    case NotDone(w) => FireResult(None, s"没触发：$w")

/** 常驻命令在内存里的运行状况（不进库）。 */
final case class Health(runningSince: Option[Long], failures: Int)

/** 改动后叫醒两个激活机制（到期扫描、常驻流托管），让它们按最新的行重新对账。 */
final class Signal private (due: Queue[Unit], stream: Queue[Unit]):
  def ping: UIO[Unit]        = due.offer(()) *> stream.offer(()).unit
  def dueWake: UIO[Unit]     = due.take
  def streamWake: UIO[Unit]  = stream.take

object Signal:
  def make: UIO[Signal] = Queue.sliding[Unit](1).zipWith(Queue.sliding[Unit](1))(Signal(_, _))

/** 本领域的核心：行与类型的转换、唯一的触发出口 [[fire]]、脚本故障。到期扫描与常驻流托管都只经 [[fire]] 触发。 */
private[triggers] final case class Core(
    db: Database,
    bus: ChangeBus,
    sessions: Sessions,
    agents: Agents,
    notifications: Notifications,
    exec: AgentExec,
    paths: ResourcePaths,
    zone: ZoneId,
    settings: TriggerSettings,
    tuning: Tuning,
    signal: Signal,
    /** 正在跑前置检查（还没开会话）的触发器：防重叠只靠它 */
    inFlight: Ref[Set[TriggerId]],
    health: Ref[Map[TriggerId, Health]]
):
  val now: UIO[Long] = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

  def changed: URIO[muliao.platform.db.TxHooks, Unit] = bus.publishAfterCommit(Triggers.Topic)

  // ---- 行与类型 ----

  def decode(r: TriggerRow): IO[AppError, Trigger] =
    val parsed =
      for
        config   <- r.config.fromJson[TriggerConfig]
        mode     <- Mode.parse(r.kind, config, zone)
        precheck <- r.precheck.fold(Right(None))(p => p.fromJson[PrecheckSpec].flatMap(Precheck.parse).map(Some(_)))
      yield Trigger(
        r.id, r.name, r.agentId, r.prompt, mode, r.enabled, precheck, r.nextAt, r.skipped, r.lastSessionId, r.lastFiredAt,
        r.blockNotified, r.baseline, r.finishedAt, r.createdAt, r.updatedAt
      )
    ZIO.fromEither(parsed).mapError(e => AppError.Internal(s"库里的触发器 ${r.name.value}（${r.id}）读不懂：$e"))

  def encode(t: Trigger): TriggerRow =
    TriggerRow(
      t.id, t.name, t.agent, t.prompt, t.mode.kind, t.mode.config(zone).toJson, t.enabled, t.precheck.map(_.spec.toJson), t.nextAt,
      t.skipped, t.lastSession, t.lastFiredAt, t.blockNotified, t.baseline, t.finishedAt, t.createdAt, t.updatedAt
    )

  def load(id: TriggerId): IO[AppError, Option[Trigger]] =
    db.read(Repo.get(id)).flatMap(ZIO.foreach(_)(decode))

  def loadTx(id: TriggerId): Tx[Trigger] =
    Repo.get(id).someOrFail(AppError.notFound(s"触发器 ${id.value} 不存在（可能已删除）")).flatMap(decode)

  def active: IO[AppError, List[Trigger]] =
    db.read(Repo.active).flatMap(rows =>
      // 一行读不懂不拖累别的：记日志、跳过
      ZIO.foreach(rows)(r => decode(r).map(Some(_)).catchAll(e => ZIO.logError(e.message).as(None))).map(_.flatten)
    )

  // ---- 触发：唯一出口 ----

  /** 交给 agent 的信：处理说明，有事件就附在后面。 */
  def letter(t: Trigger, event: Option[String]): Letter =
    val text = event.fold(t.prompt)(e => s"${t.prompt}\n\n<event>\n$e\n</event>")
    Letter(Source.Trigger(t.name), text)

  /** 触发一次：agent 能不能接活 → 防重叠 → 前置检查 → 开会话（记下触发与投信在同一事务）。 */
  def fire(t: Trigger, cause: Occasion): IO[AppError, Fired] =
    agents.get(t.agent).map(_.state).catchAll(_ => ZIO.succeed(AgentState.Deleted)).flatMap {
      case AgentState.Active   => guarded(t, cause)
      case AgentState.Archived => ZIO.succeed(unavailable(cause, "agent 已归档"))
      case AgentState.Deleting | AgentState.Deleted => ZIO.succeed(unavailable(cause, "agent 已删除"))
    }

  private def unavailable(cause: Occasion, why: String): Fired = cause match
    case Occasion.Events(_) => Fired.Quiet(s"$why，事件丢弃")
    case Occasion.Due | Occasion.Manual => Fired.NotDone(why)

  private def guarded(t: Trigger, cause: Occasion): IO[AppError, Fired] =
    inFlight.modify(s => if s(t.id) then (false, s) else (true, s + t.id)).flatMap {
      case false =>
        val busy = Fired.Quiet("上一次触发还在跑前置检查，这次没触发")
        cause match
          case Occasion.Due => countSkip(t, None).as(busy)
          case _         => ZIO.succeed(busy)
      case true =>
        (blocker(t).flatMap {
          case Some((s, state)) =>
            cause match
              case Occasion.Due       => countSkip(t, Some(s -> state)).as(Fired.Blocked(s))
              case Occasion.Events(_) => noteBlocked(t, s, state).as(Fired.Blocked(s))
              case Occasion.Manual    => ZIO.succeed(Fired.Blocked(s))
          case None => checked(t, cause)
        }).ensuring(inFlight.update(_ - t.id))
    }

  /** 上一次开的会话没做完（运行中或等待中）就挡着。 */
  def blocker(t: Trigger): IO[AppError, Option[(SessionId, WorkState)]] =
    t.lastSession match
      case None    => ZIO.none
      case Some(s) => sessions.states(List(s)).map(_.get(s).filter(_ != WorkState.Idle).map(s -> _))

  /** 跳过一次（只在定时到期时计数）。挡路的会话在等待（可能永远等不到）就通知用户，每个挡路会话一次。 */
  private def countSkip(t: Trigger, by: Option[(SessionId, WorkState)]): IO[AppError, Unit] =
    db.write(
      for
        cur <- loadTx(t.id)
        notify = by.collect { case (s, WorkState.Waiting) if !cur.blockNotified.contains(s) => s }
        _ <- ZIO.foreachDiscard(notify)(s => notifications.notifyIn(blockedNotice(cur, s)))
        _ <- Repo.save(encode(cur.copy(skipped = cur.skipped + 1, blockNotified = notify.orElse(cur.blockNotified))))
        _ <- changed
      yield ()
    )

  /** 常驻流被挡：不计数，同样每个挡路会话通知一次。 */
  def noteBlocked(t: Trigger, s: SessionId, state: WorkState): IO[AppError, Unit] =
    ZIO.when(state == WorkState.Waiting && !t.blockNotified.contains(s))(
      db.write(
        for
          cur <- loadTx(t.id)
          _ <- ZIO.when(!cur.blockNotified.contains(s))(
                 notifications.notifyIn(blockedNotice(cur, s)) *> Repo.save(encode(cur.copy(blockNotified = Some(s)))) *> changed
               )
        yield ()
      )
    ).unit

  private def blockedNotice(t: Trigger, s: SessionId): Notice =
    Notice(
      s"触发器 ${t.name.value} 被挡住了",
      "上一次触发开的会话在等待（后台任务、委派、唤醒或你的回答），它做完之前这个触发器不会再开新会话。",
      Target.Session(s)
    )

  /** 跑前置检查（如果有），再开会话。 */
  private def checked(t: Trigger, cause: Occasion): IO[AppError, Fired] =
    val event = cause match
      case Occasion.Events(lines) => Some(lines.mkString("\n"))
      case _                   => None
    t.precheck match
      case None => start(t, event, None, checkedAt = None)
      case Some(p) =>
        val stdin = cause match
          case Occasion.Events(lines) => lines.toList
          case _                   => Nil
        Checks.run(exec, t.agent, p, stdin, settings.precheckTimeout).flatMap {
          case Left(err) =>
            val why = s"前置检查失败：$err"
            cause match
              case Occasion.Manual => ZIO.succeed(Fired.NotDone(why)) // 只报给调用方，免得 agent 验证脚本时开出告警会话
              case _            => recordFault(t, FaultSource.Precheck, err).as(Fired.NotDone(why))
          case Right(out) =>
            clearFault(t, FaultSource.Precheck) *>
              (Checks.decide(p.fireWhen, t.baseline, out) match
                case Verdict.Quiet       => ZIO.succeed(Fired.Quiet("前置检查说没事（没有输出或输出没变）"))
                case Verdict.Baseline(o) => keepBaseline(t, o).as(Fired.Quiet("第一次跑前置检查（changed），只记下基线"))
                case Verdict.Fire(e, b)  => start(t, Some(e), b, checkedAt = Some(t.updatedAt)))
        }

  private def keepBaseline(t: Trigger, output: String): IO[AppError, Unit] =
    db.write(
      loadTx(t.id).flatMap(cur => ZIO.when(cur.updatedAt == t.updatedAt)(Repo.save(encode(cur.copy(baseline = Some(output)))))).unit
    )

  /** 开会话并记下这次触发（同一事务，恰好一次）。前置检查跑过时（`checkedAt` 是当时的版本），期间触发器被改了这次作废。 */
  private def start(t: Trigger, event: Option[String], baseline: Option[String], checkedAt: Option[Long]): IO[AppError, Fired] =
    now.flatMap { at =>
      db.write(
        for
          cur <- loadTx(t.id)
          _ <- ZIO.foreachDiscard(checkedAt)(v =>
                 ZIO.when(cur.updatedAt != v)(ZIO.fail(AppError.conflict("前置检查期间触发器被改了，这次结果作废")))
               )
          d <- sessions.deliverIn(Address.Fresh(t.agent, Origin.Trigger(t.name), t.name.value), letter(cur, event))
          s <- ZIO.fromOption(d).orElseFail(AppError.Internal("投信没有结果"))
          _ <- Repo.save(encode(cur.copy(lastSession = Some(s.session), lastFiredAt = Some(at), skipped = 0, blockNotified = None,
                 baseline = baseline.orElse(cur.baseline))))
          _ <- changed
        yield Fired.Started(s.session)
      ).catchSome { case e: AppError.Domain => ZIO.succeed(Fired.NotDone(e.message)) }
    }

  // ---- 一次性没做成 ----

  def notDone(t: Trigger, why: String): UIO[Unit] =
    notifications
      .notify(Notice(s"一次性触发器 ${t.name.value} 没做", s"到点时$why，这次任务没有执行。", Target.Page(Triggers.PageUrl)))
      .catchAll(e => ZIO.logError(s"通知用户「触发器 ${t.name.value} 没做」失败：${e.message}"))
      .unit

  // ---- 脚本故障 ----

  /** 记下故障（同来源只留最新一条）；从正常变为出错时给所属 agent 开会话告警一次，投不进就推送用户。 */
  def recordFault(t: Trigger, source: FaultSource, error: String): UIO[Unit] =
    (for
      at <- now
      fresh <- db.write(
                 for
                   before <- Repo.faultsOf(List(t.id))
                   live   <- Repo.get(t.id)
                   _      <- ZIO.when(live.isDefined)(Repo.putFault(FaultRow(t.id, source, error, at)) *> changed)
                 yield before.isEmpty && live.isDefined
               )
      _ <- ZIO.when(fresh)(alert(t, source, error))
    yield ()).catchAll(e => ZIO.logError(s"记录触发器 ${t.name.value} 的脚本故障失败：${e.message}"))

  def clearFault(t: Trigger, source: FaultSource): UIO[Unit] =
    db.write(Repo.clearFaults(t.id, Some(source)).tap(n => ZIO.when(n > 0)(changed)))
      .catchAll(e => ZIO.logError(s"清除触发器 ${t.name.value} 的脚本故障失败：${e.message}"))
      .unit

  private def alert(t: Trigger, source: FaultSource, error: String): UIO[Unit] =
    val what = source match
      case FaultSource.Precheck => "前置检查"
      case FaultSource.Stream   => "常驻命令"
    val fix = source match
      case FaultSource.Precheck => "改好后可以用 trigger_run 验证（立即运行时检查失败只报给调用方，不再开告警会话）。"
      case FaultSource.Stream   => s"标准错误在工作目录 ${Streams.logName(t.name)}；改了脚本要重启就先停用再启用这个触发器。"
    val text =
      s"""触发器「${t.name.value}」的$what 出错了（触发器仍然启用，脚本恢复正常后故障自动清除）：
         |
         |$error
         |
         |请排查并修好它。$fix""".stripMargin
    sessions
      .deliver(Address.Fresh(t.agent, Origin.Trigger(t.name), s"${t.name.value} 的$what 出错"), Letter(Source.Trigger(t.name), text))
      .unit
      .catchAll { e =>
        notifications
          .notify(Notice(s"触发器 ${t.name.value} 的$what 出错", s"$error\n（没能交给 agent：${e.message}）", Target.Page(Triggers.PageUrl)))
          .unit
      }
      .catchAll(e => ZIO.logError(s"触发器 ${t.name.value} 的故障告警发不出去：${e.message}"))

  def instant(ms: Long): Instant = Instant.ofEpochMilli(ms)
