package muliao.sessions

import muliao.contract.*
import muliao.models.llm.{Models, ReplyBuilder}
import muliao.models.mcp.McpConnector
import muliao.platform.AppError
import muliao.platform.db.{Database, Tx}
import muliao.platform.events.ChangeBus
import muliao.resources.{ResourcePaths, Resources}
import muliao.resources.agents.Agents
import muliao.resources.vault.Vault
import muliao.sessions.store.*
import zio.*
import zio.json.*

import java.time.ZoneId

/** 会话的基本信息（解码后）。 */
final case class SessionInfo(
    id: SessionId,
    agent: AgentId,
    origin: Origin,
    title: String,
    pendingReply: List[ReplyTo],
    createdAt: Long,
    updatedAt: Long
):
  def userWaiting: Boolean = pendingReply.contains(ReplyTo.User)

private[sessions] object SessionInfo:
  def decode(r: SessionRow): IO[AppError, SessionInfo] =
    for
      origin  <- ZIO.fromEither(originOf(r)).mapError(e => AppError.Internal(s"会话 ${r.id.value} 的来源记录坏了：$e"))
      pending <- Decode.replyTos(r.pendingReply)
    yield SessionInfo(r.id, r.agentId, origin, r.title, pending, r.createdAt, r.updatedAt)

  private def originOf(r: SessionRow): Either[String, Origin] = (r.origin, r.originRef) match
    case ("user", _)          => Right(Origin.User)
    case ("trigger", Some(n)) => Right(Origin.Trigger(TriggerName.wrap(n)))
    case ("issue", Some(n))   => n.toLongOption.map(i => Origin.Issue(IssueId.wrap(i))).toRight(s"issue 号 $n")
    case ("thread", Some(n))  => Right(Origin.Thread(r.parentId, ThreadName.wrap(n)))
    case (o, ref)             => Left(s"$o/$ref")

  def row(id: SessionId, agent: AgentId, origin: Origin, title: String, now: Long): SessionRow =
    val (ref, parent) = origin match
      case Origin.User            => (None, None)
      case Origin.Trigger(n)      => (Some(n.value), None)
      case Origin.Issue(i)        => (Some(i.value.toString), None)
      case Origin.Thread(p, n)    => (Some(n.value), p)
    SessionRow(id, agent, origin.label, ref, parent, title, "[]", now, now)

/** 一个会话的驱动槽：同一会话同时只有一个驱动 fiber。
  * @param again    驱动期间又有人踢了一下：结束前再看一遍
  * @param stopping 正在停止：驱动不再自己收尾，由停止方收尾
  * @param partial  正在生成的回复（停止时保存已生成的部分）
  * @param secrets  打码前的调用参数（(回复, 调用) → 原文）：库里只存打码后的，执行用原文；重启后没有原文的调用不再执行
  */
private[sessions] final class Slot(
    val again: Ref[Boolean],
    val stopping: Ref[Boolean],
    val partial: Ref[Option[Partial]],
    val fiber: Ref[Option[Fiber[Nothing, Unit]]],
    val secrets: Ref[Map[(Long, String), String]]
)

private[sessions] object Slot:
  def make: UIO[Slot] =
    for
      a <- Ref.make(false)
      s <- Ref.make(false)
      p <- Ref.make(Option.empty[Partial])
      f <- Ref.make(Option.empty[Fiber[Nothing, Unit]])
      r <- Ref.make(Map.empty[(Long, String), String])
    yield Slot(a, s, p, f, r)

/** 正在生成的一条回复。 */
private[sessions] final case class Partial(run: RunId, model: String, startedAt: Long, builder: ReplyBuilder)

/** 本领域的全部依赖。 */
private[sessions] final case class Deps(
    db: Database,
    bus: ChangeBus,
    hooks: SessionHooks,
    files: Files,
    live: LiveHub,
    resources: Resources,
    agents: Agents,
    models: Models,
    mcp: McpConnector,
    vault: Vault,
    paths: ResourcePaths,
    settings: SessionSettings,
    zone: ZoneId
)

/** 会话与运行的引擎：来信、驱动、停止、删除、等待、委派、压缩都在这里，按主题分在几个文件里（`*Part`）。 */
private[sessions] final class Engine(
    val deps: Deps,
    val slots: Ref.Synchronized[Map[SessionId, Slot]],
    val waitCache: Ref[Map[Long, WaitRow]],
    val beats: Ref[Map[Long, Long]],
    val scope: Scope
) extends InboxPart with DriverPart with CallsPart with ControlPart with WaitsPart with DelegationPart with CompactionPart:
  import deps.*

  val Topic: muliao.contract.Topic = muliao.contract.Topic.wrap("sessions")

  def now: UIO[Long] = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

  def changed: URIO[muliao.platform.db.TxHooks, Unit] = bus.publishAfterCommit(Topic)

  def afterCommit(effect: UIO[Unit]): URIO[muliao.platform.db.TxHooks, Unit] = muliao.platform.db.TxHooks.afterCommit(effect)

  def info(id: SessionId): IO[AppError, SessionInfo] =
    db.read(Repo.session(id)).someOrFail(AppError.notFound(s"会话 ${id.value} 不存在（可能已删除）")).flatMap(SessionInfo.decode)

  def infoTx(id: SessionId): Tx[SessionInfo] =
    Repo.session(id).someOrFail(AppError.notFound(s"会话 ${id.value} 不存在（可能已删除）")).flatMap(SessionInfo.decode)

  // ---- 工作状态（全系统唯一判断） ----

  def states(ids: List[SessionId]): IO[AppError, Map[SessionId, WorkState]] =
    if ids.isEmpty then ZIO.succeed(Map.empty)
    else
      for
        active  <- db.read(Repo.activeRuns(ids))
        queued  <- db.read(Repo.queuedCounts(ids))
        waiting <- db.read(Repo.waitCounts(ids))
        driving <- slots.get.map(_.keySet)
      yield
        val running   = active.filter(_.state == RunState.Running.label).map(_.sessionId).toSet
        val suspended = active.filter(_.state == RunState.Suspended.label).map(_.sessionId).toSet
        ids.map { id =>
          val st =
            if running(id) || driving(id) || queued.contains(id) then WorkState.Running
            else if suspended(id) || waiting.contains(id) then WorkState.Waiting
            else WorkState.Idle
          id -> st
        }.toMap

  def state(id: SessionId): IO[AppError, WorkState] = states(List(id)).map(_.getOrElse(id, WorkState.Idle))

  // ---- 驱动槽 ----

  /** 让会话动起来：没有驱动就起一个；有就让它结束前再看一遍。 */
  def kick(id: SessionId): UIO[Unit] =
    slots.modifyZIO { m =>
      m.get(id) match
        case Some(s) => s.again.set(true).as(((), m))
        case None =>
          for
            s <- Slot.make
            f <- worker(id, s).forkIn(scope)
            _ <- s.fiber.set(Some(f))
          yield ((), m.updated(id, s))
    }

  private def worker(id: SessionId, s: Slot): UIO[Unit] =
    val once = ZIO.scoped(drive(id, s)).catchAllCause(c => unexpected(id, c))
    def loop: UIO[Unit] =
      once *> slots
        .modifyZIO { m =>
          s.stopping.get.flatMap {
            case true  => ZIO.succeed((false, m)) // 停止方收尾、移除
            case false => s.again.getAndSet(false).map(again => if again then (true, m) else (false, m - id))
          }
        }
        .flatMap(again => loop.when(again).unit)
    loop

  /** 驱动里没料到的错误：记日志，把这次运行标成最终失败（让人看见），不让它静默。 */
  private def unexpected(id: SessionId, c: Cause[AppError]): UIO[Unit] =
    val msg = c.failureOption.map(_.message).getOrElse("内部错误，详情见服务日志")
    ZIO.logErrorCause(s"会话 ${id.value} 的运行出错", c) *> failLastRun(id, msg).catchAllCause(c2 => ZIO.logErrorCause("记录运行失败时出错", c2))

  /** 独占一个会话做一件事（如手动压缩）：会话必须空闲且没有驱动，做完照常看一遍有没有新信。 */
  def exclusive[A](id: SessionId)(job: IO[AppError, A]): IO[AppError, A] =
    for
      st <- state(id)
      _  <- ZIO.when(st != WorkState.Idle)(ZIO.fail(AppError.conflict(s"会话正在${if st == WorkState.Running then "运行" else "等待"}，等它空闲再做")))
      slot <- slots.modifyZIO { m =>
                if m.contains(id) then ZIO.fail(AppError.conflict("会话正忙，稍后再试"))
                else Slot.make.tap(_.stopping.set(true)).map(s => (s, m.updated(id, s)))
              }
      a <- job.ensuring(slots.update(_ - id) *> kick(id).whenZIO(db.read(Repo.queued(id)).map(_.nonEmpty).orElseSucceed(false)).unit)
    yield a

  // ---- 启动与关停 ----

  /** 启动时：收尾上次被打断的调用，续跑没跑完的运行与排队的来信，载入等待并起调度。 */
  def startup: IO[AppError, Unit] =
    for
      t       <- now
      running <- db.read(Repo.callsInState(CallState.Running.label))
      pending <- db.read(Repo.callsInState(CallState.Pending.label))
      _ <- db.write(
             ZIO.foreachDiscard(running)(c =>
               Repo.setCall(c.entryId, c.callId, CallState.Done.label, c.pause, Some(CallResult.error(Interrupted.restartedWhileRunning).toJson), c.startedAt, Some(t))
             ) *> ZIO.foreachDiscard(pending)(c =>
               Repo.setCall(c.entryId, c.callId, CallState.Done.label, c.pause, Some(CallResult.error(Interrupted.restartedBeforeStart).toJson), None, Some(t))
             )
           )
      _ <- ZIO.when(running.nonEmpty || pending.nonEmpty)(ZIO.logWarning(s"启动：${running.size} 个执行中、${pending.size} 个没开始的工具调用标成被打断"))
      runs <- db.read(Repo.runsIn(List(RunState.Running.label)))
      mail <- db.read(Repo.sessionsWithMail)
      approved <- db.read(Repo.callsInState(CallState.Approved.label)) <*> db.read(Repo.callsInState(CallState.ApprovedAlways.label))
      toKick = (runs.map(_.sessionId) ++ mail ++ (approved._1 ++ approved._2).map(_.sessionId)).distinct
      _ <- ZIO.logInfo(s"启动：续跑 ${toKick.size} 个会话")
      _ <- ZIO.foreachDiscard(toKick)(kick)
      _ <- startWaits
    yield ()

  /** 已有的部分回复折叠成段（停止时保存）。 */
  def partialOf(s: Slot): UIO[Option[Partial]] = s.partial.get

private[sessions] object Engine:
  def make(deps: Deps): ZIO[Scope, AppError, Engine] =
    for
      scope <- ZIO.scope
      slots <- Ref.Synchronized.make(Map.empty[SessionId, Slot])
      cache <- Ref.make(Map.empty[Long, WaitRow])
      beats <- Ref.make(Map.empty[Long, Long])
      e      = Engine(deps, slots, cache, beats, scope)
      _     <- e.registerKinds
    yield e
