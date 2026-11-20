package muliao.sessions

import muliao.contract.*
import muliao.models.llm.ReplyBuilder
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.{Database, Read, Tx, TxHooks}
import muliao.platform.events.ChangeBus
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
    /** 运行做完了、还没收尾（见 [[Delegations.settleTx]]） */
    settleDue: Boolean,
    createdAt: Long,
    updatedAt: Long
):
  def userWaiting: Boolean = pendingReply.contains(ReplyTo.User)

private[sessions] object SessionInfo:
  def decode(r: SessionRow): IO[AppError, SessionInfo] =
    for
      origin  <- ZIO.fromEither(origin(r)).mapError(e => AppError.Internal(s"会话 ${r.id.value} 的来源记录坏了：$e"))
      pending <- Decode.replyTos(r.pendingReply)
    yield SessionInfo(r.id, r.agentId, origin, r.title, pending, r.settleDue, r.createdAt, r.updatedAt)

  /** 从库里的种类、引用（触发器名 / issue 号 / thread 名）、发起会话还原来源。 */
  private def origin(r: SessionRow): Either[String, Origin] = (r.origin, r.originRef) match
    case (OriginKind.User, _)          => Right(Origin.User)
    case (OriginKind.Trigger, Some(n)) => TriggerName.make(n).map(Origin.Trigger(_))
    case (OriginKind.Issue, Some(n))   => n.toLongOption.flatMap(i => IssueId.make(i).toOption).map(Origin.Issue(_)).toRight(s"issue 号 $n")
    case (OriginKind.Thread, Some(n))  => ThreadName.make(n).map(Origin.Thread(r.parentId, _))
    case (k, None)                     => Left(s"${k.label} 没有引用")

  def row(id: SessionId, agent: AgentId, origin: Origin, title: String, now: Long): SessionRow =
    val (ref, parent) = origin match
      case Origin.User         => (None, None)
      case Origin.Trigger(n)   => (Some(n.value), None)
      case Origin.Issue(i)     => (Some(i.value.toString), None)
      case Origin.Thread(p, n) => (Some(n.value), p)
    SessionRow(id, agent, origin.kind, ref, parent, title, "[]", false, now, now)

/** 本领域各部分共用的底子：库、「数据变了」广播、实时事件、接进来的东西、配置，以及读会话基本信息。 */
private[sessions] final case class Kernel(db: Database, bus: ChangeBus, live: LiveHub, ports: Ports, settings: SessionSettings, general: GeneralSettings):
  def zone: ZoneId = general.timezone

  def now: UIO[Long] = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

  /** 会话的库里数据变了（时间线、运行、调用、等待、排队的信、回答去向）就在同一个写事务里调它，是通知「该刷新了」的唯一入口：
    * 更新会话的活动时间，提交后广播主题 sessions（列表）并给这个会话推实时事件 `Changed`（会话页）。
    */
  def touched(id: SessionId, t: Long): Tx[Unit] =
    Repo.touch(id, t) *> bus.publishAfterCommit(Topic.Sessions) *> afterCommit(live.publish(id, LiveEvent.Changed))

  def afterCommit(effect: UIO[Unit]): URIO[TxHooks, Unit] = TxHooks.afterCommit(effect)

  def info(id: SessionId): IO[AppError, SessionInfo] = db.read(infoTx(id))

  def infoTx(id: SessionId): Read[SessionInfo] =
    Repo.session(id).someOrFail(AppError.notFound(s"会话 ${id.value} 不存在（可能已删除）")).flatMap(SessionInfo.decode)

  /** 最后一条回复的正文。 */
  def lastAnswer(id: SessionId): IO[AppError, String] =
    db.read(Repo.lastEntry(id, EntryKind.Reply)).flatMap(ZIO.foreach(_)(Decode.entry)).map {
      case Some(Entry(_, _, EntryBody.Reply(parts, _, _, _, _), _, _)) => ModelView.answerText(parts)
      case _                                                           => ""
    }

  /** 往会话的时间线里记一条提示。 */
  def noticeTx(id: SessionId, run: Option[RunId], kind: NoticeKind, text: String, t: Long): Tx[Long] =
    Repo.insertEntry(EntryRow(0L, id, run, EntryKind.Notice, EntryBody.Notice(kind, text).encode, "", t, t))

  /** 把会话的回答去向写回库。 */
  def setPendingTx(id: SessionId, all: List[ReplyTo]): Tx[Unit] = Repo.setPendingReply(id, all.toJson)

private[sessions] object Kernel:
  val layer: URLayer[Database & ChangeBus & LiveHub & Ports & SessionSettings & GeneralSettings, Kernel] = ZLayer.derive[Kernel]

/** 正在生成的一条回复。 */
private[sessions] final case class Partial(run: RunId, model: String, startedAt: Long, builder: ReplyBuilder)

/** 一个会话的驱动槽：同一会话同时只有一个驱动 fiber（或一件独占的事）。
  * @param again    驱动期间又有人踢了一下：结束前再看一遍
  * @param stopping 正在停止或被独占：驱动不再自己收尾，由停止方（独占方）收尾
  * @param partial  正在生成的回复（停止时保存已生成的部分）
  * @param secrets  打码前的调用参数：库里只存打码后的，执行用原文；重启后没有原文的调用不再执行
  */
private[sessions] final class Slot(
    val again: Ref[Boolean],
    val stopping: Ref[Boolean],
    val partial: Ref[Option[Partial]],
    val fiber: Ref[Option[Fiber[Nothing, Unit]]],
    val secrets: Ref[Map[CallKey, String]]
)

/** 一次工具调用在库里的键。 */
final case class CallKey(entry: Long, call: CallId)

private[sessions] object Slot:
  def make(stopping: Boolean): UIO[Slot] =
    for
      a <- Ref.make(false)
      s <- Ref.make(stopping)
      p <- Ref.make(Option.empty[Partial])
      f <- Ref.make(Option.empty[Fiber[Nothing, Unit]])
      r <- Ref.make(Map.empty[CallKey, String])
    yield Slot(a, s, p, f, r)

/** 各会话的驱动槽。 */
private[sessions] final class Slots(ref: Ref.Synchronized[Map[SessionId, Slot]]):

  /** 正有驱动（或独占）的会话。 */
  def busy: UIO[Set[SessionId]] = ref.get.map(_.keySet)

  /** 没有槽就新建一个并由 `run` 起驱动 fiber；已有就让它结束前再看一遍。 */
  def kick(id: SessionId)(run: Slot => UIO[Fiber[Nothing, Unit]]): UIO[Unit] =
    ref.modifyZIO { m =>
      m.get(id) match
        case Some(s) => s.again.set(true).as(((), m))
        case None =>
          for
            s <- Slot.make(stopping = false)
            f <- run(s)
            _ <- s.fiber.set(Some(f))
          yield ((), m.updated(id, s))
    }

  /** 驱动做完一轮：被踢过就再来一轮（返回 true），否则让出槽。正在停止的槽由停止方移除。 */
  def again(id: SessionId, s: Slot): UIO[Boolean] =
    ref.modifyZIO { m =>
      s.stopping.get.flatMap {
        case true  => ZIO.succeed((false, m))
        case false => s.again.getAndSet(false).map(again => (again, if again then m else m - id))
      }
    }

  /** 停止：标记槽正在停止（没有就占一个，挡住新驱动），打断驱动 fiber，返回槽。之后由调用方 [[release]]。 */
  def halt(id: SessionId): UIO[Slot] =
    for
      s <- ref.modifyZIO { m =>
             m.get(id) match
               case Some(s) => s.stopping.set(true).as((s, m))
               case None    => Slot.make(stopping = true).map(s => (s, m.updated(id, s)))
           }
      _ <- s.fiber.get.flatMap(ZIO.foreachDiscard(_)(_.interrupt))
    yield s

  /** 独占一个会话（没有驱动时）：占上一个正在停止的槽挡住驱动。 */
  def reserve(id: SessionId): IO[AppError, Unit] =
    ref.modifyZIO { m =>
      if m.contains(id) then ZIO.fail(AppError.conflict("会话正忙，稍后再试"))
      else Slot.make(stopping = true).map(s => ((), m.updated(id, s)))
    }

  def release(id: SessionId): UIO[Unit] = ref.update(_ - id)

private[sessions] object Slots:
  val layer: ULayer[Slots] = ZLayer(Ref.Synchronized.make(Map.empty[SessionId, Slot]).map(Slots(_)))

/** 「这个会话有事要做」的门铃：投了信、等的结果到了、挂起的调用有了答复时按一下，由驱动（[[Driver]]）去踢它。
  * 本领域各部分只按门铃、不直接踢驱动，所以都不依赖驱动；驱动在 [[Driver.start]] 之后才开始应门。
  */
private[sessions] final class Doorbell(queue: Queue[SessionId]):
  def ring(id: SessionId): UIO[Unit]   = queue.offer(id).unit
  private[sessions] def next: UIO[SessionId] = queue.take

private[sessions] object Doorbell:
  val layer: ULayer[Doorbell] = ZLayer(Queue.unbounded[SessionId].map(Doorbell(_)))

/** 工作状态：全系统唯一的判断。有运行中的运行、有驱动或有排队的信是运行中；有挂起的运行或等待中的工作是等待中；否则空闲。 */
private[sessions] final case class States(db: Database, slots: Slots):
  def states(ids: List[SessionId]): IO[AppError, Map[SessionId, WorkState]] =
    if ids.isEmpty then ZIO.succeed(Map.empty)
    else
      for
        active  <- db.read(Repo.activeRuns(ids))
        queued  <- db.read(Repo.queuedCounts(ids))
        waiting <- db.read(Repo.waitCounts(ids))
        driving <- slots.busy
      yield
        val byRun = active.groupMapReduce(_.sessionId)(_.state)((a, b) => if a == RunState.Running then a else b)
        ids.map { id =>
          val runState = byRun.get(id)
          val st =
            if runState.contains(RunState.Running) || driving(id) || queued.contains(id) then WorkState.Running
            else if runState.contains(RunState.Suspended) || waiting.contains(id) then WorkState.Waiting
            else WorkState.Idle
          id -> st
        }.toMap

  def state(id: SessionId): IO[AppError, WorkState] = states(List(id)).map(_.getOrElse(id, WorkState.Idle))

  /** 要求会话空闲（删除、重试、手动压缩前）。`doing` 写在冲突说明里。 */
  def requireIdle(id: SessionId, doing: String): IO[AppError, Unit] =
    state(id).flatMap {
      case WorkState.Idle    => ZIO.unit
      case WorkState.Running => ZIO.fail(AppError.conflict(s"会话正在运行，$doing"))
      case WorkState.Waiting => ZIO.fail(AppError.conflict(s"会话正在等待，$doing"))
    }

private[sessions] object States:
  val layer: URLayer[Database & Slots, States] = ZLayer.derive[States]
