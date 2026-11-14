package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.Tx
import muliao.sessions.store.*
import zio.*

import java.time.Instant
import java.time.format.DateTimeFormatter

/** 一项进行中的异步工作（会话因此「等待中」）。 */
final case class WaitView(id: Long, session: SessionId, kind: String, ref: String, label: String, heartbeat: Option[Int], dueAt: Option[Long], createdAt: Long)

/** 异步工作的登记与结局交回：后台任务、带回报的委派、定时唤醒。结局作为事件投回发起会话；可配心跳检查。 */
private[sessions] trait WaitsPart:
  this: Engine =>
  import deps.*

  /** 调度多久看一次（唤醒到点、心跳到点）。 */
  val Tick: Duration = 1.second

  def openWait(session: SessionId, kind: String, ref: String, label: String, heartbeat: Option[Duration], due: Option[Long]): IO[AppError, Long] =
    now.flatMap(t => db.write(openWaitTx(session, kind, ref, label, heartbeat, due, t)))

  def openWaitTx(session: SessionId, kind: String, ref: String, label: String, heartbeat: Option[Duration], due: Option[Long], t: Long): Tx[Long] =
    val row = WaitRow(0L, session, kind, ref, label, heartbeat.map(_.toSeconds.toInt.max(1)), due, t)
    for
      _  <- Repo.session(session).someOrFail(AppError.notFound(s"会话 ${session.value} 不存在"))
      id <- Repo.insertWait(row)
      _  <- afterCommit(waitCache.update(_.updated(id, row.copy(id = id))) *> beats.update(_.updated(id, t)) *> live.publish(session, LiveEvent.Changed))
      _  <- changed
    yield id

  /** 结束一项等待；`letter` 给出时作为事件投回发起会话。已结束的返回 false。 */
  def closeWait(kind: String, ref: String, letter: Option[Letter]): IO[AppError, Boolean] =
    now.flatMap(t => db.write(closeWaitTx(kind, ref, letter, t))).flatMap {
      case Some(session) => kick(session).as(true)
      case None          => ZIO.succeed(false)
    }

  /** 在写事务里结束一项等待；返回发起会话（提交后由调用方踢）。 */
  def closeWaitTx(kind: String, ref: String, letter: Option[Letter], t: Long): Tx[Option[SessionId]] =
    Repo.waitByRef(kind, ref).flatMap {
      case None => ZIO.none
      case Some(w) =>
        for
          _ <- Repo.deleteWait(w.id)
          _ <- ZIO.foreachDiscard(letter)(l => deliverTx(Address.To(w.sessionId), l, None, t))
          _ <- afterCommit(waitCache.update(_ - w.id) *> beats.update(_ - w.id) *> live.publish(w.sessionId, LiveEvent.Changed))
          _ <- Repo.touch(w.sessionId, t) *> changed
        yield Some(w.sessionId)
    }

  def waitsOf(session: SessionId): IO[AppError, List[WaitView]] =
    db.read(Repo.waitsOf(session)).map(_.map(w => WaitView(w.id, w.sessionId, w.kind, w.ref, w.label, w.heartbeatSeconds, w.dueAt, w.createdAt)))

  /** 载入全部等待，起调度（到点唤醒、心跳检查）。 */
  def startWaits: IO[AppError, Unit] =
    for
      all <- db.read(Repo.allWaits)
      t   <- now
      _   <- waitCache.set(all.map(w => w.id -> w).toMap)
      _   <- beats.set(all.map(w => w.id -> t).toMap)
      _   <- tickLoop.forkIn(scope)
    yield ()

  private def tickLoop: UIO[Unit] = (tick.catchAllCause(c => ZIO.logErrorCause("等待调度出错", c)) *> ZIO.sleep(Tick)).forever

  private def tick: IO[AppError, Unit] =
    for
      t  <- now
      ws <- waitCache.get.map(_.values.toList)
      bs <- beats.get
      _ <- ZIO.foreachDiscard(ws.filter(_.dueAt.exists(_ <= t)))(w =>
             guarded(w)(closeWait(w.kind, w.ref, Some(Letter(Source.Event("wake"), s"[唤醒] ${w.label}"))).unit)
           )
      _ <- ZIO.foreachDiscard(ws.filter(w => w.dueAt.isEmpty && w.heartbeatSeconds.exists(h => t - bs.getOrElse(w.id, t) >= h * 1000L)))(w =>
             beats.update(_.updated(w.id, t)) *> guarded(w)(checkIn(w))
           )
    yield ()

  /** 一项等待投不进（会话没了、agent 停用了）：记错误，这次运行期间不再调度它（库里的记录留着，重启后再试），免得每秒刷日志。 */
  private def guarded(w: WaitRow)(effect: IO[AppError, Unit]): UIO[Unit] =
    effect.catchAll(e =>
      ZIO.logError(s"等待 ${w.id}（${w.kind} ${w.ref}，会话 ${w.sessionId.value}）投递失败，暂停调度到重启：${e.message}") *>
        waitCache.update(_ - w.id)
    )

  /** 心跳检查：投一条「还在进行」的检查（不推送）；已有检查在排队就不重复投。 */
  private def checkIn(w: WaitRow): IO[AppError, Unit] =
    for
      status <- hooks.workKind(w.kind).flatMap(_.fold(ZIO.succeed("（状态未知）"))(_.status(w.ref)))
      text = s"[检查] ${w.label}：$status\n还在进行。没有要做的就只回复「…」；不再需要它就停掉（委派用 stop_thread，后台任务用 job_kill）。"
      _ <- deliver(Address.To(w.sessionId), Letter(Source.Event("check-in"), text, checkIn = true), Some(s"check-in:${w.id}"))
    yield ()

  // ---- 定时唤醒 ----

  val WakeKind = "wake"

  private val timeFormat = DateTimeFormatter.ofPattern("MM-dd HH:mm:ss")

  /** 约一次唤醒（一个会话至多一个，再约就替换）；`at` 为 None 时取消。返回说明。 */
  def wake(session: SessionId, at: Option[Long], note: String): IO[AppError, String] =
    for
      t <- now
      old <- db.read(Repo.waitByRef(WakeKind, session.value))
      _ <- db.write(
             for
               _ <- ZIO.foreachDiscard(old)(w => Repo.deleteWait(w.id) *> afterCommit(waitCache.update(_ - w.id)))
               _ <- ZIO.foreachDiscard(at)(due => openWaitTx(session, WakeKind, session.value, note, None, Some(due), t))
               _ <- changed
             yield ()
           )
      _ <- live.publish(session, LiveEvent.Changed)
    yield at match
      case Some(due) =>
        val when = timeFormat.format(Instant.ofEpochMilli(due).atZone(zone))
        s"已约好 $when 唤醒${if old.isDefined then "（替换了之前约的）" else ""}。到时提醒会作为事件投到这里。"
      case None => if old.isDefined then "已取消唤醒。" else "本来就没有约唤醒。"

  /** 本领域自己的异步工作种类：唤醒、委派。 */
  def registerKinds: UIO[Unit] =
    hooks.addWorkKind(new WorkKind:
      val kind = WakeKind
      def status(ref: String): UIO[String] = ZIO.succeed("等到点唤醒")
      def cancel(ref: String): UIO[Unit]   = ZIO.unit
    ) *> hooks.addWorkKind(new WorkKind:
      val kind = DelegationKind
      def status(ref: String): UIO[String] = delegationStatus(ref.toLong).catchAll(e => ZIO.succeed(s"（查不到：${e.message}）"))
      def cancel(ref: String): UIO[Unit]   = cancelDelegation(ref.toLong).catchAll(e => ZIO.logWarning(s"放弃委派 $ref 失败：${e.message}"))
    )
