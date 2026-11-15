package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.Tx
import muliao.sessions.store.*
import zio.*

import java.time.Instant
import java.time.format.DateTimeFormatter

/** 等待的种类（存库）。 */
enum WaitKind(val label: String) extends Labeled:
  case Job        extends WaitKind("job")
  case Wake       extends WaitKind("wake")
  case Delegation extends WaitKind("delegation")

object WaitKind extends Labels[WaitKind]("等待种类", WaitKind.values)

/** 会话在等的一项异步工作。 */
enum WaitRef:
  /** 后台任务（工作区领域开的；状态与取消经 [[JobControl]]） */
  case Job(id: JobId)
  /** 约好的唤醒：到 `due` 投一条提醒。一个会话至多一个 */
  case Wake(due: Long)
  /** `report_back` 的委派：thread 交回结局时结束 */
  case Delegation(id: DelegationId)

  def kind: WaitKind = this match
    case Job(_)        => WaitKind.Job
    case Wake(_)       => WaitKind.Wake
    case Delegation(_) => WaitKind.Delegation

/** 一项等待在库里的键（种类 + 引用，全库唯一）。唤醒的引用是会话编号，所以一个会话至多一个。 */
private[sessions] final case class WaitKey(kind: WaitKind, ref: String)

private[sessions] object WaitKey:
  def of(session: SessionId, ref: WaitRef): WaitKey = ref match
    case WaitRef.Job(id)        => job(id)
    case WaitRef.Wake(_)        => wake(session)
    case WaitRef.Delegation(id) => delegation(id)

  def job(id: JobId): WaitKey               = WaitKey(WaitKind.Job, id.value.toString)
  def delegation(id: DelegationId): WaitKey = WaitKey(WaitKind.Delegation, id.value.toString)
  def wake(session: SessionId): WaitKey     = WaitKey(WaitKind.Wake, session.value)

/** 一项进行中的异步工作（会话因此「等待中」）。 */
final case class Wait(id: WaitId, session: SessionId, ref: WaitRef, label: String, heartbeat: Option[Int], createdAt: Long)

private[sessions] object Wait:
  def decode(r: WaitRow): Either[String, Wait] =
    def id[A](make: Long => Either[String, A]): Either[String, A] = r.ref.toLongOption.toRight(s"引用 ${r.ref} 不是数字").flatMap(make)
    val ref: Either[String, WaitRef] = r.kind match
      case WaitKind.Job        => id(JobId.make).map(WaitRef.Job(_))
      case WaitKind.Delegation => id(DelegationId.make).map(WaitRef.Delegation(_))
      case WaitKind.Wake       => r.dueAt.toRight("唤醒没有时间").map(WaitRef.Wake(_))
    ref.map(Wait(r.id, r.sessionId, _, r.label, r.heartbeatSeconds, r.createdAt)).left.map(e => s"等待 ${r.id.value}（${r.kind.label} ${r.ref}）记录坏了：$e")

  def row(session: SessionId, ref: WaitRef, label: String, heartbeat: Option[Duration], t: Long): WaitRow =
    val key = WaitKey.of(session, ref)
    val due = ref match
      case WaitRef.Wake(d) => Some(d)
      case _               => None
    WaitRow(WaitId.wrap(0L), session, key.kind, key.ref, label, heartbeat.map(_.toSeconds.toInt.max(1)), due, t)

/** 异步工作的登记与结局交回：后台任务、带回报的委派、定时唤醒。结局作为事件投回发起会话。
  * 进行中的等待在内存里有一份（[[cached]]），供 [[WaitClock]] 每秒看一遍，不必每秒查库。
  */
private[sessions] final class Waits(kernel: Kernel, mailbox: Mailbox, doorbell: Doorbell, cache: Ref[Map[WaitId, Wait]]):
  import kernel.*

  def open(session: SessionId, ref: WaitRef, label: String, heartbeat: Option[Duration]): IO[AppError, WaitId] =
    now.flatMap(t => db.write(openTx(session, ref, label, heartbeat, t)))

  def openTx(session: SessionId, ref: WaitRef, label: String, heartbeat: Option[Duration], t: Long): Tx[WaitId] =
    val row = Wait.row(session, ref, label, heartbeat, t)
    for
      _  <- Repo.session(session).someOrFail(AppError.notFound(s"会话 ${session.value} 不存在"))
      id <- Repo.insertWait(row)
      w   = Wait(id, session, ref, label, row.heartbeatSeconds, t)
      _  <- afterCommit(cache.update(_.updated(id, w)) *> live.publish(session, LiveEvent.Changed))
      _  <- changed
    yield id

  /** 结束一项等待；`letter` 给出时作为事件投回发起会话。已结束的返回 false。 */
  def close(key: WaitKey, letter: Option[Letter]): IO[AppError, Boolean] =
    now.flatMap(t => db.write(closeTx(key, letter, t))).flatMap {
      case Some(session) => doorbell.ring(session).as(true)
      case None          => ZIO.succeed(false)
    }

  /** 在写事务里结束一项等待；返回发起会话（提交后由调用方按门铃）。 */
  def closeTx(key: WaitKey, letter: Option[Letter], t: Long): Tx[Option[SessionId]] =
    Repo.waitByKey(key.kind, key.ref).flatMap {
      case None => ZIO.none
      case Some(w) =>
        for
          _ <- Repo.deleteWait(w.id)
          _ <- ZIO.foreachDiscard(letter)(l => mailbox.deliverTx(Address.To(w.sessionId), l, None, t))
          _ <- afterCommit(forget(List(w.id)) *> live.publish(w.sessionId, LiveEvent.Changed))
          _ <- Repo.touch(w.sessionId, t) *> changed
        yield Some(w.sessionId)
    }

  /** 在写事务里删掉一个会话的全部等待（停止时），返回删掉的。提交后由调用方 [[forget]] 并放弃这些工作。 */
  def dropTx(session: SessionId): Tx[List[Wait]] =
    for
      rows <- Repo.waitsOf(session)
      ws   <- ZIO.foreach(rows)(decodeTx)
      _    <- ZIO.foreachDiscard(rows)(r => Repo.deleteWait(r.id))
    yield ws

  /** 不再调度这些等待（已删掉，或投递失败暂停到重启）。 */
  def forget(ids: List[WaitId]): UIO[Unit] = cache.update(_ -- ids)

  def forgetSession(session: SessionId): UIO[Unit] = cache.update(_.filterNot(_._2.session == session))

  def list(session: SessionId): IO[AppError, List[Wait]] = db.read(Repo.waitsOf(session)).flatMap(ZIO.foreach(_)(decodeTx))

  def cached: UIO[List[Wait]] = cache.get.map(_.values.toList)

  /** 从库里载入全部等待（启动时）。 */
  def load: IO[AppError, List[Wait]] =
    db.read(Repo.allWaits).flatMap(ZIO.foreach(_)(decodeTx)).tap(all => cache.set(all.map(w => w.id -> w).toMap))

  private def decodeTx(r: WaitRow): IO[AppError, Wait] = ZIO.fromEither(Wait.decode(r)).mapError(AppError.Internal(_))

  // ---- 定时唤醒 ----

  private val timeFormat = DateTimeFormatter.ofPattern("MM-dd HH:mm:ss")

  /** 约一次唤醒（一个会话至多一个，再约就替换）；`at` 为 None 时取消。返回说明。 */
  def wake(session: SessionId, at: Option[Long], note: String): IO[AppError, String] =
    val key = WaitKey.wake(session)
    for
      t   <- now
      old <- db.read(Repo.waitByKey(key.kind, key.ref))
      _ <- db.write(
             for
               _ <- ZIO.foreachDiscard(old)(w => Repo.deleteWait(w.id) *> afterCommit(forget(List(w.id))))
               _ <- ZIO.foreachDiscard(at)(due => openTx(session, WaitRef.Wake(due), note, None, t))
               _ <- changed
             yield ()
           )
      _ <- live.publish(session, LiveEvent.Changed)
    yield at match
      case Some(due) =>
        val when = timeFormat.format(Instant.ofEpochMilli(due).atZone(zone))
        s"已约好 $when 唤醒${if old.isDefined then "（替换了之前约的）" else ""}。到时提醒会作为事件投到这里。"
      case None => if old.isDefined then "已取消唤醒。" else "本来就没有约唤醒。"

private[sessions] object Waits:
  val layer: URLayer[Kernel & Mailbox & Doorbell, Waits] =
    ZLayer(
      for
        k     <- ZIO.service[Kernel]
        m     <- ZIO.service[Mailbox]
        d     <- ZIO.service[Doorbell]
        cache <- Ref.make(Map.empty[WaitId, Wait])
      yield Waits(k, m, d, cache)
    )
