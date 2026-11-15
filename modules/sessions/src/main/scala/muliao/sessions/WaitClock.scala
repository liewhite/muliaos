package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import zio.*

/** 等待的调度：每秒看一遍进行中的等待，唤醒到点的投提醒，配了心跳的到点投一条检查。 */
private[sessions] final class WaitClock(kernel: Kernel, waits: Waits, delegations: Delegations, mailbox: Mailbox, beats: Ref[Map[WaitId, Long]], scope: Scope):
  import kernel.*

  /** 调度多久看一次。 */
  private val Tick: Duration = 1.second

  /** 载入全部等待，起调度。 */
  def start: IO[AppError, Unit] = waits.load *> tickLoop.forkIn(scope).unit

  private def tickLoop: UIO[Unit] = (tick.catchAllCause(c => ZIO.logErrorCause("等待调度出错", c)) *> ZIO.sleep(Tick)).forever

  private def tick: IO[AppError, Unit] =
    for
      t  <- now
      ws <- waits.cached
      bs <- beats.updateAndGet(m => ws.map(w => w.id -> m.getOrElse(w.id, t)).toMap) // 新登记的从现在开始计
      _ <- ZIO.foreachDiscard(ws) { w =>
             w.ref match
               case WaitRef.Wake(due) if due <= t =>
                 guarded(w)(waits.close(WaitKey.of(w.session, w.ref), Some(Letter(Source.Event(EventKind.Wake), s"[唤醒] ${w.label}"))).unit)
               case WaitRef.Wake(_) => ZIO.unit
               case _ =>
                 ZIO.foreachDiscard(w.heartbeat.filter(h => t - bs.getOrElse(w.id, t) >= h * 1000L))(_ =>
                   beats.update(_.updated(w.id, t)) *> guarded(w)(checkIn(w))
                 )
           }
    yield ()

  /** 一项等待投不进（会话没了、agent 停用了）：记错误，这次运行期间不再调度它（库里的记录留着，重启后再试），免得每秒刷日志。 */
  private def guarded(w: Wait)(effect: IO[AppError, Unit]): UIO[Unit] =
    effect.catchAll(e =>
      ZIO.logError(s"等待 ${w.id.value}（${w.label}，会话 ${w.session.value}）投递失败，暂停调度到重启：${e.message}") *> waits.forget(List(w.id))
    )

  /** 心跳检查：投一条「还在进行」的检查（不推送）；已有检查在排队就不重复投。 */
  private def checkIn(w: Wait): IO[AppError, Unit] =
    for
      status <- statusOf(w.ref)
      text = s"[检查] ${w.label}：$status\n还在进行。没有要做的就只回复「…」；不再需要它就停掉（委派用 stop_thread，后台任务用 job_kill）。"
      _ <- mailbox.deliver(Address.To(w.session), Letter(Source.Event(EventKind.CheckIn), text, checkIn = true), Some(s"check-in:${w.id.value}"))
    yield ()

  private def statusOf(ref: WaitRef): UIO[String] = ref match
    case WaitRef.Job(job)       => ports.jobs.flatMap(_.status(job))
    case WaitRef.Delegation(d)  => delegations.status(d).catchAll(e => ZIO.succeed(s"（查不到：${e.message}）"))
    case WaitRef.Wake(_)        => ZIO.succeed("等到点唤醒")

private[sessions] object WaitClock:
  val layer: ZLayer[Kernel & Waits & Delegations & Mailbox, Nothing, WaitClock] =
    ZLayer.scoped(
      for
        k     <- ZIO.service[Kernel]
        w     <- ZIO.service[Waits]
        d     <- ZIO.service[Delegations]
        m     <- ZIO.service[Mailbox]
        beats <- Ref.make(Map.empty[WaitId, Long])
        scope <- ZIO.scope
      yield WaitClock(k, w, d, m, beats, scope)
    )
