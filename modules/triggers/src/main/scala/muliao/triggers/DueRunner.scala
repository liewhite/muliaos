package muliao.triggers

import muliao.platform.AppError
import muliao.triggers.store.Repo
import zio.*

/** 定时与一次性：一个循环扫描到期的，先提交「消耗这次到期」（定时推进到下一次、一次性结束）再触发。前置检查不能占着事务，
  * 所以进程死在两者之间时跳过这一次（至多一次）。下一次到期总是从「现在」往后算，所以停机期间错过的只补一次；停用的不扫，
  * 重新启用时从那一刻重算，所以停用期间的不补。触发在 `scope` 里并发跑，不挡扫描。
  */
private[triggers] final case class DueRunner(core: Core, scope: Scope):
  import core.*

  def run: UIO[Nothing] =
    (round.catchAllCause(c => ZIO.logErrorCause("触发器到期扫描出错", c).as(None)).flatMap(wait)).forever

  /** 一轮：触发到期的，返回之后最早的到期时间。 */
  private def round: IO[AppError, Option[Long]] =
    for
      t0   <- now
      due  <- active.map(_.filter(_.mode.nextAt.exists(_ <= t0)))
      _    <- ZIO.foreachDiscard(due)(t => consume(t, t0).flatMap(ZIO.foreachDiscard(_)(fireDue(_).forkIn(scope))))
      rest <- active
    yield rest.flatMap(_.mode.nextAt).minOption

  private def wait(next: Option[Long]): UIO[Unit] =
    next match
      case None => signal.dueWake
      case Some(at) =>
        now.flatMap(t => signal.dueWake.race(Clock.sleep((at - t).max(0).millis)))

  /** 在事务里消耗这次到期；行已被改动（停用、删除、到期时间变了）就不消耗，返回 None。 */
  private def consume(t: Trigger, at: Long): IO[AppError, Option[Trigger]] =
    db.write(
      Repo.get(t.id).flatMap(ZIO.foreach(_)(decode)).flatMap {
        case Some(cur) if cur.mode.nextAt == t.mode.nextAt =>
          ZIO.foreach(cur.mode.consumed(at, zone).map(m => cur.copy(mode = m)))(next => Repo.save(encode(next)) *> changed.as(next))
        case _ => ZIO.none
      }
    )

  /** 没做成：错过就没有下一次的（一次性）告诉用户，其余记日志。 */
  private def missed(t: Trigger, why: String): UIO[Unit] =
    if t.mode.reportsMiss then notDone(t, why) else ZIO.logWarning(s"触发器 ${t.name.value} 这次没触发：$why")

  private def fireDue(t: Trigger): UIO[Unit] =
    fire(t, Occasion.Due)
      .flatMap {
        case Fired.NotDone(why) => missed(t, why)
        case _                  => ZIO.unit
      }
      .catchAll(e => ZIO.logError(s"触发器 ${t.name.value} 触发失败：${e.message}") *> missed(t, s"触发出错（${e.message}）"))
