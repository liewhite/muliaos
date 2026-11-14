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

  /** 一轮：补上缺下次到期时间的（如迁移进来的），触发到期的，返回之后最早的到期时间。 */
  private def round: IO[AppError, Option[Long]] =
    for
      t0   <- now
      all  <- active
      _    <- ZIO.foreachDiscard(all.filter(_.nextAt.isEmpty))(t => ZIO.foreachDiscard(t.mode.firstDue(instant(t0), zone))(d => schedule(t, d.toEpochMilli)))
      due   = all.filter(_.nextAt.exists(_ <= t0))
      _    <- ZIO.foreachDiscard(due)(t => consume(t, t0).flatMap(ZIO.foreachDiscard(_)(fireDue(_).forkIn(scope))))
      rest <- active
    yield rest.flatMap(_.nextAt).minOption

  private def schedule(t: Trigger, at: Long): IO[AppError, Unit] =
    db.write(Repo.get(t.id).flatMap {
      case Some(r) if r.nextAt.isEmpty && r.finishedAt.isEmpty => Repo.save(r.copy(nextAt = Some(at))) *> changed
      case _                                                   => ZIO.unit
    })

  private def wait(next: Option[Long]): UIO[Unit] =
    next match
      case None => signal.dueWake
      case Some(at) =>
        now.flatMap(t => signal.dueWake.race(Clock.sleep((at - t).max(0).millis)))

  /** 在事务里消耗这次到期；行已被改动（停用、删除、到期时间变了）就不消耗，返回 None。 */
  private def consume(t: Trigger, at: Long): IO[AppError, Option[Trigger]] =
    db.write(
      Repo.get(t.id).flatMap {
        case Some(r) if r.enabled && r.finishedAt.isEmpty && r.nextAt == t.nextAt =>
          decode(r).flatMap { cur =>
            val next = cur.mode match
              case Mode.Once(_) => cur.copy(nextAt = None, finishedAt = Some(at))
              case m            => cur.copy(nextAt = m.nextAfter(instant(at), zone).map(_.toEpochMilli))
            Repo.save(encode(next)) *> changed.as(Some(next))
          }
        case _ => ZIO.none
      }
    )

  private def fireDue(t: Trigger): UIO[Unit] =
    fire(t, Occasion.Due)
      .flatMap {
        case Fired.NotDone(why) =>
          t.mode match
            case Mode.Once(_) => notDone(t, why)
            case _            => ZIO.logWarning(s"触发器 ${t.name.value} 这次没触发：$why")
        case _ => ZIO.unit
      }
      .catchAll { e =>
        ZIO.logError(s"触发器 ${t.name.value} 触发失败：${e.message}") *>
          (t.mode match
            case Mode.Once(_) => notDone(t, s"触发出错（${e.message}）")
            case _            => ZIO.unit)
      }
