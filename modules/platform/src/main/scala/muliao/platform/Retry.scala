package muliao.platform

import zio.*

/** 退避重试。只重试 [[AppError.Unavailable]]（暂时性），其余错误立即交给调用方。 */
object Retry:

  /** @param first    第一次等待
    * @param maxDelay 单次等待上限
    * @param giveUp   从第一次失败起累计多久仍失败就放弃
    */
  final case class Policy(first: Duration, maxDelay: Duration, giveUp: Duration)

  /** 默认：2 秒起翻倍，单次最长 5 分钟，累计约 30 分钟仍失败算最终失败。 */
  val transient: Policy = Policy(2.seconds, 5.minutes, 30.minutes)

  /** 外部给了建议等待时间（如 Retry-After）就至少等那么久。 */
  def schedule(p: Policy): Schedule[Any, AppError, Any] =
    (((Schedule.exponential(p.first) || Schedule.spaced(p.maxDelay)) && Schedule.upTo(p.giveUp)).unit &&
      Schedule.identity[AppError])
      .modifyDelay { (e, d) =>
        e match
          case AppError.Unavailable(_, _, Some(after)) => after.min(p.maxDelay).max(d)
          case _                                       => d
      }
      .whileInput[AppError](_.isTransient)

  extension [R, A](zio: ZIO[R, AppError, A])
    /** 暂时性失败按策略重试；每次重试记一条日志。 */
    def retryTransient(p: Policy = transient, what: => String = ""): ZIO[R, AppError, A] =
      zio
        .tapError(e => ZIO.when(e.isTransient)(ZIO.logWarning(s"$what 暂时失败，稍后重试：${e.message}")))
        .retry(schedule(p))
