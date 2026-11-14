package muliao.platform

import muliao.contract.{ApiError, ErrorKind}

/** 全系统统一的错误模型。各领域把自己的失败归到这四类之一，上层（接口、工具、运行）只按类别处理：
  *   - [[AppError.Domain]]：领域规则拒绝（参数不对、状态不允许、找不到、冲突），调用方改了再来；
  *   - [[AppError.Unavailable]]：外部暂时不可用（断网、超时、限流、5xx），退避重试；
  *   - [[AppError.External]]：外部给了不可重试的错误（4xx、协议不符）；
  *   - [[AppError.Internal]]：系统自身的错误（bug、数据库坏了），让人看见。
  */
sealed abstract class AppError extends Exception(null, null, false, false) with Product:
  def message: String
  override def getMessage: String = message

  /** 暂时性的：值得退避重试。 */
  final def isTransient: Boolean = this match
    case _: AppError.Unavailable => true
    case _                       => false

object AppError:
  /** 领域拒绝的原因，决定接口状态码。 */
  enum Reason:
    case Invalid, NotFound, Conflict, Forbidden

  final case class Domain(reason: Reason, message: String) extends AppError

  /** @param retryAfter 外部给出的建议等待时间 */
  final case class Unavailable(message: String, cause: Option[Throwable] = None, retryAfter: Option[zio.Duration] = None)
      extends AppError

  final case class External(message: String) extends AppError

  final case class Internal(message: String, cause: Option[Throwable] = None) extends AppError

  def invalid(message: String): AppError   = Domain(Reason.Invalid, message)
  def notFound(message: String): AppError  = Domain(Reason.NotFound, message)
  def conflict(message: String): AppError  = Domain(Reason.Conflict, message)
  def forbidden(message: String): AppError = Domain(Reason.Forbidden, message)
  def internal(message: String, cause: Throwable): AppError = Internal(message, Some(cause))

  /** 接口返回给前端的错误体。内部错误不带细节（细节进日志）。 */
  def toApi(e: AppError): ApiError = e match
    case Domain(r, m) =>
      val kind = r match
        case Reason.Invalid   => ErrorKind.Invalid
        case Reason.NotFound  => ErrorKind.NotFound
        case Reason.Conflict  => ErrorKind.Conflict
        case Reason.Forbidden => ErrorKind.Forbidden
      ApiError(kind, m)
    case Unavailable(m, _, after) => ApiError(ErrorKind.Unavailable, m, after.map(_.toSeconds))
    case External(m)              => ApiError(ErrorKind.ExternalFailure, m)
    case Internal(_, _)           => ApiError(ErrorKind.Internal, "内部错误，详情见服务日志")
