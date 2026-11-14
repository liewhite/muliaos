package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import sttp.tapir.generic.auto.*
import zio.json.*

/** 接口错误的种类：决定 HTTP 状态码，前端据此决定怎么提示。 */
enum ErrorKind(val status: StatusCode):
  /** 请求不合法（参数、状态不允许）。 */
  case Invalid extends ErrorKind(StatusCode.BadRequest)
  case Unauthorized extends ErrorKind(StatusCode.Unauthorized)
  case Forbidden extends ErrorKind(StatusCode.Forbidden)
  case NotFound extends ErrorKind(StatusCode.NotFound)
  /** 与当前状态冲突（并发修改、对象忙）。 */
  case Conflict extends ErrorKind(StatusCode.Conflict)
  case TooManyAttempts extends ErrorKind(StatusCode.TooManyRequests)
  /** 依赖的外部服务给了不可重试的错误。 */
  case ExternalFailure extends ErrorKind(StatusCode.BadGateway)
  /** 依赖的外部服务暂时不可用，稍后再试。 */
  case Unavailable extends ErrorKind(StatusCode.ServiceUnavailable)
  case Internal extends ErrorKind(StatusCode.InternalServerError)

object ErrorKind:
  given JsonCodec[ErrorKind] =
    JsonCodec.string.transformOrFail(
      s => values.find(_.toString == s).toRight(s"未知错误种类 $s"),
      _.toString
    )
  given Schema[ErrorKind] = Schema.derivedEnumeration[ErrorKind].defaultStringBased

/** 所有接口统一的错误体。 */
final case class ApiError(kind: ErrorKind, message: String, retryAfterSeconds: Option[Long] = None)
    derives JsonCodec, Schema

/** 实时刷新的一条「数据变了」：前端收到后重新拉取该主题的数据。 */
object Topic extends StringId
type Topic = Topic.Type

final case class Change(topic: Topic) derives JsonCodec, Schema

object Api:
  /** 登录 cookie 的名字。 */
  final val AuthCookie = "muliao_auth"

  val errorOut: EndpointOutput[ApiError] =
    val variants = ErrorKind.values.toList.map { k =>
      oneOfVariantValueMatcher(k.status, jsonBody[ApiError]) { case e: ApiError => e.kind == k }
    }
    oneOf[ApiError](variants.head, variants.tail*)

  /** 所有接口的根：`/api`，统一错误体。 */
  val base: PublicEndpoint[Unit, ApiError, Unit, Any] = endpoint.in("api").errorOut(errorOut)

  /** 需要登录的接口：凭登录 cookie。 */
  val secure: Endpoint[Option[String], Unit, ApiError, Unit, Any] =
    base.securityIn(auth.apiKey(cookie[Option[String]](AuthCookie)))
