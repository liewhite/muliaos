package muliao.contract

import sttp.model.StatusCode
import sttp.model.headers.CookieValueWithMeta
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

final case class LoginRequest(password: String) derives JsonCodec, Schema

/** 当前登录状态。`expiresAt` 为 epoch 秒。 */
final case class AuthSession(expiresAt: Long) derives JsonCodec, Schema

object AuthApi:
  /** 登录：成功时设登录 cookie。 */
  val login: PublicEndpoint[LoginRequest, ApiError, CookieValueWithMeta, Any] =
    Api.base.post
      .in("auth" / "login")
      .in(jsonBody[LoginRequest])
      .out(setCookie(Api.AuthCookie))
      .out(statusCode(StatusCode.NoContent))
      .name("login")
      .description("单用户密码登录。连续输错按来源退避，退避期间返回 TooManyAttempts。")

  /** 退出登录：清掉本设备的登录 cookie。 */
  val logout: Endpoint[Option[String], Unit, ApiError, CookieValueWithMeta, Any] =
    Api.secure.post
      .in("auth" / "logout")
      .out(setCookie(Api.AuthCookie))
      .out(statusCode(StatusCode.NoContent))
      .name("logout")

  /** 查看当前是否已登录。 */
  val session: Endpoint[Option[String], Unit, ApiError, AuthSession, Any] =
    Api.secure.get
      .in("auth" / "session")
      .out(jsonBody[AuthSession])
      .name("authSession")

  val all = List(login, logout, session)
