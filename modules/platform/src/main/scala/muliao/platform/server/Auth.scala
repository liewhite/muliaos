package muliao.platform.server

import muliao.contract.{ApiError, ErrorKind}
import muliao.platform.AppError
import sttp.model.headers.{Cookie, CookieValueWithMeta}
import zio.*

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Instant
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** 已登录的请求：登录状态到期时间。 */
final case class Principal(expiresAt: Instant)

/** 单用户登录：校验密码（按来源退避）、签发与校验登录 cookie。
  *
  * cookie 内容是「到期时间 + 密码指纹」加 HMAC 签名：服务端不存登录状态；改密码后指纹对不上，所有设备的登录失效。
  */
trait Auth:
  /** 校验密码，成功返回要设的 cookie。`source` 是请求来源（IP）。 */
  def login(password: String, source: String): IO[ApiError, CookieValueWithMeta]

  def authenticate(cookie: Option[String]): IO[ApiError, Principal]

  /** 清除 cookie 用的值。 */
  def logoutCookie: CookieValueWithMeta

object Auth:
  val validity: Duration = 90.days

  def layer: ZLayer[ServerSettings, AppError, Auth] =
    ZLayer.fromZIO(
      for
        s <- ZIO.service[ServerSettings]
        _ <- ZIO.when(s.cookieSecret.length < 32)(ZIO.fail(AppError.invalid("[server] cookie_secret 至少 32 个字符")))
        _ <- ZIO.when(s.password.isEmpty)(ZIO.fail(AppError.invalid("[server] password 不能为空")))
        t <- LoginThrottle.make
      yield Live(s.password, s.cookieSecret, t)
    )

  private[server] final case class Live(password: String, secret: String, throttle: LoginThrottle) extends Auth:
    private val Version = "v1"

    private def hmac(data: String): String =
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(SecretKeySpec(secret.getBytes(UTF_8), "HmacSHA256"))
      mac.doFinal(data.getBytes(UTF_8)).map(b => f"${b & 0xff}%02x").mkString

    private val fingerprint: String = hmac(s"password:$password").take(16)

    private def same(a: String, b: String): Boolean = MessageDigest.isEqual(a.getBytes(UTF_8), b.getBytes(UTF_8))

    private[server] def sign(expires: Instant): String =
      val payload = s"$Version.${expires.getEpochSecond}.$fingerprint"
      s"$payload.${hmac(payload)}"

    private def cookie(value: String, maxAge: Long): CookieValueWithMeta =
      CookieValueWithMeta.unsafeApply(
        value,
        maxAge = Some(maxAge),
        path = Some("/"),
        httpOnly = true,
        sameSite = Some(Cookie.SameSite.Lax)
      )

    def login(attempt: String, source: String): IO[ApiError, CookieValueWithMeta] =
      throttle.check(source) *>
        (if same(attempt, password) then
           throttle.succeeded(source) *> Clock.instant.map(now => cookie(sign(now.plusSeconds(validity.toSeconds)), validity.toSeconds))
         else
           throttle.failed(source) *> ZIO.logWarning(s"登录密码错误，来源 $source") *>
             ZIO.fail(ApiError(ErrorKind.Unauthorized, "密码不对")))

    def authenticate(c: Option[String]): IO[ApiError, Principal] =
      val unauthorized = ApiError(ErrorKind.Unauthorized, "未登录或登录已过期")
      for
        value <- ZIO.fromOption(c).orElseFail(unauthorized)
        exp <- value.split('.') match
                 case Array(Version, exp, fp, sig) if same(sig, hmac(s"$Version.$exp.$fp")) && same(fp, fingerprint) =>
                   ZIO.fromOption(exp.toLongOption.map(Instant.ofEpochSecond)).orElseFail(unauthorized)
                 case _ => ZIO.fail(unauthorized)
        now <- Clock.instant
        _   <- ZIO.when(!exp.isAfter(now))(ZIO.fail(unauthorized))
      yield Principal(exp)

    def logoutCookie: CookieValueWithMeta = cookie("", 0)

/** 登录失败按来源退避：连续失败 3 次后，第 n 次失败要等 2^(n-3) 秒才能再试，最长 15 分钟；成功一次就清零。 */
final class LoginThrottle private (state: Ref[Map[String, LoginThrottle.Failures]]):
  import LoginThrottle.*

  def check(source: String): IO[ApiError, Unit] =
    Clock.instant.flatMap { now =>
      state.get.map(_.get(source).flatMap(_.lockedUntil).filter(_.isAfter(now))).flatMap {
        case Some(until) =>
          val wait = java.time.Duration.between(now, until).toSeconds.max(1)
          ZIO.fail(ApiError(ErrorKind.TooManyAttempts, s"密码错误次数太多，${wait} 秒后再试", Some(wait)))
        case None => ZIO.unit
      }
    }

  def failed(source: String): UIO[Unit] =
    Clock.instant.flatMap { now =>
      state.update { m =>
        val n    = m.get(source).fold(1)(_.count + 1)
        val lock = Option.when(n >= FreeAttempts)(now.plusSeconds(math.min(MaxLock, 1L << math.min(n - FreeAttempts, 20))))
        m.updated(source, Failures(n, lock))
      }
    }

  def succeeded(source: String): UIO[Unit] = state.update(_ - source)

object LoginThrottle:
  private val FreeAttempts = 3
  private val MaxLock      = 15L * 60
  final case class Failures(count: Int, lockedUntil: Option[Instant])
  def make: UIO[LoginThrottle] = Ref.make(Map.empty[String, Failures]).map(LoginThrottle(_))
