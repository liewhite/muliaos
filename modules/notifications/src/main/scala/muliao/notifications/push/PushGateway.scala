package muliao.notifications.push

import muliao.platform.http.OutboundHttp
import zio.*
import zio.http.{Body, Header, Headers, Request, URL}

import java.net.URI
import java.security.SecureRandom

/** 一台设备的推送订阅：推送服务给的地址（本身就是凭据，日志里只写主机名）与接收方密钥。 */
final case class Device(endpoint: URI, keys: DeviceKeys):
  def host: String = endpoint.getHost

object Device:
  def parse(endpoint: String, p256dh: String, auth: String): Either[String, Device] =
    for
      uri <- scala.util.Try(URI.create(endpoint)).toEither.left.map(_ => "endpoint 不是合法的地址")
      _   <- Either.cond(uri.getScheme == "https" && Option(uri.getHost).exists(_.nonEmpty), (), "endpoint 应是 https 地址")
      k   <- DeviceKeys.parse(p256dh, auth)
    yield Device(uri, k)

/** 推送服务对一次投递的回应（成功以外）。 */
enum PushFailure:
  /** 404/410：订阅已失效（RFC 8030），删掉它 */
  case Gone(status: Int)
  /** 没拿到回应，或 408/425/429/5xx：过一会儿再试 */
  case Transient(reason: String, retryAfter: Option[Duration] = None)
  /** 其余 4xx（VAPID 声明、载荷）多半是本端的问题：不删用户的订阅，也不重试 */
  case Rejected(reason: String)

object PushFailure:
  /** 按状态码归类；2xx 是成功（None）。 */
  def ofStatus(code: Int, retryAfter: Option[Duration], body: => String): Option[PushFailure] = code match
    case c if c >= 200 && c < 300                     => None
    case 404 | 410                                    => Some(Gone(code))
    case 408 | 425 | 429                              => Some(Transient(s"推送服务回 HTTP $code", retryAfter))
    case c if c >= 500 && c < 600                     => Some(Transient(s"推送服务回 HTTP $code", retryAfter))
    case _                                            => Some(Rejected(s"推送服务拒收，HTTP $code：${body.trim.take(300)}"))

/** 把一条已加密前的载荷推到一台设备。 */
trait PushGateway:
  def send(device: Device, payload: Array[Byte]): IO[PushFailure, Unit]

object PushGateway:
  /** 推送服务保留消息多久（设备离线时）。 */
  val TtlSeconds: Int = 24 * 3600

  /** 经出站 HTTP 推送：每次新的临时密钥与盐加密，带 VAPID 签名。 */
  def live(http: OutboundHttp, keys: VapidKeys, subject: String): PushGateway = new PushGateway:
    private val random = SecureRandom()

    def send(device: Device, payload: Array[Byte]): IO[PushFailure, Unit] =
      for
        now  <- Clock.instant
        body <- ZIO.succeed {
                  val salt = new Array[Byte](16)
                  random.nextBytes(salt)
                  Ece.encrypt(device.keys, P256.generate, salt, payload)
                }
        url <- ZIO.fromEither(URL.decode(device.endpoint.toString)).orElseFail(PushFailure.Rejected("endpoint 不是合法的地址"))
        req = Request
                .post(url, Body.fromArray(body))
                .addHeaders(
                  Headers(
                    Header.Custom("TTL", TtlSeconds.toString),
                    Header.Custom("Content-Encoding", "aes128gcm"),
                    Header.Custom("Content-Type", "application/octet-stream"),
                    Header.Custom("Authorization", keys.authorization(device.endpoint, subject, now))
                  )
                )
        _ <- ZIO.scoped(
               http
                 .exchange(req)
                 // 不带错误原文：它可能含 endpoint 的完整路径（即订阅凭据）
                 .mapError(_ => PushFailure.Transient(s"连不上推送服务 ${device.host}"))
                 .flatMap { resp =>
                   val retryAfter = resp.rawHeader("retry-after").flatMap(_.trim.toLongOption).map(_.seconds)
                   resp.body.asString.orElseSucceed("").flatMap { text =>
                     PushFailure.ofStatus(resp.status.code, retryAfter, text).fold(ZIO.unit)(ZIO.fail(_))
                   }
                 }
             )
      yield ()

