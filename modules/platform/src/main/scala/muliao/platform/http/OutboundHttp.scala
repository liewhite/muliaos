package muliao.platform.http

import muliao.platform.AppError
import muliao.platform.config.ConfigSection
import zio.*
import zio.http.*
import zio.json.*

/** 配置文件 `[http]` 段：出站 HTTP 的公共设置。 */
@jsonMemberNames(SnakeCase)
final case class HttpSettings(
    userAgent: String = HttpSettings.defaultUserAgent,
    /** 建立连接的超时 */
    connectTimeoutSeconds: Int = 15,
    /** [[OutboundHttp.send]] 整个请求（含读完响应体）的超时 */
    requestTimeoutSeconds: Int = 300
) derives JsonCodec

object HttpSettings:
  val defaultUserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) muliao/0.1"
  val section: ConfigSection[HttpSettings] = ConfigSection("http")

/** 出站 HTTP：走系统代理（含例外列表）、统一 User-Agent，并把失败归类为暂时 / 永久。
  *
  * 连接层面的失败（断网、DNS、连接被重置、超时）一律算暂时不可用；HTTP 状态码按 [[OutboundHttp.classify]] 归类。
  */
trait OutboundHttp:
  /** 发请求、读完响应体。只有 2xx 算成功。 */
  def send(req: Request): IO[AppError, Response]

  /** 发请求，响应体按流读（模型流式输出等）。只有 2xx 算成功；响应体在 scope 结束时释放。 */
  def open(req: Request): ZIO[Scope, AppError, Response]

object OutboundHttp:

  def send(req: Request): ZIO[OutboundHttp, AppError, Response] = ZIO.serviceWithZIO(_.send(req))

  val layer: ZLayer[HttpSettings & SystemProxy, Throwable, OutboundHttp] =
    ZLayer.makeSome[HttpSettings & SystemProxy, OutboundHttp](
      ZLayer.fromFunction((s: HttpSettings) => ZClient.Config.default.connectionTimeout(s.connectTimeoutSeconds.seconds)),
      Client.live,
      ZLayer.succeed(zio.http.netty.NettyConfig.default),
      DnsResolver.default,
      ZLayer.fromFunction(Live.apply)
    )

  /** 把非 2xx 响应归类：408/425/429/5xx（501、505 除外）为暂时不可用（带 Retry-After），其余为外部永久错误。 */
  def classify(resp: Response): IO[AppError, Response] =
    val code = resp.status.code
    if resp.status.isSuccess then ZIO.succeed(resp)
    else
      resp.body.asString
        .map(_.take(500))
        .orElseSucceed("")
        .map { body =>
          val msg = s"HTTP $code${if body.isEmpty then "" else s"：$body"}"
          if Set(408, 425, 429).contains(code) || (code >= 500 && code != 501 && code != 505) then
            AppError.Unavailable(msg, None, retryAfter(resp))
          else AppError.External(msg)
        }
        .flip

  private def retryAfter(resp: Response): Option[Duration] =
    resp.rawHeader("retry-after").flatMap(_.trim.toLongOption).map(_.seconds)

  private final case class Live(settings: HttpSettings, proxy: SystemProxy, client: Client) extends OutboundHttp:

    private def prepare(req: Request): UIO[(Client, Request)] =
      proxy.current.map { p =>
        val withUa =
          if req.headers.contains(Header.UserAgent.name) then req
          else req.addHeader(Header.UserAgent.name, settings.userAgent)
        val host   = req.url.host.getOrElse("")
        val scheme = req.url.scheme.map(_.encode).getOrElse("http")
        val c = p.forUrl(scheme, host) match
          case Some(ProxyServer(h, port)) =>
            URL.decode(s"http://$h:$port").toOption.fold(client)(u => client.proxy(Proxy(u)))
          case None => client
        (c, withUa)
      }

    private def transport(req: Request)(e: Throwable): AppError =
      AppError.Unavailable(s"连不上 ${req.url.host.getOrElse("?")}：${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}", Some(e))

    def open(req: Request): ZIO[Scope, AppError, Response] =
      prepare(req).flatMap { (c, r) =>
        c(r).mapError(transport(r)).flatMap(classify)
      }

    def send(req: Request): IO[AppError, Response] =
      ZIO
        .scoped(open(req).flatMap(resp => resp.body.asChunk.mapBoth(transport(req), c => resp.copy(body = Body.fromChunk(c)))))
        .timeoutFail(AppError.Unavailable(s"请求 ${req.url.host.getOrElse("?")} 超时"))(settings.requestTimeoutSeconds.seconds)
