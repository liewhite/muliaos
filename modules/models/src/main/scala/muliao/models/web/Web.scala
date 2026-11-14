package muliao.models.web

import muliao.models.llm.JsonPath.*
import muliao.platform.AppError
import muliao.platform.config.ConfigSection
import muliao.platform.http.OutboundHttp
import zio.*
import zio.http.{Body, Header, Headers, Request, URL}
import zio.json.*
import zio.json.ast.Json

/** 配置文件 `[web]` 段（配了才有网页工具）：DeepSeek 的搜索与抓取服务。 */
@jsonMemberNames(SnakeCase)
final case class WebSettings(apiKey: String, baseUrl: String = "https://api.deepseek.com") derives JsonCodec:
  override def toString: String = s"WebSettings($baseUrl)"

object WebSettings:
  val section: ConfigSection[WebSettings] = ConfigSection("web")

/** 一条搜索结果。`published` 是 RFC 3339 时间，不知道时为 null（字段总在，模型照说明读）。 */
@jsonMemberNames(SnakeCase)
@jsonExplicitNull
final case class SearchHit(title: String, url: String, summary: String, siteName: Option[String], published: Option[String]) derives JsonCodec

/** 抓到的网页：标题（可能为空）与 Markdown 正文。 */
final case class Page(title: String, content: String)

/** 网页搜索与抓取。 */
trait Web:
  def search(query: String, limit: Int): IO[AppError, Chunk[SearchHit]]

  /** 先经抓取服务；服务说目标抓不到或服务暂时不可用时直连再抓一次（[[DirectFetch]]）。 */
  def fetch(url: String): IO[AppError, Page]

object Web:
  val layer: ZLayer[WebSettings & OutboundHttp, Nothing, Web] = ZLayer.fromFunction(DeepSeekWeb.apply)

/** DeepSeek 的搜索与抓取（https://api-docs.deepseek.com/zh-cn/guides/search）。
  *
  * 失败怎么分：连不上、408/429/5xx 是暂时性的（整次运行稍后重试）；带契约错误体（`{"error":{"message"}}`）的 400/413 是输入
  * 不对，交还模型改；其余（key 无效、余额不足、契约外的响应）是配置或账户问题，让这一轮失败可见。
  *
  * 抓取另有一类「目标抓不到」：带契约错误体的 403/404/408/504。抓取服务在国内，境外站点常被墙或超时，它拿不到不说明页面不存在，
  * 所以改为直连核实；服务本身暂时不可用时也直连一次。
  */
final case class DeepSeekWeb(settings: WebSettings, http: OutboundHttp) extends Web:
  private val Timeout        = 60.seconds
  private val transient      = Set(408, 429, 500, 502, 503, 504)
  private val rejectedInput  = Set(400, 413)
  private val targetMissing  = Set(403, 404, 408, 504)

  /** 一次请求没有成功。`status` 为空：没收到完整响应。`contract`：带着契约错误体。 */
  private final case class Failure(status: Option[Int], contract: Boolean, reason: String):
    def targetUnavailable: Boolean = contract && status.exists(targetMissing)
    def toAppError: AppError = status match
      case Some(s) if contract && rejectedInput(s) => AppError.invalid(reason)
      case None                                     => AppError.Unavailable(reason)
      case Some(s) if transient(s)                  => AppError.Unavailable(reason)
      case _                                        => AppError.External(reason)

  private def post[A: JsonDecoder](op: String, body: Json): IO[Failure, A] =
    val failed = (status: Option[Int], contract: Boolean, reason: String) => Failure(status, contract, s"deepseek $op 失败：$reason")
    val req = ZIO
      .fromEither(URL.decode(s"${settings.baseUrl.stripSuffix("/")}/v1/tools/$op"))
      .mapError(e => failed(Some(400), false, s"地址不对：${e.getMessage}"))
      .map(u =>
        Request.post(u, Body.fromString(body.toJson))
          .addHeaders(Headers(Header.Authorization.Bearer(settings.apiKey), Header.ContentType(zio.http.MediaType.application.json)))
      )
    val exchange: IO[Failure, (Int, String)] =
      req.flatMap { r =>
        ZIO.scoped(http.exchange(r).flatMap(resp => resp.body.asString.map(resp.status.code -> _).mapError(e => AppError.Unavailable(e.getMessage))))
          .mapError(e => failed(None, false, e.message))
      }.timeoutFail(failed(None, false, s"${Timeout.toSeconds} 秒内没有完成"))(Timeout)
    exchange.flatMap {
      case (200, text) => ZIO.fromEither(text.fromJson[A]).mapError(e => failed(Some(200), false, s"响应不合预期（$e）：${text.take(300)}"))
      case (code, text) =>
        val message = text.fromJson[Json].toOption.flatMap(_.at("error").at("message").str)
        ZIO.fail(failed(Some(code), message.isDefined, s"HTTP $code：${message.getOrElse(s"契约外的响应：${text.take(300)}")}"))
    }.tapError(f => ZIO.logWarning(s"网页服务请求失败：${f.reason}"))

  private final case class RawHit(title: String, url: String, summary: String, site_name: Option[String], date: Option[String]) derives JsonDecoder
  private final case class SearchResponse(results: List[RawHit]) derives JsonDecoder
  private final case class FetchResponse(title: String, content: String) derives JsonDecoder

  def search(query: String, limit: Int): IO[AppError, Chunk[SearchHit]] =
    post[SearchResponse]("search", obj("query" -> s(query), "count" -> n(limit.toLong)))
      .mapBoth(_.toAppError, r => Chunk.fromIterable(r.results.map(h => SearchHit(h.title, h.url, h.summary, h.site_name, h.date))))

  def fetch(url: String): IO[AppError, Page] =
    post[FetchResponse]("fetch", obj("url" -> s(url))).foldZIO(
      f =>
        val e = f.toAppError
        if f.targetUnavailable || e.isTransient then DirectFetch.fetch(http, url, f.reason) else ZIO.fail(e)
      ,
      p => ZIO.succeed(Page(p.title, p.content))
    )
