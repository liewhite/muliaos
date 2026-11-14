package muliao.models.web

import com.vladsch.flexmark.html2md.converter.FlexmarkHtmlConverter
import muliao.platform.AppError
import muliao.platform.http.OutboundHttp
import net.dankito.readability4j.extended.Readability4JExtended
import org.jsoup.Jsoup
import zio.*
import zio.http.{Header, Request, URL}

import java.nio.charset.Charset
import scala.util.Try

/** 直连抓取：抓取服务拿不到或不可用时的备用。HTML 提取正文转成 Markdown（相对链接按最终地址补全），纯文本（源码、Markdown、
  * JSON……）原样返回，都在开头注明是直连。
  *
  * 拿不到时把抓取服务和直连两边的原因一起交还模型（领域错误，不算暂时性失败）：直连连不上的多半是目标站点本身；本机整体掉线时
  * 两边都报连不上，模型看得出来，由它决定稍后重试、换地址还是改用浏览器。
  */
object DirectFetch:
  /** 从连接到读完的总时限 */
  val Timeout: Duration = 20.seconds
  /** 读到这么多就停：只防下载大文件，正文最终还会按工具输出上限截断 */
  val MaxBytes: Int     = 2 * 1024 * 1024
  val MaxRedirects      = 5

  private val htmlTypes = Set("text/html", "application/xhtml+xml")
  private val textTypes = Set("application/json", "application/xml", "application/javascript", "application/yaml", "application/x-yaml", "application/toml")

  def readable(media: String): Boolean =
    htmlTypes(media) || media.startsWith("text/") || textTypes(media) || media.endsWith("+json") || media.endsWith("+xml")

  /** 响应头里的 charset：认识的才用，写错的当作没声明（UTF-8）。 */
  def charsetOf(contentType: String): Charset =
    contentType
      .split(';')
      .drop(1)
      .map(_.trim)
      .collectFirst { case p if p.toLowerCase.startsWith("charset=") => p.drop(8).trim.stripPrefix("\"").stripSuffix("\"") }
      .flatMap(n => Try(Charset.forName(n)).toOption)
      .getOrElse(Charset.forName("UTF-8"))

  private final case class Fetched(url: String, contentType: String, media: String, body: Array[Byte])

  def fetch(http: OutboundHttp, url: String, serviceReason: String): IO[AppError, Page] =
    get(http, url, MaxRedirects)
      .timeoutFail(s"直连 GET 在 ${Timeout.toSeconds} 秒内没有完成")(Timeout)
      .flatMap(f => toPage(f).map(f -> _))
      .mapBoth(
        why => AppError.invalid(s"$serviceReason。抓取服务没有拿到这个页面，这本身不说明页面不存在；$why"),
        (f, page) =>
          val note = s"（抓取服务没有拿到，以下是直连 ${f.url} 得到的内容，content-type ${f.contentType}。）"
          Page(page.title, s"$note\n\n${page.content}")
      )
      .tapBoth(
        e => ZIO.logWarning(s"直连抓取失败：${e.message.take(300)}"),
        p => ZIO.logInfo(s"直连抓取 ${URL.decode(url).toOption.flatMap(_.host).getOrElse(url)}：${p.content.length} 字符")
      )

  private def get(http: OutboundHttp, url: String, redirectsLeft: Int): IO[String, Fetched] =
    ZIO.fromEither(URL.decode(url)).mapError(_ => s"地址不对：$url").flatMap { u =>
      /** 一跳：要么跟到下一个地址（Left），要么拿到了（Right）。 */
      val hop: ZIO[Scope, String, Either[String, Fetched]] =
        http.exchange(Request.get(u)).mapError(e => s"直连 GET 失败：${e.message}").flatMap { resp =>
          val code = resp.status.code
          if code >= 300 && code < 400 then
            resp.rawHeader(Header.Location.name) match
              case Some(loc) if redirectsLeft > 0 =>
                ZIO.attempt(java.net.URI(u.encode).resolve(loc).toString).mapBoth(_ => s"跳转地址不对：$loc", Left(_))
              case Some(_)                         => ZIO.fail(s"直连 GET 重定向超过 $MaxRedirects 次")
              case None                            => ZIO.fail(s"直连 GET 返回 HTTP $code 却没有给出跳转地址")
          else if code != 200 then ZIO.fail(s"直连 GET 返回 HTTP $code")
          else
            val contentType = resp.rawHeader(Header.ContentType.name).getOrElse("")
            val media       = contentType.split(';').head.trim.toLowerCase
            if !readable(media) then
              ZIO.fail(s"直连 GET 返回的 content-type 是 ${if contentType.isEmpty then "（没有）" else contentType}，web_fetch 读不了：可以用 run_shell 加 curl")
            else
              resp.body.asStream.take(MaxBytes.toLong).runCollect
                .mapBoth(e => s"直连 GET 读响应失败：${e.getMessage}", bytes => Right(Fetched(u.encode, contentType, media, bytes.toArray)))
        }
      ZIO.scoped(hop).flatMap {
        case Left(next) => get(http, next, redirectsLeft - 1)
        case Right(f)   => ZIO.succeed(f)
      }
    }

  /** HTML 用 readability 提取正文转 Markdown；提取不出正文（多半是 JS 渲染的页面）就报错。 */
  private def toPage(f: Fetched): IO[String, Page] =
    val text = String(f.body, charsetOf(f.contentType))
    if !htmlTypes(f.media) then ZIO.succeed(Page("", text))
    else
      ZIO.attemptBlocking {
        // 先自己把相对链接补成绝对地址：readability4j 补全时会丢掉端口
        val doc = Jsoup.parse(text, f.url)
        doc.select("a[href]").forEach(a => a.attr("href", a.absUrl("href")): Unit)
        doc.select("img[src]").forEach(i => i.attr("src", i.absUrl("src")): Unit)
        val article  = Readability4JExtended(f.url, doc.outerHtml).parse()
        val markdown = FlexmarkHtmlConverter.builder().build().convert(Option(article.getContent).getOrElse("")).trim
        Page(Option(article.getTitle).getOrElse(""), markdown)
      }.mapError(e => s"解析 HTML 失败：${e.getMessage}")
        .filterOrFail(_.content.nonEmpty)("直连 GET 返回的 HTML 里提取不出正文（页面多半靠 JavaScript 渲染）：改用浏览器打开")
