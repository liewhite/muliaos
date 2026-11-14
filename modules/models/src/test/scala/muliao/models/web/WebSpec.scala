package muliao.models.web

import muliao.models.speech.{AudioFormat, Transcriber, TranscriptionSettings}
import muliao.platform.AppError
import muliao.platform.http.{HttpSettings, OutboundHttp, ProxySettings, SystemProxy}
import muliao.toolkit.{ToolFailure, ToolOutcome, ToolContent}
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

/** 网页工具与语音转写：对着本地起的假服务测请求格式、失败分类与直连兜底。 */
object WebSpec extends ZIOSpecDefault:

  val http: ZLayer[Any, Throwable, OutboundHttp] =
    (ZLayer.succeed(HttpSettings()) ++ SystemProxy.fixed(ProxySettings.direct)) >>> OutboundHttp.layer

  /** 收到的请求（路径、头、体）。 */
  final case class Seen(path: String, auth: Option[String], body: String)

  def json(status: Status, body: String): Response =
    Response(status = status, body = Body.fromString(body)).addHeader(Header.ContentType(MediaType.application.json))

  def html(body: String, contentType: String = "text/html; charset=utf-8"): Response =
    Response(body = Body.fromString(body)).addHeader(Header.Custom("content-type", contentType))

  val article =
    """<html><head><title>标题页</title></head><body><nav>导航 导航 导航</nav><article><h1>正文标题</h1>
      |<p>这是一段足够长的正文，用来让正文提取认出它是文章的主体内容。这是一段足够长的正文，用来让正文提取认出它是文章的主体内容。</p>
      |<p>第二段也写得长一些，同样是为了让提取算法有足够的文字可以判断。<a href="/docs/next">下一页</a></p></article>
      |<footer>页脚</footer></body></html>""".stripMargin

  /** 假的 DeepSeek 服务 + 目标站点。fetch 的 url 决定服务怎么答。 */
  def routes(seen: Ref[Chunk[Seen]]) =
    def record(r: Request): UIO[String] =
      r.body.asString.orDie.tap(b => seen.update(_ :+ Seen(r.url.path.encode, r.rawHeader("authorization"), b)))
    def urlOf(body: String) = body.fromJson[Json].toOption.flatMap(_.asObject).flatMap(_.get("url")).collect { case Json.Str(s) => s }.getOrElse("")
    Routes(
      Method.POST / "v1" / "tools" / "search" -> handler { (r: Request) =>
        record(r).map(b =>
          if b.contains("busy") then json(Status.ServiceUnavailable, "upstream down")
          else json(Status.Ok, """{"results":[{"title":"t","url":"https://x","summary":"s","site_name":"x.com"}]}""")
        )
      },
      Method.POST / "v1" / "tools" / "fetch" -> handler { (r: Request) =>
        record(r).map { b =>
          val target = urlOf(b)
          if target.endsWith("/ok") then json(Status.Ok, """{"title":"T","content":"服务拿到的正文"}""")
          else if target.endsWith("/badkey") then json(Status.Unauthorized, """{"error":{"message":"invalid key"}}""")
          else if target.endsWith("/badurl") then json(Status.BadRequest, """{"error":{"message":"invalid url"}}""")
          else json(Status.NotFound, """{"error":{"message":"cannot fetch"}}""")
        }
      },
      Method.GET / "site" / "article"  -> handler(html(article)),
      Method.GET / "site" / "moved"    -> handler(Response.status(Status.Found).addHeader(Header.Custom("location", "/site/plain"))),
      Method.GET / "site" / "plain"    -> handler(html("纯文本内容", "text/plain; charset=utf-8")),
      Method.GET / "site" / "gbk"      -> handler(Response(body = Body.fromArray("中文编码".getBytes("GBK"))).addHeader(Header.Custom("content-type", "text/plain; charset=GBK"))),
      Method.GET / "site" / "pdf"      -> handler(html("%PDF", "application/pdf")),
      Method.GET / "site" / "missing"  -> handler(Response.status(Status.NotFound)),
      Method.POST / "services" / "aigc" / "multimodal-generation" / "generation" -> handler { (r: Request) =>
        record(r).map(_ => json(Status.Ok, """{"output":{"text":"  你好 world  "},"usage":{}}"""))
      }
    )

  def withServer[A](f: (String, Ref[Chunk[Seen]], OutboundHttp) => ZIO[Any, Any, A]): ZIO[Scope, Any, A] =
    for
      seen <- Ref.make(Chunk.empty[Seen])
      env  <- Server.defaultWith(_.port(0)).build
      port <- Server.install(routes(seen)).provideEnvironment(env)
      out  <- http.build.map(_.get[OutboundHttp])
      a    <- f(s"http://127.0.0.1:$port", seen, out)
    yield a

  def text(o: ToolOutcome): String = o match
    case ToolOutcome.Completed(cs) => cs.collect { case ToolContent.Text(t) => t }.mkString
    case other                     => other.toString

  def spec = suite("网页与语音")(
    test("搜索：带鉴权、条数默认 10，结果是 JSON 数组；服务 5xx 是暂时性的") {
      ZIO.scoped(withServer { (base, seen, out) =>
        val web = DeepSeekWeb(WebSettings("k-1", base), out)
        for
          hits <- WebTools.search(web).invoke(Json.Obj("query" -> Json.Str("q")), ())
          busy <- WebTools.search(web).invoke(Json.Obj("query" -> Json.Str("busy")), ()).flip
          sent <- seen.get
        yield assertTrue(
          sent.head.auth.contains("Bearer k-1"),
          sent.head.body.contains("\"count\":10"),
          text(hits).fromJson[Json].toOption.exists(_.asArray.exists(_.size == 1)),
          text(hits).contains("\"published\""),
          busy.isInstanceOf[ToolFailure.Unavailable]
        )
      })
    },
    test("抓取：服务拿到直接用；目标抓不到就直连，正文转 Markdown、相对链接补全、注明直连") {
      ZIO.scoped(withServer { (base, _, out) =>
        val web = DeepSeekWeb(WebSettings("k", base), out)
        for
          ok     <- WebTools.fetch(web).invoke(Json.Obj("url" -> Json.Str(s"$base/ok")), ())
          direct <- web.fetch(s"$base/site/article")
        yield assertTrue(
          text(ok) == "# T\n\n服务拿到的正文",
          direct.content.contains("直连"),
          direct.content.contains("第二段也写得长一些"),
          direct.content.contains(s"$base/docs/next"),
          !direct.content.contains("页脚")
        )
      })
    },
    test("直连：跟随跳转、按声明的编码解码、读不了的类型和真不存在都把两边原因交还模型") {
      ZIO.scoped(withServer { (base, _, out) =>
        val web = DeepSeekWeb(WebSettings("k", base), out)
        for
          moved   <- web.fetch(s"$base/site/moved")
          gbk     <- web.fetch(s"$base/site/gbk")
          pdf     <- web.fetch(s"$base/site/pdf").flip
          missing <- web.fetch(s"$base/site/missing").flip
        yield assertTrue(
          moved.content.endsWith("纯文本内容"),
          moved.content.contains(s"$base/site/plain"),
          gbk.content.endsWith("中文编码"),
          pdf match
            case AppError.Domain(_, m) => m.contains("cannot fetch") && m.contains("application/pdf")
            case _                     => false,
          missing match
            case AppError.Domain(_, m) => m.contains("HTTP 404")
            case _                     => false
        )
      })
    },
    test("抓取：key 不对是配置问题（不直连、让人看见），地址不对交还模型") {
      ZIO.scoped(withServer { (base, _, out) =>
        val web = DeepSeekWeb(WebSettings("k", base), out)
        for
          badKey <- web.fetch(s"$base/badkey").flip
          badUrl <- web.fetch(s"$base/badurl").flip
        yield assertTrue(badKey.isInstanceOf[AppError.External], badUrl.isInstanceOf[AppError.Domain])
      })
    },
    test("语音：请求带格式与 data URI、结果去首尾空白；不支持的类型直接拒绝") {
      ZIO.scoped(withServer { (base, seen, out) =>
        val t = Transcriber.Dashscope(TranscriptionSettings("k", "asr-model", base, languageHints = List("zh", "en")), out)
        for
          text <- t.transcribe(Chunk.fromArray("audio".getBytes), "audio/webm;codecs=opus")
          bad  <- t.transcribe(Chunk.fromArray("audio".getBytes), "video/webm").flip
          sent <- seen.get
          body  = sent.last.body
        yield assertTrue(
          text == "你好 world",
          body.contains("\"format\":\"webm\""),
          body.contains("data:audio/webm;base64,"),
          body.contains("\"language_hints\":[\"zh\",\"en\"]"),
          body.contains("\"model\":\"asr-model\""),
          sent.last.auth.contains("Bearer k"),
          bad.isInstanceOf[AppError.Domain],
          AudioFormat.of("Audio/MP4").contains(AudioFormat.Mp4)
        )
      })
    }
  ) @@ withLiveClock @@ timeout(60.seconds)
