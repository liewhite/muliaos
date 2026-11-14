package muliao.sessions

import muliao.contract.*
import muliao.platform.events.ChangeBus
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import muliao.resources.agents.Agents
import muliao.sessions.TestEnv.*
import zio.*
import zio.json.*
import zio.test.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

/** HTTP 接口：经真的 HTTP 服务调用。 */
object SessionsHttpSpec extends ZIOSpecDefault:

  final case class Resp(status: Int, body: String, headers: Map[String, String])

  final class Client(base: String):
    private val http             = HttpClient.newHttpClient()
    @volatile private var cookie = ""
    def call(method: String, path: String, body: Option[String] = None, contentType: String = "application/json"): Task[Resp] =
      bytes(method, path, body.map(_.getBytes(StandardCharsets.UTF_8)), contentType)
    def bytes(method: String, path: String, body: Option[Array[Byte]], contentType: String): Task[Resp] = ZIO.attemptBlocking {
      val b = HttpRequest.newBuilder(URI.create(base + path)).header("Content-Type", contentType)
      if cookie.nonEmpty then b.header("Cookie", cookie): Unit
      val r    = body.fold(b.method(method, HttpRequest.BodyPublishers.noBody()))(s => b.method(method, HttpRequest.BodyPublishers.ofByteArray(s)))
      val resp = http.send(r.build(), HttpResponse.BodyHandlers.ofString())
      resp.headers.firstValue("set-cookie").ifPresent(c => cookie = c.takeWhile(_ != ';'))
      Resp(resp.statusCode, resp.body, resp.headers.map.asScala.view.mapValues(_.asScala.mkString(",")).toMap)
    }

    /** 读 SSE 的第一条业务事件的 data（跳过连接帧 ready、ping）。 */
    def firstEvent(path: String): Task[String] = ZIO.attemptBlocking {
      val req   = HttpRequest.newBuilder(URI.create(base + path)).header("Cookie", cookie).GET().build()
      val lines = http.send(req, HttpResponse.BodyHandlers.ofLines()).body()
      try lines.iterator().asScala.find(l => l.startsWith("data:") && !l.contains("intervalSeconds")).map(_.stripPrefix("data:").trim).getOrElse("")
      finally lines.close()
    }

  private def server: ZIO[Scope & Sessions & Files & Agents & SessionHooks & ChangeBus, Throwable, Client] =
    for
      http     <- SessionsHttp.layer.build.map(_.get[SessionsHttp])
      bus      <- ZIO.service[ChangeBus]
      settings  = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
      auth     <- Auth.layer.build.map(_.get[Auth]).provideSomeLayer[Scope](ZLayer.succeed(settings))
      port     <- HttpServer.serve(settings, auth, bus, http.endpoints(auth), "test")
      client    = Client(s"http://127.0.0.1:$port")
      _        <- client.call("POST", "/api/auth/login", Some("""{"password":"pw"}"""))
    yield client

  def spec = suite("会话 HTTP 接口")(
    test("新建、发消息、附件、列表与搜索、详情、时间线、实时事件、停止、删除、用量") {
      for
        d <- dirs
        m <- fakeModels(req => if lastUserText(req).contains("慢慢") then Script.Hang(List(text("正在写"))) else Script.Say(List(text(s"回：${lastUserText(req)}"))))
        p <- probe
        out <- ZIO.scoped(layer(d, m).build.flatMap(env =>
                 (for
                   _     <- start(p)
                   alice <- agent("alice")
                   c     <- server
                   up    <- c.bytes("POST", "/api/files?name=a.txt", Some("hello".getBytes), "text/plain")
                   file   = up.body.fromJson[FileView].toOption.get
                   mk    <- c.call("POST", "/api/sessions", Some(NewSession(alice, "第一句", List(file.id)).toJson))
                   id     = mk.body.fromJson[Created].toOption.get.id
                   _     <- idle(id)
                   tl    <- c.call("GET", s"/api/sessions/${id.value}/timeline")
                   items  = tl.body.fromJson[List[TimelineItem]].toOption.get
                   dl    <- c.call("GET", s"/api/files/${file.id}")
                   ls    <- c.call("GET", "/api/sessions")
                   hit   <- c.call("GET", "/api/sessions?q=%E7%AC%AC%E4%B8%80") // 第一
                   miss  <- c.call("GET", "/api/sessions?q=nothing")
                   _     <- c.call("POST", s"/api/sessions/${id.value}/messages", Some(MessageIn("慢慢写").toJson))
                   _     <- eventually("在写")(stateOf(id))(_ == WorkState.Running)
                   _     <- eventually("开始生成")(ZIO.serviceWithZIO[Sessions](s => ZIO.scoped(s.subscribe(id).map(_._1))))(_.exists(_.nonEmpty))
                   ev    <- c.firstEvent(s"/api/sessions/${id.value}/live")
                   busy  <- c.call("DELETE", s"/api/sessions/${id.value}")
                   st    <- c.call("POST", s"/api/sessions/${id.value}/stop")
                   det   <- c.call("GET", s"/api/sessions/${id.value}")
                   usage <- c.call("GET", "/api/usage?from=0&to=99999999999999")
                   del   <- c.call("DELETE", s"/api/sessions/${id.value}")
                   gone  <- c.call("GET", s"/api/sessions/${id.value}")
                 yield (file, items, dl, ls, hit, miss, ev, busy, st, det, usage, del, gone)).provideSomeEnvironment[Scope](_ ++ env)
               ))
        (file, items, dl, ls, hit, miss, ev, busy, st, det, usage, del, gone) = out
        reqs <- m.requests
      yield
        val letter = items.find(_.kind == "letter").get
        val page   = ls.body.fromJson[SessionPage].toOption.get
        val lines  = usage.body.fromJson[List[UsageLine]].toOption.get
        assertTrue(
          file.size == 5L,
          letter.files.map(_.name) == List("a.txt"),
          userTexts(reqs.head).exists(_.startsWith("[附件 a.txt（text/plain，5 字节）：")),
          dl.status == 200,
          dl.body == "hello",
          dl.headers.get("content-disposition").exists(_.startsWith("attachment")),
          dl.headers.get("x-content-type-options").contains("nosniff"),
          page.items.size == 1,
          hit.body.fromJson[SessionPage].toOption.get.items.size == 1,
          miss.body.fromJson[SessionPage].toOption.get.items.isEmpty,
          ev.fromJson[LiveView].toOption.exists(v => v.kind == "snapshot" && v.parts.head.text.contains("正在写")),
          busy.status == 409,
          st.status == 204,
          det.body.fromJson[SessionDetail].toOption.exists(_.run.exists(_.state == "stopped")),
          lines.map(_.requests).sum == 1L,
          lines.head.agentName == "alice",
          del.status == 204,
          gone.status == 404
        )
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds)
