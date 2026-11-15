package muliao.workspace

import muliao.contract.*
import muliao.platform.events.ChangeBus
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import muliao.sessions.SessionsHttpSpec.Client
import muliao.sessions.TestEnv as S
import muliao.workspace.TestEnv.*
import zio.*
import zio.json.*
import zio.test.*

/** 本领域的 HTTP 接口：经真的 HTTP 服务调用。 */
object WorkspaceHttpSpec extends ZIOSpecDefault:

  private def server: ZIO[Scope & TestEnv.Env, Throwable, (Client, Client)] =
    for
      http     <- ZLayer.derive[WorkspaceHttp].build.map(_.get[WorkspaceHttp])
      bus      <- ZIO.service[ChangeBus]
      settings  = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
      auth     <- Auth.layer.build.map(_.get[Auth]).provideSomeLayer[Scope](ZLayer.succeed(settings))
      port     <- HttpServer.serve(settings, auth, bus, http.routes, "test")
      in        = Client(s"http://127.0.0.1:$port")
      _        <- in.call("POST", "/api/auth/login", Some("""{"password":"pw"}"""))
    yield (in, Client(s"http://127.0.0.1:$port"))

  def spec = suite("工作区 HTTP 接口")(
    test("页面不要登录、带隔离头；目录浏览、取文件要登录且按不可信内容下载；页面列表与始终允许列表") {
      for
        d <- S.dirs
        m <- S.fakeModels(_ => S.Script.Say(List(S.text("好"))))
        out <- ZIO.scoped(layer(d, m).build.flatMap(env =>
                 (for
                   alice   <- S.agent("alice")
                   ws      <- ZIO.serviceWith[muliao.resources.ResourcePaths](_.workspace(alice))
                   _       <- write(ws.resolve("site/demo/index.html"), "<p>页面</p>")
                   _       <- write(ws.resolve("notes/x.html"), "<script>alert(1)</script>")
                   (c, anon) <- server
                   pages   <- c.call("GET", "/api/pages")
                   url      = pages.body.fromJson[List[PageView]].toOption.get.head.url
                   page    <- anon.call("GET", url)
                   forged  <- anon.call("GET", url.replaceAll("/[0-9a-f]{32}/", "/" + "0" * 32 + "/"))
                   listing <- c.call("GET", s"/api/agents/${alice.value}/workspace?path=notes")
                   noAuth  <- anon.call("GET", s"/api/agents/${alice.value}/workspace")
                   file    <- c.call("GET", s"/api/agents/${alice.value}/workspace/file?path=notes/x.html")
                   escape  <- c.call("GET", s"/api/agents/${alice.value}/workspace/file?path=../../t.db")
                   jobs    <- c.call("GET", s"/api/agents/${alice.value}/jobs")
                   grants  <- c.call("GET", "/api/script-grants")
                   revoke  <- c.call("DELETE", "/api/script-grants/42")
                 yield (page, forged, listing, noAuth, file, escape, jobs, grants, revoke)).provideSomeEnvironment[Scope](_ ++ env)
               ))
        (page, forged, listing, noAuth, file, escape, jobs, grants, revoke) = out
      yield assertTrue(
        page.status == 200,
        page.body == "<p>页面</p>",
        page.headers.get("content-security-policy").exists(_.startsWith("sandbox allow-scripts")),
        page.headers.get("access-control-allow-origin").contains("*"),
        page.headers.get("x-content-type-options").contains("nosniff"),
        forged.status == 404,
        listing.body.fromJson[DirListing].toOption.exists(_.entries.map(_.name) == List("x.html")),
        noAuth.status == 401,
        file.status == 200,
        file.headers.get("content-disposition").exists(_.startsWith("attachment")),
        file.headers.get("content-type").contains("application/octet-stream"),
        file.headers.get("content-security-policy").contains("sandbox"),
        escape.status == 404,
        jobs.body == "[]",
        grants.body == "[]",
        revoke.status == 404
      )
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds)
