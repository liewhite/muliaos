package muliao.app

import muliao.contract.*
import muliao.models.llm.{ModelRequest, ModelSettings, Models}
import muliao.notifications.Notifications
import muliao.platform.{AppError, InstanceLock}
import muliao.platform.http.OutboundHttp
import muliao.platform.server.HttpServer
import muliao.sessions.{SessionConsole, Sessions}
import muliao.sessions.TestEnv.{FakeModels, Script, call, fakeModels, lastUserText, parts, source, text}
import muliao.triggers.Triggers
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files as JFiles, Path}

/** 组装与集成：真的配置文件、全部领域、HTTP 服务，只有模型是假的。按启动顺序起整个服务，经 HTTP 与假模型驱动各领域的工具走一遍，
  * 再关停、在同一份数据上重启。
  */
object AppSpec extends ZIOSpecDefault:

  // ---- 环境 ----

  final case class Home(base: Path):
    val data: Path   = base.resolve("data")
    val assets: Path = base.resolve("assets")
    val config: Path = base.resolve("muliao.toml")

  private def home: UIO[Home] = ZIO.succeed {
    val h = Home(JFiles.createTempDirectory("muliao-app"))
    JFiles.writeString(
      h.config,
      s"""[general]
         |data_dir = "${h.data}"
         |assets_dir = "${h.assets}"
         |
         |[log]
         |level = "WARN"
         |
         |[server]
         |port = 0
         |password = "pw"
         |cookie_secret = "${"s" * 32}"
         |
         |[vault]
         |key = "${muliao.sessions.TestEnv.vaultKey}"
         |
         |[models]
         |default_model = "fake:m"
         |[models.providers.fake]
         |protocol = "anthropic"
         |base_url = "http://127.0.0.1:9"
         |api_key = "x"
         |
         |[push]
         |vapid_subject = "mailto:test@example.com"
         |
         |[resources]
         |writable_dirs = []
         |""".stripMargin
    )
    h
  }

  /** 假模型：用户方最后一段文字是「工具 <名字> <参数 JSON>」就调那个工具，否则回「好」。 */
  private val script: ModelRequest => Script = req =>
    val t = lastUserText(req)
    if t.startsWith("工具 ") then
      val (name, args) = t.drop(3).span(_ != ' ')
      Script.Say(List(call("c1", name, args.trim)))
    else Script.Say(List(text("好")))

  private def fake(m: FakeModels): ZLayer[ModelSettings & OutboundHttp, AppError, Models] = ZLayer.succeed[Models](m)

  /** 按组装入口的方式起整个服务（不含日志设置），返回 HTTP 客户端。 */
  private def boot(h: Home, m: FakeModels): ZIO[Scope, AppError, (Client, ZEnvironment[Assembly.Services])] =
    for
      cfg  <- AppConfig.load(h.config)
      _    <- InstanceLock.acquire(cfg.general.dataDir)
      env  <- Assembly.layer(cfg, fake(m)).build
      port <- Assembly.boot.provideSomeEnvironment[Scope](_ ++ env)
      c     = Client(s"http://127.0.0.1:$port")
      r    <- c.call("POST", "/api/auth/login", Some("""{"password":"pw"}"""))
      _    <- ZIO.when(r.status / 100 != 2)(ZIO.fail(AppError.Internal(s"登录失败：$r")))
    yield c -> env

  // ---- HTTP ----

  final case class Resp(status: Int, body: String)

  final class Client(base: String):
    private val http             = HttpClient.newHttpClient()
    @volatile private var cookie = ""
    def call(method: String, path: String, body: Option[String] = None): IO[AppError, Resp] = ZIO
      .attemptBlocking {
        val b = HttpRequest.newBuilder(URI.create(base + path)).header("Content-Type", "application/json")
        if cookie.nonEmpty then b.header("Cookie", cookie): Unit
        val r    = body.fold(b.method(method, HttpRequest.BodyPublishers.noBody()))(s => b.method(method, HttpRequest.BodyPublishers.ofString(s)))
        val resp = http.send(r.build(), HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
        resp.headers.firstValue("set-cookie").ifPresent(c => cookie = c.takeWhile(_ != ';'))
        Resp(resp.statusCode, resp.body)
      }
      .mapError(e => AppError.Internal(s"HTTP 请求失败：${e.getMessage}"))

    def json[A: JsonDecoder](method: String, path: String, body: Option[String] = None): IO[AppError, A] =
      call(method, path, body).flatMap { r =>
        if r.status / 100 != 2 then ZIO.fail(AppError.Internal(s"$method $path → ${r.status} ${r.body}"))
        else ZIO.fromEither(r.body.fromJson[A]).mapError(e => AppError.Internal(s"$method $path 回的不是预期的 JSON：$e\n${r.body}"))
      }

  final case class IdOnly(id: String) derives JsonDecoder
  final case class AgentOut(summary: IdOnly) derives JsonDecoder

  private def createAgent(c: Client, name: String): IO[AppError, AgentId] =
    c.json[AgentOut]("POST", "/api/agents", Some(s"""{"name":"$name","description":"测试 $name","model":"fake:m"}""")).map(a => AgentId.wrap(a.summary.id))

  private def newSession(c: Client, agent: AgentId, msg: String): IO[AppError, SessionId] =
    c.json[IdOnly]("POST", "/api/sessions", Some(Json.Obj("agent" -> Json.Str(agent.value), "text" -> Json.Str(msg)).toJson)).map(i => SessionId.wrap(i.id))

  private def say(c: Client, s: SessionId, msg: String): IO[AppError, Unit] =
    c.call("POST", s"/api/sessions/${s.value}/messages", Some(Json.Obj("text" -> Json.Str(msg)).toJson)).flatMap(r =>
      ZIO.when(r.status != 204)(ZIO.fail(AppError.Internal(s"发消息失败：$r"))).unit
    )

  private def timeline(c: Client, s: SessionId): IO[AppError, List[TimelineItem]] = c.json[List[TimelineItem]]("GET", s"/api/sessions/${s.value}/timeline")

  /** 时间线里全部工具结果：名字、正文、是否出错。 */
  private def results(items: List[TimelineItem]): List[(String, String, Boolean)] =
    items.flatMap(_.parts.flatMap(_.call)).flatMap(c => c.result.map(r => (c.name, r.text, r.isError)))

  private def eventually[A](what: String)(get: IO[AppError, A])(ok: A => Boolean): IO[AppError, A] =
    Live.live(muliao.sessions.TestEnv.eventually(what)(get)(ok).timeoutFail(AppError.Internal(s"超时：$what"))(30.seconds))

  /** 发一句、等会话回到空闲，返回本句之后新增的工具结果。 */
  private def step(c: Client, env: ZEnvironment[Assembly.Services], s: SessionId, msg: String): IO[AppError, List[(String, String, Boolean)]] =
    for
      before <- timeline(c, s).map(results(_).size)
      _      <- say(c, s, msg)
      _      <- eventually(s"「$msg」做完")(timeline(c, s).zip(env.get[Sessions].state(s))) { (items, st) =>
                  st == WorkState.Idle && items.lastOption.exists(_.parts.exists(_.text.contains("好"))) && results(items).size > before
                }
      rs     <- timeline(c, s).map(results)
    yield rs.drop(before)

  private def tool(name: String, args: String) = s"工具 $name $args"

  private def ok(rs: List[(String, String, Boolean)], name: String): Boolean = rs.exists((n, _, e) => n == name && !e)

  def spec = suite("组装与集成")(
    test("工具目录：各领域的工具定义与实际接进会话的工具一一对应，没有重名") {
      ZIO.scoped(
        for
          h   <- home
          m   <- fakeModels(script)
          cfg <- AppConfig.load(h.config)
          _   <- ZIO.succeed(JFiles.createDirectories(h.data))
          env <- Assembly.layer(cfg, fake(m)).build
          cs  <- Assembly.contributions.provideEnvironment(env)
        yield
          val wired   = Sessions.specs.map(_.name) ++ cs.flatMap(_.tools).map(_.spec.name)
          val catalog = Assembly.toolSpecs(web = false).map(_.name)
          assertTrue(
            wired.sorted == catalog.sorted,
            wired.distinct.size == wired.size,
            Assembly.toolSpecs(web = true).map(_.name).diff(catalog).sorted == List("web_fetch", "web_search")
          )
      )
    },
    test("配置：未知的段、段里未知的键拒绝启动") {
      for
        h  <- home
        _  <- ZIO.succeed(JFiles.writeString(h.config, JFiles.readString(h.config) + "\n[nope]\nx = 1\n"))
        e1 <- AppConfig.load(h.config).flip
        _  <- ZIO.succeed(JFiles.writeString(h.config, JFiles.readString(h.config).replace("[nope]\nx = 1\n", "").replace("[log]\n", "[log]\ncolour = 1\n")))
        e2 <- AppConfig.load(h.config).flip
      yield assertTrue(e1.message.contains("未知的段"), e2.message.contains("log.colour"))
    },
    test("配置：异地备份目录配了就算平台私有文件") {
      for
        h      <- home
        before <- AppConfig.load(h.config)
        _      <- ZIO.succeed(JFiles.writeString(h.config, JFiles.readString(h.config).replace("[log]\n", "offsite_backup_dir = \"~/offsite\"\n\n[log]\n")))
        after  <- AppConfig.load(h.config)
      yield assertTrue(
        before.resourcePaths.privatePaths.size == 3,
        after.resourcePaths.privatePaths.contains(Path.of(java.lang.System.getProperty("user.home"), "offsite"))
      )
    },
    test("接口描述：覆盖各领域的接口") {
      val doc = HttpServer.openApi(Main.contractEndpoints, "test")
      assertTrue(List("/api/agents", "/api/sessions", "/api/issues", "/api/triggers", "/api/history", "/api/notifications", "/api/jobs").forall(doc.contains))
    },
    test("端到端：起服务、经 HTTP 与假模型走各领域，单实例，关停后在同一份数据上重启接着用") {
      for
        h <- home
        m <- fakeModels(script)
        (first, alice, s1, bob) <- ZIO.scoped(
               for
                 (c, env) <- boot(h, m)
                 // 单实例：同一份数据目录再起一个被拒绝
                 again <- InstanceLock.acquire(h.data).either
                 alice <- createAgent(c, "alice")
                 bob   <- createAgent(c, "bob")
                 s1    <- newSession(c, alice, "你好")
                 _     <- eventually("第一句回答完")(timeline(c, s1))(_.exists(_.parts.exists(_.text.contains("好"))))
                 // 文件与命令
                 r1 <- step(c, env, s1, tool("write_file", """{"path":"a.txt","content":"hello"}"""))
                 r2 <- step(c, env, s1, tool("read_file", """{"path":"a.txt"}"""))
                 r3 <- step(c, env, s1, tool("run_shell", """{"command":"cat a.txt && pwd","timeout":20}"""))
                 // 后台任务：结束后作为事件回到会话
                 r4 <- step(c, env, s1, tool("run_shell", """{"command":"sleep 1; echo bg-done","background":true}"""))
                 _  <- eventually("后台任务结局回到会话")(timeline(c, s1))(_.exists(i => i.source.contains("EVENT")))
                 _  <- eventually("回到空闲")(env.get[Sessions].state(s1))(_ == WorkState.Idle)
                 // 委派：等 bob 回答
                 r5 <- step(c, env, s1, tool("ask_agent", """{"agent":"bob","message":"请说好","thread":"t1"}"""))
                 r6 <- step(c, env, s1, tool("list_agents", "{}"))
                 // 看板：建一个指派给 bob 的 issue，bob 在 issue 会话里收到
                 r7 <- step(c, env, s1, tool("issue_create", """{"title":"集成测试","owner":"alice","assignee":"bob","status":"in_progress"}"""))
                 bobs <- eventually("bob 的 issue 会话")(env.get[SessionConsole].list(Some(bob), None, None, 50).map(_.items))(_.exists(_.origin.kind == OriginKind.Issue))
                 // 触发器：建、立即运行，开出 alice 的触发器会话
                 r8 <- step(c, env, s1, tool("trigger_create", """{"name":"t-once","kind":"schedule","prompt":"定时说好","config":{"cron":"0 3 * * *"}}"""))
                 r9 <- step(c, env, s1, tool("trigger_run", """{"name":"t-once"}"""))
                 trig <- eventually("触发器开出的会话")(env.get[SessionConsole].list(Some(alice), None, None, 50).map(_.items))(_.exists(_.origin.kind == OriginKind.Trigger))
                 // 通知与工作历史
                 r10 <- step(c, env, s1, tool("notify_user", """{"title":"集成","body":"通知正文"}"""))
                 inbox <- env.get[Notifications].inbox(50).map(_.items)
                 r11 <- step(c, env, s1, tool("history_append", """{"entries":[{"day":"2026-11-01","title":"集成测试","summary":"跑通"}]}"""))
                 r12 <- step(c, env, s1, tool("history_search", """{"query":"集成"}"""))
                 hist <- c.call("GET", "/api/history?limit=10")
                 ts   <- env.get[Triggers].list(finished = false)
               yield
                 val all = r1 ++ r2 ++ r3 ++ r4 ++ r5 ++ r6 ++ r7 ++ r8 ++ r9 ++ r10 ++ r11 ++ r12
                 (assertTrue(
                   again.isLeft,
                   all.forall(!_._3),
                   ok(r1, "write_file"),
                   r2.exists(_._2.contains("hello")),
                   r3.exists((n, t, _) => n == "run_shell" && t.contains("hello") && t.contains(alice.value)),
                   ok(r4, "run_shell"),
                   r5.exists((n, t, _) => n == "ask_agent" && t.contains("好")),
                   r6.exists(_._2.contains("bob")),
                   ok(r7, "issue_create"),
                   bobs.exists(_.origin.kind == OriginKind.Issue),
                   ok(r8, "trigger_create"),
                   ok(r9, "trigger_run"),
                   trig.exists(_.origin.kind == OriginKind.Trigger),
                   ts.exists(_.name.value == "t-once"),
                   inbox.exists(_.title == "集成"),
                   ok(r11, "history_append"),
                   r12.exists(_._2.contains("跑通")),
                   hist.status == 200 && hist.body.contains("集成测试")
                 ), alice, s1, bob)
             )
        // 关停后重启：数据都在，会话能接着用
        res <- ZIO.scoped(
                 for
                   (c, env) <- boot(h, m)
                   r        <- step(c, env, s1, tool("list_agents", "{}"))
                   ss       <- env.get[SessionConsole].list(None, None, None, 100).map(_.items)
                 yield assertTrue(r.exists(_._2.contains("alice")), ss.exists(_.agent == alice), ss.exists(_.agent == bob))
               )
      yield first && res
    }
  ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.timeout(3.minutes)
