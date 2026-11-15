package muliao.platform.server

import muliao.contract.{ErrorKind, Topic}
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import muliao.platform.http.*
import zio.*
import zio.http.{Body, Header, Headers, Method, Request, Response, Routes, Server, Status, URL, handler}
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

object HttpServerSpec extends ZIOSpecDefault:

  val settings = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)

  def auth(password: String = "pw", secret: String = "s" * 32): UIO[Auth.Live] =
    LoginThrottle.make.map(Auth.Live(password, secret, _))

  val direct = SystemProxy.fixed(ProxySettings.direct)
  val http   = ZLayer.succeed(HttpSettings(requestTimeoutSeconds = 10)) ++ direct >>> OutboundHttp.layer

  /** 起一个测试服务，返回基地址。 */
  def server: ZIO[Scope, Throwable, (String, ChangeBus)] =
    for
      a    <- auth()
      bus  <- ChangeBus.layer.build.map(_.get[ChangeBus])
      port <- HttpServer.serve(settings, a, bus, Nil, "test")
    yield (s"http://127.0.0.1:$port", bus)

  def call(req: Request): ZIO[OutboundHttp, Nothing, Either[AppError, Response]] =
    ZIO.serviceWithZIO[OutboundHttp](_.send(req)).either

  def cookieOf(resp: Response): Option[String] =
    resp.rawHeader("set-cookie").map(_.takeWhile(_ != ';'))

  def json(path: String, body: String, base: String) =
    Request.post(URL.decode(base + path).toOption.get, Body.fromString(body)).addHeader(Header.ContentType(zio.http.MediaType.application.json))

  def spec = suite("HTTP 服务")(
    suite("登录")(
      test("cookie 可校验；改密码或改签名密钥后失效；过期失效") {
        for
          a     <- auth()
          c     <- a.login("pw", "x")
          ok    <- a.authenticate(Some(c.value))
          other <- auth(password = "new").flatMap(_.authenticate(Some(c.value)).either)
          key   <- auth(secret = "t" * 32).flatMap(_.authenticate(Some(c.value)).either)
          _     <- TestClock.adjust(Auth.validity + 1.second)
          late  <- a.authenticate(Some(c.value)).either
          junk  <- a.authenticate(Some("v1.9999999999.x.y")).either
        yield assertTrue(
          ok.expiresAt.getEpochSecond == Auth.validity.toSeconds,
          other.left.exists(_.kind == ErrorKind.Unauthorized),
          key.isLeft,
          late.isLeft,
          junk.isLeft,
          c.httpOnly,
          c.maxAge.contains(Auth.validity.toSeconds)
        )
      },
      test("连续输错按来源退避，别的来源不受影响，到时间可再试，成功清零") {
        for
          a  <- auth()
          r1 <- ZIO.foreach(1 to 3)(_ => a.login("bad", "1.1.1.1").either)
          r4 <- a.login("pw", "1.1.1.1").either // 第 3 次失败后锁 1 秒
          o  <- a.login("pw", "2.2.2.2").either
          _  <- TestClock.adjust(1.second)
          r5 <- a.login("bad", "1.1.1.1").either // 第 4 次失败，锁 2 秒
          r6 <- a.login("pw", "1.1.1.1").either
          _  <- TestClock.adjust(2.seconds)
          r7 <- a.login("pw", "1.1.1.1").either
          r8 <- a.login("bad", "1.1.1.1").either // 清零后第 1 次失败不锁
          r9 <- a.login("pw", "1.1.1.1").either
        yield assertTrue(
          r1.forall(_.left.exists(_.kind == ErrorKind.Unauthorized)),
          r4.left.exists(e => e.kind == ErrorKind.TooManyAttempts && e.retryAfterSeconds.contains(1L)),
          o.isRight,
          r5.left.exists(_.kind == ErrorKind.Unauthorized),
          r6.left.exists(e => e.retryAfterSeconds.contains(2L)),
          r7.isRight,
          r8.left.exists(_.kind == ErrorKind.Unauthorized),
          r9.isRight
        )
      }
    ),
    suite("经 HTTP")(
    test("登录 → 查看登录状态 → 退出；未登录 401；未知接口统一 404") {
      ZIO.scoped {
        for
          (base, _) <- server
          anon      <- call(Request.get(URL.decode(base + "/api/auth/session").toOption.get))
          bad       <- call(json("/api/auth/login", """{"password":"x"}""", base))
          good      <- call(json("/api/auth/login", """{"password":"pw"}""", base))
          cookie     = good.toOption.flatMap(cookieOf).get
          me        <- call(Request.get(URL.decode(base + "/api/auth/session").toOption.get).addHeader("cookie", cookie))
          out       <- call(Request.post(URL.decode(base + "/api/auth/logout").toOption.get, Body.empty).addHeader("cookie", cookie))
          missing   <- call(Request.get(URL.decode(base + "/api/nothing/here").toOption.get))
          malformed <- call(json("/api/auth/login", """{"pw":1}""", base))
          body      <- ZIO.foreach(me.toOption)(_.body.asString).orDie
        yield assertTrue(
          anon.left.exists(_.message.startsWith("HTTP 401")),
          bad.left.exists(_.message.contains("密码不对")),
          cookie.startsWith("muliao_auth=v1."),
          body.exists(_.contains("expiresAt")),
          out.toOption.flatMap(_.rawHeader("set-cookie")).exists(_.contains("Max-Age=0")),
          missing.left.exists(e => e.message.startsWith("HTTP 404") && e.message.contains("NotFound")),
          malformed.left.exists(_.message.startsWith("HTTP 400"))
        )
      }
    },
    test("实时刷新：提交后的主题经 SSE 送到（客户端要 gzip 也不被缓冲）") {
      ZIO.scoped {
        for
          (base, bus) <- server
          login       <- call(json("/api/auth/login", """{"password":"pw"}""", base))
          cookie       = login.toOption.flatMap(cookieOf).get
          resp <- ZIO.serviceWithZIO[OutboundHttp](
                    _.open(
                      Request
                        .get(URL.decode(base + "/api/events").toOption.get)
                        .addHeaders(Headers("cookie" -> cookie, "accept-encoding" -> "gzip"))
                    )
                  )
          gzipped = resp.rawHeader("content-encoding").contains("gzip")
          inflater = java.util.zip.Inflater(true)
          got <- resp.body.asStream.chunks
                   .mapAccum(Chunk.empty[Byte]) { (seen, c) =>
                     // 边收边解压：压缩流每段都要及时刷出，不能攒着
                     val all = seen ++ c
                     val text =
                       if !gzipped then String(all.toArray, "UTF-8")
                       else if all.size <= 10 then ""
                       else
                         inflater.reset(); inflater.setInput(all.drop(10).toArray)
                         val buf = new Array[Byte](65536); val n = inflater.inflate(buf)
                         String(buf, 0, n, "UTF-8")
                     (all, text)
                   }
                   .takeUntil(_.contains("issues"))
                   .runLast
                   .map(_.getOrElse(""))
                   .fork
          _ <- (bus.publish(Topic.Issues) *> ZIO.sleep(200.millis)).repeatN(5).fork
          text <- got.join.timeoutFail(new RuntimeException("SSE 没收到"))(5.seconds)
          frames = text.linesIterator.filter(_.startsWith("data:")).map(_.stripPrefix("data:").trim).toList
          change = frames.find(_.contains("topic"))
        yield assertTrue(
          // 订阅建好后先给 ready（带心跳间隔），前端据此重新拉取
          frames.headOption.contains(s"""{"intervalSeconds":${Sse.PingSeconds}}"""),
          change.flatMap(_.fromJson[Json].toOption).contains(Json.Obj("topic" -> Json.Str("issues")))
        )
      }
    },
    test("接口描述包含平台接口") {
      val doc = HttpServer.openApi(List(Endpoints.stream(muliao.contract.Api.changes), HttpServer.openApiEndpoint) ++ muliao.contract.AuthApi.all, "t")
      assertTrue(doc.contains("/api/auth/login"), doc.contains("/api/events"), doc.contains("ApiError"))
    },
    suite("出站 HTTP")(
      test("带 User-Agent；5xx/429 算暂时不可用（带 Retry-After），4xx 算永久错误；连不上算暂时不可用") {
        val routes = Routes(
          Method.GET / "ua"   -> handler((r: Request) => Response.text(r.rawHeader("user-agent").getOrElse(""))),
          Method.GET / "busy" -> handler(Response.status(Status.TooManyRequests).addHeader("retry-after", "7")),
          Method.GET / "down" -> handler(Response.status(Status.BadGateway)),
          Method.GET / "gone" -> handler(Response.text("没了").status(Status.NotFound))
        )
        ZIO.scoped {
          for
            env   <- ((ZLayer.succeed(Server.Config.default.port(0)) ++ ZLayer.succeed(zio.http.netty.NettyConfig.defaultWithFastShutdown)) >>> Server.customized).build // 快速关闭，不等 2 秒安静期
            port  <- Server.install(routes).provideEnvironment(env)
            base   = s"http://127.0.0.1:$port"
            ua    <- call(Request.get(URL.decode(base + "/ua").toOption.get)).flatMap(r => ZIO.foreach(r.toOption)(_.body.asString).orDie)
            busy  <- call(Request.get(URL.decode(base + "/busy").toOption.get))
            down  <- call(Request.get(URL.decode(base + "/down").toOption.get))
            gone  <- call(Request.get(URL.decode(base + "/gone").toOption.get))
            dead  <- call(Request.get(URL.decode("http://127.0.0.1:1/x").toOption.get))
          yield assertTrue(
            ua.contains(HttpSettings.defaultUserAgent),
            busy.left.exists { case AppError.Unavailable(_, _, after) => after.contains(7.seconds); case _ => false },
            down.left.exists(_.isTransient),
            gone.left.exists { case AppError.External(m) => m.contains("404") && m.contains("没了"); case _ => false },
            dead.left.exists(_.isTransient)
          )
        }
      }
    )
  ).provideLayer(http.orDie) @@ withLiveClock // 出站客户端的 DNS 缓存按时钟刷新，用测试时钟快进会空转
  ) @@ sequential
