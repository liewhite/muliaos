package muliao.models.mcp

import muliao.exec.{RealPath, Sandbox}
import muliao.platform.AppError
import muliao.platform.http.{ProxySettings, SystemProxy}
import muliao.toolkit.*
import zio.*
import zio.http.{Body, Header, Method, Request, Response, Routes, Server, Status, handler}
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.TestAspect.*

import java.nio.file.{Files, Path, Paths}

object McpSpec extends ZIOSpecDefault:

  val script: String = Paths.get(getClass.getResource("/mcp/demo_server.py").toURI).toString

  val secrets: SecretLookup = new SecretLookup:
    def reveal(key: String): IO[AppError, String] = key match
      case "TOKEN" => ZIO.succeed("tok-12345-secret")
      case other   => ZIO.fail(AppError.notFound(s"没有 secret $other"))

  def stdio(name: String, env: Map[String, String] = Map("TOKEN" -> "${vault:TOKEN}")) =
    McpServerSpec(name, McpTransport.Stdio("/usr/bin/python3", Chunk(script), env))

  def tempDir: UIO[Path] = ZIO.succeed(Files.createTempDirectory("muliao-mcp"))

  def call(tools: Chunk[Tool[Any]], name: String, args: String = "{}"): IO[ToolFailure, ToolOutcome] =
    tools.find(_.spec.name == name).get.invoke(args.fromJson[Json].toOption.get, ())

  def text(o: ToolOutcome): String = o match
    case ToolOutcome.Completed(cs) => cs.collect { case ToolContent.Text(t) => t }.mkString
    case other                     => other.toString

  def alive(pid: Long): UIO[Boolean] =
    ZIO.attemptBlocking(ProcessBuilder("/bin/kill", "-0", pid.toString).start().waitFor() == 0).orElseSucceed(false)

  val connector = SystemProxy.fixed(ProxySettings.direct) >>> McpConnector.layer

  /** 最小的 Streamable HTTP MCP server：只认带对 Authorization 的请求，JSON 直接回应。 */
  val httpRoutes = Routes(
    Method.POST / "mcp" -> handler { (req: Request) =>
      if !req.rawHeader("authorization").contains("Bearer tok-12345-secret") then ZIO.succeed(Response.status(Status.Unauthorized))
      else
        req.body.asString.orDie.map { body =>
          val msg = body.fromJson[Json].toOption.get
          def field(j: Json, k: String) = j match
            case Json.Obj(fs) => fs.collectFirst { case (`k`, v) => v }
            case _            => None
          field(msg, "id") match
            case None => Response.status(Status.Accepted)
            case Some(id) =>
              val method = field(msg, "method").collect { case Json.Str(s) => s }.getOrElse("")
              val params = field(msg, "params").getOrElse(Json.Obj())
              val result = method match
                case "initialize" =>
                  s"""{"protocolVersion":${field(params, "protocolVersion").map(_.toJson).getOrElse("null")},"capabilities":{"tools":{}},"serverInfo":{"name":"h","version":"1"}}"""
                case "tools/list" => """{"tools":[{"name":"hello","description":"Hi.","inputSchema":{"type":"object"}}]}"""
                case "tools/call" => """{"content":[{"type":"text","text":"hello from http"}]}"""
                case _            => "{}"
              Response(body = Body.fromString(s"""{"jsonrpc":"2.0","id":${id.toJson},"result":$result}""")).addHeader(Header.ContentType(zio.http.MediaType.application.json))
        }
    },
    Method.GET / "mcp"    -> handler(Response.status(Status.MethodNotAllowed)),
    Method.DELETE / "mcp" -> handler(Response.ok)
  )

  def spec = suite("MCP 客户端")(
    test("stdio：工具带前缀、调用、出错交还模型、secret 打码、图片；scope 结束整组结束") {
      for
        dir <- tempDir
        out <- ZIO.scoped {
                 for
                   c      <- ZIO.serviceWithZIO[McpConnector](_.connectAll(Chunk(stdio("demo")), secrets, Launch(dir, Sandbox.Unrestricted)))
                   echo   <- call(c.tools, "demo_echo", """{"text":"你好"}""")
                   leak   <- call(c.tools, "demo_secret")
                   boom   <- call(c.tools, "demo_boom").flip
                   image  <- call(c.tools, "demo_image")
                   badArg <- call(c.tools, "demo_echo", "[1]").flip
                   pid    <- ZIO.succeed(Files.readString(dir.resolve("server.pid")).trim.toLong)
                   live   <- alive(pid)
                 yield (c, echo, leak, boom, image, badArg, pid, live)
               }
        (c, echo, leak, boom, image, badArg, pid, liveDuring) = out
        _         <- ZIO.sleep(200.millis)
        liveAfter <- alive(pid)
      yield assertTrue(
        c.problems.isEmpty,
        c.tools.map(_.spec.name).toSet == Set("demo_echo", "demo_boom", "demo_secret", "demo_crash", "demo_image", "demo_touch"),
        c.tools.head.spec.inputSchema.toJson.contains("object"),
        text(echo) == "你好",
        text(leak) == "token=[secret:TOKEN]",
        boom == ToolFailure.Rejected("it broke"),
        image match
          case ToolOutcome.Completed(Chunk(ToolContent.Image("image/png", data))) => data.nonEmpty
          case _                                                                  => false,
        badArg.isInstanceOf[ToolFailure.Rejected],
        liveDuring,
        !liveAfter
      )
    },
    test("server 进程退出：调用尽快失败，不等到超时") {
      for
        dir <- tempDir
        r <- ZIO.scoped {
               for
                 c <- ZIO.serviceWithZIO[McpConnector](_.connectAll(Chunk(stdio("demo")), secrets, Launch(dir, Sandbox.Unrestricted)))
                 e <- call(c.tools, "demo_crash").flip.timed
               yield e
             }
        (took, e) = r
      yield assertTrue(took < 10.seconds, e match
        case ToolFailure.Rejected(m) => m.contains("退出")
        case _                       => false)
    },
    test("一个 server 出问题只影响它自己：取不到 secret、命令起不来的跳过并说明原因") {
      for
        dir <- tempDir
        c <- ZIO.scoped(
               ZIO.serviceWithZIO[McpConnector](
                 _.connectAll(
                   Chunk(stdio("good"), stdio("nokey", Map("X" -> "${vault:MISSING}")), McpServerSpec("nocmd", McpTransport.Stdio("/no/such/cmd", Chunk.empty, Map.empty))),
                   secrets,
                   Launch(dir, Sandbox.Unrestricted)
                 )
               )
             )
      yield assertTrue(
        c.tools.forall(_.spec.name.startsWith("good_")),
        c.tools.nonEmpty,
        c.problems.size == 2,
        c.problems.exists(p => p.contains("nokey") && p.contains("MISSING")),
        c.problems.exists(p => p.contains("nocmd") && p.contains("/no/such/cmd"))
      )
    },
    test("stdio server 关在写沙箱里：只能写给定目录") {
      for
        dir     <- tempDir
        outside <- tempDir
        real    <- RealPath.resolve(dir)
        r <- ZIO.scoped {
               for
                 c   <- ZIO.serviceWithZIO[McpConnector](_.connectAll(Chunk(stdio("demo")), secrets, Launch(dir, Sandbox.Confined(Chunk(real)))))
                 in  <- call(c.tools, "demo_touch", s"""{"path":"${dir.resolve("a")}"}""")
                 out <- call(c.tools, "demo_touch", s"""{"path":"${outside.resolve("b")}"}""").flip
               yield (in, out)
             }
      yield assertTrue(text(r._1) == "ok", r._2 match
        case ToolFailure.Rejected(m) => m.contains("denied")
        case _                       => false, !Files.exists(outside.resolve("b")))
    },
    test("HTTP：请求头代入 secret；试连列出不带前缀的工具名；连不上的试连是领域错误") {
      ZIO.scoped {
        for
          env  <- Server.defaultWith(_.port(0)).build
          port <- Server.install(httpRoutes).provideEnvironment(env)
          dir  <- tempDir
          spec  = McpServerSpec("web", McpTransport.Http(s"http://127.0.0.1:$port/mcp", Map("Authorization" -> "Bearer ${vault:TOKEN}")))
          names <- ZIO.serviceWithZIO[McpConnector](_.probe(spec, secrets, Launch(dir, Sandbox.Unrestricted)))
          c     <- ZIO.serviceWithZIO[McpConnector](_.connectAll(Chunk(spec), secrets, Launch(dir, Sandbox.Unrestricted)))
          hello <- call(c.tools, "web_hello")
          wrong  = McpServerSpec("web", McpTransport.Http(s"http://127.0.0.1:$port/mcp", Map("Authorization" -> "Bearer nope")))
          bad   <- ZIO.serviceWithZIO[McpConnector](_.probe(wrong, secrets, Launch(dir, Sandbox.Unrestricted))).flip
        yield assertTrue(names == Chunk("hello"), text(hello) == "hello from http", bad match
          case AppError.Domain(_, _) => true
          case _                     => false)
      }
    },
    test("模板：代入并交回用到的明文；不在 toString 里") {
      for (s, r) <- VaultTemplate.fill("a ${vault:TOKEN} b ${vault:TOKEN}", secrets)
      yield assertTrue(s == "a tok-12345-secret b tok-12345-secret", r.values == Map("TOKEN" -> "tok-12345-secret"), !r.toString.contains("tok-"))
    }
  ).provideLayerShared(connector) @@ withLiveClock @@ timeout(120.seconds)
