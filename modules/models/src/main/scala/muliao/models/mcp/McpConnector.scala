package muliao.models.mcp

import io.modelcontextprotocol.client.{McpClient, McpSyncClient}
import io.modelcontextprotocol.client.transport.HttpClientStreamableHttpTransport
import io.modelcontextprotocol.json.{McpJsonDefaults, McpJsonMapper}
import io.modelcontextprotocol.spec.{McpClientTransport, McpSchema}
import muliao.exec.{CommandSpec, Exec, Masker, Program, Sandbox, SecretEnv}
import muliao.platform.AppError
import muliao.platform.http.{ProxySettings, SystemProxy}
import muliao.toolkit.*
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.net.{InetSocketAddress, Proxy, ProxySelector, SocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest}
import java.nio.file.Path
import java.util.Base64
import scala.jdk.CollectionConverters.*

/** stdio server 以谁的身份跑：工作目录与写沙箱（由调用方按使用者的权限算好）。 */
final case class Launch(cwd: Path, sandbox: Sandbox)

/** 一次连接的结果：连上的 server 的全部工具，和没连上的 server 要告诉模型的话（已打码）。 */
final case class Connected(tools: Chunk[Tool[Any, Nothing]], problems: Chunk[String])

/** MCP 客户端：把 MCP server 连成给 agent 的工具。协议细节交给官方 MCP Java SDK。
  *
  *   - 一个 server 出问题只影响它自己：secret 取不到、连不上、列不出工具，这次就没有它的工具，原因进 [[Connected.problems]]；
  *   - 工具名是 `<server>_<工具名>`；
  *   - 连接、调用的错误与工具结果里出现代入的 secret 明文就打码成 `[secret:名字]`；
  *   - 连接归调用方的 scope：scope 结束时断开，stdio server 的整个进程组被结束。
  */
trait McpConnector:
  def connectAll(servers: Chunk[McpServerSpec], secrets: SecretLookup, launch: Launch): ZIO[Scope, Nothing, Connected]

  /** 试连一个 server，列出它的工具名（不带前缀）。连不上以领域错误交回（调用方改配置）。 */
  def probe(server: McpServerSpec, secrets: SecretLookup, launch: Launch): IO[AppError, Chunk[String]]

object McpConnector:
  /** 连上并列出工具最多等多久 */
  val ConnectTimeout: Duration = 30.seconds
  /** 一次工具调用最多等多久 */
  val CallTimeout: Duration = 10.minutes

  val layer: URLayer[SystemProxy, McpConnector] = ZLayer.fromFunction(Live.apply)

  private val mapper: McpJsonMapper = McpJsonDefaults.getMapper()

  private final case class Live(proxy: SystemProxy) extends McpConnector:

    def connectAll(servers: Chunk[McpServerSpec], secrets: SecretLookup, launch: Launch): ZIO[Scope, Nothing, Connected] =
      ZIO
        .foreachPar(servers)(s => isolated(connect(s, secrets, launch)).either.map(s -> _))
        .map { results =>
          val connected = results.collect { case (_, Right(tools)) => tools }.flatten
          // 两个 server 的工具前缀后撞名：都不要，免得调错
          val dup   = connected.groupBy(_.spec.name).collect { case (n, ts) if ts.size > 1 => n }.toSet
          val tools = connected.filterNot(t => dup(t.spec.name))
          val problems = results.collect { case (s, Left(why)) => s"MCP server ${s.name} 这次不可用，它的工具没有加载：$why" } ++
            Chunk.fromIterable(dup.toList.sorted.map(n => s"MCP 工具名 $n 重名，都没有加载"))
          Connected(tools, problems)
        }
        .tap(c => ZIO.foreachDiscard(c.problems)(p => ZIO.logWarning(p)))

    /** 在调用方 scope 下的子 scope 里连：连不上时当场收掉已起的进程，连上了就随调用方的 scope 一起结束。 */
    private def isolated[E, A](connecting: ZIO[Scope, E, A]): ZIO[Scope, E, A] =
      for
        child <- ZIO.serviceWithZIO[Scope](_.fork)
        a     <- child.extend[Any](connecting).tapErrorCause(c => child.close(Exit.failCause(c)))
      yield a

    def probe(server: McpServerSpec, secrets: SecretLookup, launch: Launch): IO[AppError, Chunk[String]] =
      ZIO.scoped(connect(server, secrets, launch).map(_.map(_.spec.name.stripPrefix(s"${server.name}_")))).mapError(AppError.invalid)

    /** 连一个 server；失败原因已打码。 */
    private def connect(server: McpServerSpec, secrets: SecretLookup, launch: Launch): ZIO[Scope, String, Chunk[Tool[Any, Nothing]]] =
      VaultTemplate.fill(server.transport, secrets).mapError(_.message).flatMap { case Filled(transport, revealed) =>
        val masker = Masker(Chunk.fromIterable(revealed.values.map(SecretEnv(_, _))))
        val attempt =
          for
            opened <- open(server.name, transport, launch, masker)
            Opened(t, gone, stderr) = opened
            client <- ZIO.acquireRelease(
                        ZIO.attemptBlocking(
                          McpClient
                            .sync(t)
                            .clientInfo(McpSchema.Implementation.builder("muliao", "1").build())
                            .requestTimeout(java.time.Duration.ofMillis(CallTimeout.toMillis))
                            .initializationTimeout(java.time.Duration.ofMillis(ConnectTimeout.toMillis))
                            .build()
                        )
                      )(c => ZIO.attemptBlocking(c.closeGracefully()).ignore)
            listed <- ZIO.attemptBlockingInterrupt { client.initialize(); listAll(client) }
                        .timeoutFail(new RuntimeException(s"${ConnectTimeout.toSeconds} 秒内没连上"))(ConnectTimeout)
                        .catchAll(e => ZIO.sleep(200.millis) *> stderr.flatMap(lines => ZIO.fail(withStderr(e, lines))))
          yield listed.map(tool(server.name, client, masker, gone))
        attempt.mapError(e => masker.mask(describe(e))).flatMap { tools =>
          ZIO.foreach(tools)(ZIO.fromEither(_))
        }
      }

    private def listAll(client: McpSyncClient): Chunk[McpSchema.Tool] =
      def loop(cursor: Option[String], acc: Chunk[McpSchema.Tool]): Chunk[McpSchema.Tool] =
        val page = cursor.fold(client.listTools())(client.listTools(_))
        val all  = acc ++ Chunk.fromIterable(page.tools.asScala)
        Option(page.nextCursor).filter(_.nonEmpty) match
          case Some(next) => loop(Some(next), all)
          case None       => all
      loop(None, Chunk.empty)

    private def withStderr(e: Throwable, lines: Chunk[String]): Throwable =
      if lines.isEmpty then e else RuntimeException(s"${describe(e)}（server 的标准错误：${lines.mkString(" / ")}）")

    /** 建传输；同时给出「server 没了」的信号（stdio 进程退出，调用中的请求据此尽早失败，不用等到超时）和 server 标准错误的最近几行。 */
    /** 打开的连接：传输、进程退出时给出说明、最近的标准错误。 */
    private final case class Opened(transport: McpClientTransport, gone: UIO[String], stderr: UIO[Chunk[String]])

    private def open(
        name: String,
        transport: McpTransport,
        launch: Launch,
        masker: Masker
    ): ZIO[Scope, Throwable, Opened] =
      transport match
        case McpTransport.Stdio(cmd, args, env) =>
          for
            command <- Exec.attach(CommandSpec(Program.Argv(cmd, args), launch.cwd, launch.sandbox, env))
            runtime <- ZIO.runtime[Any]
          yield
            val t = ExecStdioTransport(command, mapper, runtime, name, masker)
            Opened(t, command.exitCode.map(c => s"server 进程已退出（退出码 $c）"), t.recentErrors)
        case McpTransport.Http(url, headers) =>
          for
            uri      <- ZIO.attempt(URI(url))
            settings <- proxy.current
          yield
            val base     = s"${uri.getScheme}://${uri.getRawAuthority}"
            val endpoint = Option(uri.getRawPath).filter(_.nonEmpty).getOrElse("/") + Option(uri.getRawQuery).fold("")("?" + _)
            val request  = headers.foldLeft(HttpRequest.newBuilder())((b, h) => b.header(h._1, h._2))
            val t = HttpClientStreamableHttpTransport
              .builder(base)
              .endpoint(endpoint)
              .clientBuilder(HttpClient.newBuilder().proxy(selector(settings)).connectTimeout(java.time.Duration.ofSeconds(15)))
              .requestBuilder(request)
              .jsonMapper(mapper)
              .build()
            Opened(t, ZIO.never, ZIO.succeed(Chunk.empty))

    private def describe(e: Throwable): String =
      val msgs = Iterator.iterate(e)(_.getCause).takeWhile(_ != null).take(4).map(t => Option(t.getMessage).getOrElse(t.getClass.getSimpleName))
      msgs.toList.distinct.mkString("：")

    /** 一个 MCP 工具 → 给 agent 的工具。名字不合规的（太长、有怪字符）不加载。 */
    private def tool(server: String, client: McpSyncClient, masker: Masker, gone: UIO[String])(t: McpSchema.Tool): Either[String, Tool[Any, Nothing]] =
      val name = s"${server}_${t.name}"
      Either.cond(ToolSpec.validName(name), (), s"工具名 $name 不合规（只能用字母、数字、_、-，最长 64 个字符）").map { _ =>
        val schema = mapper.writeValueAsString(Option(t.inputSchema).getOrElse(java.util.Map.of("type", "object"))).fromJson[Json].getOrElse(Json.Obj())
        val s      = ToolSpec(name, Option(t.description).getOrElse(""), schema, Access.Write, Scheduling.Concurrent, _ => Display.Plain, Set.empty)
        new Tool[Any, Nothing]:
          val spec = s
          def invoke(args: Json, ctx: Any): IO[ToolFailure, ToolOutcome[Nothing]] =
            args match
              case o: Json.Obj =>
                ZIO
                  .attemptBlockingInterrupt {
                    val map = mapper.readValue(o.toJson, classOf[java.util.Map[String, Object]])
                    client.callTool(McpSchema.CallToolRequest.builder(t.name).arguments(map).build())
                  }
                  .mapError(e => ToolFailure.Rejected(masker.mask(s"调用 MCP 工具 $name 失败：${describe(e)}")))
                  .raceFirst(gone.flatMap(why => ZIO.fail(ToolFailure.Rejected(s"调用 MCP 工具 $name 失败：$why"))))
                  .flatMap(result(_, masker))
              case _ => ZIO.fail(ToolFailure.Rejected("参数整体：应是一个 JSON 对象。请对照参数说明改了再调用。"))
      }

    private def result(r: McpSchema.CallToolResult, masker: Masker): IO[ToolFailure, ToolOutcome[Nothing]] =
      val blocks: Chunk[ToolContent] = Chunk.fromIterable(Option(r.content).map(_.asScala).getOrElse(Nil)).map {
        case c: McpSchema.TextContent  => ToolContent.Text(masker.mask(c.text))
        case c: McpSchema.ImageContent => ToolContent.Image(Option(c.mimeType).getOrElse("image/png"), Chunk.fromArray(Base64.getDecoder.decode(c.data)))
        case c: McpSchema.AudioContent => ToolContent.Text(s"[音频内容（${c.mimeType}），无法展示]")
        case c: McpSchema.EmbeddedResource =>
          c.resource match
            case t: McpSchema.TextResourceContents => ToolContent.Text(masker.mask(t.text))
            case b                                => ToolContent.Text(s"[二进制资源 ${b.uri}（${Option(b.mimeType).getOrElse("未知类型")}）]")
        case c: McpSchema.ResourceLink => ToolContent.Text(s"[资源链接 ${c.name}：${c.uri}]")
        case other                     => ToolContent.Text(s"[不认识的内容类型 ${other.getClass.getSimpleName}]")
      }
      // 只有结构化结果、没有内容块时，把结构化结果作为 JSON 文本
      val content =
        if blocks.nonEmpty then blocks
        else Chunk.fromIterable(Option(r.structuredContent).map(sc => ToolContent.Text(masker.mask(mapper.writeValueAsString(sc)))))
      if Option(r.isError).exists(_.booleanValue) then
        ZIO.fail(ToolFailure.Rejected(content.collect { case ToolContent.Text(t) => t }.mkString("\n")))
      else ZIO.succeed(ToolOutcome.Completed(content))

  /** 按系统代理设置选代理（JDK HttpClient 用）。 */
  private def selector(s: ProxySettings): ProxySelector = new ProxySelector:
    def select(uri: URI): java.util.List[Proxy] =
      s.forUrl(uri.getScheme, uri.getHost) match
        case Some(p) => java.util.List.of(Proxy(Proxy.Type.HTTP, InetSocketAddress.createUnresolved(p.host, p.port)))
        case None    => java.util.List.of(Proxy.NO_PROXY)
    def connectFailed(uri: URI, sa: SocketAddress, ioe: java.io.IOException): Unit = ()
