package muliao.platform.server

import io.circe.Printer
import io.circe.syntax.*
import muliao.contract.{Api, ApiError, AuthApi, AuthSession, ErrorKind, Feed}
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import sttp.apispec.openapi.circe.*
import sttp.capabilities.zio.ZioStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.{AnyEndpoint, CodecFormat, Endpoint, PublicEndpoint}
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.json.zio.*
import sttp.tapir.server.interceptor.exception.ExceptionHandler
import sttp.tapir.server.model.ValuedEndpointOutput
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}
import sttp.tapir.ztapir.*
import zio.*
import zio.http.Server
import zio.stream.*

import java.nio.charset.StandardCharsets

/** 交给 HTTP 解释器的接口实现。 */
type ApiEndpoint = ZServerEndpoint[Any, ZioStreams]

/** 一个领域的接口实现：声明加逻辑。要登录的由 [[HttpServer.serve]] 统一套上登录校验，领域不经手登录。 */
type Route = Auth => ApiEndpoint

object Route:
  /** 不要登录的领域接口（如靠签名的页面文件）。 */
  def unsecured(e: ApiEndpoint): Route = _ => e

object Endpoints:
  extension [I, O](e: Endpoint[Option[String], I, ApiError, O, Any])
    /** 需要登录的接口：先校验 cookie，再执行领域逻辑；领域错误统一转成接口错误体。 */
    def secured(logic: I => IO[AppError, O]): Route =
      auth => e.zServerSecurityLogic(auth.authenticate).serverLogic[Any](_ => i => toApi(e.showShort, logic(i)))

  extension [I, O](e: Endpoint[Option[String], I, ApiError, O, ZioStreams])
    /** 需要登录的流式接口（SSE）：逻辑自己给出接口错误体。 */
    private def securedStream(logic: I => IO[ApiError, O]): Route =
      auth => e.zServerSecurityLogic(auth.authenticate).serverLogic[Any](_ => logic)

  extension [I, E](f: Feed[I, E])
    /** 实时连接的完整声明：输出是 SSE 字节流。 */
    def stream: Endpoint[Option[String], I, ApiError, ZStream[Any, Throwable, Byte], ZioStreams] =
      f.endpoint.out(streamTextBody(ZioStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8)))

    /** 需要登录的实时连接：逻辑给出「在 scope 里建好订阅、得到业务事件流」，连接帧与事件名、JSON 编码在这里统一做（见 [[Sse]]）。 */
    def securedFeed(logic: I => IO[AppError, URIO[Scope, ZStream[Any, Throwable, E]]]): Route =
      stream.securedStream { i =>
        toApi(f.endpoint.showShort, logic(i)).map { subscribed =>
          Sse.frames(subscribed.map(_.map(e => ServerSentEvent(data = Some(f.codec.encoder.encodeJson(e, None).toString), eventType = Some(f.event)))))
        }
      }

  extension [I, O](e: PublicEndpoint[I, ApiError, O, Any])
    def public(logic: I => IO[AppError, O]): ApiEndpoint =
      e.zServerLogic(i => toApi(e.showShort, logic(i)))

  /** 领域错误转成接口错误体；内部错误给前端的只有「详情见服务日志」，所以细节在这里进日志。 */
  private def toApi[O](what: String, io: IO[AppError, O]): IO[ApiError, O] =
    io.tapError {
      case AppError.Internal(m, cause) => ZIO.logErrorCause(s"接口 $what 内部错误：$m", cause.fold(Cause.empty)(Cause.die(_)))
      case _                           => ZIO.unit
    }.mapError(AppError.toApi)

/** HTTP 服务骨架：登录、实时刷新（SSE）、OpenAPI、静态前端、统一错误体、gzip。各领域的接口由组装入口传进来。 */
object HttpServer:

  val openApiEndpoint: PublicEndpoint[Unit, ApiError, String, Any] =
    Api.base.get.in("openapi.json").out(stringJsonBody).name("openapi")

  /** `/api` 下没有匹配的接口：统一错误体的 404（放在全部接口之后、静态文件之前）。 */
  private val apiNotFound: ApiEndpoint =
    Api.base.in(paths).zServerLogic[Any](segs => ZIO.fail(ApiError(ErrorKind.NotFound, s"没有这个接口：/api/${segs.mkString("/")}")))

  /** 平台自带的接口：登录、退出、登录状态、实时刷新。 */
  def platformEndpoints(auth: Auth, bus: ChangeBus): List[ApiEndpoint] =
    val login: ApiEndpoint =
      AuthApi.login
        .in(extractFromRequest(sourceOf))
        .zServerLogic((req, source) => auth.login(req.password, source))
    val logout: ApiEndpoint = AuthApi.logout.zServerSecurityLogic(auth.authenticate).serverLogic[Any](_ => _ => ZIO.succeed(auth.logoutCookie))
    val session: ApiEndpoint =
      AuthApi.session.zServerSecurityLogic(auth.authenticate).serverLogic[Any](p => _ => ZIO.succeed(AuthSession(p.expiresAt.getEpochSecond)))
    val sse: ApiEndpoint = Endpoints.securedFeed(Api.changes)(_ => ZIO.succeed(bus.subscribe))(auth)
    List(login, logout, session, sse)

  /** 请求来源：经本机反向代理时取 X-Forwarded-For 的最后一跳（代理自己加上的），否则取连接对端地址。 */
  private def sourceOf(req: sttp.tapir.model.ServerRequest): String =
    val peer = req.connectionInfo.remote.flatMap(a => Option(a.getAddress)).map(_.getHostAddress)
    val viaLocalProxy = peer.exists(p => p.startsWith("127.") || p == "0:0:0:0:0:0:0:1" || p == "::1")
    val forwarded = req.header("X-Forwarded-For").flatMap(_.split(',').map(_.trim).filter(_.nonEmpty).lastOption)
    (if viaLocalProxy then forwarded.orElse(peer) else peer).getOrElse("unknown")

  /** 接口描述（OpenAPI 3.1 JSON）。 */
  def openApi(endpoints: List[AnyEndpoint], version: String): String =
    OpenAPIDocsInterpreter().toOpenAPI(endpoints, "muliao", version).asJson.printWith(Printer.spaces2.copy(dropNullValues = true))

  /** 请求体上限。zio-http 默认只收约 100KB，附件上传（截图、语音、文档）会被 413 拒收；请求体整个读进内存，
    * 所以仍留一个远大于日常附件的值，只防误拖进超大文件把内存撑爆，不是业务限制。 */
  private val MaxRequestBytes: Int = 256 * 1024 * 1024

  /** 起服务，直到 scope 结束。`domain` 是各领域的接口；返回实际监听的端口（配置 0 时随机）。 */
  def serve(
      settings: ServerSettings,
      auth: Auth,
      bus: ChangeBus,
      domain: List[Route],
      version: String
  ): ZIO[Scope, Throwable, Int] =
    import Endpoints.*
    val api: List[ApiEndpoint] = platformEndpoints(auth, bus) ++ domain.map(_(auth))
    val docJson  = openApi((api :+ openApiEndpoint.public(_ => ZIO.succeed(""))).map(_.endpoint), version)
    val docs     = openApiEndpoint.public(_ => ZIO.succeed(docJson))
    val all      = api ++ List(docs, apiNotFound, StaticFiles.serverEndpoint(settings.webDir))
    val routes   = ZioHttpInterpreter(serverOptions).toHttp(all)
    val config   = Server.Config.default
      .binding(settings.host, settings.port)
      .disableRequestStreaming(MaxRequestBytes)
      .responseCompression(Server.Config.ResponseCompressionConfig.default)
    for
      started <- Promise.make[Nothing, Int]
      _ <- (Server.install(routes).flatMap(started.succeed) *> ZIO.never)
             .provide(Server.live, ZLayer.succeed(config))
             .forkScoped
      port <- started.await
      _    <- ZIO.logInfo(s"HTTP 服务已启动：${settings.host}:$port")
    yield port

  /** 解码失败、找不到接口、未处理的异常也回统一错误体；异常细节只进日志。 */
  private val serverOptions: ZioHttpServerOptions[Any] =
    ZioHttpServerOptions.customiseInterceptors
      .defaultHandlers(
        msg => ValuedEndpointOutput(jsonBody[ApiError], ApiError(ErrorKind.Invalid, msg)),
        notFoundWhenRejected = true
      )
      .exceptionHandler(ExceptionHandler.pure[Task] { _ =>
        Some(
          ValuedEndpointOutput(
            statusCode.and(jsonBody[ApiError]),
            (ErrorKind.Internal.status, ApiError(ErrorKind.Internal, "内部错误，详情见服务日志"))
          )
        )
      })
      .options // 未处理异常由默认的 serverLog 记进日志

