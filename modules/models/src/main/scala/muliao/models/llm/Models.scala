package muliao.models.llm

import muliao.platform.AppError
import muliao.platform.config.ConfigSection
import muliao.platform.http.OutboundHttp
import zio.*
import zio.http.{Body, Header, Headers, Request, URL}
import zio.json.*
import zio.json.ast.Json
import zio.stream.*

/** 一个模型服务商。
  * @param maxConcurrency  同时最多几个请求（全部 agent 共享，超出的排队）
  * @param maxOutputTokens 回复最多多少 token（不给时：Anthropic 用协议默认值，OpenAI 用模型自己的）
  */
@jsonMemberNames(SnakeCase)
final case class ProviderSettings(
    protocol: Protocol,
    baseUrl: String,
    apiKey: String,
    maxConcurrency: Int = 4,
    maxOutputTokens: Option[Int] = None
) derives JsonCodec:
  override def toString: String = s"ProviderSettings($protocol, $baseUrl, maxConcurrency=$maxConcurrency)"

/** 配置文件 `[models]` 段。
  * @param defaultModel       没指定模型的 agent 用哪个，`<服务商>:<模型名>`
  * @param providers          服务商：`[models.providers.<名字>]`
  * @param idleTimeoutSeconds 回复流多久没有任何数据（含心跳）就算断了
  */
@jsonMemberNames(SnakeCase)
final case class ModelSettings(
    defaultModel: ModelRef,
    providers: Map[String, ProviderSettings],
    idleTimeoutSeconds: Int = 180
) derives JsonCodec

object ModelSettings:
  val section: ConfigSection[ModelSettings] = ConfigSection("models")

  def validate(s: ModelSettings): Either[String, ModelSettings] =
    val bad = s.providers.collect {
      case (n, p) if p.maxConcurrency < 1                    => s"服务商 $n 的 max_concurrency 至少是 1"
      case (n, p) if !p.baseUrl.matches("^https?://.+")      => s"服务商 $n 的 base_url 要以 http:// 或 https:// 开头"
      case (n, _) if !n.matches("^[A-Za-z0-9_.-]+$")         => s"服务商名 $n 只能用字母、数字和 _.-"
    }.toList
    val missingDefault = Option.unless(s.providers.contains(s.defaultModel.provider))(s"默认模型 ${s.defaultModel} 的服务商没有配置")
    (bad ++ missingDefault) match
      case Nil  => Right(s)
      case errs => Left(s"配置文件 [models] 段不对：${errs.mkString("；")}")

/** 模型接入：向服务商请求一次、以流的形式得到一条回复；查服务商的模型列表。不做 agent 循环，也不重试（是否重试、重试多久由调用方
  * 按 [[AppError]] 的类别决定：`Unavailable` 是暂时性的）。
  */
trait Models:
  def defaultModel: ModelRef

  /** 配置了的服务商名。 */
  def providers: Chunk[String]

  /** 这个模型的服务商是否配置了（建 agent、改模型时校验）。 */
  def check(model: ModelRef): IO[AppError, Unit]

  /** 请求一次。占用服务商的一个并发名额直到流结束；最后一个事件是 [[ReplyEvent.Finished]]。图片在这里按服务商限制整理。 */
  def request(model: ModelRef, req: ModelRequest): ZStream[Any, AppError, ReplyEvent]

  /** 服务商提供的模型名（界面上同步等待，每页最多等 30 秒）。 */
  def listModels(provider: String): IO[AppError, Chunk[String]]

object Models:
  val ListTimeout: Duration = 30.seconds

  val layer: ZLayer[ModelSettings & OutboundHttp, AppError, Models] =
    ZLayer.fromZIO {
      for
        settings <- ZIO.serviceWithZIO[ModelSettings](s => ZIO.fromEither(ModelSettings.validate(s)).mapError(AppError.invalid))
        http     <- ZIO.service[OutboundHttp]
        limits   <- ZIO.foreach(settings.providers.toList)((n, p) => Semaphore.make(p.maxConcurrency.toLong).map(n -> _))
      yield Live(settings, http, limits.toMap)
    }

  private final case class Live(settings: ModelSettings, http: OutboundHttp, limits: Map[String, Semaphore]) extends Models:
    val defaultModel: ModelRef   = settings.defaultModel
    val providers: Chunk[String] = Chunk.fromIterable(settings.providers.keys.toList.sorted)

    private def provider(name: String): IO[AppError, ProviderSettings] =
      ZIO.fromOption(settings.providers.get(name)).orElseFail(AppError.invalid(s"没有配置服务商 $name（已配置：${providers.mkString("、")}）"))

    def check(model: ModelRef): IO[AppError, Unit] = provider(model.provider).unit

    private def base(p: ProviderSettings) = p.baseUrl.stripSuffix("/")

    private def url(s: String): IO[AppError, URL] =
      ZIO.fromEither(URL.decode(s)).mapError(e => AppError.invalid(s"服务商地址不对：$s（${e.getMessage}）"))

    private def withHeaders(req: Request, hs: Seq[(String, String)]): Request =
      req.addHeaders(Headers(hs.map((k, v) => Header.Custom(k, v))*))

    def request(model: ModelRef, req: ModelRequest): ZStream[Any, AppError, ReplyEvent] =
      ZStream.unwrapScoped {
        for
          p    <- provider(model.provider)
          wire  = Wire.of(p.protocol)
          idle  = settings.idleTimeoutSeconds.seconds
          _    <- limits(model.provider).withPermitScoped
          msgs <- ZIO.attemptBlocking(ImagePrep.prepare(req.messages, wire.maxImages)).mapError(e => AppError.internal("整理图片失败", e))
          body  = wire.body(model.model, req.copy(messages = msgs), req.maxTokens.orElse(p.maxOutputTokens))
          u    <- url(wire.endpoint(base(p)))
          post  = withHeaders(Request.post(u, Body.fromString(body.toJson)), wire.headers(p.apiKey) :+ ("content-type" -> "application/json"))
          resp <- http.open(post).timeoutFail(AppError.Unavailable(s"${model.provider} 超过 ${idle.toSeconds} 秒没有响应"))(idle)
        yield decode(wire, resp.body.asStream, idle, model.provider)
      }

    def listModels(name: String): IO[AppError, Chunk[String]] =
      provider(name).flatMap { p =>
        val wire = Wire.of(p.protocol)
        def page(cursor: Option[String]): IO[AppError, (Chunk[String], Option[String])] =
          for
            u    <- url(wire.modelsUrl(base(p), cursor))
            resp <- http.send(withHeaders(Request.get(u), wire.headers(p.apiKey))).timeoutFail(AppError.Unavailable(s"查 $name 的模型列表超时"))(ListTimeout)
            text <- resp.body.asString.mapError(e => AppError.Unavailable(s"读 $name 的模型列表失败：${e.getMessage}", Some(e)))
            json <- ZIO.fromEither(text.fromJson[Json]).mapError(e => AppError.External(s"$name 的模型列表不是 JSON：$e"))
          yield wire.modelsPage(json)
        def loop(cursor: Option[String], acc: Chunk[String]): IO[AppError, Chunk[String]] =
          page(cursor).flatMap {
            case (names, Some(next)) => loop(Some(next), acc ++ names)
            case (names, None)       => ZIO.succeed(acc ++ names)
          }
        loop(None, Chunk.empty)
      }

  /** 把响应体读成事件：SSE 解析 → 按协议逐条读 → 到 [[ReplyEvent.Finished]] 为止；没读到它流就断了算暂时性失败。 */
  private[llm] def decode(wire: Wire, bytes: ZStream[Any, Throwable, Byte], idle: Duration, provider: String): ZStream[Any, AppError, ReplyEvent] =
    val events = bytes
      .via(Sse.decode)
      .mapError(e => AppError.Unavailable(s"读 $provider 的回复中断：${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}", Some(e)))
      .timeoutFail(AppError.Unavailable(s"$provider 的回复超过 ${idle.toSeconds} 秒没有动静"))(idle)
      .mapAccumZIO(wire.initial)((st, e) => ZIO.fromEither(wire.step(st, e)))
      .flattenChunks
    (events ++ ZStream.fail(Wire.endedEarly)).takeUntil {
      case _: ReplyEvent.Finished => true
      case _                      => false
    }
