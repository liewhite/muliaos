package muliao.models

import muliao.contract.{ModelsOverview, Transcript, TranscriptionInfo}
import muliao.models.llm.{ModelRef, ModelRequest, Models, ReplyEvent}
import muliao.models.speech.Transcriber
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import muliao.platform.http.{HttpSettings, OutboundHttp, ProxySettings, SystemProxy}
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import zio.*
import zio.http.{Body, Header, Request, URL}
import zio.json.*
import zio.stream.ZStream
import zio.test.*
import zio.test.TestAspect.*

object ModelsHttpSpec extends ZIOSpecDefault:

  val models: Models = new Models:
    val defaultModel                                   = ModelRef("claude", "opus")
    val providers                                      = Chunk("claude", "openai")
    def check(model: ModelRef)                         = ZIO.unit
    def request(model: ModelRef, req: ModelRequest)    = ZStream.empty: ZStream[Any, AppError, ReplyEvent]
    def listModels(provider: String)                   =
      if provider == "claude" then ZIO.succeed(Chunk("opus", "sonnet")) else ZIO.fail(AppError.invalid(s"没有配置服务商 $provider"))

  val transcriber: Transcriber = new Transcriber:
    val maxBytes                                         = 100L
    def transcribe(audio: Chunk[Byte], mediaType: String) = ZIO.succeed(s"${audio.size} bytes of $mediaType")

  val http = ZLayer.succeed(HttpSettings(requestTimeoutSeconds = 10)) ++ SystemProxy.fixed(ProxySettings.direct) >>> OutboundHttp.layer

  def serve(t: Option[Transcriber]): ZIO[Scope & OutboundHttp, Any, (String, String)] =
    val settings = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
    for
      auth     <- Auth.layer.build.provideSomeLayer[Scope](ZLayer.succeed(settings)).map(_.get[Auth])
      bus      <- ChangeBus.layer.build.map(_.get[ChangeBus])
      port     <- HttpServer.serve(settings, auth, bus, ModelsHttp.endpoints(auth, models, t), "test")
      base      = s"http://127.0.0.1:$port"
      login    <- ZIO.serviceWithZIO[OutboundHttp](_.send(Request.post(URL.decode(s"$base/api/auth/login").toOption.get, Body.fromString("""{"password":"pw"}"""))
                    .addHeader(Header.ContentType(zio.http.MediaType.application.json))))
    yield (base, login.rawHeader("set-cookie").get.takeWhile(_ != ';'))

  def get(base: String, cookie: String, path: String) =
    ZIO.serviceWithZIO[OutboundHttp](_.send(Request.get(URL.decode(base + path).toOption.get).addHeader("cookie", cookie)).flatMap(_.body.asString.orDie).either)

  def spec = suite("模型与外部服务的接口")(
    test("模型概况、服务商的模型列表；不认识的服务商是请求错误") {
      ZIO.scoped {
        for
          (base, cookie) <- serve(None)
          overview       <- get(base, cookie, "/api/models")
          list           <- get(base, cookie, "/api/models/claude")
          bad            <- get(base, cookie, "/api/models/nope")
          info           <- get(base, cookie, "/api/transcription")
        yield assertTrue(
          overview.toOption.flatMap(_.fromJson[ModelsOverview].toOption).contains(ModelsOverview("claude:opus", List("claude", "openai"))),
          list.toOption.flatMap(_.fromJson[List[String]].toOption).contains(List("opus", "sonnet")),
          bad.left.exists(_.message.contains("400")),
          info.toOption.flatMap(_.fromJson[TranscriptionInfo].toOption).contains(TranscriptionInfo(false, None))
        )
      }
    },
    test("语音：配了才可用，录音原样交给转写，类型取自 Content-Type") {
      ZIO.scoped {
        for
          (base, cookie) <- serve(Some(transcriber))
          info           <- get(base, cookie, "/api/transcription")
          resp <- ZIO.serviceWithZIO[OutboundHttp](
                    _.send(Request.post(URL.decode(s"$base/api/transcription").toOption.get, Body.fromArray(Array[Byte](1, 2, 3)))
                      .addHeader("cookie", cookie).addHeader("content-type", "audio/webm;codecs=opus")).flatMap(_.body.asString.orDie)
                  )
        yield assertTrue(
          info.toOption.flatMap(_.fromJson[TranscriptionInfo].toOption).contains(TranscriptionInfo(true, Some(100L))),
          resp.fromJson[Transcript].toOption.contains(Transcript("3 bytes of audio/webm;codecs=opus"))
        )
      }
    }
  ).provideLayerShared(http.orDie) @@ withLiveClock @@ sequential
