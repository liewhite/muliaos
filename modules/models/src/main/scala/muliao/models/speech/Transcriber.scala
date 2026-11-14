package muliao.models.speech

import muliao.models.llm.JsonPath.*
import muliao.platform.AppError
import muliao.platform.config.ConfigSection
import muliao.platform.http.OutboundHttp
import zio.*
import zio.http.{Body, Header, Headers, Request, URL}
import zio.json.*
import zio.json.ast.Json

import java.util.Base64

/** 配置文件 `[transcription]` 段（配了才有语音输入）：百炼录音识别。
  * @param maxBytes      一段录音最大字节数
  * @param vocabulary    热词及权重
  * @param languageHints 语种提示，如 `["zh", "en"]`
  */
@jsonMemberNames(SnakeCase)
final case class TranscriptionSettings(
    apiKey: String,
    model: String,
    baseUrl: String = "https://dashscope.aliyuncs.com/api/v1",
    maxBytes: Long = 10L * 1024 * 1024,
    vocabulary: Map[String, Int] = Map.empty,
    languageHints: List[String] = Nil
) derives JsonCodec:
  override def toString: String = s"TranscriptionSettings($model, $baseUrl)"

object TranscriptionSettings:
  val section: ConfigSection[TranscriptionSettings] = ConfigSection("transcription")

/** 浏览器录下的音频格式。Chrome/Firefox 录成 webm 或 ogg（Opus），iOS Safari 录成 mp4（AAC）。 */
enum AudioFormat(val id: String, val mediaTypes: Set[String]):
  case Webm extends AudioFormat("webm", Set("audio/webm"))
  case Ogg  extends AudioFormat("ogg", Set("audio/ogg"))
  case Mp4  extends AudioFormat("mp4", Set("audio/mp4"))
  case M4a  extends AudioFormat("m4a", Set("audio/x-m4a", "audio/m4a"))
  case Aac  extends AudioFormat("aac", Set("audio/aac"))
  case Mp3  extends AudioFormat("mp3", Set("audio/mpeg", "audio/mp3"))
  case Wav  extends AudioFormat("wav", Set("audio/wav", "audio/x-wav", "audio/wave"))
  case Flac extends AudioFormat("flac", Set("audio/flac"))
  case Amr  extends AudioFormat("amr", Set("audio/amr"))

object AudioFormat:
  /** 按媒体类型认格式（忽略大小写和 `;codecs=` 参数）。 */
  def of(mediaType: String): Either[AppError, AudioFormat] =
    val bare = mediaType.split(';').head.trim.toLowerCase
    values.find(_.mediaTypes(bare)).toRight(
      AppError.invalid(s"不支持的音频类型 $mediaType（支持：${values.flatMap(_.mediaTypes).mkString("、")}）")
    )

/** 语音转文字：界面录下的一段语音转成文字（中英混杂原样输出），录音不落盘。
  *
  * 用户在界面上同步等待，所以不重试：服务出错或不可达时把原因交给界面，由用户决定重录。用专门的语音识别模型，不用全模态对话模型
  * （它会把语音当成提问来回答，还要先思考，慢）。
  */
trait Transcriber:
  def maxBytes: Long

  /** 转写出的文字（首尾空白已去掉）；听不到人说话时为空。 */
  def transcribe(audio: Chunk[Byte], mediaType: String): IO[AppError, String]

object Transcriber:
  val Timeout: Duration = 60.seconds

  val layer: ZLayer[TranscriptionSettings & OutboundHttp, Nothing, Transcriber] = ZLayer.fromFunction(Dashscope.apply)

  /** 百炼录音识别的同步接口（multimodal-generation，非流式）：音频以 data URI 内联，一次返回整段文字。 */
  final case class Dashscope(settings: TranscriptionSettings, http: OutboundHttp) extends Transcriber:
    def maxBytes: Long = settings.maxBytes

    private[speech] def body(audio: Chunk[Byte], mediaType: String, format: AudioFormat): Json =
      val data = Base64.getEncoder.encodeToString(audio.toArray)
      // parameters.format 必填
      val parameters = objOpt(
        "format"         -> Some(s(format.id)),
        "vocabulary"     -> Option.when(settings.vocabulary.nonEmpty)(Json.Obj(Chunk.fromIterable(settings.vocabulary.map((k, v) => k -> n(v.toLong))))),
        "language_hints" -> Option.when(settings.languageHints.nonEmpty)(arr(settings.languageHints.map(s)))
      )
      obj(
        "model" -> s(settings.model),
        "input" -> obj("messages" -> arr(List(obj(
          "role"    -> s("user"),
          "content" -> arr(List(obj("type" -> s("input_audio"), "input_audio" -> obj("data" -> s(s"data:$mediaType;base64,$data")))))
        )))),
        "parameters" -> parameters
      )

    def transcribe(audio: Chunk[Byte], mediaType: String): IO[AppError, String] =
      for
        format <- ZIO.fromEither(AudioFormat.of(mediaType))
        _      <- ZIO.when(audio.isEmpty)(ZIO.fail(AppError.invalid("录音是空的")))
        _      <- ZIO.when(audio.size > settings.maxBytes)(ZIO.fail(AppError.invalid(s"录音太大（上限 ${settings.maxBytes} 字节）")))
        url    <- ZIO.fromEither(URL.decode(s"${settings.baseUrl.stripSuffix("/")}/services/aigc/multimodal-generation/generation"))
                    .mapError(e => AppError.invalid(s"转写服务地址不对：${e.getMessage}"))
        req = Request
                .post(url, Body.fromString(body(audio, mediaType.split(';').head.trim.toLowerCase, format).toJson))
                .addHeaders(Headers(
                  Header.Authorization.Bearer(settings.apiKey),
                  Header.ContentType(zio.http.MediaType.application.json),
                  Header.Custom("X-DashScope-SSE", "disable")
                ))
        started <- Clock.nanoTime
        resp    <- http.send(req).timeoutFail(AppError.Unavailable(s"转写超过 ${Timeout.toSeconds} 秒没有完成"))(Timeout)
        textRaw <- resp.body.asString.mapError(e => AppError.Unavailable(s"读转写结果失败：${e.getMessage}", Some(e)))
        json    <- ZIO.fromEither(textRaw.fromJson[Json]).mapError(e => AppError.External(s"转写结果不是 JSON：$e"))
        text    <- ZIO.fromOption(json.at("output").at("text").str)
                     .orElseFail(AppError.External(s"转写结果不合预期：${textRaw.take(200)}"))
        ended   <- Clock.nanoTime
        _ <- ZIO.logInfo(f"语音转写：${settings.model} ${format.id} ${audio.size} 字节 → ${text.trim.length} 字，${(ended - started) / 1e9}%.1f 秒")
      yield text.trim
    end transcribe
