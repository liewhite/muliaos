package muliao.contract

import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

/** 模型概况：默认模型（`<服务商>:<模型名>`）与配置了的服务商。 */
final case class ModelsOverview(defaultModel: String, providers: List[String]) derives JsonCodec, Schema

/** 语音输入是否可用；可用时一段录音的字节上限。 */
final case class TranscriptionInfo(enabled: Boolean, maxBytes: Option[Long]) derives JsonCodec, Schema

final case class Transcript(text: String) derives JsonCodec, Schema

/** 模型接入与外部服务的接口。 */
object ModelsApi:
  val overview: Endpoint[Option[String], Unit, ApiError, ModelsOverview, Any] =
    Api.secure.get.in("models").out(jsonBody[ModelsOverview]).name("modelsOverview")

  val listModels: Endpoint[Option[String], String, ApiError, List[String], Any] =
    Api.secure.get
      .in("models" / path[String]("provider"))
      .out(jsonBody[List[String]])
      .name("listProviderModels")
      .description("实时向服务商查询它提供的模型名（同步等待，最多约 30 秒一页）。")

  val transcriptionInfo: Endpoint[Option[String], Unit, ApiError, TranscriptionInfo, Any] =
    Api.secure.get.in("transcription").out(jsonBody[TranscriptionInfo]).name("transcriptionInfo")

  val transcribe: Endpoint[Option[String], (String, Array[Byte]), ApiError, Transcript, Any] =
    Api.secure.post
      .in("transcription")
      .in(header[String]("Content-Type"))
      .in(byteArrayBody)
      .out(jsonBody[Transcript])
      .name("transcribe")
      .description("把一段录音（请求体，Content-Type 为音频类型）转成文字。录音不落盘；服务出错时直接报错，不重试。")

  val all = List(overview, listModels, transcriptionInfo, transcribe)
