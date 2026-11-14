package muliao.migrate

import zio.json.*
import zio.json.ast.Json

/** 旧版 `messages.payload` 的存储形态（只取迁移用得到的字段，别的字段忽略）。 */
object OldPayload:

  /** 一段内容；`meta` 是协议要回传的数据（Anthropic 的签名，OpenAI 的输出项编号与加密推理）。 */
  @jsonDiscriminator("type")
  sealed trait Part
  object Part:
    @jsonHint("text") final case class Text(text: String, meta: Option[Json] = None) extends Part
    @jsonHint("thinking") final case class Thinking(text: String, meta: Option[Json] = None) extends Part
    @jsonHint("tool_call") final case class ToolCall(id: String, name: String, args: String, meta: Option[Json] = None) extends Part
    @jsonHint("tool_result") final case class ToolResult(
        call_id: String,
        content: List[Content],
        is_error: Option[Boolean] = None,
        metadata: Option[ResultMeta] = None
    ) extends Part
    /** 附件：`url` 是 `/api/files/<文件编号>/<名字>` */
    @jsonHint("file") final case class File(media_type: String, url: String) extends Part
    given JsonDecoder[Part] = DeriveJsonDecoder.gen

  @jsonDiscriminator("type")
  sealed trait Content
  object Content:
    @jsonHint("text") final case class Text(text: String) extends Content
    @jsonHint("file") final case class File(media_type: String, url: String) extends Content
    given JsonDecoder[Content] = DeriveJsonDecoder.gen

  final case class ResultMeta(muliao_finished_at: Option[String] = None)
  object ResultMeta:
    given JsonDecoder[ResultMeta] = DeriveJsonDecoder.gen

  /** 请求里某段信件内容是什么：消息（`sender`：user / trigger / issue / delegation）或事件（`event`：job:12、thread:…、wake_me）。 */
  final case class Mark(part: Int, sender: Option[String] = None, event: Option[String] = None, check_in: Option[Boolean] = None)
  object Mark:
    given JsonDecoder[Mark] = DeriveJsonDecoder.gen

  final case class Meta(
      muliao_parts: Option[List[Mark]] = None,
      muliao_run_context: Option[Int] = None,
      muliao_interrupted: Option[Boolean] = None
  )
  object Meta:
    given JsonDecoder[Meta] = DeriveJsonDecoder.gen

  final case class Usage(input_tokens: Long = 0, output_tokens: Long = 0, cache_read_tokens: Long = 0, cache_write_tokens: Long = 0)
  object Usage:
    given JsonDecoder[Usage] = DeriveJsonDecoder.gen

  final case class Message(
      role: String,
      parts: List[Part],
      protocol: Option[String] = None,
      model: Option[String] = None,
      finish_reason: Option[String] = None,
      usage: Option[Usage] = None,
      metadata: Option[Meta] = None
  )
  object Message:
    given JsonDecoder[Message] = DeriveJsonDecoder.gen

  def decode(payload: String): Either[String, Message] = payload.fromJson[Message]

  /** `/api/files/<编号>/<名字>` 里的文件编号。 */
  def fileId(url: String): Option[String] =
    url.stripPrefix("/api/files/").split('/').headOption.filter(_ => url.startsWith("/api/files/")).filter(_.nonEmpty)
