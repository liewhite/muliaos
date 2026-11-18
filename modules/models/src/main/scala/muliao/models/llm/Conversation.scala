package muliao.models.llm

import muliao.contract.{Labeled, Labels}
import muliao.toolkit.ToolContent
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** 模型服务商说的协议。加一种协议：加一个枚举值和它的 [[Wire]] 实现。 */
enum Protocol(val label: String) extends Labeled:
  case Anthropic       extends Protocol("anthropic")
  case OpenAiResponses extends Protocol("openai-responses")

object Protocol extends Labels[Protocol]("协议", Protocol.values)

/** 交给模型的一个工具：名字、说明、参数的 JSON Schema。 */
final case class ToolDef(name: String, description: String, inputSchema: Json)

/** 用户一方（含工具结果）的一块内容。 */
enum UserPart:
  case Text(text: String)

  /** 图片（附件或工具返回的）。发给模型前按服务商限制整理，见 [[ImageFit]]。 */
  case Image(mediaType: String, data: Chunk[Byte])

  case Pdf(data: Chunk[Byte])

  /** 一次工具调用的结果。`content` 里的图片同样会被整理。 */
  case ToolResult(callId: String, content: Chunk[ToolContent], isError: Boolean)

/** 协议给的、回传时要原样带上的数据（输出项编号、`phase`、整个思考块等）。
  *
  * 只有协议给出完整一项时才有：生成到一半被打断的段没有它。回传时只用与目标协议相同的。
  */
final case class Seal(protocol: Protocol, data: Json) derives JsonCodec

/** 模型回复的一段。 */
enum ReplyPart derives JsonCodec:
  case Text(text: String, seal: Option[Seal] = None)

  /** 模型自己的思考块（整块原样在 `block` 里）。平台不请求、不展示思考，只因协议要求后续请求原样回传而保留；
    * 只在收完的回复里出现（读流中的实时事件不带它，也就不占界面上的段序号）。
    */
  case Reasoning(block: Seal)

  /** 工具调用；`args` 是模型生成的参数原文（应是 JSON 对象，生成坏了时由执行方按「请重试」处理）。 */
  case ToolCall(id: String, name: String, args: String, seal: Option[Seal] = None)

/** 一次请求的 token 用量。`input` 含缓存读、写的部分。 */
final case class Usage(input: Long, output: Long, cacheRead: Long, cacheWrite: Long) derives JsonCodec

object Usage:
  val zero: Usage = Usage(0, 0, 0, 0)

/** 模型的一条完整回复。
  * @param model      服务端报告的模型名
  * @param responseId 服务端给的回复编号（排查用）
  * @param stopReason 服务端给的结束原因（如 `end_turn`、`max_tokens`）
  */
final case class Reply(
    parts: Chunk[ReplyPart],
    protocol: Protocol,
    model: String,
    responseId: Option[String],
    stopReason: Option[String],
    usage: Usage
) derives JsonCodec:
  def toolCalls: Chunk[ReplyPart.ToolCall] = parts.collect { case c: ReplyPart.ToolCall => c }

  /** 读流时报告过的段（不含只为回传而留的思考块）。 */
  def shown: Chunk[ShownPart] = ShownPart.of(parts)

/** 展示给人、实时报告的段：正文与工具调用。思考块不在其中，由类型保证它不进实时事件与界面。 */
type ShownPart = ReplyPart.Text | ReplyPart.ToolCall

object ShownPart:
  def of(parts: Chunk[ReplyPart]): Chunk[ShownPart] = parts.collect {
    case t: ReplyPart.Text     => t
    case c: ReplyPart.ToolCall => c
  }

/** 对话里的一条消息。 */
enum Message:
  case User(parts: Chunk[UserPart])
  case Assistant(parts: Chunk[ReplyPart])

/** 一次模型请求。
  * @param system    固定说明（缓存前缀的开头）
  * @param maxTokens 回复最多多少 token；不给时用服务商配置的
  */
final case class ModelRequest(
    system: String,
    tools: Chunk[ToolDef],
    messages: Chunk[Message],
    maxTokens: Option[Int] = None,
    /** 前缀相同的一组请求的标识（如会话编号）：OpenAI Responses 据此把请求路由到同一份提示缓存；Anthropic 靠缓存断点，不用它 */
    cacheKey: Option[String] = None
)

/** `<服务商>:<模型名>`，如 `claude:claude-opus-4`。 */
final case class ModelRef(provider: String, model: String):
  override def toString: String = s"$provider:$model"

object ModelRef:
  def parse(s: String): Either[String, ModelRef] =
    s.split(":", 2) match
      case Array(p, m) if p.nonEmpty && m.nonEmpty => Right(ModelRef(p, m))
      case _                                        => Left(s"模型要写成 <服务商>:<模型名>，收到 $s")

  given JsonCodec[ModelRef] = JsonCodec.string.transformOrFail(parse, _.toString)
