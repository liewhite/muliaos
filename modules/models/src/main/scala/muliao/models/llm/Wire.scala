package muliao.models.llm

import muliao.platform.AppError
import zio.Chunk
import zio.json.ast.Json

/** 一种协议的线上格式：请求体怎么写、流式事件怎么读成 [[ReplyEvent]]、模型列表怎么查。全是纯函数，收发在 [[Models]] 里。
  *
  * 加一种协议就加一个实现，并在 [[Wire.of]] 里登记。
  */
trait Wire:
  def protocol: Protocol

  /** 请求的地址（`base` 是配置里的服务商地址，已去掉末尾的 `/`）。 */
  def endpoint(base: String): String

  /** 鉴权等请求头。 */
  def headers(apiKey: String): Seq[(String, String)]

  /** 请求体。`req` 里的图片已按服务商限制整理好（[[ImagePrep]]）。
    * @param maxTokens 回复最多多少 token（请求给的优先，其次服务商配置的）；都没给时用协议自己的默认
    */
  def body(model: String, req: ModelRequest, maxTokens: Option[Int]): Json

  /** 一次请求最多带几张图片（没有已知上限为空）。 */
  def maxImages: Option[Int]

  /** 读流的状态。 */
  type State
  def initial: State

  /** 读一条事件：给出新状态和要报告的事件。流内错误、违反协议的事件以失败返回（暂时性的归 `Unavailable`）。 */
  def step(s: State, e: SseEvent): Either[AppError, (State, Chunk[ReplyEvent])]

  /** 查模型列表：第一页（`cursor` 为空）或下一页的地址。 */
  def modelsUrl(base: String, cursor: Option[String]): String

  /** 一页模型列表里的模型名，以及下一页的游标（没有下一页为空）。 */
  def modelsPage(page: Json): (Chunk[String], Option[String])

object Wire:
  def of(p: Protocol): Wire = p match
    case Protocol.Anthropic       => AnthropicWire
    case Protocol.OpenAiResponses => OpenAiResponsesWire

  /** 流在给出完整回复之前就结束了：多半是连接中途断了，算暂时性。 */
  val endedEarly: AppError = AppError.Unavailable("模型的回复流没收完就断了")

  def outOfProtocol(why: String): AppError = AppError.External(s"模型的回复流不合协议：$why")

/** 读流时攒下的各段与本次要报告的事件。每个事件先作用到 [[ReplyBuilder]] 上（核对与已有的段对得上）再报告。 */
private[llm] final case class Emitted(builder: ReplyBuilder, events: Chunk[ReplyEvent]):
  def emit(e: ReplyEvent): Either[AppError, Emitted] =
    builder(e).map(b => Emitted(b, events :+ e)).left.map(Wire.outOfProtocol)

  def parts: Chunk[ReplyPart] = builder.parts

private[llm] object Emitted:
  def from(b: ReplyBuilder): Emitted = Emitted(b, Chunk.empty)
