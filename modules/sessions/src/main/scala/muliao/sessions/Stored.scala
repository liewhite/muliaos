package muliao.sessions

import muliao.models.llm.{ReplyPart, Usage}
import zio.Chunk
import zio.json.*

/** 时间线里一条的内容（存成 JSON）。 */
enum EntryBody derives JsonCodec:
  /** 本次运行上下文：当前时间、回答交给谁、资源提醒等，放在一次运行新增的第一条请求里 */
  case Context(text: String)
  case Mail(letter: Letter)
  /** 模型回复。`interrupted`：生成到一半被停止，只保存了已生成的部分 */
  case Reply(parts: Chunk[ReplyPart], model: String, usage: Usage, stopReason: Option[String], interrupted: Boolean)
  /** 压缩点：`from` 之前的时间线由 `summary` 代替（原文仍保留） */
  case Compaction(summary: String, from: Long)
  case Notice(kind: NoticeKind, text: String)

  def encode: String = (this: EntryBody).toJson

  def tag: String = this match
    case _: Context    => "context"
    case _: Mail       => "letter"
    case _: Reply      => "reply"
    case _: Compaction => "compaction"
    case _: Notice     => "notice"

enum NoticeKind derives JsonCodec:
  case Failed, Stopped, Info

/** 工具结果里的一块（图片存附件，只留内容摘要）。 */
enum StoredPart derives JsonCodec:
  case Text(text: String)
  case Image(mediaType: String, sha256: String)

final case class CallResult(content: Chunk[StoredPart], isError: Boolean) derives JsonCodec:
  def text: String = content.collect { case StoredPart.Text(t) => t }.mkString

object CallResult:
  def error(text: String): CallResult = CallResult(Chunk(StoredPart.Text(text)), isError = true)
  def ok(text: String): CallResult    = CallResult(Chunk(StoredPart.Text(text)), isError = false)

/** 工具调用的状态。 */
enum CallState(val label: String):
  /** 还没开始 */
  case Pending   extends CallState("pending")
  case Running   extends CallState("running")
  /** 挂起，见 [[Pause]] */
  case Suspended extends CallState("suspended")
  /** 用户批准了，等接着执行 */
  case Approved  extends CallState("approved")
  /** 用户批准了并选了以后不再问，等接着执行 */
  case ApprovedAlways extends CallState("approved-always")
  case Done      extends CallState("done")

object CallState:
  def parse(s: String): CallState = values.find(_.label == s).getOrElse(throw IllegalStateException(s"未知的调用状态 $s"))

/** 被打断、没有结果的调用给模型的说明。 */
object Interrupted:
  val restartedWhileRunning =
    "[这个调用在执行中被打断（服务重启），没有重新执行；它的副作用可能已经发生，先核实再决定要不要重做。]"
  val restartedBeforeStart = "[服务重启时这个调用还没开始执行，没有执行；需要的话重新调用。]"
  val stoppedWhileRunning  = "[被停止：这个调用在执行中被打断，没有重新执行；它的副作用可能已经发生，先核实再决定要不要重做。]"
  val stoppedBeforeStart   = "[被停止：这个调用没有执行。]"
  val stoppedWhileWaiting  = "[被停止：没等到答复。]"
