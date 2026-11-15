package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- 状态与种类 ----

/** 会话的工作状态。全系统（会话列表、看板、触发器防重叠、thread 列表）都用这一个判断。 */
enum WorkState(val label: String) extends Labeled:
  case Running extends WorkState("running")
  case Waiting extends WorkState("waiting")
  case Idle    extends WorkState("idle")

  /** 给人看的说法。 */
  def text: String = this match
    case Running => "运行中"
    case Waiting => "等待中"
    case Idle    => "空闲"

object WorkState extends Labels[WorkState]("工作状态", WorkState.values)

/** 运行的状态。 */
enum RunState(val label: String) extends Labeled:
  case Running   extends RunState("running")
  /** 有工具调用挂起（等用户回答、批准，或等别的 agent 回答） */
  case Suspended extends RunState("suspended")
  case Finished  extends RunState("finished")
  /** 最终失败，可重试 */
  case Failed    extends RunState("failed")
  case Stopped   extends RunState("stopped")

  /** 还没结束（运行中或挂起）。 */
  def active: Boolean = this match
    case Running | Suspended          => true
    case Finished | Failed | Stopped  => false

object RunState extends Labels[RunState]("运行状态", RunState.values):
  val activeOnes: List[RunState] = values.toList.filter(_.active)

/** 工具调用的状态。 */
enum CallState(val label: String) extends Labeled:
  /** 还没开始 */
  case Pending   extends CallState("pending")
  case Running   extends CallState("running")
  /** 挂起：等用户回答、批准，或等别的 agent 回答 */
  case Suspended extends CallState("suspended")
  /** 用户批准了，等接着执行 */
  case Approved  extends CallState("approved")
  /** 用户批准了并选了以后不再问，等接着执行 */
  case ApprovedAlways extends CallState("approved-always")
  case Done      extends CallState("done")

  /** 驱动接下来要执行它。 */
  def runnable: Boolean = this match
    case Pending | Approved | ApprovedAlways => true
    case Running | Suspended | Done          => false

object CallState extends Labels[CallState]("调用状态", CallState.values)

/** 会话来源的种类（谁开的）。 */
enum OriginKind(val label: String) extends Labeled:
  /** 用户在界面上新建 */
  case User    extends OriginKind("user")
  case Trigger extends OriginKind("trigger")
  /** 某 agent 处理某 issue 的专属会话 */
  case Issue   extends OriginKind("issue")
  /** 委派开出的 thread */
  case Thread  extends OriginKind("thread")

object OriginKind extends Labels[OriginKind]("会话来源", OriginKind.values)

/** 界面怎么展示一个工具的调用与结果（由工具声明）。 */
enum Display(val label: String) extends Labeled:
  /** 原文 */
  case Plain      extends Display("Plain")
  /** 终端输出（命令） */
  case Terminal   extends Display("Terminal")
  /** 带行号的文件内容 */
  case Code       extends Display("Code")
  /** 改动对比 */
  case Diff       extends Display("Diff")
  /** 一段 JSON（列表、详情） */
  case JsonData   extends Display("JsonData")
  /** Markdown 文本 */
  case Markdown   extends Display("Markdown")
  /** 发给用户的文件：结果是附件的 JSON（id、name、mediaType、size），界面显示成附件 */
  case SharedFile extends Display("SharedFile")

object Display extends Labels[Display]("展示方式", Display.values)

// ---- 会话列表 ----

/** 会话列表里的一行。
  * @param originRef 触发器名 / issue 号 / thread 名
  * @param parent    thread 的发起会话
  */
final case class SessionSummary(
    id: SessionId,
    agent: AgentId,
    agentName: String,
    title: String,
    origin: OriginKind,
    originRef: Option[String],
    parent: Option[SessionId],
    state: WorkState,
    unread: Boolean,
    createdAt: Long,
    updatedAt: Long
) derives JsonCodec, Schema

/** 一页会话；`next` 是取下一页时的 `before`。运行中、等待中的总在第一页最前。 */
final case class SessionPage(items: List[SessionSummary], next: Option[Long]) derives JsonCodec, Schema

final case class FileView(id: FileId, name: String, mediaType: String, size: Long) derives JsonCodec, Schema

final case class NewSession(agent: AgentId, text: String, files: List[FileId] = Nil) derives JsonCodec, Schema

final case class MessageIn(text: String, files: List[FileId] = Nil) derives JsonCodec, Schema

final case class Created(id: SessionId) derives JsonCodec, Schema

// ---- 时间线 ----

/** 工具结果里的图片：`sha256` 用 `/api/blobs/{sha256}` 取。 */
final case class ImageRef(mediaType: String, sha256: String) derives JsonCodec, Schema

final case class ResultView(text: String, images: List[ImageRef], isError: Boolean) derives JsonCodec, Schema

final case class OptionView(label: String, description: String) derives JsonCodec, Schema

final case class QuestionView(question: String, header: String, options: List[OptionView], multiSelect: Boolean) derives JsonCodec, Schema

/** 挂起的调用在等什么。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum PendingView derives JsonCodec:
  /** 等用户回答选择题 */
  case Question(questions: List[QuestionView])
  /** 等用户批准 */
  case Approval(summary: String)
  /** 等别的 agent 的 thread 回答 */
  case Agent(thread: ThreadName, agent: AgentName)

object PendingView:
  given Schema[PendingView] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

/** 一次工具调用。 */
final case class CallView(
    id: CallId,
    name: String,
    args: String,
    display: Display,
    state: CallState,
    result: Option[ResultView],
    pending: Option[PendingView],
    startedAt: Option[Long],
    finishedAt: Option[Long]
) derives JsonCodec, Schema

/** 回复的一段。`kind`：text / thinking / call。 */
final case class PartView(kind: String, text: Option[String], call: Option[CallView]) derives JsonCodec, Schema

final case class UsageView(input: Long, output: Long, cacheRead: Long, cacheWrite: Long) derives JsonCodec, Schema

/** 时间线里的一条。`kind`：
  *   - context：本次运行上下文（`text`）
  *   - letter：来信（`source` 为 USER / TRIGGER / ISSUE / DELEGATION / EVENT，`sourceDetail` 是触发器名、issue 号、发起方，`text`、`files`、`checkIn`）
  *   - reply：模型回复（`parts`、`model`、`usage`、`interrupted`）
  *   - compaction：压缩点（`text` 是摘要；它之前的部分已由摘要代替，原文仍在）
  *   - notice：失败、停止等提示（`noticeKind`：Failed / Stopped / Info，`text`）
  */
final case class TimelineItem(
    id: Long,
    kind: String,
    run: Option[Long],
    startedAt: Long,
    finishedAt: Long,
    source: Option[String] = None,
    sourceDetail: Option[String] = None,
    text: Option[String] = None,
    files: List[FileView] = Nil,
    checkIn: Boolean = false,
    parts: List[PartView] = Nil,
    model: Option[String] = None,
    usage: Option[UsageView] = None,
    interrupted: Boolean = false,
    noticeKind: Option[String] = None
) derives JsonCodec, Schema

// ---- 会话详情 ----

final case class QueuedView(id: Long, source: String, text: String, files: List[FileView], checkIn: Boolean, at: Long) derives JsonCodec, Schema

/** 进行中的异步工作在等什么。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum WaitTarget derives JsonCodec:
  /** 后台任务 */
  case Job(job: JobId)
  /** 约好的唤醒 */
  case Wake(dueAt: Long)
  /** 带回报的委派：`thread` 是做它的 thread 会话 */
  case Delegation(thread: Option[SessionId])

object WaitTarget:
  given Schema[WaitTarget] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

/** 进行中的异步工作。 */
final case class WaitItem(id: Long, target: WaitTarget, label: String, heartbeatSeconds: Option[Int], createdAt: Long) derives JsonCodec, Schema

final case class PendingCall(entry: Long, call: CallId, name: String, pending: PendingView) derives JsonCodec, Schema

/** 最近一次运行。`retryNote`：暂时失败、正在退避重试的原因。 */
final case class RunView(id: Long, state: RunState, error: Option[String], retryNote: Option[String], startedAt: Long, finishedAt: Option[Long])
    derives JsonCodec, Schema

final case class SessionDetail(
    summary: SessionSummary,
    run: Option[RunView],
    queued: List[QueuedView],
    waits: List[WaitItem],
    pending: List[PendingCall],
    /** 当前上下文大小（最近一次请求的输入 token） */
    contextTokens: Long,
    compactionThreshold: Long,
    usage: UsageView,
    /** 这个会话开出的 thread（委派），按开出的先后 */
    threads: List[SessionSummary] = Nil
) derives JsonCodec, Schema

/** 答复挂起的调用：提问给 `answers`（每题一个），批准给 `approved`（与 `always`）。 */
final case class AnswerIn(answers: Option[List[String]] = None, approved: Option[Boolean] = None, always: Boolean = false)
    derives JsonCodec, Schema

/** 会话页的实时事件（SSE 的 data）。`kind`：
  *   - snapshot：打开时正在生成的回复（`parts`）
  *   - begin：开始生成新的一条回复
  *   - part：第 `index` 段开始或整段替换（`part`）
  *   - grew：第 `index` 段长了 `text`（`growth`：Text / Thinking / ToolArgs）
  *   - changed：库里变了，重新拉取详情与时间线
  */
final case class LiveView(
    kind: String,
    index: Option[Int] = None,
    part: Option[PartView] = None,
    growth: Option[String] = None,
    text: Option[String] = None,
    parts: List[PartView] = Nil
) derives JsonCodec, Schema

// ---- 用量 ----

/** 一天、一个 agent、一个模型的用量。`input` 含缓存读、写。 */
final case class UsageLine(day: String, agent: AgentId, agentName: String, model: String, requests: Long, input: Long, output: Long, cacheRead: Long, cacheWrite: Long)
    derives JsonCodec, Schema

object SessionsApi:
  private val sessions = Api.secure.in("sessions")
  /** 一个会话下的接口（实时事件 `GET /api/sessions/{id}/live` 是 SSE，声明在服务端）。 */
  val one = sessions.in(path[SessionId]("id"))

  val list: Endpoint[Option[String], (Option[AgentId], List[OriginKind], Option[String], Option[Long], Option[Int]), ApiError, SessionPage, Any] =
    sessions.get
      .in(
        query[Option[AgentId]]("agent")
          .and(query[List[OriginKind]]("origin"))
          .and(query[Option[String]]("q"))
          .and(query[Option[Long]]("before"))
          .and(query[Option[Int]]("limit"))
      )
      .out(jsonBody[SessionPage])
      .name("listSessions")
      .description("会话列表：按最近活动倒序，运行中/等待中的置顶（第一页）。`agent` 筛选；`origin` 只要这些来源（可重复：user / trigger / issue / thread）；`q` 搜来信、回复与工具调用参数（不搜工具结果）。")

  val create: Endpoint[Option[String], NewSession, ApiError, Created, Any] =
    sessions.post.in(jsonBody[NewSession]).out(jsonBody[Created]).name("createSession").description("新建会话并发出第一条消息。")

  val get: Endpoint[Option[String], SessionId, ApiError, SessionDetail, Any] =
    one.get.out(jsonBody[SessionDetail]).name("getSession")

  val timeline: Endpoint[Option[String], SessionId, ApiError, List[TimelineItem], Any] =
    one.get.in("timeline").out(jsonBody[List[TimelineItem]]).name("sessionTimeline").description("会话的完整时间线（含压缩点之前的原文）。")

  val send: Endpoint[Option[String], (SessionId, MessageIn), ApiError, Unit, Any] =
    one.post.in("messages").in(jsonBody[MessageIn]).out(statusCode(StatusCode.NoContent)).name("sendMessage")
      .description("给会话发消息；运行中发的会在下一次请求模型前一起交给它。")

  private def action(name: String, op: String, doc: String): Endpoint[Option[String], SessionId, ApiError, Unit, Any] =
    one.post.in(name).out(statusCode(StatusCode.NoContent)).name(op).description(doc)

  val stop    = action("stop", "stopSession", "停止：保存已产生的部分，丢弃排队的消息，放弃等待中的工作，逐层停下它开出的 thread。")
  val retry   = action("retry", "retrySession", "最终失败的运行从失败处接着跑。")
  val compact = action("compact", "compactSession", "空闲时手动压缩上下文。")
  val read    = action("read", "markSessionRead", "标为已读。")

  val delete: Endpoint[Option[String], SessionId, ApiError, Unit, Any] =
    one.delete.out(statusCode(StatusCode.NoContent)).name("deleteSession").description("删除会话：忙时拒绝；它开出的 thread 保留。")

  val answer: Endpoint[Option[String], (SessionId, Long, CallId, AnswerIn), ApiError, Unit, Any] =
    one.post
      .in("calls" / path[Long]("entry") / path[CallId]("call") / "answer")
      .in(jsonBody[AnswerIn])
      .out(statusCode(StatusCode.NoContent))
      .name("answerCall")
      .description("回答 ask_user 的提问，或批准/拒绝等批准的调用。")

  val upload: Endpoint[Option[String], (String, String, Array[Byte]), ApiError, FileView, Any] =
    Api.secure.post
      .in("files")
      .in(query[String]("name"))
      .in(header[String]("Content-Type"))
      .in(byteArrayBody)
      .out(jsonBody[FileView])
      .name("uploadFile")
      .description("上传一个附件（请求体是文件内容），返回的 id 在发消息时引用。内容相同只存一份。")

  /** 下载：附件按不可信内容处理，只有常见图片内联显示，其余一律作为下载。 */
  val download: Endpoint[Option[String], FileId, ApiError, (String, String, Array[Byte]), Any] =
    Api.secure.get
      .in("files" / path[FileId]("id"))
      .out(header[String]("Content-Type").and(header[String]("Content-Disposition")).and(byteArrayBody))
      .out(header("X-Content-Type-Options", "nosniff").and(header("Content-Security-Policy", "sandbox")))
      .name("downloadFile")

  val blob: Endpoint[Option[String], String, ApiError, (String, String, Array[Byte]), Any] =
    Api.secure.get
      .in("blobs" / path[String]("sha256"))
      .out(header[String]("Content-Type").and(header[String]("Content-Disposition")).and(byteArrayBody))
      .out(header("X-Content-Type-Options", "nosniff").and(header("Content-Security-Policy", "sandbox")))
      .name("downloadBlob")
      .description("按内容摘要取工具结果里的图片。")

  val usage: Endpoint[Option[String], (Long, Long), ApiError, List[UsageLine], Any] =
    Api.secure.get
      .in("usage")
      .in(query[Long]("from").and(query[Long]("to")))
      .out(jsonBody[List[UsageLine]])
      .name("usage")
      .description("[from, to)（毫秒）之间按天（配置时区）、agent、模型汇总的 token 用量。")

  val all = List(list, create, get, timeline, send, stop, retry, compact, read, delete, answer, upload, download, blob, usage)
