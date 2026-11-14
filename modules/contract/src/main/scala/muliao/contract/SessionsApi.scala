package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- 会话列表 ----

/** 会话列表里的一行。
  * @param origin    来源：user / trigger / issue / thread
  * @param originRef 触发器名 / issue 号 / thread 名
  * @param parent    thread 的发起会话
  * @param state     工作状态：running / waiting / idle
  */
final case class SessionSummary(
    id: SessionId,
    agent: AgentId,
    agentName: String,
    title: String,
    origin: String,
    originRef: Option[String],
    parent: Option[SessionId],
    state: String,
    unread: Boolean,
    createdAt: Long,
    updatedAt: Long
) derives JsonCodec, Schema

/** 一页会话；`next` 是取下一页时的 `before`。运行中、等待中的总在第一页最前。 */
final case class SessionPage(items: List[SessionSummary], next: Option[Long]) derives JsonCodec, Schema

final case class FileView(id: String, name: String, mediaType: String, size: Long) derives JsonCodec, Schema

final case class NewSession(agent: AgentId, text: String, files: List[String] = Nil) derives JsonCodec, Schema

final case class MessageIn(text: String, files: List[String] = Nil) derives JsonCodec, Schema

final case class Created(id: SessionId) derives JsonCodec, Schema

// ---- 时间线 ----

/** 工具结果里的图片：`sha256` 用 `/api/blobs/{sha256}` 取。 */
final case class ImageRef(mediaType: String, sha256: String) derives JsonCodec, Schema

final case class ResultView(text: String, images: List[ImageRef], isError: Boolean) derives JsonCodec, Schema

final case class OptionView(label: String, description: String) derives JsonCodec, Schema

final case class QuestionView(question: String, header: String, options: List[OptionView], multiSelect: Boolean) derives JsonCodec, Schema

/** 挂起的调用在等什么。`kind`：question（等用户回答）/ approval（等用户批准）/ agent（等别的 agent 回答）。 */
final case class PendingView(
    kind: String,
    questions: List[QuestionView],
    summary: Option[String],
    thread: Option[String],
    agent: Option[String]
) derives JsonCodec, Schema

/** 一次工具调用。
  * @param display 结果怎么展示：Plain / Terminal / Code / Diff / JsonData / Markdown / SharedFile（结果是 FileView 的 JSON）
  * @param state   pending / running / suspended / approved / approved-always / done
  */
final case class CallView(
    id: String,
    name: String,
    args: String,
    display: String,
    state: String,
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

/** 进行中的异步工作。`kind`：job / delegation / wake ...。 */
final case class WaitItem(id: Long, kind: String, ref: String, label: String, heartbeatSeconds: Option[Int], dueAt: Option[Long], createdAt: Long)
    derives JsonCodec, Schema

final case class PendingCall(entry: Long, call: String, name: String, pending: PendingView) derives JsonCodec, Schema

/** 最近一次运行。`state`：running / suspended / finished / failed / stopped；`retryNote`：暂时失败、正在退避重试的原因。 */
final case class RunView(id: Long, state: String, error: Option[String], retryNote: Option[String], startedAt: Long, finishedAt: Option[Long])
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
    usage: UsageView
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

  val list: Endpoint[Option[String], (Option[AgentId], Option[String], Option[Long], Option[Int]), ApiError, SessionPage, Any] =
    sessions.get
      .in(query[Option[AgentId]]("agent").and(query[Option[String]]("q")).and(query[Option[Long]]("before")).and(query[Option[Int]]("limit")))
      .out(jsonBody[SessionPage])
      .name("listSessions")
      .description("会话列表：按最近活动倒序，运行中/等待中的置顶（第一页）。`agent` 筛选；`q` 搜来信、回复与工具调用参数（不搜工具结果）。")

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

  val answer: Endpoint[Option[String], (SessionId, Long, String, AnswerIn), ApiError, Unit, Any] =
    one.post
      .in("calls" / path[Long]("entry") / path[String]("call") / "answer")
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
  val download: Endpoint[Option[String], String, ApiError, (String, String, Array[Byte]), Any] =
    Api.secure.get
      .in("files" / path[String]("id"))
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
