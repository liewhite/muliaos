package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- issue 看板 ----

/** issue 的状态。`done`、`cancelled` 是「结束了」：下游只在全部上游结束后才开工。 */
enum IssueStatus(val label: String) extends Labeled:
  /** 以后再做 */
  case Backlog extends IssueStatus("backlog")
  /** 在做：指派给 agent 时，每条新动态投进它处理这个 issue 的会话 */
  case InProgress extends IssueStatus("in_progress")
  /** 做完了等用户审核（指派给用户） */
  case InReview extends IssueStatus("in_review")
  /** 卡住了，等指派人（通常是用户）处理 */
  case Blocked extends IssueStatus("blocked")
  case Done extends IssueStatus("done")
  case Cancelled extends IssueStatus("cancelled")

  def finished: Boolean = this == Done || this == Cancelled

object IssueStatus extends Labels[IssueStatus]("issue 状态", IssueStatus.values)

enum Priority(val label: String) extends Labeled:
  case Unset extends Priority("none")
  case Urgent extends Priority("urgent")
  case High extends Priority("high")
  case Medium extends Priority("medium")
  case Low extends Priority("low")

object Priority extends Labels[Priority]("优先级", Priority.values)

/** 指派给谁：用户，或某个 agent。JSON 里写 `"user"` 或 agent 编号（agent 编号与名字都不会是 `user`）。 */
enum Assignee:
  case User
  case Agent(id: AgentId)

  def agent: Option[AgentId] = this match
    case Agent(id) => Some(id)
    case User      => None

object Assignee:
  final val UserKey = "user"

  def encode(a: Assignee): String = a match
    case User      => UserKey
    case Agent(id) => id.value

  def decode(s: String): Assignee = if s == UserKey then User else Agent(AgentId.wrap(s))

  given JsonCodec[Assignee] = JsonCodec.string.transform(decode, encode)
  given Schema[Assignee]    = Schema.string[Assignee].description("`user` 或 agent 编号")
  given Codec[String, Assignee, CodecFormat.TextPlain] = Codec.string.map(decode)(encode)

/** 列表里的一个 issue。
  * @param assigneeState 指派的 agent 处理它的会话在做什么；没指派 agent 或还没有会话时为空
  * @param stalled       在做（in_progress）、指派给 agent、不等上游，但它的 issue 会话空闲或还没有：没人在处理
  * @param waitingOn     还没结束的上游
  */
final case class IssueSummary(
    id: IssueId,
    title: String,
    status: IssueStatus,
    priority: Priority,
    owner: Option[AgentId],
    ownerName: Option[String],
    assignee: Option[Assignee],
    assigneeName: Option[String],
    assigneeState: Option[WorkState],
    stalled: Boolean,
    dependsOn: List[IssueId],
    waitingOn: List[IssueId],
    updatedAt: Long
) derives JsonCodec, Schema

/** 依赖关系另一头的 issue。 */
final case class IssueLink(id: IssueId, title: String, status: IssueStatus) derives JsonCodec, Schema

/** 时间线的一条。
  * @param actor   谁做的（显示名；「用户」「平台」或 agent 名）
  * @param session 发生在哪个会话（agent 经工具做的、会话失败）
  * @param text    一句话说明
  * @param comment 评论、打回理由、留言的正文（Markdown）
  */
final case class IssueEventView(
    id: Long,
    at: Long,
    actor: String,
    session: Option[SessionId],
    text: String,
    comment: Option[String]
) derives JsonCodec, Schema

/** 处理这个 issue 的一个会话。 */
final case class IssueSessionView(agent: AgentId, agentName: String, session: SessionId, state: WorkState) derives JsonCodec, Schema

final case class IssueDetail(
    summary: IssueSummary,
    description: String,
    createdBy: String,
    createdAt: Long,
    dependsOn: List[IssueLink],
    blocks: List[IssueLink],
    events: List[IssueEventView],
    sessions: List[IssueSessionView]
) derives JsonCodec, Schema

/** 新建一个 issue。 */
final case class NewIssue(
    title: String,
    owner: AgentId,
    description: String = "",
    status: IssueStatus = IssueStatus.Backlog,
    priority: Priority = Priority.Unset,
    assignee: Option[Assignee] = None,
    dependsOn: List[IssueId] = Nil
) derives JsonCodec, Schema

/** 改一个 issue：只改给出的字段（界面只提交改过的，不覆盖别人同时改的其他字段）。`unassign` 去掉指派。`dependsOn` 是完整的新列表。 */
final case class IssuePatch(
    title: Option[String] = None,
    description: Option[String] = None,
    status: Option[IssueStatus] = None,
    priority: Option[Priority] = None,
    owner: Option[AgentId] = None,
    assignee: Option[Assignee] = None,
    unassign: Boolean = false,
    dependsOn: Option[List[IssueId]] = None
) derives JsonCodec, Schema

final case class CommentIn(body: String) derives JsonCodec, Schema

/** 打回的理由。 */
final case class SendBackIn(reason: String) derives JsonCodec, Schema

/** 叫停：停掉在做的会话，再改成 `status`（backlog / blocked / cancelled / in_progress；in_progress 是改方向，要留言）。 */
final case class StopIssueIn(status: IssueStatus, message: Option[String] = None) derives JsonCodec, Schema

/** issue 列表的筛选：`status` 不给是全部；`assignee` 是 `user` 或 agent 编号；`owner` 是 agent 编号。 */
final case class IssueQuery(status: List[IssueStatus] = Nil, assignee: Option[Assignee] = None, owner: Option[AgentId] = None)

object BoardApi:
  private val issues = Api.secure.in("issues")
  private val one    = issues.in(path[IssueId]("id"))

  val list: Endpoint[Option[String], IssueQuery, ApiError, List[IssueSummary], Any] =
    issues.get
      .in(query[List[IssueStatus]]("status").and(query[Option[Assignee]]("assignee")).and(query[Option[AgentId]]("owner")).mapTo[IssueQuery])
      .out(jsonBody[List[IssueSummary]])
      .name("listIssues")
      .refreshedBy(Topic.Issues)
      .description("issue 列表，最近更新的在前。`status` 可重复给多个；`assignee` 是 `user` 或 agent 编号；`owner` 是 agent 编号。")

  // 详情里有处理会话的工作状态，所以也跟着 sessions（列表不跟：任何会话有动静就重拉整个看板太频繁）。
  val get: Endpoint[Option[String], IssueId, ApiError, IssueDetail, Any] =
    one.get.out(jsonBody[IssueDetail]).name("getIssue").refreshedBy(Topic.Issues, Topic.Sessions).description("issue 详情：描述、依赖、被谁依赖、时间线、处理它的会话。")

  val create: Endpoint[Option[String], NewIssue, ApiError, IssueDetail, Any] =
    issues.post.in(jsonBody[NewIssue]).out(jsonBody[IssueDetail]).name("createIssue").description("新建 issue。")

  val update: Endpoint[Option[String], (IssueId, IssuePatch), ApiError, IssueDetail, Any] =
    one.patch.in(jsonBody[IssuePatch]).out(jsonBody[IssueDetail]).name("updateIssue").description("改 issue（只改给出的字段）。")

  val comment: Endpoint[Option[String], (IssueId, CommentIn), ApiError, Unit, Any] =
    one.post.in("comments").in(jsonBody[CommentIn]).out(statusCode(StatusCode.NoContent)).name("commentIssue")

  val approve: Endpoint[Option[String], IssueId, ApiError, Unit, Any] =
    one.post.in("approve").out(statusCode(StatusCode.NoContent)).name("approveIssue")
      .description("审核通过：改成 done。只对当前指派给用户、还没结束的 issue 有效（防过时的操作覆盖），否则 409。")

  val sendBack: Endpoint[Option[String], (IssueId, SendBackIn), ApiError, Unit, Any] =
    one.post.in("send-back").in(jsonBody[SendBackIn]).out(statusCode(StatusCode.NoContent)).name("sendBackIssue")
      .description("打回：回到 in_progress，指派回把它交给用户的 agent（找不到就是 owner），理由投给它。条件同通过。")

  val stop: Endpoint[Option[String], (IssueId, StopIssueIn), ApiError, Unit, Any] =
    one.post.in("stop").in(jsonBody[StopIssueIn]).out(statusCode(StatusCode.NoContent)).name("stopIssue")
      .description("叫停：停掉指派的 agent 处理它的会话，再改状态；有留言时记成评论并投给那个会话让它收尾或改方向。")

  val start: Endpoint[Option[String], IssueId, ApiError, Unit, Any] =
    one.post.in("start").out(statusCode(StatusCode.NoContent)).name("startIssue")
      .description("开始：改成 in_progress 并投给指派的 agent（已经在做就重新投一次，用于叫醒停滞的 issue）。")

  val delete: Endpoint[Option[String], IssueId, ApiError, Unit, Any] =
    one.delete.out(statusCode(StatusCode.NoContent)).name("deleteIssue").description("删除 issue；下游对它的依赖一并去掉。")

  val all = List(list, get, create, update, comment, approve, sendBack, stop, start, delete)
