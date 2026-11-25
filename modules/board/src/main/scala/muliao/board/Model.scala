package muliao.board

import muliao.contract.*
import zio.json.*

/** 谁做的。时间线按它记，同时记下发生在哪个会话（agent 经工具做的、平台记下的会话失败）。 */
enum Actor:
  case User
  /** 平台自己记的（会话失败、投递失败、agent 删除后置空） */
  case System(session: Option[SessionId])
  case Agent(id: AgentId, session: Option[SessionId])

  def agent: Option[AgentId] = this match
    case Agent(id, _) => Some(id)
    case _            => None

  /** 发生在哪个会话。 */
  def where: Option[SessionId] = this match
    case Agent(_, s) => s
    case System(s)   => s
    case User        => None

object Actor:
  /** 库里的写法：`user`、`system` 或 agent 编号（会话另存一列）。 */
  def encode(a: Actor): String = a match
    case User         => "user"
    case System(_)    => "system"
    case Agent(id, _) => id.value

  def decode(s: String, session: Option[SessionId]): Actor = s match
    case "user"   => User
    case "system" => System(session)
    case id       => Agent(AgentId.wrap(id), session)

/** 一次改动的发起方：谁，以及是不是在某个 issue 的专属会话里做的（`inIssue`）。agent 在处理这个 issue 的会话里自己做的改动不投回给它。 */
final case class By(actor: Actor, inIssue: Option[IssueId] = None):
  /** 这是 `agent` 在处理 `issue` 的会话里做的。 */
  def isOwnWork(agent: AgentId, issue: IssueId): Boolean = actor.agent.contains(agent) && inIssue.contains(issue)

object By:
  val user: By   = By(Actor.User)
  val system: By = By(Actor.System(None))

/** 时间线上的一条动态。字段改动每个字段一条。 */
enum Activity derives JsonCodec:
  case Created
  case Commented(text: String)
  case StatusChanged(from: IssueStatus, to: IssueStatus)
  case AssigneeChanged(from: Option[Assignee], to: Option[Assignee])
  case PriorityChanged(from: Priority, to: Priority)
  case TitleChanged(from: String, to: String)
  case DescriptionChanged
  case OwnerChanged(from: Option[AgentId], to: Option[AgentId])
  case DependsChanged(from: List[IssueId], to: List[IssueId])
  /** 上游的状态变了（结束、或结束后又重开） */
  case UpstreamChanged(upstream: IssueId, from: IssueStatus, to: IssueStatus)
  /** 上游被删除，对它的依赖随之去掉 */
  case UpstreamDeleted(upstream: IssueId)
  /** 用户审核通过 */
  case Approved
  /** 用户打回，理由 */
  case SentBack(reason: String)
  /** 用户手动「开始」（已在做时就是重新投一次） */
  case Started
  /** 叫停了处理它的会话；留言另记一条评论 */
  case Stopped
  /** issue 会话最终失败（不自动重投） */
  case SessionFailed(error: String)
  /** 投不进 agent 的 issue 会话（如 agent 已归档） */
  case DeliveryFailed(agent: AgentId, reason: String)

/** 时间线的一条：何时、谁、做了什么。 */
final case class IssueEvent(id: Long, issue: IssueId, at: Long, actor: Actor, activity: Activity)

/** 一个 issue 的当前状态（不含时间线）。 */
final case class Issue(
    id: IssueId,
    title: String,
    description: String,
    status: IssueStatus,
    priority: Priority,
    owner: Option[AgentId],
    assignee: Option[Assignee],
    dependsOn: List[IssueId],
    createdBy: Actor,
    createdAt: Long,
    updatedAt: Long
):
  def assignedAgent: Option[AgentId] = assignee.flatMap(_.agent)

/** 依赖的上游：已有的 issue，或同一批里另一项的键。 */
enum Upstream:
  case Existing(id: IssueId)
  case Key(key: String)

/** 要新建的一项（一批里可以互相依赖）。名字都已解析成编号。 */
final case class Draft(
    key: Option[String],
    title: String,
    owner: AgentId,
    description: String = "",
    status: IssueStatus = IssueStatus.Backlog,
    priority: Priority = Priority.Unset,
    assignee: Option[Assignee] = None,
    dependsOn: List[Upstream] = Nil
)

/** 改一个 issue：只改给出的字段。`assignee = Some(None)` 是去掉指派。`dependsOn` 是完整的新列表。 */
final case class Patch(
    title: Option[String] = None,
    description: Option[String] = None,
    status: Option[IssueStatus] = None,
    priority: Option[Priority] = None,
    owner: Option[AgentId] = None,
    assignee: Option[Option[Assignee]] = None,
    dependsOn: Option[List[IssueId]] = None
)

/** 列表筛选。 */
final case class Filter(statuses: Set[IssueStatus] = Set.empty, assignee: Option[Assignee] = None, owner: Option[AgentId] = None):
  def accepts(i: Issue): Boolean =
    (statuses.isEmpty || statuses(i.status)) && assignee.forall(a => i.assignee.contains(a)) && owner.forall(o => i.owner.contains(o))

/** 叫停：停掉处理它的会话后改成 `status`，`message` 记成评论并投给那个会话（让它收尾或改方向）。
  * 只能由 [[Halt.make]] 造出：停下不是做完，不能交审或完成；改成 in_progress 是改方向，必须留言。
  */
final case class Halt private (status: IssueStatus, message: Option[String])

object Halt:
  def make(status: IssueStatus, message: Option[String]): Either[String, Halt] =
    val m = message.map(_.trim).filter(_.nonEmpty)
    status match
      case IssueStatus.Backlog | IssueStatus.Blocked | IssueStatus.Cancelled => Right(Halt(status, m))
      case IssueStatus.InProgress =>
        m.toRight("改成 in_progress 是改方向，要给留言（message）说清接下来做什么").map(_ => Halt(status, m))
      case s => Left(s"叫停后不能改成 ${s.label}：停下不是做完，只能是 backlog / blocked / cancelled / in_progress（改方向，要留言）")

/** 一个 issue 在一段时间里的动态（活动回顾用），文字已按 [[Render]] 写好。
  * 状态、指派的改动各压成一条变化路径（如 backlog → in_progress → in_review），其余动态逐条在 `events`；`count` 是全部动态条数，`first` / `last` 最早、最晚一条的时刻。
  */
final case class IssueActivity(
    issue: IssueId,
    title: String,
    count: Int,
    first: Long,
    last: Long,
    statuses: List[IssueStatus],
    assignees: List[String],
    events: List[IssueActivity.Event]
)

object IssueActivity:
  /** 一条动态：谁、做了什么、正文（评论、打回理由、错误）。 */
  final case class Event(at: Long, actor: Actor, who: String, text: String, body: Option[String])

  /** 一串改动接成的变化路径（前后相同的并掉）；没有改动是空。 */
  def path[A](changes: List[(A, A)]): List[A] =
    changes match
      case Nil            => Nil
      case (from, _) :: _ => (from :: changes.map(_._2)).foldRight(List.empty[A])((a, acc) => if acc.headOption.contains(a) then acc else a :: acc)
