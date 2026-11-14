package muliao.web.pages.issues

import muliao.contract.*
import org.scalajs.dom

/** issue 看板的纯逻辑：状态与优先级的显示、筛选条件与地址的互换、按状态分列、指派选项、负责项目配色。 */
object IssueModel:

  // ---- 状态与优先级 ----

  /** 看板的列顺序（也是列表视图的分组顺序）：受阻排在进行中和待审核之间。 */
  val Statuses: List[IssueStatus] =
    List(IssueStatus.Backlog, IssueStatus.InProgress, IssueStatus.Blocked, IssueStatus.InReview, IssueStatus.Done, IssueStatus.Cancelled)

  def statusLabel(s: IssueStatus): String = s match
    case IssueStatus.Backlog    => "Backlog"
    case IssueStatus.InProgress => "进行中"
    case IssueStatus.Blocked    => "受阻"
    case IssueStatus.InReview   => "待审核"
    case IssueStatus.Done       => "完成"
    case IssueStatus.Cancelled  => "已取消"

  /** 状态圆点的颜色（Tailwind 类）。 */
  def statusColor(s: IssueStatus): String = s match
    case IssueStatus.Backlog    => "border-muted"
    case IssueStatus.InProgress => "border-warning"
    case IssueStatus.InReview   => "border-accent"
    case IssueStatus.Blocked    => "border-danger"
    case IssueStatus.Done       => "border-success bg-success"
    case IssueStatus.Cancelled  => "border-muted bg-muted"

  /** 优先级的显示顺序。 */
  val Priorities: List[Priority] = List(Priority.Urgent, Priority.High, Priority.Medium, Priority.Low, Priority.Unset)

  def priorityLabel(p: Priority): String = p match
    case Priority.Urgent => "紧急"
    case Priority.High   => "高"
    case Priority.Medium => "中"
    case Priority.Low    => "低"
    case Priority.Unset  => "无优先级"

  // ---- 指派 ----

  /** 指派下拉框的一项：未指派、我、某个 agent。 */
  enum AssigneeChoice:
    case Nobody
    case Me
    case Agent(id: AgentId)

    def toAssignee: Option[Assignee] = this match
      case Nobody    => None
      case Me        => Some(Assignee.User)
      case Agent(id) => Some(Assignee.Agent(id))

  object AssigneeChoice:
    def of(a: Option[Assignee]): AssigneeChoice = a match
      case None                      => Nobody
      case Some(Assignee.User)       => Me
      case Some(Assignee.Agent(id))  => Agent(id)

  def assigneeLabel(agents: List[AgentSummary], a: Option[Assignee]): String = a match
    case None                     => "未指派"
    case Some(Assignee.User)      => "我"
    case Some(Assignee.Agent(id)) => agents.find(_.id == id).fold(id.value)(_.name.value)

  /** 列表里一条 issue 的指派人名字：接口给的名字（用户显示「我」）。 */
  def assigneeLabel(i: IssueSummary): String = i.assignee match
    case None                => "未指派"
    case Some(Assignee.User) => "我"
    case Some(Assignee.Agent(id)) => i.assigneeName.getOrElse(id.value)

  /** 被指派 agent 处理这个 issue 的会话在做什么。在等后台任务、委派、唤醒或用户回答对看板来说也是有人在处理。 */
  enum Work:
    case Busy, Idle

  object Work:
    def parse(s: String): Option[Work] = s match
      case "running" | "waiting" => Some(Busy)
      case "idle"                => Some(Idle)
      case _                     => None

  def workLabel(w: Work): String = w match
    case Work.Busy => "处理中"
    case Work.Idle => "空闲"

  /** 卡片上的处理状态：没人在处理（后端判定的停滞）优先，否则是会话状态。 */
  enum Handling:
    case Stalled
    case Session(w: Work)

  def handling(i: IssueSummary): Option[Handling] =
    if i.stalled then Some(Handling.Stalled) else i.assigneeState.flatMap(Work.parse).map(Handling.Session(_))

  // ---- 手动操作的可用性 ----

  /** 能不能点「开始」：指派给 agent、没结束、不等上游；已经在做时只有停滞了才需要（重新交给它）。 */
  enum Startable(val hint: Option[String]):
    case No    extends Startable(None)
    case Begin extends Startable(Some("改成进行中，交给指派的 agent"))
    case Wake  extends Startable(Some("没人在处理：再交给指派的 agent 一次"))

  def startable(i: IssueSummary): Startable =
    (i.assignee, i.status) match
      case (Some(Assignee.Agent(_)), s) if !s.finished && i.waitingOn.isEmpty =>
        s match
          case IssueStatus.InProgress => if i.stalled then Startable.Wake else Startable.No
          case _                      => Startable.Begin
      case _ => Startable.No

  /** 能不能叫停：指派给 agent、没结束，且在做或它的会话正忙（停掉它处理这个 issue 的会话再改状态）。 */
  def stoppable(i: IssueSummary): Boolean =
    i.assignee.exists(_.agent.isDefined) && !i.status.finished &&
      (i.status == IssueStatus.InProgress || i.assigneeState.flatMap(Work.parse).contains(Work.Busy))

  /** 能不能审核：指派给用户且没结束（与接口的条件一致）。 */
  def reviewable(i: IssueSummary): Boolean = i.assignee.contains(Assignee.User) && !i.status.finished

  /** 叫停后改成什么。改方向（回到进行中）必须留言。 */
  enum Halt(val status: IssueStatus, val label: String):
    case Backlog   extends Halt(IssueStatus.Backlog, "放回 Backlog")
    case Blocked   extends Halt(IssueStatus.Blocked, "受阻")
    case Cancelled extends Halt(IssueStatus.Cancelled, "已取消")
    case Redirect  extends Halt(IssueStatus.InProgress, "改方向")

    def needsMessage: Boolean = this == Redirect

  // ---- 负责项目配色 ----

  /** 相邻两个 agent 的色相间隔：黄金角，每加一个 agent 都落在已有色相间较大的空隙里。 */
  private val GoldenAngle = 137.508

  /** 负责项目的色相（0–360）：按 agent 的创建顺序（接口按创建时间列出）依次取黄金角，新建的 agent 不改已有颜色。
    * agent 已删除或列表还没加载时为 None（不着色）。
    */
  def ownerHue(agents: List[AgentSummary], owner: Option[AgentId]): Option[Int] =
    owner.flatMap(o => Option(agents.indexWhere(_.id == o)).filter(_ >= 0)).map(rank => math.round((rank * GoldenAngle) % 360).toInt)

  /** 配合 owner-color 类（styles.css）得到 --owner-fg / --owner-bg。 */
  def ownerStyle(hue: Int): String = s"--owner-hue: $hue"

  def ownerLabel(agents: List[AgentSummary], owner: Option[AgentId], name: Option[String] = None): String = owner match
    case None    => "无负责项目"
    case Some(o) => agents.find(_.id == o).map(_.name.value).orElse(name).getOrElse(o.value)

  // ---- 筛选（地址查询参数） ----

  /** 列表的筛选条件，与地址的查询参数 `status`（逗号分隔）、`assignee`（user 或 agent 编号）、`owner` 互换。 */
  final case class Filters(statuses: List[IssueStatus], assignee: Option[String], owner: Option[AgentId]):
    def query: (List[IssueStatus], Option[String], Option[AgentId]) = (statuses, assignee, owner)

    /** 看板显示的列：没筛状态时全部。 */
    def columns: List[IssueStatus] = if statuses.isEmpty then Statuses else Statuses.filter(statuses.contains)

    def toggle(s: IssueStatus): List[IssueStatus] =
      if statuses.contains(s) then statuses.filterNot(_ == s) else Statuses.filter(x => x == s || statuses.contains(x))

  object Filters:
    def parse(params: Map[String, String]): Filters =
      Filters(
        statuses = params.get("status").toList.flatMap(_.split(',').toList).flatMap(IssueStatus.parse).distinct,
        assignee = params.get("assignee").filter(_.nonEmpty),
        owner = params.get("owner").filter(_.nonEmpty).flatMap(o => AgentId.make(o).toOption)
      )

    def statusParam(ss: List[IssueStatus]): Option[String] = Option.when(ss.nonEmpty)(ss.map(_.label).mkString(","))

  // ---- 分列 ----

  /** 按状态分组，组内最近更新的在前（刚拖进来的排在最上面）。 */
  def groupByStatus(issues: List[IssueSummary]): Map[IssueStatus, List[IssueSummary]] =
    val g = issues.groupBy(_.status).view.mapValues(_.sortBy(-_.updatedAt)).toMap
    Statuses.map(s => s -> g.getOrElse(s, Nil)).toMap

  /** 每个状态默认显示的 issue 数，其余折叠：「完成」一栏会越堆越多。 */
  val FoldLimit = 10

  def fold(issues: List[IssueSummary], expanded: Boolean): List[IssueSummary] = if expanded then issues else issues.take(FoldLimit)

  /** 一次还没被服务端确认的拖拽。`settled`：接口已成功，等下一次拉到的数据落下再撤掉。 */
  final case class Move(status: IssueStatus, at: Long, token: Int, settled: Boolean)

  /** 本地乐观更新：还在提交的 issue 移到新状态、按服务端的做法刷新更新时间。 */
  def applyMoves(issues: List[IssueSummary], moves: Map[IssueId, Move]): List[IssueSummary] =
    issues.map(i => moves.get(i.id).fold(i)(m => i.copy(status = m.status, updatedAt = m.at)))

  // ---- 依赖编号 ----

  /** 编号列表的文字：「#1, #2」，空为「无」。 */
  def refs(ids: List[IssueId]): String = if ids.isEmpty then "无" else ids.map(i => s"#${i.value}").mkString(", ")

  /** 解析用户输入的依赖列表（「#1, 2 #3」）；有不是编号的内容为 None。 */
  def parseRefs(text: String): Option[List[IssueId]] =
    val parts = text.split("[\\s,，]+").toList.filter(_.nonEmpty)
    val ids   = parts.map(p => p.stripPrefix("#").toLongOption.flatMap(n => IssueId.make(n).toOption))
    Option.when(ids.forall(_.isDefined))(ids.flatten)

  // ---- 跨刷新记住的界面选择 ----

  /** 存在 localStorage 里的一项，取值不认识（旧版本留下的、被手改坏的）时取默认。 */
  final class Stored[T](key: String, options: List[T], show: T => String, fallback: T):
    def load(): T             = Option(dom.window.localStorage.getItem(key)).flatMap(s => options.find(show(_) == s)).getOrElse(fallback)
    def save(v: T): Unit      = dom.window.localStorage.setItem(key, show(v))
