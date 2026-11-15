package muliao.board

import muliao.contract.*
import muliao.contract.WorkState

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

/** 看板的规则，都是纯函数：谁在处理、投给谁、停滞、改动成哪些动态、交给用户、打回给谁、依赖成环。 */
object Rules:

  /** 还没结束的上游。`statusOf` 缺的（理论上不会：删 issue 时一并去掉依赖）当作已结束。 */
  def waitingOn(i: Issue, statusOf: IssueId => Option[IssueStatus]): List[IssueId] =
    i.dependsOn.filter(d => statusOf(d).exists(!_.finished))

  /** 有 agent 在处理：在做（in_progress）、指派给 agent、不等上游。 */
  def worker(i: Issue, waiting: List[IssueId]): Option[AgentId] =
    if i.status == IssueStatus.InProgress && waiting.isEmpty then i.assignedAgent else None

  /** 该有人处理却没人在处理：它的 issue 会话空闲或还没有。 */
  def stalled(i: Issue, waiting: List[IssueId], session: Option[WorkState]): Boolean =
    worker(i, waiting).isDefined && session.forall(_ == WorkState.Idle)

  /** 一批动态投给谁：有 agent 在处理就投给它（它在自己的 issue 会话里做的不投回）；没人处理时，用户的评论投给 owner。 */
  def recipients(i: Issue, waiting: List[IssueId], by: By, activities: List[Activity]): List[AgentId] =
    worker(i, waiting) match
      case Some(a) => if by.isOwnWork(a, i.id) then Nil else List(a)
      case None =>
        val userComment = by.actor == Actor.User && activities.exists {
          case Activity.Commented(_) => true
          case _                     => false
        }
        if userComment then i.owner.toList else Nil

  /** 按补丁改，得到改后的 issue 与每个字段一条动态（没变的字段不记）。 */
  def diff(i: Issue, p: Patch): (Issue, List[Activity]) =
    val after = i.copy(
      title = p.title.getOrElse(i.title),
      description = p.description.getOrElse(i.description),
      status = p.status.getOrElse(i.status),
      priority = p.priority.getOrElse(i.priority),
      owner = p.owner.orElse(i.owner),
      assignee = p.assignee.getOrElse(i.assignee),
      dependsOn = p.dependsOn.map(_.distinct.sortBy(_.value)).getOrElse(i.dependsOn)
    )
    def changed[A](get: Issue => A)(make: (A, A) => Activity): Option[Activity] =
      Option.when(get(i) != get(after))(make(get(i), get(after)))
    val activities = List(
      changed(_.title)(Activity.TitleChanged(_, _)),
      changed(_.description)((_, _) => Activity.DescriptionChanged),
      changed(_.status)(Activity.StatusChanged(_, _)),
      changed(_.priority)(Activity.PriorityChanged(_, _)),
      changed(_.owner)(Activity.OwnerChanged(_, _)),
      changed(_.assignee)(Activity.AssigneeChanged(_, _)),
      changed(_.dependsOn.toSet)((_, _) => Activity.DependsChanged(i.dependsOn, after.dependsOn))
    ).flatten
    (after, activities)

  /** 这次改动是不是把 issue 交给了用户（要通知用户）：不是用户自己改的、改后指派给用户且没结束，并且指派或状态变了。 */
  def handedToUser(before: Option[Issue], after: Issue, by: By): Boolean =
    by.actor != Actor.User && after.assignee.contains(Assignee.User) && !after.status.finished &&
      before.forall(b => !b.assignee.contains(Assignee.User) || b.status != after.status)

  /** 打回给谁：最近一次把指派改成用户的 agent；没有就是 owner。 */
  def sendBackTo(i: Issue, events: List[IssueEvent]): Option[AgentId] =
    events.reverseIterator
      .collectFirst { case IssueEvent(_, _, _, Actor.Agent(a, _), Activity.AssigneeChanged(_, Some(Assignee.User))) => a }
      .orElse(i.owner)

  /** 审核（通过、打回）只对当前指派给用户、还没结束的 issue 有效，防过时的操作覆盖。 */
  def reviewable(i: Issue): Either[String, Unit] =
    if !i.assignee.contains(Assignee.User) then Left(s"#${i.id.value} 现在没有指派给你（可能已被别人处理），没有改")
    else if i.status.finished then Left(s"#${i.id.value} 已经是 ${i.status.label}，没有改")
    else Right(())

  /** 依赖图里的一个环（首尾相同的路径），没有为 None。`edges`：每个点依赖哪些点。 */
  def cycle[A](edges: Map[A, Seq[A]]): Option[List[A]] =
    // 深度优先：`path` 是当前路径（逆序），`done` 是已查完、确认不在环上的点
    def visit(a: A, path: List[A], onPath: Set[A], done: Set[A]): Either[List[A], Set[A]] =
      if onPath(a) then Left((a :: path.takeWhile(_ != a) ::: List(a)).reverse)
      else if done(a) then Right(done)
      else
        edges.getOrElse(a, Nil).foldLeft[Either[List[A], Set[A]]](Right(done)) { (acc, b) =>
          acc.flatMap(d => visit(b, a :: path, onPath + a, d))
        }.map(_ + a)
    edges.keys.foldLeft[Either[List[A], Set[A]]](Right(Set.empty))((acc, a) => acc.flatMap(d => visit(a, Nil, Set.empty, d))).left.toOption

/** 动态、issue 摘要的文字写法（时间线、投给 agent 的信、issue_show 共用）。 */
final class Render(names: AgentId => String, zone: ZoneId):
  private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(zone)

  def time(at: Long): String = fmt.format(Instant.ofEpochMilli(at))

  def actor(a: Actor): String = a match
    case Actor.User         => "用户"
    case Actor.System(_)    => "平台"
    case Actor.Agent(id, _) => names(id)

  def assignee(a: Option[Assignee]): String = a match
    case None                    => "无"
    case Some(Assignee.User)     => Assignee.UserKey
    case Some(Assignee.Agent(i)) => names(i)

  def owner(o: Option[AgentId]): String = o.fold("无")(names)

  private def ids(xs: List[IssueId]): String = if xs.isEmpty then "无" else xs.map(i => s"#${i.value}").mkString(", ")

  /** 一条动态：一句话与正文（评论、打回理由、错误）。 */
  def activity(a: Activity): (String, Option[String]) = a match
    case Activity.Created                  => ("建了这个 issue", None)
    case Activity.Commented(t)             => ("评论", Some(t))
    case Activity.StatusChanged(f, t)      => (s"把状态从 ${f.label} 改成 ${t.label}", None)
    case Activity.AssigneeChanged(f, t)    => (s"把指派从 ${assignee(f)} 改成 ${assignee(t)}", None)
    case Activity.PriorityChanged(f, t)    => (s"把优先级从 ${f.label} 改成 ${t.label}", None)
    case Activity.TitleChanged(f, t)       => (s"把标题从「$f」改成「$t」", None)
    case Activity.DescriptionChanged       => ("改了描述", None)
    case Activity.OwnerChanged(f, t)       => (s"把 owner 从 ${owner(f)} 改成 ${owner(t)}", None)
    case Activity.DependsChanged(f, t)     => (s"把依赖从 ${ids(f)} 改成 ${ids(t)}", None)
    case Activity.UpstreamChanged(u, f, t) => (s"把依赖 #${u.value} 的状态从 ${f.label} 改成 ${t.label}", None)
    case Activity.UpstreamDeleted(u)       => (s"删除了依赖 #${u.value}，对它的依赖随之去掉", None)
    case Activity.Approved                 => ("审核通过", None)
    case Activity.SentBack(r)              => ("打回", Some(r))
    case Activity.Started                  => ("点了开始", None)
    case Activity.Stopped                  => ("叫停了处理它的会话", None)
    case Activity.SessionFailed(e)         => ("记下：issue 会话最终失败，不自动重投（会话页可重试）", Some(e))
    case Activity.DeliveryFailed(a, r)     => (s"没能投给 ${names(a)}：$r", None)

  /** 时间线的一行（正文缩进跟在下面）。 */
  def line(e: IssueEvent): String =
    val (text, body) = activity(e.activity)
    val head         = s"- [${time(e.at)}] ${actor(e.actor)} $text"
    body.fold(head)(b => head + "：\n" + b.linesIterator.map("  " + _).mkString("\n"))

  /** 一行摘要：标题、状态、指派、依赖。 */
  def summary(i: Issue, statusOf: IssueId => Option[IssueStatus]): String =
    val deps = if i.dependsOn.isEmpty then "无" else i.dependsOn.map(d => s"#${d.value} (${statusOf(d).fold("?")(_.label)})").mkString(", ")
    s"${i.title} | 状态 ${i.status.label} | 指派 ${assignee(i.assignee)} | 依赖 $deps"

  /** 投给 agent 的信：一行摘要与本次动态。 */
  def letter(i: Issue, statusOf: IssueId => Option[IssueStatus], events: List[IssueEvent]): String =
    s"""issue #${i.id.value} 有新动态：${summary(i, statusOf)}
       |${events.map(line).mkString("\n")}
       |
       |全文（描述、依赖、完整时间线）用 issue_show(${i.id.value}) 看。""".stripMargin

  /** issue 会话一次运行结束后变成停滞时，请 agent 表态。 */
  def nudge(i: Issue): String =
    s"""issue #${i.id.value}（${i.title}）还是 in_progress、指派给你，但这个会话已经空闲，也没有在等的事（后台任务、委派回报、唤醒）。请表态：
       |- 做完了：issue_comment 记结果，再 issue_update 设 in_review 指派 user（有下游依赖它时设 done，下游才会开始）；
       |- 只有用户能处理：设 blocked、指派 user，评论写清要用户做什么；
       |- 还要等别的事：用后台任务、report_back 的委派或 wake_me 让这个会话等着。
       |这条只提醒一次；之后有新动态才会再投到这里。""".stripMargin
