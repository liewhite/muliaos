package muliao.board

import muliao.board.store.Repo
import muliao.contract.*
import muliao.notifications.{Notice, Notifications, Target}
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.{Database, Tx}
import muliao.platform.events.ChangeBus
import muliao.resources.agents.Agents
import muliao.sessions.*
import zio.*

import java.time.ZoneId

/** issue 看板：issue 与时间线、依赖、投递到 issue 会话、停滞、审核、叫停。
  *
  * 投递：每次改动在同一个写事务里记下动态、按 [[Rules.recipients]] 投进 agent 处理这个 issue 的会话（[[Sessions.deliverIn]]），
  * 提交后会话才开始跑；投不进（如 agent 已归档）记一条「没能投给」的动态，改动照常提交。上游结束时在下游记一条动态并按同一规则投递，
  * 所以下游在最后一个上游结束时收到第一封信。
  */
trait Board:
  def list(f: Filter): IO[AppError, List[IssueSummary]]
  def show(id: IssueId): IO[AppError, IssueDetail]

  /** 建一批（可以互相依赖，任一项不合法整批不建）。返回按输入顺序的编号。 */
  def create(by: By, drafts: List[Draft]): IO[AppError, List[IssueId]]
  def update(by: By, id: IssueId, p: Patch): IO[AppError, Unit]
  def comment(by: By, id: IssueId, text: String): IO[AppError, Unit]

  /** 审核通过：改成 done。 */
  def approve(id: IssueId): IO[AppError, Unit]

  /** 打回：回到 in_progress，指派回把它交给用户的 agent，理由投给它。 */
  def sendBack(id: IssueId, reason: String): IO[AppError, Unit]

  /** 叫停：停掉指派的 agent 处理它的会话，再改状态；有留言时记成评论并投给那个会话。不能在这个 issue 自己的会话里叫停。 */
  def stop(by: By, id: IssueId, halt: Halt): IO[AppError, Unit]

  /** 用户点「开始」：改成 in_progress 并投给指派的 agent（已在做就重新投一次）。 */
  def start(id: IssueId): IO[AppError, Unit]

  def delete(id: IssueId): IO[AppError, Unit]

  /** [from, to) 里对 issue 的操作（活动回顾用），按时间排：`agent` 给出时是它做的，加上用户在它负责或被指派的 issue 上做的；
    * 不给是用户与全部 agent 做的。平台自己记的不算。
    */
  def activity(agent: Option[AgentId], from: Long, to: Long): IO[AppError, List[IssueActivity]]

  /** 会话事件：issue 会话最终失败记一条动态；一次运行结束后停滞时请 agent 表态（一次）。 */
  def onSession(e: SessionEvent): UIO[Unit]

  /** 删除 agent：它负责、被指派的 issue 置空（可重复执行）。 */
  def forgetAgent(agent: AgentId): IO[AppError, Unit]

object Board:
  val Topic: muliao.contract.Topic = muliao.contract.Topic.wrap("issues")

  /** issue 页的前端路由（交给用户的通知点开去这里）。 */
  def pageOf(id: IssueId): String = s"/issues/${id.value}"

  val layer: ZLayer[Database & ChangeBus & Sessions & Notifications & Agents & GeneralSettings, Nothing, Board] =
    ZLayer {
      for
        db <- ZIO.service[Database]
        bus <- ZIO.service[ChangeBus]
        s   <- ZIO.service[Sessions]
        n   <- ZIO.service[Notifications]
        a   <- ZIO.service[Agents]
        g   <- ZIO.service[GeneralSettings]
      yield Live(db, bus, s, n, a, g.timezone)
    }

  private final case class Live(db: Database, bus: ChangeBus, sessions: Sessions, notifications: Notifications, agents: Agents, zone: ZoneId)
      extends Board:

    private val now: UIO[Long] = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    private val changed: URIO[muliao.platform.db.TxHooks, Unit] = bus.publishAfterCommit(Topic)

    // ---- 名字与文字 ----

    private def agentsIn(issues: List[Issue], events: List[IssueEvent]): Set[AgentId] =
      val fromIssues = issues.flatMap(i => i.owner.toList ++ i.assignedAgent.toList ++ i.createdBy.agent.toList)
      val fromEvents = events.flatMap { e =>
        e.actor.agent.toList ++ (e.activity match
          case Activity.AssigneeChanged(f, t) => (f ++ t).flatMap(_.agent).toList
          case Activity.OwnerChanged(f, t)    => (f ++ t).toList
          case Activity.DeliveryFailed(a, _)  => List(a)
          case _                              => Nil
        )
      }
      (fromIssues ++ fromEvents).toSet

    /** agent 编号到显示名（删除了的也有名字；查不到用编号）。 */
    private def names(ids: Set[AgentId]): UIO[Map[AgentId, String]] =
      ZIO.foreach(ids.toList)(id => agents.get(id).map(a => id -> a.name.value).orElseSucceed(id -> id.value)).map(_.toMap)

    private def render(issues: List[Issue], events: List[IssueEvent]): UIO[Render] =
      names(agentsIn(issues, events)).map(ns => Render(id => ns.getOrElse(id, id.value), zone))

    // ---- 校验 ----

    private def checkTitle(t: String): IO[AppError, Unit] =
      ZIO.fail(AppError.invalid("标题不能为空")).when(t.trim.isEmpty).unit

    private def checkOwner(o: AgentId): IO[AppError, Unit] =
      agents.get(o).filterOrFail(_.live)(AppError.invalid(s"owner ${o.value} 已删除")).unit

    /** 指派给 agent 时它得能接新工作（归档、删除的说明原因）。 */
    private def checkAssignee(a: Option[Assignee]): IO[AppError, Unit] =
      ZIO.foreachDiscard(a.flatMap(_.agent))(agents.requireActive)

    private def checkUpstream(id: IssueId, ups: List[IssueId]): Tx[Unit] =
      for
        _      <- ZIO.fail(AppError.invalid(s"#${id.value} 不能依赖自己")).when(ups.contains(id))
        found  <- Repo.statuses(ups)
        missing = ups.filterNot(found.contains)
        _      <- ZIO.fail(AppError.invalid(s"依赖的 issue 不存在：${missing.map(m => s"#${m.value}").mkString(", ")}")).when(missing.nonEmpty)
        edges  <- Repo.edges
        _ <- ZIO.foreachDiscard(Rules.cycle(edges.updated(id, ups))) { c =>
               ZIO.fail(AppError.invalid(s"依赖成环：${c.map(i => s"#${i.value}").mkString(" → ")}"))
             }
      yield ()

    // ---- 记动态、投递、通知 ----

    private def record(issue: IssueId, actor: Actor, activities: List[Activity]): Tx[List[IssueEvent]] =
      now.flatMap(t => ZIO.foreach(activities)(Repo.addEvent(issue, t, actor, _)))

    /** 把这批动态投给该收的 agent（`also`：另外一定要投的，如叫停留言）。 */
    private def dispatch(by: By, issue: Issue, events: List[IssueEvent], also: Option[AgentId] = None): Tx[Unit] =
      for
        st     <- Repo.statuses(issue.dependsOn)
        waiting = Rules.waitingOn(issue, st.get)
        targets = (Rules.recipients(issue, waiting, by, events.map(_.activity)) ++ also).distinct
        r      <- render(List(issue), events)
        _ <- ZIO.foreachDiscard(targets) { a =>
               sessions
                 .deliverIn(Address.IssueOf(a, issue.id, s"#${issue.id.value} ${issue.title}"), Letter(Source.Issue(issue.id.value), r.letter(issue, st.get, events)))
                 .zipRight(Repo.clearNudge(issue.id, a))
                 .catchSome { case AppError.Domain(_, why) =>
                   record(issue.id, Actor.System(None), List(Activity.DeliveryFailed(a, why))).unit
                 }
             }
      yield ()

    /** 状态的「结束与否」变了：在每个下游记一条动态并投递（最后一个上游结束时下游收到第一封信）。 */
    private def cascade(by: By, before: IssueStatus, after: Issue): Tx[Unit] =
      ZIO.when(before.finished != after.status.finished) {
        Repo.downstream(after.id).flatMap(ZIO.foreachDiscard(_) { d =>
          for
            down <- Repo.require(d)
            t    <- now
            _    <- Repo.save(down.copy(updatedAt = t))
            evs  <- record(d, by.actor, List(Activity.UpstreamChanged(after.id, before, after.status)))
            _    <- dispatch(by, down, evs)
          yield ()
        })
      }.unit

    /** 交给用户时发通知，点开去 issue 页（可直接通过、打回）。正文取它刚写的评论。 */
    private def handover(by: By, before: Option[Issue], after: Issue): Tx[Unit] =
      ZIO.when(Rules.handedToUser(before, after, by)) {
        for
          evs <- Repo.eventsOf(after.id)
          r   <- render(List(after), Nil)
          said = evs.reverseIterator.collectFirst { case IssueEvent(_, _, _, a, Activity.Commented(t)) if a.agent == by.actor.agent => t }
          body = said.getOrElse(s"${r.actor(by.actor)} 把它改成 ${after.status.label}，交给你处理。")
          _   <- notifications.notifyIn(Notice(s"#${after.id.value} 交给你：${after.title}", body, Target.Page(pageOf(after.id))))
        yield ()
      }.unit

    /** 改一个 issue 的通用流程：校验、写回、记动态、下游、交给用户的通知、投递。 */
    private def apply(by: By, before: Issue, after0: Issue, activities: List[Activity], also: Option[AgentId] = None): Tx[Unit] =
      ZIO.when(activities.nonEmpty) {
        for
          t    <- now
          after = after0.copy(updatedAt = t)
          _    <- Repo.save(after)
          evs  <- record(after.id, by.actor, activities)
          _    <- cascade(by, before.status, after)
          _    <- handover(by, Some(before), after)
          _    <- dispatch(by, after, evs, also)
          _    <- changed
        yield ()
      }.unit

    // ---- 读 ----

    private def summaries(issues: List[Issue], statusOf: IssueId => Option[IssueStatus], ns: Map[AgentId, String]): IO[AppError, List[IssueSummary]] =
      for
        links  <- sessions.issueSessions(issues.filter(_.assignedAgent.isDefined).map(_.id))
        byPair  = links.map(l => (l.issue, l.agent) -> l.session).toMap
        states <- sessions.states(links.map(_.session))
      yield issues.map { i =>
        val waiting = Rules.waitingOn(i, statusOf)
        val state   = i.assignedAgent.flatMap(a => byPair.get((i.id, a))).flatMap(states.get)
        IssueSummary(
          id = i.id,
          title = i.title,
          status = i.status,
          priority = i.priority,
          owner = i.owner,
          ownerName = i.owner.map(o => ns.getOrElse(o, o.value)),
          assignee = i.assignee,
          assigneeName = i.assignee.map {
            case Assignee.User     => Assignee.UserKey
            case Assignee.Agent(a) => ns.getOrElse(a, a.value)
          },
          assigneeState = state.map(_.label),
          stalled = Rules.stalled(i, waiting, state),
          dependsOn = i.dependsOn,
          waitingOn = waiting,
          updatedAt = i.updatedAt
        )
      }

    def list(f: Filter): IO[AppError, List[IssueSummary]] =
      for
        all <- db.read(Repo.all)
        st   = all.map(i => i.id -> i.status).toMap
        sel  = all.filter(f.accepts)
        ns  <- names(agentsIn(sel, Nil))
        out <- summaries(sel, st.get, ns)
      yield out

    def show(id: IssueId): IO[AppError, IssueDetail] =
      for
        (issue, events, downs) <- db.read(Repo.require(id).zip(Repo.eventsOf(id)).zip(Repo.downstream(id)))
        ups   <- db.read(Repo.titles(issue.dependsOn))
        blk   <- db.read(Repo.titles(downs))
        st     = ups.flatMap((i, _, s) => IssueStatus.parse(s).map(i -> _)).toMap
        ns    <- names(agentsIn(List(issue), events))
        r      = Render(a => ns.getOrElse(a, a.value), zone)
        sum   <- summaries(List(issue), st.get, ns).map(_.head)
        links <- sessions.issueSessions(List(id))
        lns   <- names(links.map(_.agent).toSet)
        sts   <- sessions.states(links.map(_.session))
      yield
        def link(rows: List[(IssueId, String, String)]) =
          rows.flatMap((i, t, s) => IssueStatus.parse(s).map(IssueLink(i, t, _))).sortBy(_.id.value)
        IssueDetail(
          summary = sum,
          description = issue.description,
          createdBy = r.actor(issue.createdBy),
          createdAt = issue.createdAt,
          dependsOn = link(ups),
          blocks = link(blk),
          events = events.map { e =>
            val (text, body) = r.activity(e.activity)
            IssueEventView(e.id, e.at, r.actor(e.actor), e.actor.where, text, body)
          },
          sessions = links.map(l => IssueSessionView(l.agent, lns.getOrElse(l.agent, l.agent.value), l.session, sts.get(l.session).fold("idle")(_.label)))
        )

    def activity(agent: Option[AgentId], from: Long, to: Long): IO[AppError, List[IssueActivity]] =
      for
        events <- db.read(Repo.eventsBetween(from, to))
        mine   <- ZIO.foreach(agent)(a => db.read(Repo.ofAgent(a))).map(_.map(_.toSet))
        picked  = events.filter { e =>
                    (e.actor, agent) match
                      case (Actor.System(_), _)           => false
                      case (_, None)                      => true
                      case (Actor.Agent(id, _), Some(a))  => id == a
                      case (Actor.User, Some(_))          => mine.exists(_(e.issue))
                  }
        titles <- db.read(Repo.titles(picked.map(_.issue).distinct)).map(_.map((i, t, _) => i -> t).toMap)
        r      <- render(Nil, picked)
      yield picked.map { e =>
        val (text, body) = r.activity(e.activity)
        IssueActivity(e.issue, titles.getOrElse(e.issue, ""), e.at, r.actor(e.actor), e.actor.where, text, body)
      }

    // ---- 写 ----

    def create(by: By, drafts: List[Draft]): IO[AppError, List[IssueId]] =
      val keys = drafts.flatMap(_.key)
      for
        _ <- ZIO.fail(AppError.invalid("至少要有一项")).when(drafts.isEmpty)
        _ <- ZIO.fail(AppError.invalid(s"键重复：${keys.diff(keys.distinct).distinct.mkString(", ")}")).when(keys.distinct.size != keys.size)
        _ <- ZIO.foreachDiscard(drafts) { d =>
               val unknown = d.dependsOn.collect { case Upstream.Key(k) if !keys.contains(k) => k }
               checkTitle(d.title) *> checkOwner(d.owner) *> checkAssignee(d.assignee) *>
                 ZIO.fail(AppError.invalid(s"「${d.title}」依赖的键不在这一批里：${unknown.mkString(", ")}")).when(unknown.nonEmpty)
             }
        keyGraph = drafts.flatMap(d => d.key.map(k => k -> d.dependsOn.collect { case Upstream.Key(u) => u })).toMap
        _ <- ZIO.foreachDiscard(Rules.cycle(keyGraph))(c => ZIO.fail(AppError.invalid(s"这一批的依赖成环：${c.mkString(" → ")}")))
        ids <- db.write(
                 for
                   t   <- now
                   ids <- ZIO.foreach(drafts)(d =>
                            Repo.insert(Issue(IssueId.wrap(1L), d.title.trim, d.description, d.status, d.priority, Some(d.owner), d.assignee, Nil, by.actor, t, t))
                          )
                   byKey = drafts.zip(ids).flatMap((d, id) => d.key.map(_ -> id)).toMap
                   issues <- ZIO.foreach(drafts.zip(ids)) { (d, id) =>
                               val ups = d.dependsOn.map {
                                 case Upstream.Existing(i) => i
                                 case Upstream.Key(k)      => byKey(k)
                               }.distinct.sortBy(_.value)
                               val issue = Issue(id, d.title.trim, d.description, d.status, d.priority, Some(d.owner), d.assignee, ups, by.actor, t, t)
                               checkUpstream(id, ups.filterNot(ids.contains)).as(issue)
                             }
                   _ <- ZIO.foreachDiscard(issues)(Repo.save)
                   _ <- ZIO.foreachDiscard(issues) { i =>
                          record(i.id, by.actor, List(Activity.Created)).flatMap(evs => handover(by, None, i) *> dispatch(by, i, evs))
                        }
                   _ <- changed
                 yield ids
               )
      yield ids

    def update(by: By, id: IssueId, p: Patch): IO[AppError, Unit] =
      db.write(
        for
          before          <- Repo.require(id)
          (after, changes) = Rules.diff(before, p)
          _               <- ZIO.foreachDiscard(p.title)(checkTitle)
          _               <- ZIO.foreachDiscard(p.owner.filterNot(before.owner.contains))(checkOwner)
          _               <- checkAssignee(after.assignee).when(after.assignee != before.assignee)
          _               <- checkUpstream(id, after.dependsOn).when(after.dependsOn != before.dependsOn)
          _               <- apply(by, before, after.copy(title = after.title.trim), changes)
        yield ()
      )

    def comment(by: By, id: IssueId, text: String): IO[AppError, Unit] =
      for
        _ <- ZIO.fail(AppError.invalid("评论不能为空")).when(text.trim.isEmpty)
        _ <- db.write(
               for
                 i   <- Repo.require(id)
                 t   <- now
                 _   <- Repo.save(i.copy(updatedAt = t))
                 evs <- record(id, by.actor, List(Activity.Commented(text)))
                 _   <- dispatch(by, i, evs)
                 _   <- changed
               yield ()
             )
      yield ()

    private def reviewed(i: Issue): IO[AppError, Unit] = ZIO.fromEither(Rules.reviewable(i)).mapError(AppError.conflict)

    def approve(id: IssueId): IO[AppError, Unit] =
      db.write(
        for
          i <- Repo.require(id)
          _ <- reviewed(i)
          after = i.copy(status = IssueStatus.Done)
          _ <- apply(By.user, i, after, List(Activity.Approved, Activity.StatusChanged(i.status, after.status)))
        yield ()
      )

    def sendBack(id: IssueId, reason: String): IO[AppError, Unit] =
      for
        _ <- ZIO.fail(AppError.invalid("打回要写理由")).when(reason.trim.isEmpty)
        _ <- db.write(
               for
                 i      <- Repo.require(id)
                 _      <- reviewed(i)
                 events <- Repo.eventsOf(id)
                 to     <- ZIO.fromOption(Rules.sendBackTo(i, events)).orElseFail(AppError.conflict(s"#${id.value} 找不到可以交回的 agent（没有 owner）"))
                 _      <- agents.requireActive(to)
                 after   = i.copy(status = IssueStatus.InProgress, assignee = Some(Assignee.Agent(to)))
                 _ <- apply(
                        By.user,
                        i,
                        after,
                        List(Activity.SentBack(reason), Activity.StatusChanged(i.status, after.status), Activity.AssigneeChanged(i.assignee, after.assignee))
                      )
               yield ()
             )
      yield ()

    def stop(by: By, id: IssueId, halt: Halt): IO[AppError, Unit] =
      for
        _      <- ZIO.fail(AppError.invalid("不能在这个 issue 自己的会话里叫停它：要改状态用 issue_update")).when(by.inIssue.contains(id))
        issue  <- db.read(Repo.require(id))
        links  <- sessions.issueSessions(List(id))
        worker  = issue.assignedAgent
        _      <- ZIO.foreachDiscard(links.filter(l => worker.contains(l.agent)))(l => sessions.stop(l.session))
        _ <- db.write(
               for
                 i     <- Repo.require(id)
                 after  = i.copy(status = halt.status)
                 changes = List(Some(Activity.Stopped), Option.when(i.status != after.status)(Activity.StatusChanged(i.status, after.status)), halt.message.map(Activity.Commented(_))).flatten
                 _     <- apply(by, i, after, changes, also = halt.message.flatMap(_ => after.assignedAgent))
               yield ()
             )
      yield ()

    def start(id: IssueId): IO[AppError, Unit] =
      db.write(
        for
          i  <- Repo.require(id)
          _  <- ZIO.fromOption(i.assignedAgent).orElseFail(AppError.invalid(s"#${id.value} 没有指派给 agent，先指派再开始"))
          _  <- checkAssignee(i.assignee)
          st <- Repo.statuses(i.dependsOn)
          waiting = Rules.waitingOn(i, st.get)
          _  <- ZIO.fail(AppError.invalid(s"#${id.value} 还在等上游 ${waiting.map(w => s"#${w.value}").mkString(", ")} 结束")).when(waiting.nonEmpty)
          after = i.copy(status = IssueStatus.InProgress)
          _  <- apply(By.user, i, after, List(Some(Activity.Started), Option.when(i.status != after.status)(Activity.StatusChanged(i.status, after.status))).flatten)
        yield ()
      )

    def delete(id: IssueId): IO[AppError, Unit] =
      db.write(
        for
          _     <- Repo.require(id)
          downs <- Repo.downstream(id)
          _     <- Repo.delete(id)
          _ <- ZIO.foreachDiscard(downs) { d =>
                 for
                   down <- Repo.require(d)
                   t    <- now
                   _    <- Repo.save(down.copy(updatedAt = t))
                   evs  <- record(d, Actor.User, List(Activity.UpstreamDeleted(id)))
                   _    <- dispatch(By.user, down, evs)
                 yield ()
               }
          _ <- changed
        yield ()
      )

    // ---- 会话事件 ----

    def onSession(e: SessionEvent): UIO[Unit] =
      val work: IO[AppError, Unit] = e match
        case SessionEvent.Settled(_, agent, Origin.Issue(i), Outcome.Answered(_), _) => nudge(i, agent)
        case SessionEvent.Settled(s, _, Origin.Issue(i), Outcome.Failed(err), _) =>
          db.write(
            Repo.get(i).flatMap(ZIO.foreachDiscard(_) { _ =>
              record(i, Actor.System(Some(s)), List(Activity.SessionFailed(err))) *> changed
            })
          )
        case _ => ZIO.unit
      work.catchAll(err => ZIO.logError(s"看板处理会话事件失败：${err.message}"))

    /** issue 会话做完一段工作（回到空闲、没有在等的事）后，issue 还该它处理：请它表态，直到下次有动态投给它之前只请一次。
      * 「空闲」就是这个事件本身（事件发出时驱动还没退出，不再另查工作状态）。
      */
    private def nudge(id: IssueId, agent: AgentId): IO[AppError, Unit] =
      db.write(
        Repo.get(id).flatMap(ZIO.foreachDiscard(_) { i =>
          for
            st    <- Repo.statuses(i.dependsOn)
            asked <- Repo.nudged(id, agent)
            r     <- render(List(i), Nil)
            _ <- ZIO.when(Rules.worker(i, Rules.waitingOn(i, st.get)).contains(agent) && !asked) {
                   sessions.deliverIn(Address.IssueOf(agent, id, s"#${id.value} ${i.title}"), Letter(Source.Issue(id.value), r.nudge(i))) *>
                     Repo.markNudged(id, agent)
                 }
          yield ()
        })
      )

    def forgetAgent(agent: AgentId): IO[AppError, Unit] =
      db.write(
        for
          ids <- Repo.ofAgent(agent)
          _ <- ZIO.foreachDiscard(ids) { id =>
                 for
                   i    <- Repo.require(id)
                   after = i.copy(owner = i.owner.filterNot(_ == agent), assignee = i.assignee.filterNot(_.agent.contains(agent)))
                   (_, changes) = Rules.diff(i, Patch(assignee = Some(after.assignee)))
                   ownerChange  = Option.when(after.owner != i.owner)(Activity.OwnerChanged(i.owner, after.owner))
                   t    <- now
                   _    <- Repo.save(after.copy(updatedAt = t))
                   _    <- record(id, Actor.System(None), ownerChange.toList ++ changes)
                 yield ()
               }
          _ <- Repo.clearNudgesOf(agent)
          _ <- changed
        yield ()
      )
