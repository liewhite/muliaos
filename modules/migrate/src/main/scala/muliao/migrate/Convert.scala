package muliao.migrate

import muliao.board.Activity
import muliao.board.store.{DepRow, EventRow, IssueRow}
import muliao.contract.*
import muliao.history.store.HistoryRow
import muliao.notifications.store.{NotificationRow, SubscriptionRow}
import muliao.contract.AgentState
import muliao.resources.agents.AgentRecord
import muliao.sessions.{DelegationId, TrackedMode}
import muliao.sessions.store.{DelegationRow, UploadRow, UsageRow}
import muliao.triggers.{Mode, Precheck}
import muliao.triggers.store.{FaultRow, TriggerRow}
import muliao.workspace.store.{GrantRow, JobRow}
import zio.json.*

import java.time.ZoneId

/** 旧版各表的行 → 新版的行（会话时间线见 [[Transcript]]）。纯函数；转不了的返回 Left 说明原因，由调用方计入报告。 */
object Convert:

  def agent(a: Old.Agent): AgentRecord =
    val state = if a.deleting then AgentState.Deleting else if a.archived then AgentState.Archived else AgentState.Active
    AgentRecord(AgentId.wrap(a.id), AgentName.wrap(a.name), AgentRecord.keyOf(a.name), state, a.isDefault, None, a.createdAt, a.updatedAt)

  // ---- 看板 ----

  def issue(i: Old.Issue): Either[String, IssueRow] =
    for
      st <- IssueStatus.parse(i.status).left.map(e => s"issue #${i.id}：$e")
      pr <- Priority.parse(i.priority).left.map(e => s"issue #${i.id}：$e")
    yield IssueRow(IssueId.wrap(i.id), i.title, i.description, st, pr, i.owner.map(AgentId.wrap), i.assignee.map(Assignee.decode), i.creator,
      i.createdAt, i.updatedAt)

  def dependency(d: Old.Dependency): DepRow = DepRow(IssueId.wrap(d.issueId), IssueId.wrap(d.upstream))

  private final case class FromTo[A](from: A, to: A)
  private object FromTo:
    given [A: JsonDecoder]: JsonDecoder[FromTo[A]] = DeriveJsonDecoder.gen

  /** 旧版的指派写法：`{"kind":"user"}` / `{"kind":"agent","agent_id":…}`。 */
  private final case class OldAssignee(kind: String, agent_id: Option[String] = None):
    def value: Either[String, Assignee] = (kind, agent_id) match
      case ("user", _)        => Right(Assignee.User)
      case ("agent", Some(a)) => Right(Assignee.Agent(AgentId.wrap(a)))
      case _                  => Left(s"指派写法认不出：$kind")
  private object OldAssignee:
    given JsonDecoder[OldAssignee] = DeriveJsonDecoder.gen

  private final case class UpstreamChange(issue: Long, from: String, to: String)
  private object UpstreamChange:
    given JsonDecoder[UpstreamChange] = DeriveJsonDecoder.gen

  /** 旧版早期的状态（后来取消了）只留在历史动态里：todo（交给 agent 去做）→ in_progress，waiting（等别人）→ blocked。 */
  val legacyStatus: Map[String, IssueStatus] = Map("todo" -> IssueStatus.InProgress, "waiting" -> IssueStatus.Blocked)

  private def status(s: String): Either[String, IssueStatus] =
    IssueStatus.parse(s).orElse(legacyStatus.get(s).toRight(s"状态 $s 认不出"))
  private def priority(s: String): Either[String, Priority]  = Priority.parse(s)

  /** 时间线的一条。旧版的 `unsettled`（请 agent 表态）新版不记在时间线上（只记在请表态表里），返回 Right(None)。 */
  def event(a: Old.Activity): Either[String, Option[EventRow]] =
    def change[A: JsonDecoder]: Either[String, A] = a.change.toRight("缺少 change").flatMap(_.fromJson[A])
    val activity: Either[String, Option[Activity]] = a.kind match
      case "created"     => Right(Some(Activity.Created))
      case "comment"     => Right(Some(Activity.Commented(a.body.getOrElse(""))))
      case "description" => Right(Some(Activity.DescriptionChanged))
      case "stopped"     => Right(Some(Activity.Stopped))
      case "run_failed"  => Right(Some(Activity.SessionFailed(a.body.getOrElse(""))))
      case "unsettled"   => Right(None)
      case "status" =>
        change[FromTo[String]].flatMap(c => for f <- status(c.from); t <- status(c.to) yield Some(Activity.StatusChanged(f, t)))
      case "priority" =>
        change[FromTo[String]].flatMap(c => for f <- priority(c.from); t <- priority(c.to) yield Some(Activity.PriorityChanged(f, t)))
      case "title" => change[FromTo[String]].map(c => Some(Activity.TitleChanged(c.from, c.to)))
      case "owner" =>
        change[FromTo[Option[String]]].map(c => Some(Activity.OwnerChanged(c.from.map(AgentId.wrap), c.to.map(AgentId.wrap))))
      case "assignee" =>
        change[FromTo[Option[OldAssignee]]].flatMap { c =>
          for
            f <- c.from.fold(Right(None))(_.value.map(Some(_)))
            t <- c.to.fold(Right(None))(_.value.map(Some(_)))
          yield Some(Activity.AssigneeChanged(f, t))
        }
      case "depends_on" =>
        change[FromTo[List[Long]]].map(c => Some(Activity.DependsChanged(c.from.map(IssueId.wrap), c.to.map(IssueId.wrap))))
      case "dependency" =>
        change[UpstreamChange].flatMap(c =>
          for f <- status(c.from); t <- status(c.to) yield Some(Activity.UpstreamChanged(IssueId.wrap(c.issue), f, t))
        )
      case other => Left(s"动态种类 $other 认不出")
    activity.left
      .map(e => s"issue #${a.issueId} 的动态 ${a.id}（${a.kind}）：$e")
      .map(_.map(act => EventRow(a.id, IssueId.wrap(a.issueId), a.at, a.actor, a.sessionId.map(SessionId.wrap), act.toJson)))

  // ---- 触发器 ----

  private final case class OldConfig(
      cron: Option[String] = None,
      at: Option[String] = None,
      command: Option[String] = None,
      secrets: Option[List[String]] = None,
      debounce_seconds: Option[Double] = None
  )
  private object OldConfig:
    given JsonDecoder[OldConfig] = DeriveJsonDecoder.gen

  private final case class OldPrecheck(command: String, secrets: Option[List[String]] = None, fire_when: Option[String] = None)
  private object OldPrecheck:
    given JsonDecoder[OldPrecheck] = DeriveJsonDecoder.gen

  private final case class OldState(skipped: Option[Int] = None)
  private object OldState:
    given JsonDecoder[OldState] = DeriveJsonDecoder.gen

  private final case class OldFault(source: String, error: String)
  private object OldFault:
    given JsonDecoder[OldFault] = DeriveJsonDecoder.gen

  /** 开关照搬旧版（正式切换时旧版已停，不会两边同时触发，#881）；配置与前置检查按新版的规则校验一遍；提示、命令、前置检查里的旧主目录路径经 `path` 换成新版的。 */
  def trigger(t: Old.Trigger, zone: ZoneId, now: Long, path: String => String = identity): Either[String, (TriggerRow, List[FaultRow])] =
    val converted = for
      kind  <- TriggerKind.parse(t.kind)
      old   <- t.config.fromJson[OldConfig]
      config = TriggerConfig(old.cron, old.at, old.command.map(path), old.secrets, old.debounce_seconds)
      _     <- Mode.parse(kind, config, zone)
      pre   <- t.precheck.filter(_.trim.nonEmpty).fold(Right(None))(_.fromJson[OldPrecheck].map(Some(_)))
      when  <- pre.flatMap(_.fire_when).fold(Right(FireWhen.Output))(FireWhen.parse)
      spec   = pre.map(p => PrecheckSpec(path(p.command), when, p.secrets.getOrElse(Nil)))
      _     <- spec.fold(Right(()))(Precheck.parse(_).map(_ => ()))
      state <- t.state.filter(_.trim.nonEmpty).fold(Right(OldState()))(_.fromJson[OldState])
      faults <- t.faults.filter(_.trim.nonEmpty).fold(Right(Nil))(_.fromJson[List[OldFault]])
      srcs   <- faults.foldRight[Either[String, List[(FaultSource, OldFault)]]](Right(Nil))((f, acc) => FaultSource.parse(f.source).flatMap(s => acc.map((s, f) :: _)))
    yield
      val baseline = spec.filter(_.fireWhen == FireWhen.Changed).flatMap(_ => t.precheckOutput)
      val row = TriggerRow(TriggerId.wrap(t.id), TriggerName.wrap(t.name), AgentId.wrap(t.agentId), path(t.prompt), kind, config.toJson, enabled = t.enabled,
        spec.map(_.toJson), None, state.skipped.getOrElse(0), t.lastSessionId.map(SessionId.wrap), t.lastFiredAt, None, baseline,
        t.finishedAt, t.createdAt, t.lastFiredAt.getOrElse(t.createdAt))
      (row, srcs.map((s, f) => FaultRow(TriggerId.wrap(t.id), s, f.error, now)))
    converted.left.map(e => s"触发器 ${t.name}（${t.id}）：$e")

  // ---- 其他 ----

  def notification(n: Old.Notification): NotificationRow =
    NotificationRow(n.id, n.title, n.body, n.sessionId.map(SessionId.wrap), n.link, n.unread, n.createdAt)

  def push(p: Old.Push): SubscriptionRow = SubscriptionRow(p.endpoint, p.p256dh, p.auth, p.createdAt)

  def history(h: Old.History): Either[String, HistoryRow] =
    h.refs.fromJson[List[String]].left.map(e => s"工作历史 ${h.id} 的引用解不开：$e").map(refs =>
      HistoryRow(h.id, AgentId.wrap(h.agentId), h.day, h.title, h.summary, refs.toJson, h.importance, None, h.createdAt)
    )

  /** 旧版一行可能是几次请求的合计，原样记一行（总量不变）；会话已删的记空串。 */
  def usage(u: Old.Usage): UsageRow =
    UsageRow(u.id, u.at, AgentId.wrap(u.agentId), SessionId.wrap(u.sessionId.getOrElse("")), u.model, u.input, u.output, u.cacheRead, u.cacheWrite)

  def upload(f: Old.File): UploadRow = UploadRow(FileId.wrap(f.id), f.sha256, f.name, f.mediaType, f.size, f.createdAt)

  /** 后台任务只搬记录：旧版还在跑的记成被中断，结局算已交回（新版不补交、不接管进程）。 */
  def job(j: Old.Job): JobRow =
    val state = JobStatus.parse(j.status).toOption.filter(_ != JobStatus.Running).getOrElse(JobStatus.Interrupted)
    JobRow(JobId.wrap(j.id), AgentId.wrap(j.agentId), SessionId.wrap(j.sessionId), j.command, state, j.exitCode.filter(_ => state != JobStatus.Interrupted),
      reported = true, j.startedAt, j.finishedAt.orElse(Some(j.startedAt)))

  def grant(g: Old.Grant): GrantRow =
    GrantRow(g.id, AgentId.wrap(g.agentId), SkillName.wrap(g.skill), g.script, g.args, g.secrets, g.fingerprint, g.createdAt)

  /** 委派记录（都已结束）。`callEntry`：发起会话里那次调用所在的回复条目。回答推给用户的委派新版不记，返回 None。 */
  def delegation(t: Old.ThreadCall, callEntry: Option[Long], at: Long): Option[DelegationRow] =
    val mode = t.mode match
      case "ask"         => Some(TrackedMode.Ask)
      case "report_back" => Some(TrackedMode.Report)
      case _             => None
    mode.map(m => DelegationRow(DelegationId.wrap(t.id), SessionId.wrap(t.parent), SessionId.wrap(t.thread), m, callEntry, Some(CallId.wrap(t.callId)), done = true, None, at))
