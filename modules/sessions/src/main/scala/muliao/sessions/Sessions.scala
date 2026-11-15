package muliao.sessions

import muliao.contract.*
import muliao.models.llm.Models
import muliao.models.mcp.McpConnector
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.{Database, Tx}
import muliao.platform.events.ChangeBus
import muliao.resources.{AgentHook, ResourcePaths, Resources}
import muliao.resources.agents.Agents
import muliao.resources.vault.Vault
import muliao.sessions.store.*
import zio.*

import java.time.{Instant, LocalDate}

/** 会话与运行领域给别的领域的入口。
  *
  * 投信（[[deliver]]，触发器开会话、看板投 issue 动态）、查工作状态（[[states]]，全系统唯一判断）、停止（[[stop]]）、
  * 登记与结束会话在等的后台任务（[[openJobWait]] / [[closeJobWait]]）、读时间线（[[timeline]]，`session_read`）。
  * 别的领域往本领域接的东西（工具、事件订阅、批准后的执行者、后台任务、未读）在 [[start]] 时以 [[SessionPorts]] 一次交进来。
  * 界面用的操作在 [[SessionConsole]]。
  */
trait Sessions:
  /** 启动：定下各领域接进来的端口，再收尾上次被打断的调用、续跑、起等待调度。只调一次；之前投进来的信等到这时才开始跑。 */
  def start(ports: SessionPorts): IO[AppError, Unit]

  /** 本领域对 agent 生命周期的参与：有会话在跑或在等时忙，删除时删掉它的全部会话。 */
  def agentHook: AgentHook

  /** 投一封信，投不进时以领域错误说明原因。`dedupe`：同样键的信已在排队就不投（返回 None）。 */
  def deliver(to: Address, letter: Letter, dedupe: Option[String] = None): IO[AppError, Option[Delivered]]

  /** 在调用方的写事务里投信（看板：与 issue 的改动同一事务提交，提交后才开始跑）。投不进以领域错误说明原因，此时本函数还没写任何东西，
    * 调用方可以接住错误、照常提交自己的改动。只有文字，不带附件。
    */
  def deliverIn(to: Address, source: Source, text: String, dedupe: Option[String] = None): Tx[Option[Delivered]]

  /** 这些 issue 的 issue 会话（每个 issue + agent 一个）。 */
  def issueSessions(issues: List[IssueId]): IO[AppError, List[IssueSession]]

  def info(id: SessionId): IO[AppError, SessionInfo]
  def states(ids: List[SessionId]): IO[AppError, Map[SessionId, WorkState]]
  def state(id: SessionId): IO[AppError, WorkState]

  /** 停止（空闲时什么也不做）。 */
  def stop(id: SessionId): IO[AppError, Unit]

  /** 会话开始等一个后台任务（因此「等待中」）；`heartbeat` 给出时定期投检查（状态取自 [[JobControl]]）。 */
  def openJobWait(session: SessionId, job: JobId, label: String, heartbeat: Option[Duration]): IO[AppError, Unit]

  /** 后台任务结束，`letter` 给出时作为事件投回在等它的会话。没人在等（已结束过、会话被停止）返回 false。 */
  def closeJobWait(job: JobId, letter: Option[Letter]): IO[AppError, Boolean]

  def timeline(id: SessionId): IO[AppError, List[TimelineItem]]

  /** 按编号或它的唯一前缀找会话（`session_read` 用）。 */
  def find(prefix: String): IO[AppError, SessionInfo]

  /** [from, to) 里的会话活动（活动回顾用）：有来信或回复的会话、用户亲手发的消息、最终失败的运行。`agent` 不给是全部。 */
  def activity(agent: Option[AgentId], from: Long, to: Long): IO[AppError, SessionActivity]
  /** 最后一条回复的正文。 */
  def lastAnswer(id: SessionId): IO[AppError, String]

/** 界面（会话页、会话列表、用量页）用的操作，只有本领域的 HTTP 接口用。 */
trait SessionConsole:
  /** @param origins 只要这些来源（空是不限） */
  def list(agent: Option[AgentId], q: Option[String], before: Option[Long], limit: Int, origins: List[OriginKind] = Nil): IO[AppError, SessionPage]
  def detail(id: SessionId): IO[AppError, SessionDetail]
  def retry(id: SessionId): IO[AppError, Unit]
  def answer(id: SessionId, call: CallKey, a: Answer): IO[AppError, Unit]
  def compact(id: SessionId): IO[AppError, Boolean]
  def delete(id: SessionId): IO[AppError, Unit]
  def markRead(id: SessionId): IO[AppError, Unit]
  def usage(from: Long, to: Long): IO[AppError, List[UsageLine]]
  def subscribe(id: SessionId): URIO[Scope, (Option[zio.Chunk[muliao.models.llm.ReplyPart]], zio.stream.ZStream[Any, Nothing, LiveEvent])]
  /** 界面怎么展示某个工具的调用与结果。 */
  def display: UIO[Timeline.Shows]

object Sessions:
  type Env = Database & ChangeBus & Files & LiveHub & Resources & Agents & Models & McpConnector & Vault & ResourcePaths & SessionSettings &
    GeneralSettings

  /** 按子领域装起本领域的各部分（各自是一个服务，依赖只朝下：底子 → 信箱 → 等待 → 委派 → 请求模型 → 驱动 / 控制 / 等待调度），
    * 对别的领域给 [[Sessions]]，对界面给 [[SessionConsole]]。
    */
  val layer: ZLayer[Env, AppError, Sessions & SessionConsole] =
    ZLayer.makeSome[Env, Sessions & SessionConsole](
      Ports.layer,
      Kernel.layer,
      Slots.layer,
      Doorbell.layer,
      States.layer,
      Mailbox.layer,
      Waits.layer,
      Delegations.layer,
      Requests.layer,
      Compaction.layer,
      CallRunner.layer,
      Driver.layer,
      Control.layer,
      WaitClock.layer,
      ZLayer.derive[Parts],
      ZLayer.fromZIOEnvironment(ZIO.serviceWith[Parts] { p =>
        val l = Live(p)
        ZEnvironment[Sessions, SessionConsole](l, l)
      })
    )

  /** 本领域自己的工具（组装时汇进全部内置工具目录）。 */
  val specs: List[muliao.toolkit.ToolSpec] = SessionTools.specs

  /** 本领域的各部分。 */
  private final case class Parts(
      kernel: Kernel,
      mailbox: Mailbox,
      workStates: States,
      waits: Waits,
      delegations: Delegations,
      requests: Requests,
      driver: Driver,
      control: Control,
      clock: WaitClock,
      agents: Agents
  )

  private final class Live(parts: Parts) extends Sessions, SessionConsole:
    import parts.*
    import parts.kernel.*

    def start(p: SessionPorts): IO[AppError, Unit] =
      val own = Contribution(tools = SessionTools.all(delegations, control, waits, workStates, agents, zone))
      ports.settle(p.copy(contributions = own :: p.contributions)) *> driver.start *> clock.start

    val agentHook: AgentHook = AgentHook("会话", control.agentCleanup, control.agentBusy)

    def deliver(to: Address, letter: Letter, dedupe: Option[String])   = mailbox.deliver(to, letter, dedupe)
    def deliverIn(to: Address, source: Source, text: String, dedupe: Option[String]) = mailbox.deliverIn(to, source, text, dedupe)

    def issueSessions(issues: List[IssueId]): IO[AppError, List[IssueSession]] =
      if issues.isEmpty then ZIO.succeed(Nil)
      else
        db.read(Repo.issueSessions(issues)).flatMap(ZIO.foreach(_)(SessionInfo.decode)).map(_.collect { case SessionInfo(id, agent, Origin.Issue(i), _, _, _, _) =>
          IssueSession(i, agent, id)
        })
    def info(id: SessionId)          = kernel.info(id)
    def states(ids: List[SessionId]) = workStates.states(ids)
    def state(id: SessionId)         = workStates.state(id)
    def stop(id: SessionId)          = control.stop(id, byUser = false)
    def openJobWait(session: SessionId, job: JobId, label: String, heartbeat: Option[Duration]) =
      waits.open(session, WaitRef.Job(job), label, heartbeat).unit
    def closeJobWait(job: JobId, letter: Option[Letter])        = waits.close(WaitKey.job(job), letter)
    def lastAnswer(id: SessionId)                               = kernel.lastAnswer(id)
    def retry(id: SessionId)                                    = control.retry(id)
    def answer(id: SessionId, call: CallKey, a: Answer)         = control.answer(id, call, a)
    def compact(id: SessionId)                                  = control.compactNow(id)
    def delete(id: SessionId)                                   = control.delete(id)
    def markRead(id: SessionId)                                 = kernel.info(id) *> ports.unread.flatMap(_.markRead(id))
    def subscribe(id: SessionId)                                = live.subscribe(id)
    def display: UIO[Timeline.Shows]                         = ports.display

    private final case class Queued(row: InboxRow, letter: Letter)

    def timeline(id: SessionId): IO[AppError, List[TimelineItem]] =
      for
        _       <- kernel.info(id)
        entries <- db.read(Repo.entriesOf(id)).flatMap(Decode.entries)
        calls   <- db.read(Repo.callsOfSession(id)).flatMap(Decode.calls).map(_.groupBy(_.entryId))
        fileIds  = entries.flatMap {
                     case Entry(_, _, EntryBody.Mail(l), _, _) => l.files.toList
                     case _                                    => Nil
                   }
        uploads <- db.read(Repo.uploadsOf(fileIds.distinct))
        d       <- display
      yield Timeline.items(entries, calls, uploads.map(u => u.id -> u).toMap, d)

    def find(prefix: String): IO[AppError, SessionInfo] =
      val p = prefix.trim.toLowerCase
      if p.isEmpty || !p.forall(c => c.isDigit || ('a' to 'f').contains(c)) then ZIO.fail(AppError.invalid(s"会话编号「$prefix」不对：是十六进制字符（给前缀也行）"))
      else
        db.read(Repo.sessionsWithPrefix(p)).flatMap {
          case List(row) => SessionInfo.decode(row)
          case Nil       => ZIO.fail(AppError.notFound(s"没有编号以 $p 开头的会话（可能已删除）"))
          case _         => ZIO.fail(AppError.invalid(s"编号以 $p 开头的会话不止一个，多给几位"))
        }

    def activity(agent: Option[AgentId], from: Long, to: Long): IO[AppError, SessionActivity] =
      for
        rows    <- db.read(Repo.touchedSince(agent, from))
        ids      = rows.map(_.id)
        msgs    <- db.read(Repo.messagesBetween(ids, from, to))
        runs    <- db.read(Repo.runsBetween(ids, from, to))
        infos   <- ZIO.foreach(rows)(SessionInfo.decode).map(_.map(i => i.id -> i).toMap)
        letters <- ZIO.foreach(msgs.filter(_.kind == EntryKind.Letter))(m => Decode.entry(m).map(m -> _.body))
        states  <- workStates.states(ids)
      yield
        val bySession = msgs.groupBy(_.sessionId)
        val runsOf    = runs.groupBy(_.sessionId)
        val failed    = runs.filter(r => r.state == RunState.Failed && r.finishedAt.exists(f => f >= from && f < to))
        def inRange(t: Long) = t >= from && t < to
        val active = (bySession.keys ++ runsOf.keys).toList.distinct.flatMap { id =>
          infos.get(id).map { i =>
            val ms    = bySession.getOrElse(id, Nil)
            val rs    = runsOf.getOrElse(id, Nil)
            val times = ms.map(_.startedAt) ++ rs.flatMap(r => (r.startedAt :: r.finishedAt.toList).filter(inRange))
            ActiveSession(i.id, i.agent, i.origin, i.title, states.getOrElse(id, WorkState.Idle), ms.size, times.min, times.max, rs.size, rs.count(failed.contains))
          }
        }
        val said = letters.collect { case (row, EntryBody.Mail(l)) if l.source == Source.User => row -> l.text }.flatMap { (row, text) =>
          infos.get(row.sessionId).map(i => UserSaid(row.sessionId, i.agent, row.startedAt, text))
        }
        val failures = failed.flatMap(r => infos.get(r.sessionId).map(i => RunFailure(r.sessionId, i.agent, r.id, r.finishedAt.getOrElse(r.startedAt), r.error.getOrElse("（没有记下原因）"))))
        SessionActivity(active.sortBy(_.first), said.sortBy(_.at), failures.sortBy(_.at))
    private def summaries(rows: List[SessionRow]): IO[AppError, List[SessionSummary]] =
      for
        st     <- workStates.states(rows.map(_.id))
        unread <- ports.unread.flatMap(_.unread(rows.map(_.id)))
        ns     <- agents.names(rows.map(_.agentId))
        infos  <- ZIO.foreach(rows)(SessionInfo.decode)
      yield infos.map(i =>
        SessionSummary(i.id, i.agent, ns(i.agent), i.title, i.origin, st.getOrElse(i.id, WorkState.Idle), unread(i.id), i.createdAt, i.updatedAt)
      )

    def list(agent: Option[AgentId], q: Option[String], before: Option[Long], limit: Int, origins: List[OriginKind]): IO[AppError, SessionPage] =
      val n    = limit.max(1).min(200)
      val text = q.map(_.trim).filter(_.nonEmpty)
      for
        rows   <- db.read(Repo.listSessions(agent, origins, text, before.getOrElse(Long.MaxValue), n))
        pinned <- (if before.isEmpty && text.isEmpty then db.read(Repo.busySessions) else ZIO.succeed(Nil))
                    .map(_.filter(r => agent.forall(_ == r.agentId) && (origins.isEmpty || origins.contains(r.origin))))
        ids     = pinned.map(_.id).toSet
        all    <- summaries(pinned.sortBy(-_.updatedAt) ++ rows.filterNot(r => ids(r.id)))
      yield SessionPage(all, Option.when(rows.size == n)(rows.last.updatedAt))

    def detail(id: SessionId): IO[AppError, SessionDetail] =
      for
        row     <- db.read(Repo.session(id)).someOrFail(AppError.notFound(s"会话 ${id.value} 不存在（可能已删除）"))
        summary <- summaries(List(row)).map(_.head)
        run     <- db.read(Repo.lastRun(id))
        queued  <- db.read(Repo.queued(id))
        letters <- ZIO.foreach(queued)(q => Decode.letter(q.letter).map(Queued(q, _)))
        uploads <- db.read(Repo.uploadsOf(letters.flatMap(_.letter.files.toList).distinct)).map(_.map(u => u.id -> u).toMap)
        ws      <- waits.list(id)
        threads <- db.read(Repo.threadsOf(id)).flatMap(summaries)
        dels    <- delegations.threadsOf(ws.collect { case Wait(_, _, WaitRef.Delegation(d), _, _, _) => d })
        open    <- db.read(Repo.openCalls(id)).flatMap(Decode.calls)
        size    <- requests.contextSize(id)
        used    <- db.read(Repo.usageOfSession(id))
      yield SessionDetail(
        summary = summary,
        run = run.map(r => RunView(r.id.value, r.state, r.error, r.retryNote, r.startedAt, r.finishedAt)),
        queued = letters.map(q => QueuedView(q.row.id, q.letter.source.view, q.letter.text, q.letter.files.toList.flatMap(uploads.get).map(Timeline.file), q.letter.checkIn, q.row.createdAt)),
        waits = ws.map { w =>
          val target = w.ref match
            case WaitRef.Job(job)      => WaitTarget.Job(job)
            case WaitRef.Wake(due)     => WaitTarget.Wake(due)
            case WaitRef.Delegation(d) => WaitTarget.Delegation(dels.get(d))
          WaitItem(w.id.value, target, w.label, w.heartbeat, w.createdAt)
        },
        threads = threads,
        pending = open.collect { case Call(e, cid, _, name, _, CallStatus.Suspended(p, _)) => PendingCall(e, cid, name, Timeline.pending(p)) },
        contextTokens = size,
        compactionThreshold = settings.compactionThresholdTokens,
        usage = UsageView(used.map(_.input).sum, used.map(_.output).sum, used.map(_.cacheRead).sum, used.map(_.cacheWrite).sum)
      )

    def usage(from: Long, to: Long): IO[AppError, List[UsageLine]] =
      for
        rows <- db.read(Repo.usageBetween(from, to))
        ns   <- agents.names(rows.map(_.agentId))
      yield
        def day(at: Long): LocalDate = Instant.ofEpochMilli(at).atZone(zone).toLocalDate
        rows
          .groupBy(r => (day(r.at), r.agentId, r.model))
          .toList
          .map { case ((d, a, m), rs) =>
            UsageLine(d.toString, a, ns(a), m, rs.size.toLong, rs.map(_.input).sum, rs.map(_.output).sum, rs.map(_.cacheRead).sum, rs.map(_.cacheWrite).sum)
          }
          .sortBy(l => (l.day, l.agentName, l.model))
