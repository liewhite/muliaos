package muliao.sessions

import muliao.contract.*
import muliao.models.llm.Models
import muliao.models.mcp.McpConnector
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.{Database, Tx}
import muliao.platform.events.ChangeBus
import muliao.resources.{AgentHook, AgentHooks, ResourcePaths, Resources}
import muliao.resources.agents.Agents
import muliao.resources.vault.Vault
import muliao.sessions.store.*
import zio.*

import java.time.{Instant, LocalDate}

/** 会话与运行领域给别的领域与界面的入口。
  *
  * 别的领域：投信（[[deliver]]，触发器开会话、看板投 issue 动态）、查工作状态（[[states]]，全系统唯一判断）、停止（[[stop]]）、
  * 登记与结束异步工作（[[openWait]] / [[closeWait]]，如后台任务）、读时间线（[[timeline]]，`session_read`）。
  * 往本领域接东西（工具、事件订阅、异步工作种类、批准后的执行者、未读来源）用 [[SessionHooks]]。
  */
trait Sessions:
  /** 启动：收尾上次被打断的调用、续跑、起等待调度。要在各领域都往 [[SessionHooks]] 接好工具后调用。 */
  def start: IO[AppError, Unit]

  /** 投一封信，投不进时以领域错误说明原因。`dedupe`：同样键的信已在排队就不投（返回 None）。 */
  def deliver(to: Address, letter: Letter, dedupe: Option[String] = None): IO[AppError, Option[Delivered]]

  /** 在调用方的写事务里投信（看板：与 issue 的改动同一事务提交，提交后才开始跑）。投不进以领域错误说明原因，此时本函数还没写任何东西，
    * 调用方可以接住错误、照常提交自己的改动。不带附件。
    */
  def deliverIn(to: Address, letter: Letter, dedupe: Option[String] = None): Tx[Option[Delivered]]

  /** 这些 issue 的 issue 会话（每个 issue + agent 一个）。 */
  def issueSessions(issues: List[IssueId]): IO[AppError, List[IssueSession]]

  def info(id: SessionId): IO[AppError, SessionInfo]
  def states(ids: List[SessionId]): IO[AppError, Map[SessionId, WorkState]]
  def state(id: SessionId): IO[AppError, WorkState]

  /** 停止（空闲时什么也不做）。 */
  def stop(id: SessionId): IO[AppError, Unit]

  /** 登记一项异步工作（会话因此「等待中」）；`heartbeat` 给出时定期投检查。返回等待编号。 */
  def openWait(session: SessionId, kind: String, ref: String, label: String, heartbeat: Option[Duration]): IO[AppError, Long]

  /** 结束一项异步工作，`letter` 给出时作为事件投回发起会话。已结束的返回 false。 */
  def closeWait(kind: String, ref: String, letter: Option[Letter]): IO[AppError, Boolean]

  def timeline(id: SessionId): IO[AppError, List[TimelineItem]]

  /** 按编号或它的唯一前缀找会话（`session_read` 用）。 */
  def find(prefix: String): IO[AppError, SessionInfo]

  /** [from, to) 里的会话活动（活动回顾用）：有来信或回复的会话、用户亲手发的消息、最终失败的运行。`agent` 不给是全部。 */
  def activity(agent: Option[AgentId], from: Long, to: Long): IO[AppError, SessionActivity]
  /** 最后一条回复的正文。 */
  def lastAnswer(id: SessionId): IO[AppError, String]

  // ---- 界面 ----
  def list(agent: Option[AgentId], q: Option[String], before: Option[Long], limit: Int): IO[AppError, SessionPage]
  def detail(id: SessionId): IO[AppError, SessionDetail]
  def retry(id: SessionId): IO[AppError, Unit]
  def answer(id: SessionId, entry: Long, call: String, a: Answer): IO[AppError, Unit]
  def compact(id: SessionId): IO[AppError, Boolean]
  def delete(id: SessionId): IO[AppError, Unit]
  def markRead(id: SessionId): IO[AppError, Unit]
  def usage(from: Long, to: Long): IO[AppError, List[UsageLine]]
  def subscribe(id: SessionId): URIO[Scope, (Option[zio.Chunk[muliao.models.llm.ReplyPart]], zio.stream.ZStream[Any, Nothing, LiveEvent])]

object Sessions:
  type Env = Database & ChangeBus & SessionHooks & Files & LiveHub & Resources & Agents & Models & McpConnector & Vault & ResourcePaths &
    SessionSettings & GeneralSettings & AgentHooks

  val layer: ZLayer[Env, AppError, Sessions] = ZLayer.scoped(
    for
      deps <- ZIO.service[Database].zip(ZIO.service[ChangeBus]).zip(ZIO.service[SessionHooks]).zip(ZIO.service[Files]).zip(ZIO.service[LiveHub])
      res  <- ZIO.service[Resources].zip(ZIO.service[Agents]).zip(ZIO.service[Models]).zip(ZIO.service[McpConnector]).zip(ZIO.service[Vault])
      more <- ZIO.service[ResourcePaths].zip(ZIO.service[SessionSettings]).zip(ZIO.service[GeneralSettings])
      (db, bus, hooks, files, live)                = deps
      (resources, agents, models, mcp, vault)      = res
      (paths, settings, general)                   = more
      engine <- Engine.make(Deps(db, bus, hooks, files, live, resources, agents, models, mcp, vault, paths, settings, general.timezone))
      _      <- hooks.addTools(SessionTools.all(engine))
      ah     <- ZIO.service[AgentHooks]
      _ <- ah.register(new AgentHook:
             val name                        = "会话"
             def busy(agent: AgentId)        = engine.agentBusy(agent)
             def cleanup(agent: AgentId)     = engine.agentCleanup(agent)
           )
    yield Live(engine)
  )

  /** 本领域自己的工具名与类别（组装时汇进全部内置工具目录）。 */
  val toolNames: List[String] = List("ask_user", "delegate_task", "ask_agent", "list_threads", "stop_thread", "wake_me")

  private final case class Live(e: Engine) extends Sessions:
    import e.deps.*

    def start: IO[AppError, Unit] = e.startup

    def deliver(to: Address, letter: Letter, dedupe: Option[String]) = e.deliver(to, letter, dedupe)
    def deliverIn(to: Address, letter: Letter, dedupe: Option[String]) = e.deliverIn(to, letter, dedupe)

    def issueSessions(issues: List[IssueId]): IO[AppError, List[IssueSession]] =
      if issues.isEmpty then ZIO.succeed(Nil)
      else
        db.read(Repo.issueSessions(issues.map(_.value.toString))).map(_.flatMap { r =>
          r.originRef.flatMap(_.toLongOption).map(i => IssueSession(IssueId.wrap(i), r.agentId, r.id))
        })
    def info(id: SessionId)                                          = e.info(id)
    def states(ids: List[SessionId])                                 = e.states(ids)
    def state(id: SessionId)                                         = e.state(id)
    def stop(id: SessionId)                                          = e.stop(id, byUser = false)
    def openWait(session: SessionId, kind: String, ref: String, label: String, heartbeat: Option[Duration]) =
      e.openWait(session, kind, ref, label, heartbeat, None)
    def closeWait(kind: String, ref: String, letter: Option[Letter]) = e.closeWait(kind, ref, letter)
    def lastAnswer(id: SessionId)                                    = e.lastAnswer(id)
    def retry(id: SessionId)                                         = e.retry(id)
    def answer(id: SessionId, entry: Long, call: String, a: Answer)  = e.answer(id, entry, call, a)
    def compact(id: SessionId)                                       = e.compactNow(id)
    def delete(id: SessionId)                                        = e.delete(id)
    def markRead(id: SessionId)                                      = e.info(id) *> hooks.unread.flatMap(_.markRead(id))
    def subscribe(id: SessionId)                                     = live.subscribe(id)

    private def display: UIO[String => String] =
      hooks.tools.map(ts => ts.map(t => t.spec.name -> t.spec.display.toString).toMap).map(m => n => m.getOrElse(n, "Plain"))

    def timeline(id: SessionId): IO[AppError, List[TimelineItem]] =
      for
        _       <- e.info(id)
        entries <- db.read(Repo.entriesOf(id)).flatMap(Decode.entries)
        calls   <- db.read(Repo.callsOfSession(id)).flatMap(Decode.calls).map(_.groupBy(_.entryId))
        fileIds  = entries.flatMap {
                     case Entry(_, _, EntryBody.Mail(l), _, _) => l.files.toList
                     case _                                    => Nil
                   }
        uploads <- db.read(Repo.uploadsOf(fileIds.distinct.map(FileId.wrap)))
        d       <- display
      yield Timeline.items(entries, calls, uploads.map(u => u.id.value -> u).toMap, d)

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
        letters <- ZIO.foreach(msgs.filter(_.kind == "letter"))(m => Decode.entry(m).map(m -> _.body))
        states  <- e.states(ids)
      yield
        val bySession = msgs.groupBy(_.sessionId)
        val runsOf    = runs.groupBy(_.sessionId)
        val failed    = runs.filter(r => r.state == RunState.Failed.label && r.finishedAt.exists(f => f >= from && f < to))
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
    private def names(ids: List[AgentId]): UIO[Map[AgentId, String]] =
      ZIO.foreach(ids.distinct)(id => agents.get(id).map(a => id -> a.name.value).orElseSucceed(id -> id.value)).map(_.toMap)

    private def summaries(rows: List[SessionRow]): IO[AppError, List[SessionSummary]] =
      for
        st     <- e.states(rows.map(_.id))
        unread <- hooks.unread.flatMap(_.unread(rows.map(_.id)))
        ns     <- names(rows.map(_.agentId))
      yield rows.map(r =>
        SessionSummary(r.id, r.agentId, ns.getOrElse(r.agentId, r.agentId.value), r.title, r.origin, r.originRef, r.parentId,
          st.getOrElse(r.id, WorkState.Idle).label, unread(r.id), r.createdAt, r.updatedAt)
      )

    def list(agent: Option[AgentId], q: Option[String], before: Option[Long], limit: Int): IO[AppError, SessionPage] =
      val n    = limit.max(1).min(200)
      val text = q.map(_.trim).filter(_.nonEmpty)
      for
        rows   <- db.read(Repo.listSessions(agent, text, before.getOrElse(Long.MaxValue), n))
        pinned <- if before.isEmpty && text.isEmpty then db.read(Repo.busySessions).map(_.filter(r => agent.forall(_ == r.agentId))) else ZIO.succeed(Nil)
        ids     = pinned.map(_.id).toSet
        all    <- summaries(pinned.sortBy(-_.updatedAt) ++ rows.filterNot(r => ids(r.id)))
      yield SessionPage(all, Option.when(rows.size == n)(rows.last.updatedAt))

    def detail(id: SessionId): IO[AppError, SessionDetail] =
      for
        row     <- db.read(Repo.session(id)).someOrFail(AppError.notFound(s"会话 ${id.value} 不存在（可能已删除）"))
        summary <- summaries(List(row)).map(_.head)
        run     <- db.read(Repo.lastRun(id))
        queued  <- db.read(Repo.queued(id))
        letters <- ZIO.foreach(queued)(q => Decode.letter(q.letter).map(q -> _))
        uploads <- db.read(Repo.uploadsOf(letters.flatMap(_._2.files.toList).distinct.map(FileId.wrap))).map(_.map(u => u.id.value -> u).toMap)
        waits   <- e.waitsOf(id)
        open    <- db.read(Repo.openCalls(id)).flatMap(Decode.calls)
        size    <- e.contextSize(id)
        used    <- db.read(Repo.usageOfSession(id))
      yield SessionDetail(
        summary = summary,
        run = run.map(r => RunView(r.id.value, r.state, r.error, r.retryNote, r.startedAt, r.finishedAt)),
        queued = letters.map((q, l) => QueuedView(q.id, l.source.label, l.text, l.files.toList.flatMap(uploads.get).map(Timeline.file), l.checkIn, q.createdAt)),
        waits = waits.map(w => WaitItem(w.id, w.kind, w.ref, w.label, w.heartbeat, w.dueAt, w.createdAt)),
        pending = open.collect { case c if c.state == CallState.Suspended => c.pause.map(p => PendingCall(c.entryId, c.callId, c.name, Timeline.pending(p))) }.flatten,
        contextTokens = size,
        compactionThreshold = settings.compactionThresholdTokens,
        usage = UsageView(used.map(_.input).sum, used.map(_.output).sum, used.map(_.cacheRead).sum, used.map(_.cacheWrite).sum)
      )

    def usage(from: Long, to: Long): IO[AppError, List[UsageLine]] =
      for
        rows <- db.read(Repo.usageBetween(from, to))
        ns   <- names(rows.map(_.agentId))
      yield
        def day(at: Long): LocalDate = Instant.ofEpochMilli(at).atZone(zone).toLocalDate
        rows
          .groupBy(r => (day(r.at), r.agentId, r.model))
          .toList
          .map { case ((d, a, m), rs) =>
            UsageLine(d.toString, a, ns.getOrElse(a, a.value), m, rs.size.toLong, rs.map(_.input).sum, rs.map(_.output).sum, rs.map(_.cacheRead).sum, rs.map(_.cacheWrite).sum)
          }
          .sortBy(l => (l.day, l.agentName, l.model))
