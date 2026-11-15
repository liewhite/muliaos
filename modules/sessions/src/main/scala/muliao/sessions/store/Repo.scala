package muliao.sessions.store

import io.getquill.*
import muliao.contract.*
import muliao.platform.db.*
import muliao.platform.db.Sql.*
import muliao.sessions.{DelegationId, EntryKind, TrackedMode, WaitId, WaitKind}

// ---- 行 ----

final case class SessionRow(
    id: SessionId,
    agentId: AgentId,
    origin: OriginKind,
    originRef: Option[String],
    parentId: Option[SessionId],
    title: String,
    pendingReply: String,
    createdAt: Long,
    updatedAt: Long
)

final case class InboxRow(id: Long, sessionId: SessionId, letter: String, dedupeKey: Option[String], createdAt: Long)

final case class RunRow(
    id: RunId,
    sessionId: SessionId,
    state: RunState,
    error: Option[String],
    retryAt: Option[Long],
    retryNote: Option[String],
    startedAt: Long,
    finishedAt: Option[Long]
)

final case class EntryRow(
    id: Long,
    sessionId: SessionId,
    runId: Option[RunId],
    kind: EntryKind,
    body: String,
    search: String,
    startedAt: Long,
    finishedAt: Long
)

final case class ToolCallRow(
    entryId: Long,
    callId: CallId,
    sessionId: SessionId,
    seq: Int,
    name: String,
    args: String,
    state: CallState,
    pause: Option[String],
    result: Option[String],
    startedAt: Option[Long],
    finishedAt: Option[Long]
)

/** `kind` 与 `ref` 合起来是 [[muliao.sessions.WaitRef]]（见 `WaitRef.key` / `WaitRef.of`）。 */
final case class WaitRow(
    id: WaitId,
    sessionId: SessionId,
    kind: WaitKind,
    ref: String,
    label: String,
    heartbeatSeconds: Option[Int],
    dueAt: Option[Long],
    createdAt: Long
)

final case class DelegationRow(
    id: DelegationId,
    requesterId: SessionId,
    threadId: SessionId,
    mode: TrackedMode,
    callEntry: Option[Long],
    callId: Option[CallId],
    done: Boolean,
    result: Option[String],
    createdAt: Long
)

final case class UploadRow(id: FileId, sha256: String, name: String, mediaType: String, size: Long, createdAt: Long)

final case class UsageRow(
    id: Long,
    at: Long,
    agentId: AgentId,
    sessionId: SessionId,
    model: String,
    input: Long,
    output: Long,
    cacheRead: Long,
    cacheWrite: Long
)

/** 本领域全部表的读写。只做存取，不含规则。 */
private[sessions] object Repo:
  private inline def sessions    = quote(querySchema[SessionRow]("session"))
  private inline def inbox       = quote(querySchema[InboxRow]("inbox"))
  private inline def runs        = quote(querySchema[RunRow]("run"))
  private inline def entries     = quote(querySchema[EntryRow]("entry"))
  private inline def calls       = quote(querySchema[ToolCallRow]("tool_call"))
  private inline def waits       = quote(querySchema[WaitRow]("wait"))
  private inline def delegations = quote(querySchema[DelegationRow]("delegation"))
  private inline def uploads     = quote(querySchema[UploadRow]("upload"))
  private inline def usages      = quote(querySchema[UsageRow]("usage"))

  // ---- 会话 ----

  def session(id: SessionId): Read[Option[SessionRow]] =
    Sql.run(sessions.filter(_.id == lift(id))).map(_.headOption).orInternal

  def sessionsOf(ids: List[SessionId]): Read[List[SessionRow]] =
    Sql.run(sessions.filter(s => liftQuery(ids).contains(s.id))).orInternal

  def sessionsOfAgent(agent: AgentId): Read[List[SessionRow]] =
    Sql.run(sessions.filter(_.agentId == lift(agent))).orInternal

  def issueSession(agent: AgentId, issue: IssueId): Read[Option[SessionRow]] =
    val ref = issue.value.toString
    Sql.run(sessions.filter(s => s.agentId == lift(agent) && s.origin == lift(OriginKind.Issue) && s.originRef.exists(_ == lift(ref))))
      .map(_.headOption)
      .orInternal

  /** 这些 issue（`origin_ref` 是 issue 号）的全部 issue 会话。 */
  def issueSessions(issues: List[IssueId]): Read[List[SessionRow]] =
    val refs = issues.map(_.value.toString)
    Sql.run(sessions.filter(s => s.origin == lift(OriginKind.Issue) && s.originRef.exists(r => liftQuery(refs).contains(r)))).orInternal

  def thread(parent: SessionId, agent: AgentId, name: ThreadName): Read[Option[SessionRow]] =
    val ref = name.value
    Sql.run(
      sessions.filter(s => s.parentId.exists(_ == lift(parent)) && s.agentId == lift(agent) && s.origin == lift(OriginKind.Thread) && s.originRef.exists(_ == lift(ref)))
    ).map(_.headOption).orInternal

  def threadsOf(parent: SessionId): Read[List[SessionRow]] =
    Sql.run(sessions.filter(s => s.parentId.exists(_ == lift(parent))).sortBy(_.createdAt)).orInternal

  /** 会话列表：按最近活动倒序。`agent` 筛选，`origins` 只要这些来源（空是不限），`text`（不分大小写的子串）搜来信、回复与工具调用参数。 */
  def listSessions(agent: Option[AgentId], origins: List[OriginKind], text: Option[String], before: Long, limit: Int): Read[List[SessionRow]] =
    val anyOrigin = origins.isEmpty
    val os        = if anyOrigin then OriginKind.values.toList else origins
    (agent, text) match
      case (None, None) =>
        Sql.run(
          sessions
            .filter(s => s.updatedAt < lift(before) && (lift(anyOrigin) || liftQuery(os).contains(s.origin)))
            .sortBy(_.updatedAt)(using Ord.desc)
            .take(lift(limit))
        ).orInternal
      case (Some(a), None) =>
        Sql.run(
          sessions
            .filter(s => s.updatedAt < lift(before) && s.agentId == lift(a) && (lift(anyOrigin) || liftQuery(os).contains(s.origin)))
            .sortBy(_.updatedAt)(using Ord.desc)
            .take(lift(limit))
        ).orInternal
      case (None, Some(p)) =>
        Sql.run(
          sessions
            .filter(s =>
              s.updatedAt < lift(before) && (lift(anyOrigin) || liftQuery(os).contains(s.origin)) &&
                entries.filter(e => e.sessionId == s.id && sql"instr(lower(${e.search}), lower(${lift(p)})) > 0".asCondition).nonEmpty
            )
            .sortBy(_.updatedAt)(using Ord.desc)
            .take(lift(limit))
        ).orInternal
      case (Some(a), Some(p)) =>
        Sql.run(
          sessions
            .filter(s =>
              s.updatedAt < lift(before) && s.agentId == lift(a) && (lift(anyOrigin) || liftQuery(os).contains(s.origin)) &&
                entries.filter(e => e.sessionId == s.id && sql"instr(lower(${e.search}), lower(${lift(p)})) > 0".asCondition).nonEmpty
            )
            .sortBy(_.updatedAt)(using Ord.desc)
            .take(lift(limit))
        ).orInternal

  /** 编号以 `prefix` 开头的会话（`prefix` 由调用方保证只含编号用的字符）。最多取两个，够判断唯一与否。 */
  def sessionsWithPrefix(prefix: String): Read[List[SessionRow]] =
    Sql.run(sessions.filter(s => sql"substr(${s.id}, 1, ${lift(prefix.length)}) = ${lift(prefix)}".asCondition).take(2)).orInternal

  /** `from` 之后有过活动的会话（`agent` 给出时只要它的）。 */
  def touchedSince(agent: Option[AgentId], from: Long): Read[List[SessionRow]] =
    agent match
      case None    => Sql.run(sessions.filter(_.updatedAt >= lift(from))).orInternal
      case Some(a) => Sql.run(sessions.filter(s => s.updatedAt >= lift(from) && s.agentId == lift(a))).orInternal

  /** 不空闲的会话（有进行中的运行、等待中的工作或排队的信）。 */
  val busySessions: Read[List[SessionRow]] =
    Sql.run(
      sessions.filter(s =>
        runs.filter(r => r.sessionId == s.id && liftQuery(RunState.activeOnes).contains(r.state)).nonEmpty ||
          waits.filter(_.sessionId == s.id).nonEmpty || inbox.filter(_.sessionId == s.id).nonEmpty
      )
    ).orInternal

  def insertSession(r: SessionRow): Tx[Unit] = Sql.run(sessions.insertValue(lift(r))).unit.orInternal

  def touch(id: SessionId, at: Long): Tx[Unit] =
    Sql.run(sessions.filter(_.id == lift(id)).update(_.updatedAt -> lift(at))).unit.orInternal

  def setPendingReply(id: SessionId, json: String): Tx[Unit] =
    Sql.run(sessions.filter(_.id == lift(id)).update(_.pendingReply -> lift(json))).unit.orInternal

  /** 发起会话删了：它开出的 thread 保留，脱离发起会话。 */
  def orphanThreads(parent: SessionId): Tx[Unit] =
    Sql.run(sessions.filter(_.parentId.exists(_ == lift(parent))).update(_.parentId -> None)).unit.orInternal

  def deleteSession(id: SessionId): Tx[Unit] =
    for
      _ <- Sql.run(calls.filter(_.sessionId == lift(id)).delete).orInternal
      _ <- Sql.run(entries.filter(_.sessionId == lift(id)).delete).orInternal
      _ <- Sql.run(runs.filter(_.sessionId == lift(id)).delete).orInternal
      _ <- Sql.run(inbox.filter(_.sessionId == lift(id)).delete).orInternal
      _ <- Sql.run(waits.filter(_.sessionId == lift(id)).delete).orInternal
      _ <- Sql.run(delegations.filter(d => d.threadId == lift(id) || d.requesterId == lift(id)).delete).orInternal
      _ <- Sql.run(sessions.filter(_.id == lift(id)).delete).orInternal
    yield ()

  // ---- 排队的来信 ----

  def queued(session: SessionId): Read[List[InboxRow]] =
    Sql.run(inbox.filter(_.sessionId == lift(session)).sortBy(_.id)).orInternal

  def queuedCounts(ids: List[SessionId]): Read[Map[SessionId, Long]] =
    Sql.run(inbox.filter(i => liftQuery(ids).contains(i.sessionId)).groupByMap(_.sessionId)(i => (i.sessionId, count(i.id))))
      .map(_.toMap)
      .orInternal

  /** 有排队来信的会话（启动时续跑）。 */
  val sessionsWithMail: Read[List[SessionId]] = Sql.run(inbox.map(_.sessionId).distinct).orInternal

  def hasQueuedKey(session: SessionId, key: String): Read[Boolean] =
    Sql.run(inbox.filter(i => i.sessionId == lift(session) && i.dedupeKey.exists(_ == lift(key))).nonEmpty).orInternal

  def enqueue(session: SessionId, letter: String, key: Option[String], at: Long): Tx[Long] =
    Sql.run(inbox.insertValue(lift(InboxRow(0L, session, letter, key, at))).returningGenerated(_.id)).orInternal

  def dequeue(ids: List[Long]): Tx[Unit] =
    Sql.run(inbox.filter(i => liftQuery(ids).contains(i.id)).delete).unit.orInternal

  def dropQueued(session: SessionId): Tx[Long] =
    Sql.run(inbox.filter(_.sessionId == lift(session)).delete).orInternal

  // ---- 运行 ----

  def lastRun(session: SessionId): Read[Option[RunRow]] =
    Sql.run(runs.filter(_.sessionId == lift(session)).sortBy(_.id)(using Ord.desc).take(1)).map(_.headOption).orInternal

  /** 这些会话里在 [from, to) 之间开始或结束的运行。 */
  def runsBetween(ids: List[SessionId], from: Long, to: Long): Read[List[RunRow]] =
    Sql.run(
      runs.filter(r =>
        liftQuery(ids).contains(r.sessionId) &&
          ((r.startedAt >= lift(from) && r.startedAt < lift(to)) || r.finishedAt.exists(f => f >= lift(from) && f < lift(to)))
      )
    ).orInternal

  def runsIn(states: List[RunState]): Read[List[RunRow]] =
    Sql.run(runs.filter(r => liftQuery(states).contains(r.state))).orInternal

  /** 这些会话里没结束的运行（运行中或挂起）。 */
  def activeRuns(ids: List[SessionId]): Read[List[RunRow]] =
    Sql.run(runs.filter(r => liftQuery(ids).contains(r.sessionId) && liftQuery(RunState.activeOnes).contains(r.state))).orInternal

  def insertRun(session: SessionId, at: Long): Tx[RunId] =
    Sql.run(runs.insertValue(lift(RunRow(RunId.wrap(1L), session, RunState.Running, None, None, None, at, None))).returningGenerated(_.id)).orInternal

  def setRunState(id: RunId, state: RunState, error: Option[String], finishedAt: Option[Long]): Tx[Unit] =
    Sql.run(
      runs
        .filter(_.id == lift(id))
        .update(_.state -> lift(state), _.error -> lift(error), _.finishedAt -> lift(finishedAt), _.retryAt -> None, _.retryNote -> None)
    ).unit.orInternal

  def setRetry(id: RunId, at: Option[Long], note: Option[String]): Tx[Unit] =
    Sql.run(runs.filter(_.id == lift(id)).update(_.retryAt -> lift(at), _.retryNote -> lift(note))).unit.orInternal

  // ---- 时间线 ----

  def insertEntry(r: EntryRow): Tx[Long] =
    Sql.run(entries.insertValue(lift(r)).returningGenerated(_.id)).orInternal

  def updateEntryBody(id: Long, body: String, search: String, finishedAt: Long): Tx[Unit] =
    Sql.run(entries.filter(_.id == lift(id)).update(_.body -> lift(body), _.search -> lift(search), _.finishedAt -> lift(finishedAt))).unit.orInternal

  def entriesOf(session: SessionId): Read[List[EntryRow]] =
    Sql.run(entries.filter(_.sessionId == lift(session)).sortBy(_.id)).orInternal

  def entriesFrom(session: SessionId, from: Long): Read[List[EntryRow]] =
    Sql.run(entries.filter(e => e.sessionId == lift(session) && e.id >= lift(from)).sortBy(_.id)).orInternal

  /** 这些会话里在 [from, to) 之间开始的来信与回复。 */
  def messagesBetween(ids: List[SessionId], from: Long, to: Long): Read[List[EntryRow]] =
    Sql.run(
      entries
        .filter(e =>
          liftQuery(ids).contains(e.sessionId) && e.startedAt >= lift(from) && e.startedAt < lift(to) &&
            liftQuery(List(EntryKind.Letter, EntryKind.Reply)).contains(e.kind)
        )
        .sortBy(_.id)
    ).orInternal

  def entry(id: Long): Read[Option[EntryRow]] =
    Sql.run(entries.filter(_.id == lift(id))).map(_.headOption).orInternal

  /** 最近一条某种类的。 */
  def lastEntry(session: SessionId, kind: EntryKind): Read[Option[EntryRow]] =
    Sql.run(entries.filter(e => e.sessionId == lift(session) && e.kind == lift(kind)).sortBy(_.id)(using Ord.desc).take(1))
      .map(_.headOption)
      .orInternal

  /** `after` 之后这种的第一条。 */
  def firstEntryAfter(session: SessionId, kind: EntryKind, after: Long): Read[Option[EntryRow]] =
    Sql.run(entries.filter(e => e.sessionId == lift(session) && e.kind == lift(kind) && e.id > lift(after)).sortBy(_.id).take(1))
      .map(_.headOption)
      .orInternal

  // ---- 工具调用 ----

  def insertCalls(rs: List[ToolCallRow]): Tx[Unit] =
    Sql.run(liftQuery(rs).foreach(r => calls.insertValue(r))).unit.orInternal

  def callsOf(entryId: Long): Read[List[ToolCallRow]] =
    Sql.run(calls.filter(_.entryId == lift(entryId)).sortBy(_.seq)).orInternal

  def callsOfSession(session: SessionId): Read[List[ToolCallRow]] =
    Sql.run(calls.filter(_.sessionId == lift(session))).orInternal

  def call(entryId: Long, callId: CallId): Read[Option[ToolCallRow]] =
    Sql.run(calls.filter(c => c.entryId == lift(entryId) && c.callId == lift(callId))).map(_.headOption).orInternal

  def openCalls(session: SessionId): Read[List[ToolCallRow]] =
    Sql.run(calls.filter(c => c.sessionId == lift(session) && c.state != lift(CallState.Done)).sortBy(c => (c.entryId, c.seq))).orInternal

  def callsIn(states: List[CallState]): Read[List[ToolCallRow]] =
    Sql.run(calls.filter(c => liftQuery(states).contains(c.state))).orInternal

  def setCall(entryId: Long, callId: CallId, state: CallState, pause: Option[String], result: Option[String], startedAt: Option[Long], finishedAt: Option[Long]): Tx[Unit] =
    Sql.run(
      calls
        .filter(c => c.entryId == lift(entryId) && c.callId == lift(callId))
        .update(_.state -> lift(state), _.pause -> lift(pause), _.result -> lift(result), _.startedAt -> lift(startedAt), _.finishedAt -> lift(finishedAt))
    ).unit.orInternal

  // ---- 等待 ----

  def waitsOf(session: SessionId): Read[List[WaitRow]] =
    Sql.run(waits.filter(_.sessionId == lift(session)).sortBy(_.id)).orInternal

  def waitCounts(ids: List[SessionId]): Read[Map[SessionId, Long]] =
    Sql.run(waits.filter(w => liftQuery(ids).contains(w.sessionId)).groupByMap(_.sessionId)(w => (w.sessionId, count(w.createdAt))))
      .map(_.toMap)
      .orInternal

  val allWaits: Read[List[WaitRow]] = Sql.run(waits).orInternal

  def waitByKey(kind: WaitKind, ref: String): Read[Option[WaitRow]] =
    Sql.run(waits.filter(w => w.kind == lift(kind) && w.ref == lift(ref))).map(_.headOption).orInternal

  def insertWait(r: WaitRow): Tx[WaitId] =
    Sql.run(waits.insertValue(lift(r)).returningGenerated(_.id)).orInternal

  def deleteWait(id: WaitId): Tx[Long] =
    Sql.run(waits.filter(_.id == lift(id)).delete).orInternal

  // ---- 委派 ----

  def insertDelegation(r: DelegationRow): Tx[DelegationId] =
    Sql.run(delegations.insertValue(lift(r)).returningGenerated(_.id)).orInternal

  def delegation(id: DelegationId): Read[Option[DelegationRow]] =
    Sql.run(delegations.filter(_.id == lift(id))).map(_.headOption).orInternal

  def delegationsOf(ids: List[DelegationId]): Read[List[DelegationRow]] =
    Sql.run(delegations.filter(d => liftQuery(ids).contains(d.id))).orInternal

  def openDelegationsTo(thread: SessionId): Read[List[DelegationRow]] =
    Sql.run(delegations.filter(d => d.threadId == lift(thread) && !d.done)).orInternal

  def openDelegationsFrom(requester: SessionId): Read[List[DelegationRow]] =
    Sql.run(delegations.filter(d => d.requesterId == lift(requester) && !d.done)).orInternal

  /** 交回结局；已交回过的不再改（返回 0）。 */
  def finishDelegation(id: DelegationId, result: String): Tx[Long] =
    Sql.run(delegations.filter(d => d.id == lift(id) && !d.done).update(_.done -> true, _.result -> lift(Option(result)))).orInternal

  def setDelegationCall(id: DelegationId, entry: Long, call: CallId): Tx[Unit] =
    Sql.run(delegations.filter(_.id == lift(id)).update(_.callEntry -> lift(Option(entry)), _.callId -> lift(Option(call)))).unit.orInternal

  // ---- 附件 ----

  def insertUpload(r: UploadRow): Tx[Unit] = Sql.run(uploads.insertValue(lift(r))).unit.orInternal

  def upload(id: FileId): Read[Option[UploadRow]] =
    Sql.run(uploads.filter(_.id == lift(id))).map(_.headOption).orInternal

  def uploadsOf(ids: List[FileId]): Read[List[UploadRow]] =
    Sql.run(uploads.filter(u => liftQuery(ids).contains(u.id))).orInternal

  // ---- 用量 ----

  def insertUsage(r: UsageRow): Tx[Unit] = Sql.run(usages.insertValue(lift(r)).returningGenerated(_.id)).unit.orInternal

  def usageBetween(from: Long, to: Long): Read[List[UsageRow]] =
    Sql.run(usages.filter(u => u.at >= lift(from) && u.at < lift(to))).orInternal

  def usageOfSession(session: SessionId): Read[List[UsageRow]] =
    Sql.run(usages.filter(_.sessionId == lift(session))).orInternal
