package muliao.board.store

import io.getquill.*
import muliao.board.*
import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.*
import muliao.platform.db.Sql.*
import zio.*
import zio.json.*

final case class IssueRow(
    id: IssueId,
    title: String,
    description: String,
    status: String,
    priority: String,
    ownerId: Option[AgentId],
    assignee: Option[String],
    createdBy: String,
    createdAt: Long,
    updatedAt: Long
)

final case class DepRow(issueId: IssueId, upstreamId: IssueId)

final case class EventRow(id: Long, issueId: IssueId, at: Long, actor: String, sessionId: Option[SessionId], activity: String)

final case class NudgeRow(issueId: IssueId, agentId: AgentId)

/** 本领域的表的读写，以及行与领域类型之间的转换。只做存取，不含规则。 */
private[board] object Repo:
  private inline def issues = quote(querySchema[IssueRow]("issue"))
  private inline def deps   = quote(querySchema[DepRow]("issue_dep"))
  private inline def events = quote(querySchema[EventRow]("issue_event"))
  private inline def nudges = quote(querySchema[NudgeRow]("issue_nudge"))

  // ---- 行 ↔ 领域 ----

  private def corrupt(what: String): AppError = AppError.Internal(s"库里的 issue 数据坏了：$what")

  private def decode(r: IssueRow, upstream: List[IssueId]): IO[AppError, Issue] =
    for
      st <- ZIO.fromOption(IssueStatus.parse(r.status)).orElseFail(corrupt(s"#${r.id.value} 状态 ${r.status}"))
      pr <- ZIO.fromOption(Priority.parse(r.priority)).orElseFail(corrupt(s"#${r.id.value} 优先级 ${r.priority}"))
    yield Issue(r.id, r.title, r.description, st, pr, r.ownerId, r.assignee.map(Assignee.decode), upstream.sortBy(_.value), Actor.decode(r.createdBy, None), r.createdAt, r.updatedAt)

  private def encode(i: Issue): IssueRow =
    IssueRow(i.id, i.title, i.description, i.status.label, i.priority.label, i.owner, i.assignee.map(Assignee.encode), Actor.encode(i.createdBy), i.createdAt, i.updatedAt)

  private def decodeEvent(r: EventRow): IO[AppError, IssueEvent] =
    ZIO
      .fromEither(r.activity.fromJson[Activity])
      .mapBoth(e => corrupt(s"动态 ${r.id}：$e"), a => IssueEvent(r.id, r.issueId, r.at, Actor.decode(r.actor, r.sessionId), a))

  // ---- issue ----

  def get(id: IssueId): Read[Option[Issue]] =
    for
      row <- Sql.run(issues.filter(_.id == lift(id))).orInternal
      up  <- Sql.run(deps.filter(_.issueId == lift(id)).map(_.upstreamId)).orInternal
      i   <- ZIO.foreach(row.headOption)(decode(_, up))
    yield i

  def require(id: IssueId): Read[Issue] =
    get(id).someOrFail(AppError.notFound(s"issue #${id.value} 不存在"))

  /** 全部 issue（个人看板，量小，列表与依赖图都整表取）。 */
  def all: Read[List[Issue]] =
    for
      rows <- Sql.run(issues.sortBy(_.updatedAt)(using Ord.desc)).orInternal
      ds   <- Sql.run(deps).orInternal
      up    = ds.groupMap(_.issueId)(_.upstreamId)
      is   <- ZIO.foreach(rows)(r => decode(r, up.getOrElse(r.id, Nil)))
    yield is

  def statuses(ids: List[IssueId]): Read[Map[IssueId, IssueStatus]] =
    if ids.isEmpty then ZIO.succeed(Map.empty)
    else
      Sql.run(issues.filter(i => liftQuery(ids).contains(i.id)).map(i => (i.id, i.status))).orInternal.flatMap { rows =>
        ZIO.foreach(rows)((id, s) => ZIO.fromOption(IssueStatus.parse(s)).orElseFail(corrupt(s"#${id.value} 状态 $s")).map(id -> _)).map(_.toMap)
      }

  def titles(ids: List[IssueId]): Read[List[(IssueId, String, String)]] =
    if ids.isEmpty then ZIO.succeed(Nil)
    else Sql.run(issues.filter(i => liftQuery(ids).contains(i.id)).map(i => (i.id, i.title, i.status))).orInternal

  /** 依赖它的 issue。 */
  def downstream(id: IssueId): Read[List[IssueId]] =
    Sql.run(deps.filter(_.upstreamId == lift(id)).map(_.issueId)).orInternal

  /** 全部依赖边（查环用）。 */
  def edges: Read[Map[IssueId, List[IssueId]]] =
    Sql.run(deps).orInternal.map(_.groupMap(_.issueId)(_.upstreamId))

  def insert(i: Issue): Tx[IssueId] =
    Sql.run(issues.insertValue(lift(encode(i))).returningGenerated(_.id)).orInternal

  /** 写回字段与依赖。 */
  def save(i: Issue): Tx[Unit] =
    val row = encode(i)
    for
      _ <- Sql.run(issues.filter(_.id == lift(i.id)).updateValue(lift(row))).orInternal
      _ <- Sql.run(deps.filter(_.issueId == lift(i.id)).delete).orInternal
      _ <- Sql.run(liftQuery(i.dependsOn.map(u => DepRow(i.id, u))).foreach(d => deps.insertValue(d))).orInternal
    yield ()

  def delete(id: IssueId): Tx[Unit] =
    for
      _ <- Sql.run(issues.filter(_.id == lift(id)).delete).orInternal
      _ <- Sql.run(deps.filter(d => d.issueId == lift(id) || d.upstreamId == lift(id)).delete).orInternal
      _ <- Sql.run(events.filter(_.issueId == lift(id)).delete).orInternal
      _ <- Sql.run(nudges.filter(_.issueId == lift(id)).delete).orInternal
    yield ()

  /** 负责或指派给这个 agent 的 issue。 */
  def ofAgent(agent: AgentId): Read[List[IssueId]] =
    val key = Assignee.encode(Assignee.Agent(agent))
    Sql.run(issues.filter(i => i.ownerId.exists(_ == lift(agent)) || i.assignee.exists(_ == lift(key))).map(_.id)).orInternal

  // ---- 时间线 ----

  def addEvent(issue: IssueId, at: Long, actor: Actor, activity: Activity): Tx[IssueEvent] =
    val row = EventRow(0L, issue, at, Actor.encode(actor), actor.where, activity.toJson)
    Sql.run(events.insertValue(lift(row)).returningGenerated(_.id)).orInternal.map(id => IssueEvent(id, issue, at, actor, activity))

  def eventsOf(issue: IssueId): Read[List[IssueEvent]] =
    Sql.run(events.filter(_.issueId == lift(issue)).sortBy(_.id)).orInternal.flatMap(ZIO.foreach(_)(decodeEvent))

  // ---- 已请表态 ----

  def nudged(issue: IssueId, agent: AgentId): Read[Boolean] =
    Sql.run(nudges.filter(n => n.issueId == lift(issue) && n.agentId == lift(agent)).nonEmpty).orInternal

  def markNudged(issue: IssueId, agent: AgentId): Tx[Unit] =
    clearNudge(issue, agent) *> Sql.run(nudges.insertValue(lift(NudgeRow(issue, agent)))).unit.orInternal

  def clearNudge(issue: IssueId, agent: AgentId): Tx[Unit] =
    Sql.run(nudges.filter(n => n.issueId == lift(issue) && n.agentId == lift(agent)).delete).unit.orInternal

  def clearNudgesOf(agent: AgentId): Tx[Unit] =
    Sql.run(nudges.filter(_.agentId == lift(agent)).delete).unit.orInternal
