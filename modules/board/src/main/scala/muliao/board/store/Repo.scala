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
    status: IssueStatus,
    priority: Priority,
    ownerId: Option[AgentId],
    assignee: Option[Assignee],
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

  /** 指派存成 `user` 或 agent 编号。 */
  private implicit val assigneeEncode: MappedEncoding[Assignee, String] = MappedEncoding(Assignee.encode)
  private implicit val assigneeDecode: MappedEncoding[String, Assignee] = MappedEncoding(Assignee.decode)

  private def corrupt(what: String): AppError = AppError.Internal(s"库里的 issue 数据坏了：$what")

  private def decode(r: IssueRow, upstream: List[IssueId]): Issue =
    Issue(r.id, r.title, r.description, r.status, r.priority, r.ownerId, r.assignee, upstream.sortBy(_.value), Actor.decode(r.createdBy, None), r.createdAt, r.updatedAt)

  private def encode(i: Issue): IssueRow =
    IssueRow(i.id, i.title, i.description, i.status, i.priority, i.owner, i.assignee, Actor.encode(i.createdBy), i.createdAt, i.updatedAt)

  private def decodeEvent(r: EventRow): IO[AppError, IssueEvent] =
    ZIO
      .fromEither(r.activity.fromJson[Activity])
      .mapBoth(e => corrupt(s"动态 ${r.id}：$e"), a => IssueEvent(r.id, r.issueId, r.at, Actor.decode(r.actor, r.sessionId), a))

  // ---- issue ----

  def get(id: IssueId): Read[Option[Issue]] =
    for
      row <- Sql.run(issues.filter(_.id == lift(id))).orInternal
      up  <- Sql.run(deps.filter(_.issueId == lift(id)).map(_.upstreamId)).orInternal
    yield row.headOption.map(decode(_, up))

  def require(id: IssueId): Read[Issue] =
    get(id).someOrFail(AppError.notFound(s"issue #${id.value} 不存在"))

  /** 全部 issue（个人看板，量小，列表与依赖图都整表取）。 */
  def all: Read[List[Issue]] =
    for
      rows <- Sql.run(issues.sortBy(_.updatedAt)(using Ord.desc)).orInternal
      ds   <- Sql.run(deps).orInternal
      up    = ds.groupMap(_.issueId)(_.upstreamId)
    yield rows.map(r => decode(r, up.getOrElse(r.id, Nil)))

  def statuses(ids: List[IssueId]): Read[Map[IssueId, IssueStatus]] =
    if ids.isEmpty then ZIO.succeed(Map.empty)
    else
      Sql.run(issues.filter(i => liftQuery(ids).contains(i.id)).map(i => (i.id, i.status))).orInternal.map(_.toMap)

  /** 这些 issue 的编号、标题与状态，按编号排。 */
  def links(ids: List[IssueId]): Read[List[IssueLink]] =
    if ids.isEmpty then ZIO.succeed(Nil)
    else Sql.run(issues.filter(i => liftQuery(ids).contains(i.id)).map(i => IssueLink(i.id, i.title, i.status))).orInternal.map(_.sortBy(_.id.value))

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
    val key: Assignee = Assignee.Agent(agent)
    Sql.run(issues.filter(i => i.ownerId.exists(_ == lift(agent)) || i.assignee.exists(_ == lift(key))).map(_.id)).orInternal

  // ---- 时间线 ----

  def addEvent(issue: IssueId, at: Long, actor: Actor, activity: Activity): Tx[IssueEvent] =
    val row = EventRow(0L, issue, at, Actor.encode(actor), actor.where, activity.toJson)
    Sql.run(events.insertValue(lift(row)).returningGenerated(_.id)).orInternal.map(id => IssueEvent(id, issue, at, actor, activity))

  def eventsOf(issue: IssueId): Read[List[IssueEvent]] =
    Sql.run(events.filter(_.issueId == lift(issue)).sortBy(_.id)).orInternal.flatMap(ZIO.foreach(_)(decodeEvent))

  /** [from, to) 之间的全部动态，按时间排。 */
  def eventsBetween(from: Long, to: Long): Read[List[IssueEvent]] =
    Sql.run(events.filter(e => e.at >= lift(from) && e.at < lift(to)).sortBy(_.id)).orInternal.flatMap(ZIO.foreach(_)(decodeEvent))

  // ---- 已请表态 ----

  def nudged(issue: IssueId, agent: AgentId): Read[Boolean] =
    Sql.run(nudges.filter(n => n.issueId == lift(issue) && n.agentId == lift(agent)).nonEmpty).orInternal

  def markNudged(issue: IssueId, agent: AgentId): Tx[Unit] =
    clearNudge(issue, agent) *> Sql.run(nudges.insertValue(lift(NudgeRow(issue, agent)))).unit.orInternal

  def clearNudge(issue: IssueId, agent: AgentId): Tx[Unit] =
    Sql.run(nudges.filter(n => n.issueId == lift(issue) && n.agentId == lift(agent)).delete).unit.orInternal

  def clearNudgesOf(agent: AgentId): Tx[Unit] =
    Sql.run(nudges.filter(_.agentId == lift(agent)).delete).unit.orInternal
