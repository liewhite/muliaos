package muliao.workspace.store

import io.getquill.*
import muliao.contract.{AgentId, JobId, SessionId}
import muliao.platform.db.*
import muliao.platform.db.Sql.*

final case class JobRow(
    id: JobId,
    agentId: AgentId,
    sessionId: SessionId,
    label: String,
    state: String,
    exitCode: Option[Int],
    reported: Boolean,
    startedAt: Long,
    endedAt: Option[Long]
)

final case class GrantRow(
    id: Long,
    agentId: AgentId,
    skill: String,
    script: String,
    args: String,
    secrets: String,
    filesHash: String,
    createdAt: Long
)

/** 本领域的表的读写。 */
object Repo:
  private inline def jobs   = quote(querySchema[JobRow]("job"))
  private inline def grants = quote(querySchema[GrantRow]("script_grant"))

  // ---- 后台任务 ----

  def insertJob(r: JobRow): Tx[JobId] =
    Sql.run(jobs.insertValue(lift(r)).returningGenerated(_.id)).orInternal

  def job(id: JobId): Read[Option[JobRow]] =
    Sql.run(jobs.filter(_.id == lift(id))).map(_.headOption).orInternal

  def jobsOf(agent: AgentId, limit: Int): Read[List[JobRow]] =
    Sql.run(jobs.filter(_.agentId == lift(agent)).sortBy(_.id)(using Ord.desc).take(lift(limit))).orInternal

  /** 还在跑的，或结局还没交回的。 */
  val unsettled: Read[List[JobRow]] =
    Sql.run(jobs.filter(j => j.state == "running" || !j.reported)).orInternal

  def setOutcome(id: JobId, state: String, exitCode: Option[Int], endedAt: Long): Tx[Unit] =
    Sql.run(jobs.filter(_.id == lift(id)).update(_.state -> lift(state), _.exitCode -> lift(exitCode), _.endedAt -> Some(lift(endedAt)), _.reported -> false))
      .unit
      .orInternal

  def setReported(id: JobId): Tx[Unit] =
    Sql.run(jobs.filter(_.id == lift(id)).update(_.reported -> true)).unit.orInternal

  def deleteJobsOf(agent: AgentId): Tx[Unit] =
    Sql.run(jobs.filter(_.agentId == lift(agent)).delete).unit.orInternal

  // ---- 始终允许 ----

  def findGrant(agent: AgentId, skill: String, script: String, args: String, secrets: String, filesHash: String): Read[Boolean] =
    Sql.run(
      grants.filter(g =>
        g.agentId == lift(agent) && g.skill == lift(skill) && g.script == lift(script) && g.args == lift(args) && g.secrets == lift(secrets) &&
          g.filesHash == lift(filesHash)
      ).nonEmpty
    ).orInternal

  def insertGrant(r: GrantRow): Tx[Unit] =
    findGrantTx(r).flatMap(exists => if exists then zio.ZIO.unit else Sql.run(grants.insertValue(lift(r)).returningGenerated(_.id)).unit.orInternal)

  private def findGrantTx(r: GrantRow): Tx[Boolean] = findGrant(r.agentId, r.skill, r.script, r.args, r.secrets, r.filesHash)

  val allGrants: Read[List[GrantRow]] = Sql.run(grants.sortBy(_.id)).orInternal

  def deleteGrant(id: Long): Tx[Long] = Sql.run(grants.filter(_.id == lift(id)).delete).orInternal

  def deleteGrantsOf(agent: AgentId): Tx[Unit] =
    Sql.run(grants.filter(_.agentId == lift(agent)).delete).unit.orInternal
