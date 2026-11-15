package muliao.triggers.store

import io.getquill.*
import muliao.contract.{AgentId, FaultSource, SessionId, TriggerId, TriggerKind, TriggerName}
import muliao.platform.db.*
import muliao.platform.db.Sql.*

final case class TriggerRow(
    id: TriggerId,
    name: TriggerName,
    agentId: AgentId,
    prompt: String,
    kind: TriggerKind,
    config: String,
    enabled: Boolean,
    precheck: Option[String],
    nextAt: Option[Long],
    skipped: Int,
    lastSessionId: Option[SessionId],
    lastFiredAt: Option[Long],
    blockNotified: Option[SessionId],
    baseline: Option[String],
    finishedAt: Option[Long],
    createdAt: Long,
    updatedAt: Long
)

final case class FaultRow(triggerId: TriggerId, source: FaultSource, error: String, at: Long)

/** 本领域的表的读写。只做存取，不含规则。 */
private[triggers] object Repo:
  private inline def triggers = quote(querySchema[TriggerRow]("trigger_def"))
  private inline def faults   = quote(querySchema[FaultRow]("trigger_fault"))

  def insert(row: TriggerRow): Tx[TriggerId] =
    Sql.run(triggers.insertValue(lift(row)).returningGenerated(_.id)).orInternal

  def get(id: TriggerId): Read[Option[TriggerRow]] =
    Sql.run(triggers.filter(_.id == lift(id))).map(_.headOption).orInternal

  /** 没结束的同名触发器。 */
  def live(name: TriggerName): Read[Option[TriggerRow]] =
    Sql.run(triggers.filter(t => t.name == lift(name) && t.finishedAt.isEmpty)).map(_.headOption).orInternal

  /** 已结束的同名触发器，新的在前。 */
  def finishedNamed(name: TriggerName): Read[List[TriggerRow]] =
    Sql.run(triggers.filter(t => t.name == lift(name) && t.finishedAt.isDefined).sortBy(_.id)(using Ord.desc)).orInternal

  def list(finished: Boolean): Read[List[TriggerRow]] =
    Sql.run(triggers.filter(t => t.finishedAt.isDefined == lift(finished)).sortBy(_.name)).orInternal

  /** 启用、没结束的。 */
  val active: Read[List[TriggerRow]] =
    Sql.run(triggers.filter(t => t.enabled && t.finishedAt.isEmpty)).orInternal

  def ofAgent(agent: AgentId): Read[List[TriggerRow]] =
    Sql.run(triggers.filter(_.agentId == lift(agent))).orInternal

  /** 整行写回（改动由调用方在同一事务里读出后算好）。 */
  def save(row: TriggerRow): Tx[Unit] =
    Sql.run(triggers.filter(_.id == lift(row.id)).updateValue(lift(row))).unit.orInternal

  def delete(id: TriggerId): Tx[Unit] =
    (Sql.run(triggers.filter(_.id == lift(id)).delete) *> Sql.run(faults.filter(_.triggerId == lift(id)).delete)).unit.orInternal

  // ---- 脚本故障 ----

  def faultsOf(ids: List[TriggerId]): Read[List[FaultRow]] =
    Sql.run(faults.filter(f => liftQuery(ids).contains(f.triggerId))).orInternal

  def putFault(row: FaultRow): Tx[Unit] =
    (Sql.run(faults.filter(f => f.triggerId == lift(row.triggerId) && f.source == lift(row.source)).delete) *>
      Sql.run(faults.insertValue(lift(row)))).unit.orInternal

  /** 删一个来源的故障；`source` 为空删全部。返回删了几条。 */
  def clearFaults(id: TriggerId, source: Option[FaultSource]): Tx[Long] =
    source match
      case Some(s) => Sql.run(faults.filter(f => f.triggerId == lift(id) && f.source == lift(s)).delete).orInternal
      case None    => Sql.run(faults.filter(_.triggerId == lift(id)).delete).orInternal
