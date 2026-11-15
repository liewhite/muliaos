package muliao.resources.agents

import io.getquill.*
import muliao.contract.{AgentId, AgentName, AgentState}
import muliao.platform.db.*
import muliao.platform.db.Sql.*

import java.text.Normalizer

/** 一个 agent 的身份信息（库里的一行）。 */
final case class AgentRecord(
    id: AgentId,
    name: AgentName,
    nameKey: String,
    state: AgentState,
    isDefault: Boolean,
    renamedFrom: Option[AgentName],
    createdAt: Long,
    updatedAt: Long
):
  /** 还存在（没删、没在删）。 */
  def live: Boolean = state.live

object AgentRecord:
  /** 名字的比较形式：Unicode 规范化后转小写，「不区分大小写形式」的两个名字算同一个。 */
  def keyOf(name: String): String = Normalizer.normalize(name, Normalizer.Form.NFC).toLowerCase(java.util.Locale.ROOT)

/** agent 表的读写。 */
private[resources] object AgentRepo:
  private inline def agents = quote(querySchema[AgentRecord]("agent"))

  val all: Read[List[AgentRecord]] = Sql.run(agents.sortBy(_.createdAt)).orInternal

  def byId(id: AgentId): Read[Option[AgentRecord]] =
    Sql.run(agents.filter(_.id == lift(id))).map(_.headOption).orInternal

  def liveByKey(key: String): Read[Option[AgentRecord]] =
    Sql.run(agents.filter(a => a.nameKey == lift(key) && a.state != lift(AgentState.Deleted))).map(_.headOption).orInternal

  def insert(r: AgentRecord): Tx[Unit] = Sql.run(agents.insertValue(lift(r))).unit.orInternal

  def update(r: AgentRecord): Tx[Unit] =
    Sql.run(agents.filter(_.id == lift(r.id)).updateValue(lift(r))).unit.orInternal

  def clearDefault: Tx[Unit] =
    Sql.run(agents.filter(_.isDefault).update(_.isDefault -> false)).unit.orInternal
