package muliao.history.store

import io.getquill.*
import muliao.contract.{AgentId, SessionId}
import muliao.history.*
import muliao.platform.AppError
import muliao.platform.db.*
import muliao.platform.db.Sql.*
import zio.*
import zio.json.*

import java.time.LocalDate

final case class HistoryRow(
    id: Long,
    agentId: AgentId,
    day: String,
    title: String,
    summary: String,
    refs: String,
    importance: Int,
    sessionId: Option[SessionId],
    createdAt: Long
)

/** 本领域的表的读写，以及行与领域类型之间的转换。只做存取，不含规则。表只能追加（库里的触发器拒绝改、删）。 */
private[history] object Repo:
  private inline def entries = quote(querySchema[HistoryRow]("history_entry"))

  private def corrupt(r: HistoryRow, what: String): AppError = AppError.Internal(s"库里的工作历史 ${r.id} 坏了：$what")

  private def decode(r: HistoryRow): IO[AppError, HistoryEntry] =
    for
      day  <- ZIO.attempt(LocalDate.parse(r.day)).orElseFail(corrupt(r, s"日期 ${r.day}"))
      refs <- ZIO.fromEither(r.refs.fromJson[List[String]]).mapError(e => corrupt(r, s"引用 $e"))
      imp  <- ZIO.fromEither(Importance.of(r.importance)).mapError(corrupt(r, _))
    yield HistoryEntry(r.id, r.agentId, day, r.title, r.summary, refs, imp, r.sessionId, r.createdAt)

  def append(agent: AgentId, session: Option[SessionId], es: List[NewEntry], at: Long): Tx[List[Long]] =
    ZIO.foreach(es) { e =>
      val row = HistoryRow(0L, agent, e.day.toString, e.title, e.summary, e.refs.toJson, e.importance.level, session, at)
      Sql.run(entries.insertValue(lift(row)).returningGenerated(_.id)).orInternal
    }

  /** 日期、agent、重要程度的条件（`anyAgent` 时不看 agent）。日期是 YYYY-MM-DD，按字符串比较就是按日期比较。 */
  private final case class Bounds(anyAgent: Boolean, agent: AgentId, since: String, until: String, minImportance: Int)

  private def bounds(q: HistoryQuery): Bounds =
    Bounds(q.agent.isEmpty, q.agent.getOrElse(AgentId.wrap("")), q.since.fold("0000-01-01")(_.toString), q.until.fold("9999-12-31")(_.toString), q.minImportance.level)

  /** 按日期倒序（同一天后记的在前）的前 `limit` 条。 */
  def recent(q: HistoryQuery): Read[List[HistoryEntry]] =
    val b = bounds(q)
    Sql
      .run(
        entries
          .filter(e =>
            (lift(b.anyAgent) || e.agentId == lift(b.agent)) && sql"${e.day} BETWEEN ${lift(b.since)} AND ${lift(b.until)}".asCondition && e.importance >= lift(b.minImportance)
          )
          .sortBy(e => (e.day, e.id))(using Ord(Ord.desc, Ord.desc))
          .take(lift(q.limit))
      )
      .orInternal
      .flatMap(ZIO.foreach(_)(decode))

  /** 标题、摘要或引用里含 `word`（不分大小写）的，相关度由调用方排。 */
  def containing(q: HistoryQuery, word: String): Read[List[HistoryEntry]] =
    val b = bounds(q)
    Sql
      .run(
        entries.filter(e =>
          (lift(b.anyAgent) || e.agentId == lift(b.agent)) && sql"${e.day} BETWEEN ${lift(b.since)} AND ${lift(b.until)}".asCondition && e.importance >= lift(b.minImportance) &&
            sql"instr(lower(${e.title} || ' ' || ${e.summary} || ' ' || ${e.refs}), ${lift(word)}) > 0".asCondition
        )
      )
      .orInternal
      .flatMap(ZIO.foreach(_)(decode))
