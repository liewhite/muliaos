package muliao.history

import muliao.contract.{AgentId, SessionId}

import java.time.LocalDate

/** 重要程度。 */
enum Importance(val level: Int):
  /** 日常 */
  case Routine extends Importance(1)
  /** 值得注意 */
  case Notable extends Importance(2)
  /** 重大：决定、事故、里程碑 */
  case Major extends Importance(3)

object Importance:
  def of(level: Int): Either[String, Importance] =
    values.find(_.level == level).toRight(s"重要程度只能是 1（日常）、2（值得注意）、3（重大），不是 $level")

/** 工作历史的一条：某 agent 某天做过的一件事。记下后不能改、删。 */
final case class HistoryEntry(
    id: Long,
    agent: AgentId,
    day: LocalDate,
    title: String,
    summary: String,
    refs: List[String],
    importance: Importance,
    session: Option[SessionId],
    createdAt: Long
)

/** 要记的一条（已校验）。只能由 [[NewEntry.make]] 造出：标题、摘要不空，日期不在今天之后。 */
final case class NewEntry private (day: LocalDate, title: String, summary: String, refs: List[String], importance: Importance)

object NewEntry:
  def make(day: LocalDate, title: String, summary: String, refs: List[String], importance: Importance, today: LocalDate): Either[String, NewEntry] =
    val t = title.trim
    val s = summary.trim
    if t.isEmpty then Left("标题不能为空")
    else if s.isEmpty then Left(s"「$t」的摘要不能为空")
    else if day.isAfter(today) then Left(s"「$t」的日期 $day 在今天（$today）之后：写做这件事的那天")
    else Right(NewEntry(day, t, s, refs.map(_.trim).filter(_.nonEmpty), importance))

/** 检索条件。`words` 为空时按日期倒序列出；否则每个词都要出现在标题、摘要或引用里，按相关度排。 */
final case class HistoryQuery(
    agent: Option[AgentId] = None,
    words: List[String] = Nil,
    since: Option[LocalDate] = None,
    until: Option[LocalDate] = None,
    minImportance: Importance = Importance.Routine,
    limit: Int = 20
)

object HistoryQuery:
  /** 把检索词按空白拆开（不分大小写）。 */
  def words(q: Option[String]): List[String] =
    q.toList.flatMap(_.trim.split("\\s+")).map(_.toLowerCase).filter(_.nonEmpty).distinct
