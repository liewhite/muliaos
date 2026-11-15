package muliao.history

import muliao.board.IssueActivity
import muliao.contract.{AgentId, Origin}
import muliao.sessions.*

import java.time.{Instant, LocalDate, ZoneId}
import java.time.format.DateTimeFormatter

/** 某天的活动：会话、对 issue 的操作、当天的工作历史。`agent` 为 None 是全部 agent。 */
final case class DayOverview(
    agent: Option[AgentId],
    day: LocalDate,
    zone: ZoneId,
    sessions: SessionActivity,
    issues: List[IssueActivity],
    history: List[HistoryEntry]
)

/** 活动回顾的文字写法（纯函数）：一行一项，正文缩进跟在下面。 */
final class OverviewText(names: AgentId => String, zone: ZoneId):
  private val clock = DateTimeFormatter.ofPattern("HH:mm").withZone(zone)

  private def at(t: Long): String = clock.format(Instant.ofEpochMilli(t))

  /** 会话编号只给前 8 位（session_read 认前缀）。 */
  private def short(id: muliao.contract.SessionId): String = id.value.take(8)

  private def indent(text: String): List[String] = text.linesIterator.map("  " + _).toList

  def lines(o: DayOverview): List[String] =
    val who                  = o.agent.fold("全部 agent")(names)
    def by(a: AgentId): String = if o.agent.isEmpty then s"${names(a)} " else ""
    def section(title: String, items: List[List[String]]): List[String] =
      "" :: s"## $title（${items.size}）" :: (if items.isEmpty then List("（无）") else items.flatten)

    val sessions = o.sessions.sessions.map { s =>
      val failed = if s.failedRuns > 0 then s"，失败 ${s.failedRuns} 次" else ""
      List(s"- ${short(s.id)} ${by(s.agent)}[${OverviewText.origin(s.origin)}] ${s.title} | ${s.state.label} | ${s.messages} 条消息 ${at(s.first)}–${at(s.last)} | 运行 ${s.runs} 次$failed")
    }
    val said = o.sessions.said.map(u => s"- [${at(u.at)}] ${short(u.session)} ${by(u.agent)}说：" :: indent(u.text))
    val issues = o.issues.map { e =>
      val head = s"- [${at(e.at)}] #${e.issue.value} ${e.title}：${e.actor} ${e.text}"
      e.comment.fold(List(head))(c => (head + "：") :: indent(c))
    }
    val failures = o.sessions.failures.map(f => s"- [${at(f.at)}] ${short(f.session)} ${by(f.agent)}运行 ${f.run.value}：" :: indent(f.error))
    val history = o.history.map { h =>
      val refs = if h.refs.isEmpty then "" else s"（${h.refs.mkString(", ")}）"
      s"- ${by(h.agent)}[${h.importance.level}] ${h.title}$refs" :: indent(h.summary)
    }
    s"# ${o.day} $who 的活动（时区 ${zone.getId}）" ::
      (section("活跃的会话", sessions) ++ section("用户说的话", said) ++ section("对 issue 的操作", issues) ++
        section("失败的运行", failures) ++ section("当天的工作历史", history))

object OverviewText:
  /** 会话是怎么开的。 */
  def origin(o: Origin): String = o match
    case Origin.User          => "用户新建"
    case Origin.Trigger(n)    => s"触发器 ${n.value}"
    case Origin.Issue(i)      => s"issue #${i.value}"
    case Origin.Thread(_, th) => s"thread ${th.value}"
