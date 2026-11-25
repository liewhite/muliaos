package muliao.history

import muliao.board.{Actor, IssueActivity}
import muliao.contract.{AgentId, IssueId, Origin, SessionId, TriggerName, WorkState}
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

/** 活动回顾的文字写法（纯函数）：列重点、给索引（会话编号、issue 号），细节由 agent 用 session_read / issue_show 去翻。
  * 同一触发器的会话合成一项，只单列最近的、失败的、没空闲的；同一 issue 的动态合成一项，状态与指派压成变化路径，
  * 用户的评论给全文，agent 与平台的正文只给开头。
  */
final class OverviewText(names: AgentId => String, zone: ZoneId):
  private val clock = DateTimeFormatter.ofPattern("HH:mm").withZone(zone)

  private def at(t: Long): String = clock.format(Instant.ofEpochMilli(t))

  /** 会话编号只给前 8 位（session_read 认前缀）。 */
  private def short(id: SessionId): String = id.value.take(8)

  private def indent(text: String, prefix: String = "  "): List[String] = text.linesIterator.map(prefix + _).toList

  def lines(o: DayOverview): List[String] =
    val who                  = o.agent.fold("全部 agent")(names)
    def by(a: AgentId): String = if o.agent.isEmpty then s"${names(a)} " else ""
    def section(title: String, items: List[List[String]]): List[String] =
      "" :: s"## $title" :: (if items.isEmpty then List("（无）") else items.flatten)

    def session(s: ActiveSession): List[String] =
      val failed = if s.failedRuns > 0 then s"，失败 ${s.failedRuns} 次" else ""
      List(s"- ${short(s.id)} ${by(s.agent)}[${OverviewText.origin(s.origin)}] ${s.title} | ${s.state.label} | ${s.messages} 条消息 ${at(s.first)}–${at(s.last)} | 运行 ${s.runs} 次$failed")

    def triggerRuns(ss: List[ActiveSession]): List[String] =
      val s0     = ss.head
      val failed = ss.map(_.failedRuns).sum
      val latest = ss.maxBy(_.last)
      val marked = (latest :: ss.filter(s => s.failedRuns > 0 || s.state != WorkState.Idle)).distinct.map { s =>
        val tags = List(Option.when(s.id == latest.id)("最近"), Option.when(s.failedRuns > 0)(s"失败 ${s.failedRuns} 次"), Option.when(s.state != WorkState.Idle)(s.state.label)).flatten
        s"${short(s.id)}（${tags.mkString("，")}）"
      }
      val rest = ss.size - marked.size
      List(
        s"- ${by(s0.agent)}[${OverviewText.origin(s0.origin)}] ${ss.size} 个会话 | ${ss.map(_.messages).sum} 条消息 ${at(ss.map(_.first).min)}–${at(ss.map(_.last).max)} | 运行 ${ss.map(_.runs).sum} 次${if failed > 0 then s"，失败 $failed 次" else ""}",
        s"  ${marked.mkString("、")}${if rest > 0 then s"；其余 $rest 个空闲、无失败" else ""}"
      )

    val sessions = OverviewText.byTrigger(o.sessions.sessions).map {
      case (s, Nil)  => session(s)
      case (s, more) => triggerRuns(s :: more)
    }
    val said = o.sessions.said.map(u => s"- [${at(u.at)}] ${short(u.session)} ${by(u.agent)}说：" :: indent(u.text))
    val issues = o.issues.map { i =>
      val span    = if i.first == i.last then at(i.first) else s"${at(i.first)}–${at(i.last)}"
      val status  = Option.when(i.statuses.nonEmpty)(s"  状态：${i.statuses.map(_.label).mkString(" → ")}")
      val assign  = Option.when(i.assignees.nonEmpty)(s"  指派：${i.assignees.mkString(" → ")}")
      val events = i.events.flatMap { e =>
        val head = s"  [${at(e.at)}] ${e.who} ${e.text}"
        e.body.fold(List(head)) { b =>
          val shown = e.actor match
            case Actor.User => indent(b, "    ")
            case _          => OverviewText.excerpt(b, i.issue).map("    " + _)
          (head + "：") :: shown
        }
      }
      (s"- #${i.issue.value} ${i.title} | ${i.count} 条动态 $span" :: status.toList) ++ assign.toList ++ events
    }
    val failures = o.sessions.failures.map(f => s"- [${at(f.at)}] ${short(f.session)} ${by(f.agent)}运行 ${f.run.value}：" :: indent(f.error))
    val history = o.history.map { h =>
      val refs = if h.refs.isEmpty then "" else s"（${h.refs.mkString(", ")}）"
      s"- ${by(h.agent)}[${h.importance.level}] ${h.title}$refs" :: indent(h.summary)
    }
    s"# ${o.day} $who 的活动（时区 ${zone.getId}）" ::
      (section(s"活跃的会话（${o.sessions.sessions.size}）", sessions) ++ section(s"用户说的话（${said.size}）", said) ++
        section(s"对 issue 的操作（${issues.size} 个 issue，${o.issues.map(_.count).sum} 条动态）", issues) ++
        section(s"失败的运行（${failures.size}）", failures) ++ section(s"当天的工作历史（${history.size}）", history))

object OverviewText:
  /** 正文开头给几行、每行至多几字；省掉了就加一行指向全文。 */
  private val ExcerptLines = 3
  private val ExcerptWidth = 160

  /** agent 与平台写的正文只给开头（跳过空行），全文用 issue_show 看。 */
  def excerpt(text: String, issue: IssueId): List[String] =
    val all   = text.linesIterator.filter(_.trim.nonEmpty).toList
    val shown = all.take(ExcerptLines).map(l => if l.length > ExcerptWidth then l.take(ExcerptWidth) + "…" else l)
    if shown == all then shown else shown :+ s"…（共 ${text.length} 字，全文用 issue_show(${issue.value}) 看）"

  /** 同一 agent 同一触发器的会话合成一组（第一个，其余的），其余会话各自一组；组按各自第一个会话的位置排。 */
  def byTrigger(ss: List[ActiveSession]): List[(ActiveSession, List[ActiveSession])] =
    val key: ActiveSession => Either[SessionId, (AgentId, TriggerName)] = s =>
      s.origin match
        case Origin.Trigger(n) => Right((s.agent, n))
        case _                 => Left(s.id)
    val groups = ss.groupBy(key)
    ss.map(key).distinct.map(k => (groups(k).head, groups(k).tail))

  /** 会话是怎么开的。 */
  def origin(o: Origin): String = o match
    case Origin.User          => "用户新建"
    case Origin.Trigger(n)    => s"触发器 ${n.value}"
    case Origin.Issue(i)      => s"issue #${i.value}"
    case Origin.Thread(_, th) => s"thread ${th.value}"
