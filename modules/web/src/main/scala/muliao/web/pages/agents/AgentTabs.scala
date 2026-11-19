package muliao.web.pages.agents

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** agent 的工作历史（只读）：agent 每天自进化时自己写，一件事一条，写了就不能改。 */
object AgentHistory:
  /** 重要程度：1 日常不标。 */
  private def importance(n: Int): Option[(String, Tone)] = n match
    case 2 => Some("值得注意" -> Tone.Warning)
    case 3 => Some("重大" -> Tone.Danger)
    case _ => None

  /** 按日期分组（不搜索时：接口按日期倒序给出）；保持顺序。 */
  def byDay(entries: List[HistoryEntryView]): List[(String, List[HistoryEntryView])] =
    entries.foldRight(List.empty[(String, List[HistoryEntryView])]) {
      case (e, (day, es) :: rest) if day == e.day => (day, e :: es) :: rest
      case (e, acc)                               => (e.day, List(e)) :: acc
    }

  def view(id: AgentId): HtmlElement =
    val input = Var("")
    val query = Var("")
    div(
      cls := "flex flex-col gap-3",
      p(cls := "text-sm text-muted", "agent 每天自进化时总结前一天做过的事，只追加、不能改；agent 用 history_search 查。"),
      form(
        cls := "flex items-end gap-2",
        onSubmit.preventDefault --> (_ => query.set(input.now().trim)),
        div(cls := "flex-1", Fields.text("关键词（空格分隔，全部出现）", input)),
        Ui.btn(Variant.Secondary)(tpe := "submit", Icon("Search"), "搜索")
      ),
      child <-- query.signal.distinct.map { q =>
        val list = Remote(HistoryApi.list)(HistorySearch(agent = Some(id), q = Option(q).filter(_.nonEmpty), limit = Some(200)))
        div(
          cls := "flex flex-col gap-3",
          Ui.loading(list)(entries =>
            child <-- entries.map {
              case Nil                => Ui.emptyHint(if q.nonEmpty then "没有匹配的记录" else "还没有历史记录")
              case es if q.nonEmpty   => div(cls := "flex flex-col gap-2", es.map(entry(_, showDay = true)))
              case es =>
                div(
                  cls := "flex flex-col gap-4",
                  byDay(es).map((day, items) => sectionTag(cls := "flex flex-col gap-2", h2(cls := "text-sm font-medium text-muted", day), items.map(entry(_, showDay = false))))
                )
            }
          )
        )
      }
    )

  private def entry(e: HistoryEntryView, showDay: Boolean): HtmlElement =
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-1 text-sm",
        div(
          cls := "flex flex-wrap items-center gap-2",
          Option.when(showDay)(span(cls := "text-muted", e.day)),
          span(cls := "font-medium", e.title),
          importance(e.importance).map((t, tone) => Ui.chip(t, tone)())
        ),
        p(cls := "whitespace-pre-wrap text-muted", e.summary),
        Option.when(e.refs.nonEmpty)(div(cls := "flex flex-wrap gap-1", e.refs.map(r => Ui.inlineCode(cls := "text-xs", r)))),
        e.session.map(s => div(Ui.appLink(Route.session(s).url)(cls := "text-xs text-accent", "记下它的会话")))
      )
    )

/** agent 的后台任务：列表、看日志、结束在跑的。随 sessions 主题刷新（任务的起止随会话变化）。 */
object AgentJobs:
  enum State(val label: String, val tone: Tone):
    case Running     extends State("运行中", Tone.Accent)
    case Exited      extends State("已结束", Tone.Default)
    case Killed      extends State("被结束", Tone.Warning)
    case Interrupted extends State("被中断", Tone.Warning)

  object State:
    def of(s: JobStatus): State = s match
      case JobStatus.Running     => Running
      case JobStatus.Exited      => Exited
      case JobStatus.Killed      => Killed
      case JobStatus.Interrupted => Interrupted

  def view(id: AgentId): HtmlElement =
    val jobs = Remote(WorkspaceApi.listJobs)(id)
    div(
      cls := "flex flex-col gap-3",
      p(cls := "text-sm text-muted", "agent 用 run_shell 后台运行的命令（最近 200 个）；日志在它工作目录里，结束后结局作为事件回到发起它的会话。"),
      Ui.loading(jobs)(list =>
        div(
          cls := "flex flex-col gap-2",
          child.maybe <-- list.map(l => Option.when(l.isEmpty)(Ui.emptyHint("没有后台任务"))),
          children <-- list.splitSeq(_.id)(job)
        )
      )
    )

  private def job(j: StrictSignal[JobView]): HtmlElement =
    val jobId   = j.now().id
    val state   = j.map(x => State.of(x.state))
    val showLog = Var(false)
    val act     = new Action
    def kill(): Unit =
      Confirm
        .ask(s"结束后台任务「${j.now().label}」？", Some("结束它的整个进程树；结局作为事件回到发起它的会话。"), "结束", danger = true)
        .foreach(ok => if ok then act.fire(Http.call(WorkspaceApi.killJob)(jobId), toast = Some("结束任务")))
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-2 text-sm",
        div(
          cls := "flex flex-wrap items-center gap-2",
          child <-- state.map(s => Ui.chip(s.label, s.tone)()),
          span(cls := "min-w-0 flex-1 break-all font-mono", child.text <-- j.map(_.label)),
          child.maybe <-- j.map(_.exitCode.map(c => span(cls := "text-xs text-muted", s"退出码 $c")))
        ),
        div(
          cls := "flex flex-wrap items-center gap-x-3 gap-y-1 text-xs text-muted",
          span(child.text <-- j.map(x => s"#${x.id.value} · 开始于 ${Format.time(x.startedAt)}" + x.endedAt.fold("")(e => s" · 结束于 ${Format.time(e)}"))),
          Ui.appLink(Route.session(j.now().session).url)(cls := "text-accent", "发起它的会话"),
          span(cls := "font-mono break-all", child.text <-- j.map(_.log))
        ),
        div(
          cls := "flex flex-wrap gap-2",
          Ui.btn(Variant.Ghost, Size.Sm)(Icon("ScrollText"), child.text <-- showLog.signal.map(if _ then "收起日志" else "看日志"), onClick --> (_ => showLog.update(!_))),
          child.maybe <-- state.map(s => Option.when(s == State.Running)(Ui.btn(Variant.DangerSoft, Size.Sm, pending = act.busy.signal)(Icon("Square"), "结束", onClick --> (_ => kill()))))
        ),
        child.maybe <-- showLog.signal.map(on =>
          Option.when(on)(JobLog.view(jobId, 200, child.maybe <-- state.map(s => Option.when(s == State.Running)(span("（运行中）")))))
        )
      )
    )
