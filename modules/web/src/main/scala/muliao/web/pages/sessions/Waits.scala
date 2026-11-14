package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import org.scalajs.dom

import scala.scalajs.js


/** 等待面板：会话在等的后台任务、委派、到点唤醒、提问与批准——让人看得出它为什么挂起、在等谁。 */
object Waits:
  def askAnchor(call: String): String      = s"ask-$call"
  def approvalAnchor(call: String): String = s"approval-$call"

  private def scrollTo(id: String): Unit =
    Option(dom.document.getElementById(id)).foreach(_.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(behavior = "smooth")))

  /** 已经等了多久 / 还有多久到点。 */
  private def since(createdAt: Long, now: Double): String = s"已等 ${Model.formatDuration((now - createdAt).max(0), whole = true)}"
  private def until(due: Long, now: Double): String =
    if due <= now then "就要到了" else s"${Model.formatDuration(due - now, whole = true)} 后（${Ui.formatTime(due)}）"

  private def row(mods: Modifier[HtmlElement]*): HtmlElement = div(cls := "flex min-w-0 flex-wrap items-center gap-x-2 gap-y-0.5", mods)

  /** 一个后台任务：命令说明、日志最后几行（展开时取）、结束任务。 */
  private def job(w: WaitItem, now: Signal[Double], action: Action): HtmlElement =
    val open = Var(false)
    val jobId = w.ref.toLongOption.map(JobId.wrap)
    div(
      cls := "rounded-xl border border-dashed border-separator px-3 py-1.5",
      div(
        cls := "flex items-center gap-2",
        Icon("SquareTerminal", "size-3.5 shrink-0"),
        button(
          tpe := "button",
          cls := "flex min-w-0 flex-1 items-center gap-2 text-left",
          onClick --> (_ => open.update(!_)),
          span(cls := "truncate font-mono text-foreground", w.label),
          span(cls := "shrink-0 text-muted", child.text <-- now.map(since(w.createdAt, _))),
          child <-- open.signal.map(o => Icon("ChevronDown", "size-4 shrink-0 transition-transform " + (if o then "rotate-180" else "")))
        )
      ),
      child.maybe <-- open.signal.map(o =>
        Option.when(o)(
          div(
            cls := "flex flex-col gap-2 pt-1",
            jobId.map { id =>
              val log = Remote("sessions")(Http.call(WorkspaceApi.jobLog)((id, Some(40))))
              Seq[Modifier[HtmlElement]](
                div(cls := "text-xs text-muted", "日志最后几行："),
                Ui.loading(log)(l => pre(cls := Ui.CodeBlock, child.text <-- l.map(t => if t.isEmpty then "（还没有输出）" else t))),
                Ui.btn(Variant.DangerSoft, Size.Sm, disabledIf = action.busy.signal)(
                  cls := "self-start",
                  Icon("Square", "size-3.5"),
                  "结束任务",
                  onClick --> (_ => action.fire(Http.call(WorkspaceApi.killJob)(id)))
                )
              )
            }
          )
        )
      )
    )

  private def item(w: WaitItem, pending: List[PendingCall], now: Signal[Double], action: Action): HtmlElement =
    w.kind match
      case "job" => job(w, now, action)
      case "delegation" =>
        row(
          Icon("GitBranch", "size-3.5 shrink-0 text-muted"),
          w.thread match
            case Some(t) => Ui.appLink(Route.Session(t.value).url)(cls := "max-w-full truncate text-accent hover:underline", w.label)
            case None    => span(cls := "max-w-full truncate", w.label)
          ,
          span(cls := "text-muted", child.text <-- now.map(since(w.createdAt, _))),
          w.thread.map(t =>
            Ui.btn(Variant.Ghost, Size.Sm, disabledIf = action.busy.signal)(
              cls := "ml-auto shrink-0",
              title := "停止做这项委派的 thread，结局以「被停止」交回",
              Icon("Square", "size-3.5"),
              "停止 thread",
              onClick --> (_ => action.fire(Http.call(SessionsApi.stop)(t)))
            )
          )
        )
      case "wake" =>
        row(
          title := w.label,
          Icon("AlarmClock", "size-3.5 shrink-0 text-muted"),
          span(cls := "shrink-0", "到点唤醒 ", span(cls := "text-muted", child.text <-- now.map(n => w.dueAt.fold(since(w.createdAt, n))(until(_, n))))),
          span(cls := "max-w-full truncate text-muted", w.label)
        )
      case _ =>
        row(Icon("Hourglass", "size-3.5 shrink-0 text-muted"), span(cls := "truncate", w.label), span(cls := "text-muted", child.text <-- now.map(since(w.createdAt, _))))

  private def pendingRow(p: PendingCall): HtmlElement = p.pending.kind match
    case "question" =>
      row(
        Icon("MessageCircleQuestion", "size-3.5 shrink-0 text-muted"),
        span(cls := "shrink-0", "等你回答"),
        Ui.btn(Variant.Ghost, Size.Sm)(cls := "ml-auto shrink-0", "跳到提问", onClick --> (_ => scrollTo(askAnchor(p.call))))
      )
    case "approval" =>
      row(
        Icon("ShieldQuestion", "size-3.5 shrink-0 text-muted"),
        span(cls := "shrink-0", "等你批准"),
        span(cls := "font-mono text-muted", p.name),
        Ui.btn(Variant.Ghost, Size.Sm)(cls := "ml-auto shrink-0", "去批准", onClick --> (_ => scrollTo(approvalAnchor(p.call))))
      )
    case _ =>
      row(
        Icon("GitBranch", "size-3.5 shrink-0 text-muted"),
        span(cls := "shrink-0", s"等 ${p.pending.agent.getOrElse("agent")} 回答"),
        p.pending.thread.map(t => span(cls := "truncate text-muted", s"thread「$t」"))
      )

  def panel(detail: Signal[SessionDetail], action: Action): HtmlElement =
    val now = EventStream.periodic(1000).map(_ => js.Date.now()).toSignal(js.Date.now())
    div(
      child.maybe <-- detail.map(d => (d.waits, d.pending)).distinct.map { (ws, ps) =>
        Option.when(ws.nonEmpty || ps.nonEmpty)(
          div(
            cls := "flex flex-col gap-1.5 rounded-xl border border-dashed border-separator p-3 text-sm",
            div(cls := "flex items-center gap-1.5 text-muted", Icon("Hourglass", "size-3.5"), "等待中"),
            ps.map(pendingRow),
            ws.map(item(_, ps, now, action))
          )
        )
      }
    )
