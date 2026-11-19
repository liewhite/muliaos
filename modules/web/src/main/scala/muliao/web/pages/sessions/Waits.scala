package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import org.scalajs.dom

import scala.scalajs.js


/** 等待面板：会话在等的后台任务、委派、到点唤醒、提问与批准——让人看得出它为什么挂起、在等谁。 */
object Waits:
  def askAnchor(call: CallId): String      = s"ask-${call.value}"
  def approvalAnchor(call: CallId): String = s"approval-${call.value}"

  private def scrollTo(id: String): Unit =
    Option(dom.document.getElementById(id)).foreach(_.asInstanceOf[js.Dynamic].scrollIntoView(js.Dynamic.literal(behavior = "smooth")))

  /** 已经等了多久 / 还有多久到点。 */
  private def since(createdAt: Long, now: Double): String = s"已等 ${Format.duration((now - createdAt).max(0), whole = true)}"
  private def until(due: Long, now: Double): String =
    if due <= now then "就要到了" else s"${Format.duration(due - now, whole = true)} 后（${Format.time(due)}）"

  private def row(mods: Modifier[HtmlElement]*): HtmlElement = div(cls := "flex min-w-0 flex-wrap items-center gap-x-2 gap-y-0.5", mods)

  /** 一个后台任务：命令说明、日志最后几行（展开时取）、结束任务。 */
  private def job(w: WaitItem, id: JobId, now: Signal[Double], action: Action): HtmlElement =
    Fold.dashed(
      Var(false),
      Seq[Modifier[HtmlElement]](
        span(cls := "truncate font-mono text-foreground", w.label),
        span(cls := "shrink-0", child.text <-- now.map(since(w.createdAt, _)))
      ),
      lead = Icon("SquareTerminal", "size-3.5 shrink-0")
    )(
      div(
        cls := "flex flex-col gap-2",
        JobLog.view(id, 40),
        Ui.btn(Variant.DangerSoft, Size.Sm, disabledIf = action.busy.signal)(
          cls := "self-start",
          Icon("Square", "size-3.5"),
          "结束任务",
          onClick --> (_ => action.fire(Http.call(WorkspaceApi.killJob)(id)))
        )
      )
    )

  private def item(w: WaitItem, pending: List[PendingCall], now: Signal[Double], action: Action): HtmlElement =
    w.target match
      case WaitTarget.Job(id) => job(w, id, now, action)
      case WaitTarget.Delegation(thread) =>
        row(
          Icon("GitBranch", "size-3.5 shrink-0 text-muted"),
          thread match
            case Some(t) => Ui.appLink(Route.session(t).url)(cls := "max-w-full truncate text-accent hover:underline", w.label)
            case None    => span(cls := "max-w-full truncate", w.label)
          ,
          span(cls := "text-muted", child.text <-- now.map(since(w.createdAt, _))),
          thread.map(t =>
            Ui.btn(Variant.Ghost, Size.Sm, disabledIf = action.busy.signal)(
              cls := "ml-auto shrink-0",
              title := "停止做这项委派的 thread，结局以「被停止」交回",
              Icon("Square", "size-3.5"),
              "停止 thread",
              onClick --> (_ => action.fire(Http.call(SessionsApi.stop)(t)))
            )
          )
        )
      case WaitTarget.Wake(due) =>
        row(
          title := w.label,
          Icon("AlarmClock", "size-3.5 shrink-0 text-muted"),
          span(cls := "shrink-0", "到点唤醒 ", span(cls := "text-muted", child.text <-- now.map(until(due, _)))),
          span(cls := "max-w-full truncate text-muted", w.label)
        )

  private def pendingRow(p: PendingCall): HtmlElement = p.pending match
    case PendingView.Question(_) =>
      row(
        Icon("MessageCircleQuestion", "size-3.5 shrink-0 text-muted"),
        span(cls := "shrink-0", "等你回答"),
        Ui.btn(Variant.Ghost, Size.Sm)(cls := "ml-auto shrink-0", "跳到提问", onClick --> (_ => scrollTo(askAnchor(p.call))))
      )
    case PendingView.Approval(_) =>
      row(
        Icon("ShieldQuestion", "size-3.5 shrink-0 text-muted"),
        span(cls := "shrink-0", "等你批准"),
        span(cls := "font-mono text-muted", p.name),
        Ui.btn(Variant.Ghost, Size.Sm)(cls := "ml-auto shrink-0", "去批准", onClick --> (_ => scrollTo(approvalAnchor(p.call))))
      )
    case PendingView.Agent(thread, agent) =>
      row(
        Icon("GitBranch", "size-3.5 shrink-0 text-muted"),
        span(cls := "shrink-0", s"等 ${agent.value} 回答"),
        span(cls := "truncate text-muted", s"thread「${thread.value}」")
      )

  def panel(detail: Signal[SessionDetail], action: Action): HtmlElement =
    val now = Clock.every(1000)
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
