package muliao.web.pages.issues

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import com.raquo.airstream.state.StrictSignal
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout

import IssueModel.*
import IssueParts.*

/** 看板视图：按状态分列，拖动卡片改状态（鼠标拖一小段、触屏长按后拖起）。 */
object IssueBoard:
  private def chips(i: IssueSummary, agents: List[AgentSummary]): List[HtmlElement] =
    List(
      Option.when(i.priority != Priority.Unset)(
        Ui.chip(i.priority.shown, if i.priority == Priority.Urgent then Tone.Danger else Tone.Default, ChipLook.Secondary)()
      ),
      i.owner.map(o => ownerChip(ownerHue(agents, Some(o)), AgentsData.label(agents, o, i.ownerName))),
      Some(Ui.chip(s"→ ${assigneeLabel(i)}")()),
      Option.when(i.waitingOn.nonEmpty)(Ui.chip(s"等待 ${refs(i.waitingOn)}")()),
      handling(i).map {
        case Handling.Stalled          => Ui.chip("没人在处理", Tone.Warning)()
        case Handling.Session(Work.Busy) => Ui.chip(workLabel(Work.Busy), Tone.Success)()
        case Handling.Session(Work.Idle) => Ui.chip(workLabel(Work.Idle))()
      }
    ).flatten

  private def cardBody(i: IssueSummary, agents: List[AgentSummary]): HtmlElement =
    Ui.card(
      cls := "cursor-pointer border-l-4 transition-colors hover:bg-surface-hover",
      ownerBar(ownerHue(agents, i.owner)),
      div(
        cls := "card__content flex flex-col gap-1.5 p-3",
        div(cls := "flex items-center gap-2 text-xs text-muted", span(s"#${i.id.value}"), span(cls := "ml-auto", Format.relative(i.updatedAt))),
        div(cls := "text-sm font-medium leading-snug", i.title),
        div(cls := "flex flex-wrap gap-1.5", chips(i, agents))
      )
    )

  /** 一次拖拽：`active` 前是按下了还没拖起来（鼠标移动一小段、触屏长按后才算拖起）。 */
  private final case class Drag(id: IssueId, from: IssueStatus, x0: Double, y0: Double, dx: Double, dy: Double, active: Boolean, over: Option[IssueStatus], touch: Boolean):
    def moved(x: Double, y: Double): Drag = copy(dx = x - x0, dy = y - y0, over = columnAt(x, y))
    def distance: Double                  = math.hypot(dx, dy)

  /** 指针下面是哪一列（拖着的卡片不接收指针事件，不会挡住）。 */
  private def columnAt(x: Double, y: Double): Option[IssueStatus] =
    Option(dom.document.elementFromPoint(x, y))
      .flatMap(e => Option(e.closest("[data-column]")))
      .flatMap(c => IssueStatus.parse(c.getAttribute("data-column")).toOption)

  private val MouseThreshold = 6.0
  private val TouchSlop      = 8.0
  private val LongPressMs    = 250.0

  def view(
      issues: Signal[List[IssueSummary]],
      columns: Signal[List[IssueStatus]],
      agents: Signal[List[AgentSummary]],
      onMove: (IssueId, IssueStatus) => Unit
  ): HtmlElement =
    val drag = Var(Option.empty[Drag])
    // 拖拽刚结束的这一刻：松手后浏览器紧接着会派发一次 click，不能把它当成「单击打开」
    var justDragged = false
    def finish(): Unit =
      drag.now().foreach { d =>
        if d.active then
          justDragged = true
          setTimeout(0) { justDragged = false }: Unit
          d.over.filter(_ != d.from).foreach(onMove(d.id, _))
      }
      drag.set(None)
    def open(id: IssueId): Unit = if !justDragged then Router.go(Route.issue(id).url, withFrom = true)
    val groups = issues.map(groupByStatus)
    val overCol = drag.signal.map(_.filter(_.active).flatMap(_.over))

    def card(sig: StrictSignal[IssueSummary], status: IssueStatus): HtmlElement =
      val id      = sig.now().id
      val mine    = drag.signal.map(_.filter(d => d.id == id && d.active))
      div(
        cls := "touch-manipulation select-none [-webkit-touch-callout:none]",
        role := "link",
        tabIndex := 0,
        cls <-- mine.map(_.isDefined).map(on => if on then "pointer-events-none relative z-10 opacity-80" else ""),
        styleAttr <-- mine.map(_.fold("")(d => s"transform: translate3d(${d.dx}px, ${d.dy}px, 0)")),
        onPointerDown.filter(e => e.pointerType == "mouse" && e.button == 0) --> { e =>
          drag.set(Some(Drag(id, status, e.clientX, e.clientY, 0, 0, active = false, None, touch = false)))
        },
        onTouchStart --> { e =>
          val t = e.touches(0)
          drag.set(Some(Drag(id, status, t.clientX, t.clientY, 0, 0, active = false, None, touch = true)))
          setTimeout(LongPressMs) {
            drag.update(_.map(d => if d.id == id && d.touch then d.copy(active = true, over = Some(status)) else d))
          }: Unit
        },
        // 长按拖起时不弹系统菜单
        onContextMenu.filter(_ => drag.now().exists(_.touch)) --> (_.preventDefault()),
        onClick --> (_ => open(id)),
        onKeyDown.filter(_.key == "Enter") --> (_ => open(id)),
        child <-- sig.combineWith(agents).map(cardBody)
      )

    def column(status: IssueStatus): HtmlElement =
      val expanded = Var(false)
      val items    = groups.map(_(status))
      sectionTag(
        dataAttr("column") := status.label,
        cls <-- overCol.map(o => "flex w-72 shrink-0 flex-col gap-2 rounded-xl p-2 " + (if o.contains(status) then "bg-surface-hover" else "bg-surface-secondary")),
        h2(cls := "flex items-center gap-2 px-1 text-sm font-semibold", statusDot(status), status.shown, span(cls := "font-normal text-muted", child.text <-- items.map(_.size.toString))),
        children <-- items.combineWith(expanded.signal).map(fold).splitSeq(_.id)(sig => card(sig, status)),
        foldToggle(items.map(_.size), expanded)
      )

    div(
      cls := "flex min-h-full gap-3 overflow-x-auto px-4 py-3",
      documentEvents(_.onPointerMove.filter(_.pointerType == "mouse")) --> { e =>
        drag.update(_.map { d =>
          val m = d.moved(e.clientX, e.clientY)
          if m.active || m.distance > MouseThreshold then m.copy(active = true) else m
        })
      },
      documentEvents(_.onPointerUp.filter(_.pointerType == "mouse")) --> (_ => finish()),
      // 触屏：拖起前移动超过一点点是在滚动，放弃；拖起后阻止滚动（要非被动监听才能 preventDefault）
      onMountUnmountCallbackWithState(
        ctx =>
          val el = ctx.thisNode.ref
          val onMove: js.Function1[dom.TouchEvent, Unit] = e =>
            drag.now().filter(_.touch).foreach { d =>
              val t = e.touches(0)
              val m = d.moved(t.clientX, t.clientY)
              if d.active then
                e.preventDefault()
                drag.set(Some(m))
              else if m.distance > TouchSlop then drag.set(None)
            }
          val onEnd: js.Function1[dom.TouchEvent, Unit] = _ => if drag.now().exists(_.touch) then finish()
          el.addEventListener("touchmove", onMove, js.Dynamic.literal(passive = false).asInstanceOf[dom.EventListenerOptions])
          el.addEventListener("touchend", onEnd)
          el.addEventListener("touchcancel", onEnd)
          () =>
            el.removeEventListener("touchmove", onMove)
            el.removeEventListener("touchend", onEnd)
            el.removeEventListener("touchcancel", onEnd)
        ,
        (_, cleanup) => cleanup.foreach(_())
      ),
      children <-- columns.splitSeq(identity)(sig => column(sig.now()))
    )
