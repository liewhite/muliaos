package muliao.web.pages.issues

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import com.raquo.airstream.state.StrictSignal
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers.setTimeout

import Http.ec
import IssueModel.*
import IssueParts.*

/** /issues：看板、列表、依赖图三种视图，按状态、指派、负责项目筛选（条件在地址的查询参数里），新建 issue。 */
object IssuesPage:
  private enum View(val label: String) extends Shown.Self:
    case Board extends View("看板")
    case List  extends View("列表")
    case Graph extends View("依赖图")

  // 选过的视图与依赖图范围跨刷新记住
  private val viewVar = Stored.choice("muliao.issues.view", View.values.toList, View.Board)
  private val scope   = Stored.choice("muliao.issues.graphScope", IssueGraph.Scope.values.toList, IssueGraph.Scope.Open)

  /** @param filters 地址里的筛选条件（改它就是改地址） */
  def view(filters: Signal[IssueQuery]): HtmlElement =
    val agents  = AgentsData.list
    val loaded  = filters.distinct.flatMapSwitch(f => Remote(BoardApi.list)(f))
    val edits   = new EventBus[IssueQuery => IssueQuery]
    // 拖拽后先在本地挪动，接口成功、重新拉到的数据落下后以服务端为准；连着拖几张时每张各占一项
    val moves   = Var(Map.empty[IssueId, Move])
    val shown   = loaded.combineWith(moves.signal).map((l, m) => l.map(applyMoves(_, m)))
    val mover   = new Mover(moves)
    Ui.page("Issues", wide = true)(
      loaded.updates --> (_ => moves.update(_.filterNot(_._2.settled))),
      edits.events.withCurrentValueOf(filters) --> ((edit, f) => Router.update(Route.issues(edit(f)))),
      div(cls := "mx-auto w-full max-w-3xl px-4 pt-3", toolbar(viewVar, scope, filters, edits.writer, agents)),
      Ui.loading(shown) { issues =>
        child <-- viewVar.signal.map {
          case View.Board => board(issues, filters.map(_.columns), agents, mover.move)
          case View.List  => div(cls := "mx-auto flex w-full max-w-3xl flex-col gap-3 px-4 py-3", list(issues, agents))
          case View.Graph => graph(issues, scope.signal, agents)
        }
      }
    )

  /** 拖拽改状态：失败用 toast 提示（连着拖几张时各自报，不互相冲掉）。 */
  private final class Mover(moves: Var[Map[IssueId, Move]]):
    private val act   = new Action
    private var token = 0

    def move(id: IssueId, status: IssueStatus): Unit =
      token += 1
      val m = Move(status, js.Date.now().toLong, token, settled = false)
      moves.update(_ + (id -> m))
      act.run(Http.call(BoardApi.update)((id, IssuePatch(status = Some(status)))), toast = Some(s"把 #${id.value} 移到「${status.shown}」")).foreach { ok =>
        // 同一张又被拖了一次：那一次还在提交，留着它的
        moves.update { all =>
          all.get(id) match
            case Some(cur) if cur.token == m.token => if ok then all.updated(id, cur.copy(settled = true)) else all - id
            case _                                 => all
        }
      }

  // ---- 工具栏 ----

  private def toolbar(
      viewVar: Var[View],
      scope: Var[IssueGraph.Scope],
      filters: Signal[IssueQuery],
      edit: Observer[IssueQuery => IssueQuery],
      agents: Signal[List[AgentSummary]]
  ): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      Fields.segmented("视图", viewVar.signal, viewVar.set, View.values.toList),
      child.maybe <-- viewVar.signal.map(v =>
        Option.when(v == View.Graph)(
          Fields.segmented("依赖图范围", scope.signal, scope.set, IssueGraph.Scope.values.toList)
        )
      ),
      div(
        cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
        Fields.select[Option[AgentId]](
          "负责项目",
          filters.map(f => Some(f.owner)),
          o => edit.onNext(_.copy(owner = o)),
          agents.map(as => Opt(Option.empty[AgentId], "所有项目") :: ownerOptions(as, None, assignable = false).map(o => o.copy(id = Some(o.id))))
        ),
        Fields.select[Option[String]](
          "指派给",
          filters.map(f => Some(f.assignee)),
          a => edit.onNext(_.copy(assignee = a)),
          agents.map(as => Opt(Option.empty[String], "所有人") :: Opt(Some(Assignee.UserKey), "我") :: as.map(a => Opt(Some(a.id.value), a.name.value)))
        )
      ),
      statusFilter(filters, edit),
      newIssueForm(agents)
    )

  /** 按状态筛选：可多选，都不选是全部。 */
  private def statusFilter(filters: Signal[IssueQuery], edit: Observer[IssueQuery => IssueQuery]): HtmlElement =
    div(
      cls := "flex flex-col gap-1",
      span(cls := "label", "状态"),
      div(
        cls := "flex flex-wrap gap-1.5",
        role := "group",
        aria.label := "按状态筛选",
        Statuses.map { s =>
          val on = filters.map(_.status.contains(s))
          button(
            tpe := "button",
            cls := "toggle-button toggle-button--default toggle-button--sm flex items-center gap-1.5",
            aria.pressed <-- on.map(_.toString),
            dataAttr("selected") <-- on.map(_.toString),
            onClick --> (_ => edit.onNext(_.toggle(s))),
            statusDot(s),
            s.shown
          )
        }
      )
    )

  // ---- 新建 ----

  private def newIssueForm(agents: Signal[List[AgentSummary]]): HtmlElement =
    val title    = Var("")
    val desc     = Var("")
    val status   = Var(IssueStatus.InProgress)
    val priority = Var(Priority.Unset)
    val assignee = Var(AssigneeChoice.Nobody: AssigneeChoice)
    // 负责的项目必填：不猜
    val owner    = Var(Option.empty[AgentId])
    val deps     = Var("")
    val parsed   = deps.signal.map(parseRefs)
    val act      = new Action
    val ready    = title.signal.combineWith(owner.signal, parsed).map((t, o, p) => t.trim.nonEmpty && o.isDefined && p.isDefined)
    def submit(): Unit =
      (owner.now(), parseRefs(deps.now())) match
        case (Some(o), Some(ds)) if title.now().trim.nonEmpty =>
          val req = NewIssue(title.now().trim, o, desc.now(), status.now(), priority.now(), assignee.now().toAssignee, ds)
          act.run(Http.call(BoardApi.create)(req)).foreach { ok =>
            if ok then
              title.set("")
              desc.set("")
              deps.set("")
          }
        case _ => ()
    Ui.collapsibleForm("新建 issue")(Seq[Modifier[HtmlElement]](
      Fields.text("标题", title),
      Fields.area("描述（Markdown）", desc, rowsN = 4),
      div(
        cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
        Fields.select[AgentId]("负责项目（owner）", owner.signal, o => owner.set(Some(o)), agents.combineWith(owner.signal).map((as, o) => ownerOptions(as, o, assignable = true)), hint = Some("转派给别人做也不变")),
        Fields.select[IssueStatus]("状态", status.signal.map(Some(_)), status.set, Val(statusOptions)),
        Fields.select[Priority]("优先级", priority.signal.map(Some(_)), priority.set, Val(priorityOptions)),
        Fields.select[AssigneeChoice]("指派给", assignee.signal.map(Some(_)), assignee.set, agents.combineWith(assignee.signal).map(assigneeOptions))
      ),
      Fields.text("依赖的 issue（编号，逗号分隔，可不填）", deps),
      child.maybe <-- parsed.map(r => Option.when(r.isEmpty)(p(cls := "text-xs text-danger", "只能填 issue 编号，如 #12, #15"))),
      p(cls := "text-xs text-muted", "指派给 agent 且状态为进行中时，会唤醒它来处理；有依赖的要等依赖全部完成或取消后才唤醒。"),
      act.errorView(),
      div(Ui.btn(pending = act.busy.signal)("创建", disabled <-- ready.combineWith(act.busy.signal).map((r, b) => !r || b), onClick --> (_ => submit())))
    ))

  // ---- 卡片 ----

  private def chips(i: IssueSummary, agents: List[AgentSummary]): List[HtmlElement] =
    List(
      Option.when(i.priority != Priority.Unset)(
        Ui.chip(i.priority.shown, if i.priority == Priority.Urgent then Tone.Danger else Tone.Default, soft = false)(cls := "chip--secondary")
      ),
      i.owner.map(o => ownerChip(ownerHue(agents, Some(o)), ownerLabel(agents, Some(o), i.ownerName))),
      Some(Ui.chip(s"→ ${assigneeLabel(i)}")()),
      Option.when(i.waitingOn.nonEmpty)(Ui.chip(s"等待 ${refs(i.waitingOn)}")()),
      handling(i).map {
        case Handling.Stalled          => Ui.chip("没人在处理", Tone.Warning)()
        case Handling.Session(Work.Busy) => Ui.chip(workLabel(Work.Busy), Tone.Success)()
        case Handling.Session(Work.Idle) => Ui.chip(workLabel(Work.Idle))()
      }
    ).flatten

  private def cardBody(i: IssueSummary, agents: List[AgentSummary]): HtmlElement =
    div(
      cls := "card card--default cursor-pointer border-l-4 transition-colors hover:bg-surface-hover",
      dataAttr("slot") := "card",
      ownerBar(ownerHue(agents, i.owner)),
      div(
        cls := "card__content flex flex-col gap-1.5 p-3",
        div(cls := "flex items-center gap-2 text-xs text-muted", span(s"#${i.id.value}"), span(cls := "ml-auto", Ui.relativeTime(i.updatedAt))),
        div(cls := "text-sm font-medium leading-snug", i.title),
        div(cls := "flex flex-wrap gap-1.5", chips(i, agents))
      )
    )

  // ---- 看板与拖拽 ----

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

  private def board(
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

  /** 折叠起来的 issue 的展开 / 收起；不超过 FoldLimit 个时不显示。 */
  private def foldToggle(total: Signal[Int], expanded: Var[Boolean]): Modifier[HtmlElement] =
    child.maybe <-- total.combineWith(expanded.signal).map((n, e) =>
      Option.when(n > FoldLimit)(
        button(
          tpe := "button",
          cls := "self-start px-1 py-1 text-xs text-muted hover:text-foreground",
          onClick --> (_ => expanded.update(!_)),
          if e then "收起" else s"展开其余 ${n - FoldLimit} 个"
        )
      )
    )

  // ---- 列表 ----

  private def list(issues: Signal[List[IssueSummary]], agents: Signal[List[AgentSummary]]): Modifier[HtmlElement] =
    val groups   = issues.map(groupByStatus)
    val nonEmpty = groups.map(g => Statuses.filter(g(_).nonEmpty))
    Seq(
      child.maybe <-- nonEmpty.map(n => Option.when(n.isEmpty)(Ui.emptyHint("还没有 issue"))),
      div(
        cls := "flex flex-col gap-4",
        children <-- nonEmpty.splitSeq(identity) { sig =>
          val status   = sig.now()
          val items    = groups.map(_(status))
          val expanded = Var(false)
          sectionTag(
            cls := "flex flex-col",
            h2(cls := "flex items-center gap-2 py-1 text-sm font-semibold text-muted", statusDot(status), status.shown, span(cls := "font-normal", child.text <-- items.map(_.size.toString))),
            children <-- items.combineWith(expanded.signal).map(fold).splitSeq(_.id)(row(agents)),
            foldToggle(items.map(_.size), expanded)
          )
        }
      )
    )

  private def row(agents: Signal[List[AgentSummary]])(sig: StrictSignal[IssueSummary]): HtmlElement =
    val i0 = sig.now()
    Ui.appLink(Route.issue(i0.id).url)(
      cls := "flex items-center gap-3 border-b border-separator px-1 py-2 text-sm hover:bg-surface-hover",
      children <-- sig.combineWith(agents).map { (i, as) =>
        List(
          Some(span(cls := "w-10 shrink-0 text-xs text-muted", s"#${i.id.value}")),
          Some(statusDot(i.status)),
          Some(ownerDot(ownerHue(as, i.owner))),
          Some(span(cls := "min-w-0 flex-1 truncate", i.title)),
          Option.when(i.stalled)(span(cls := "shrink-0 text-xs text-warning", "没人在处理")),
          Option.when(i.priority != Priority.Unset)(span(cls := "shrink-0 text-xs text-muted", i.priority.shown)),
          Some(span(cls := "w-20 shrink-0 truncate text-right text-xs text-muted", assigneeLabel(i)))
        ).flatten
      }
    )

  // ---- 依赖图 ----

  private enum EdgeKind(val stroke: String, val fill: String):
    case Hover   extends EdgeKind("stroke-accent", "fill-accent")
    case Settled extends EdgeKind("stroke-muted opacity-60", "fill-muted")
    case Pending extends EdgeKind("stroke-warning", "fill-warning")

  private def graph(issues: Signal[List[IssueSummary]], scope: Signal[IssueGraph.Scope], agents: Signal[List[AgentSummary]]): HtmlElement =
    import com.raquo.laminar.api.L.svg as s
    def a(name: String) = s.svgAttr(name, com.raquo.laminar.codecs.Codec.stringAsIs, None)
    // 悬停的卡片：高亮与它直接相连的边；悬停只改边的样式，不重新布局
    val hover  = Var(Option.empty[IssueId])
    // agent 列表只用来配色，很少变：变了整张图重画
    val layout = issues.combineWith(scope, agents).map((is, sc, as) => (sc, as, IssueGraph.layout(IssueGraph.visible(is, sc))))
    div(
      child <-- layout.map { (sc, as, g) =>
        if g.nodes.isEmpty then Ui.emptyHint(if sc == IssueGraph.Scope.Open then "没有未完成的依赖关系" else "还没有带依赖关系的 issue")
        else
          div(
            cls := "overflow-auto px-4 py-3",
            div(
              cls := "relative",
              styleAttr := s"width: ${g.width}px; height: ${g.height}px",
              s.svg(
                s.cls := "pointer-events-none absolute inset-0",
                s.width := g.width.toString,
                s.height := g.height.toString,
                a("aria-hidden") := "true",
                // 箭头按边的颜色各备一个：context-stroke 在旧浏览器上不生效
                s.defs(
                  EdgeKind.values.toList.map(k =>
                    s.marker(
                      s.idAttr := s"issue-arrow-${k.toString}",
                      s.viewBox := "0 0 10 10",
                      a("refX") := "9",
                      a("refY") := "5",
                      a("markerWidth") := "7",
                      a("markerHeight") := "7",
                      a("orient") := "auto",
                      s.path(s.d := "M0,0 L10,5 L0,10 z", s.cls := k.fill)
                    )
                  )
                ),
                children <-- hover.signal.map { h =>
                  def touches(e: IssueGraph.Edge) = h.exists(x => e.from == x || e.to == x)
                  // 高亮的边最后画，压在别的边上面
                  g.edges.sortBy(touches).map { e =>
                    val k = if touches(e) then EdgeKind.Hover else if e.settled then EdgeKind.Settled else EdgeKind.Pending
                    s.path(
                      s.d := IssueGraph.path(e.points),
                      s.fill := "none",
                      a("marker-end") := s"url(#issue-arrow-${k.toString})",
                      s.strokeWidth := (if k == EdgeKind.Hover then "2.5" else "1.5"),
                      a("stroke-dasharray") := (if e.settled then "5 4" else ""),
                      s.cls := k.stroke
                    )
                  }
                }
              ),
              g.nodes.map(n => graphNode(n, hover, ownerHue(as, n.issue.owner)))
            )
          )
      }
    )

  private def graphNode(n: IssueGraph.Node, hover: Var[Option[IssueId]], hue: Option[Int]): HtmlElement =
    val i   = n.issue
    val pos = s"left: ${n.x}px; top: ${n.y}px; width: ${IssueGraph.NodeWidth}px; height: ${IssueGraph.NodeHeight}px"
    Ui.appLink(Route.issue(i.id).url)(
      cls := "absolute flex flex-col justify-between gap-1 rounded-lg border border-l-4 border-separator bg-surface px-2.5 py-1.5 text-sm shadow-sm transition-colors hover:bg-surface-hover",
      cls := (if i.status.finished then "opacity-60" else ""),
      hue match
        case Some(h) => Seq(cls := "owner-color border-l-(--owner-fg)", styleAttr := s"${ownerStyle(h)}; $pos")
        case None    => Seq(styleAttr := pos)
      ,
      title := s"#${i.id.value} ${i.title}",
      onMouseEnter --> (_ => hover.set(Some(i.id))),
      onMouseLeave --> (_ => hover.set(None)),
      span(cls := "line-clamp-2 leading-snug font-medium", i.title),
      span(
        cls := "flex items-center gap-1.5 text-xs text-muted",
        statusDot(i.status),
        span(s"#${i.id.value}"),
        span(i.status.shown),
        Option.when(i.stalled)(span(cls := "text-warning", "没人在处理")),
        span(cls := "ml-auto truncate", s"→ ${assigneeLabel(i)}")
      )
    )
