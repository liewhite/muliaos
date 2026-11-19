package muliao.web.pages.issues

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import IssueModel.*
import IssueParts.*

/** 依赖图视图：布局见 [[IssueGraph]]，这里只画。 */
object IssueGraphView:
  private enum EdgeKind(val stroke: String, val fill: String):
    case Hover   extends EdgeKind("stroke-accent", "fill-accent")
    case Settled extends EdgeKind("stroke-muted opacity-60", "fill-muted")
    case Pending extends EdgeKind("stroke-warning", "fill-warning")

  def view(issues: Signal[List[IssueSummary]], scope: Signal[IssueGraph.Scope], agents: Signal[List[AgentSummary]]): HtmlElement =
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
    val i = n.issue
    Ui.appLink(Route.issue(i.id).url)(
      cls := "absolute flex flex-col justify-between gap-1 rounded-lg border border-l-4 border-separator bg-surface px-2.5 py-1.5 text-sm shadow-sm transition-colors hover:bg-surface-hover",
      cls := (if i.status.finished then "opacity-60" else ""),
      // ownerBar 整体设 style（色相变量），位置在它之后逐项设
      ownerBar(hue),
      left := s"${n.x}px",
      top := s"${n.y}px",
      width.px(IssueGraph.NodeWidth),
      height.px(IssueGraph.NodeHeight),
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
