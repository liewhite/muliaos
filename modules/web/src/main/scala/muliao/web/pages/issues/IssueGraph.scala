package muliao.web.pages.issues

import muliao.contract.*
import muliao.web.core.Dagre

import scala.scalajs.js

/** 依赖图的纯布局逻辑：把 dependsOn 排成有向无环图（上游在左、依赖它的在右）。 */
object IssueGraph:
  /** 节点（卡片）的尺寸，渲染与布局共用。 */
  val NodeWidth  = 224
  val NodeHeight = 72

  /** 图里显示哪些：Open＝至少有一个没结束的依赖链；All＝全部有依赖关系的。 */
  enum Scope(val label: String) extends muliao.web.core.Shown.Self:
    case Open extends Scope("有未完成的依赖链")
    case All  extends Scope("全部依赖链")

  final case class Point(x: Double, y: Double)
  final case class Node(issue: IssueSummary, x: Double, y: Double)
  /** @param settled 上游已结束（不在下游的 waitingOn 里）：不再挡着下游 */
  final case class Edge(from: IssueId, to: IssueId, settled: Boolean, points: List[Point])
  final case class Layout(nodes: List[Node], edges: List[Edge], width: Double, height: Double)

  /** 已加载的 issue 之间的依赖边（上游 → 下游）；指向没加载的 issue（被筛掉的）不画。 */
  private def edgesOf(issues: List[IssueSummary]): List[(IssueId, IssueId)] =
    val ids = issues.map(_.id).toSet
    issues.flatMap(i => i.dependsOn.filter(ids.contains).map(_ -> i.id))

  /** 按依赖关系连通的分组（无向）；没有任何依赖关系的 issue 不在其中。 */
  def groups(issues: List[IssueSummary]): List[Set[IssueId]] =
    val adj = edgesOf(issues).flatMap((a, b) => List(a -> b, b -> a)).groupMap(_._1)(_._2)
    def reach(todo: List[IssueId], seen: Set[IssueId]): Set[IssueId] = todo match
      case Nil                        => seen
      case h :: t if seen.contains(h) => reach(t, seen)
      case h :: t                     => reach(adj.getOrElse(h, Nil) ++ t, seen + h)
    adj.keys.toList.sortBy(_.value).foldLeft(List.empty[Set[IssueId]]) { (acc, id) =>
      if acc.exists(_.contains(id)) then acc else acc :+ reach(List(id), Set.empty)
    }

  /** 要画进图里的：有依赖关系的；Open 时去掉整组都已结束的。 */
  def visible(issues: List[IssueSummary], scope: Scope): List[IssueSummary] =
    val byId = issues.map(i => i.id -> i).toMap
    val keep = groups(issues).filter(g => scope == Scope.All || g.exists(id => !byId(id).status.finished)).flatten.toSet
    issues.filter(i => keep.contains(i.id))

  /** 用 dagre 分层布局：从左到右，同一层上下排开；按编号加入，布局稳定、同层里旧的在上。 */
  def layout(issues: List[IssueSummary]): Layout =
    val g = js.Dynamic.newInstance(Dagre.graphlib.Graph)()
    g.setGraph(js.Dynamic.literal(rankdir = "LR", nodesep = 20, ranksep = 64, marginx = 16, marginy = 16))
    g.setDefaultEdgeLabel((() => js.Dynamic.literal()): js.Function0[js.Object])
    val sorted = issues.sortBy(_.id.value)
    sorted.foreach(i => g.setNode(i.id.value.toString, js.Dynamic.literal(width = NodeWidth, height = NodeHeight)))
    val edges = edgesOf(sorted)
    edges.foreach((f, t) => g.setEdge(f.value.toString, t.value.toString))
    Dagre.layout(g)
    val byId = issues.map(i => i.id -> i).toMap
    val info = g.graph()
    Layout(
      nodes = sorted.map { i =>
        val n = g.node(i.id.value.toString)
        Node(i, n.x.asInstanceOf[Double] - NodeWidth / 2.0, n.y.asInstanceOf[Double] - NodeHeight / 2.0)
      },
      edges = edges.map { (f, t) =>
        val pts = g.edge(f.value.toString, t.value.toString).points.asInstanceOf[js.Array[js.Dynamic]].toList
        Edge(f, t, !byId(t).waitingOn.contains(f), pts.map(p => Point(p.x.asInstanceOf[Double], p.y.asInstanceOf[Double])))
      },
      width = info.width.asInstanceOf[Double],
      height = info.height.asInstanceOf[Double]
    )

  /** 边的 SVG 路径：依次连接 dagre 给的折点。 */
  def path(points: List[Point]): String =
    points.zipWithIndex.map((p, k) => f"${if k == 0 then "M" else "L"}${p.x}%.1f,${p.y}%.1f").mkString(" ")
