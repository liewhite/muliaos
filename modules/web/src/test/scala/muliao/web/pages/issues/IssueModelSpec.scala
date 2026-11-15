package muliao.web.pages.issues

import muliao.contract.*
import zio.test.*

import IssueModel.*

object IssueModelSpec extends ZIOSpecDefault:
  private def issue(id: Long, status: IssueStatus = IssueStatus.Backlog, deps: List[Long] = Nil, waiting: List[Long] = Nil, assignee: Option[Assignee] = None, stalled: Boolean = false) =
    IssueSummary(IssueId.wrap(id), s"#$id", status, Priority.Unset, None, None, assignee, None, None, stalled, deps.map(IssueId.wrap), waiting.map(IssueId.wrap), id)

  private val bob = Assignee.Agent(AgentId.wrap("bob"))

  def spec = suite("IssueModel")(
    test("筛选条件与查询参数互换：状态逗号分隔、认不出的丢掉，切换后按看板列序") {
      val f = Filters.parse(Map("status" -> "done,xx,backlog", "assignee" -> "user", "owner" -> ""))
      assertTrue(
        f.statuses == List(IssueStatus.Done, IssueStatus.Backlog),
        f.assignee.contains("user"),
        f.owner.isEmpty,
        f.columns == List(IssueStatus.Backlog, IssueStatus.Done),
        Filters.statusParam(f.toggle(IssueStatus.InProgress)).contains("backlog,in_progress,done"),
        Filters.statusParam(Filters.parse(Map.empty).toggle(IssueStatus.Done).filterNot(_ == IssueStatus.Done)).isEmpty,
        Filters.parse(Map.empty).columns == Statuses
      )
    },
    test("依赖编号：# 可省、逗号空格都行，有非编号内容为 None") {
      assertTrue(
        parseRefs("#1, 2 #3，4").contains(List(1L, 2L, 3L, 4L).map(IssueId.wrap)),
        parseRefs("  ").contains(Nil),
        parseRefs("#1 x").isEmpty,
        parseRefs("#0").isEmpty
      )
    },
    test("开始：指派给 agent、没结束、不等上游；在做时只有停滞才能再交一次") {
      assertTrue(
        startable(issue(1, assignee = Some(bob))) == Startable.Begin,
        startable(issue(1, assignee = Some(Assignee.User))) == Startable.No,
        startable(issue(1, assignee = Some(bob), waiting = List(2))) == Startable.No,
        startable(issue(1, IssueStatus.InProgress, assignee = Some(bob))) == Startable.No,
        startable(issue(1, IssueStatus.InProgress, assignee = Some(bob), stalled = true)) == Startable.Wake,
        startable(issue(1, IssueStatus.Done, assignee = Some(bob))) == Startable.No
      )
    },
    test("审核只对指派给用户且没结束的") {
      assertTrue(
        reviewable(issue(1, IssueStatus.InReview, assignee = Some(Assignee.User))),
        !reviewable(issue(1, IssueStatus.Done, assignee = Some(Assignee.User))),
        !reviewable(issue(1, IssueStatus.InReview, assignee = Some(bob)))
      )
    },
    test("拖拽的本地挪动覆盖状态与更新时间，分组里最近更新的在前") {
      val moved = applyMoves(List(issue(1), issue(2)), Map(IssueId.wrap(1) -> Move(IssueStatus.Done, 99, 1, settled = false)))
      val g     = groupByStatus(moved ++ List(issue(3, IssueStatus.Done)))
      assertTrue(g(IssueStatus.Done).map(_.id.value) == List(1L, 3L), g(IssueStatus.Backlog).map(_.id.value) == List(2L))
    },
    test("负责项目色相按 agent 列表顺序取黄金角，找不到不着色") {
      val as = List("a", "b").map(n => AgentSummary(AgentId.wrap(n), AgentName.wrap(n), None, None, None, AgentState.Active, false))
      assertTrue(
        ownerHue(as, Some(AgentId.wrap("a"))).contains(0),
        ownerHue(as, Some(AgentId.wrap("b"))).contains(138),
        ownerHue(as, Some(AgentId.wrap("x"))).isEmpty,
        ownerHue(as, None).isEmpty
      )
    },
    test("依赖图：只画有依赖关系的；Open 时去掉整组都已结束的") {
      val is = List(issue(1, IssueStatus.Done), issue(2, IssueStatus.Done, deps = List(1)), issue(3), issue(4, deps = List(3)), issue(5))
      assertTrue(
        IssueGraph.visible(is, IssueGraph.Scope.All).map(_.id.value) == List(1L, 2L, 3L, 4L),
        IssueGraph.visible(is, IssueGraph.Scope.Open).map(_.id.value) == List(3L, 4L)
      )
    }
  )
