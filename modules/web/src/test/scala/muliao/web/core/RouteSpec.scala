package muliao.web.core

import muliao.contract.*
import zio.test.*

object RouteSpec extends ZIOSpecDefault:
  private def at(url: String): Option[Route] =
    val (path, search) = url.indexOf('?') match
      case -1 => (url, "")
      case i  => (url.take(i), url.drop(i))
    Route.parse(path, search)

  private val samples: List[Route] = List(
    Route.home,
    Route.newSession(),
    Route.newSession(Some(AgentId.wrap("a b"))),
    Route.session(SessionId.wrap("s/1")),
    Route.agents,
    Route.agentNew,
    Route.agent(AgentId.wrap("x")),
    Route.issues(),
    Route.issues(IssueQuery(List(IssueStatus.InProgress, IssueStatus.Done), Some("user"), Some(AgentId.wrap("o")))),
    Route.issue(IssueId.wrap(5)),
    Route.Single(Page.Issue(IssueId.wrap(5), reject = true)),
    Route.assets(),
    Route.assets(AssetTab.Memory, Some("名字 & 空格")),
    Route.skill(None),
    Route.skill(Some(SkillRef(SkillName.wrap("s-1"), Some("platform")))),
    Route.Single(Page.Site(AgentId.wrap("a1"), "页 面")),
    Route.Single(Page.Notifications),
    Route.Single(Page.Roles),
    Route.Single(Page.Triggers),
    Route.Single(Page.Settings),
    Route.Single(Page.Pages),
    Route.Single(Page.Usage)
  )

  def spec = suite("前端地址")(
    test("每个页面的地址解析回同一个页面（含查询参数）") {
      val bad = samples.filterNot(r => at(r.url).contains(r))
      assertTrue(bad.isEmpty)
    },
    test("通知的「打回」（Service Worker 在 issue 地址后加 ?review=reject）解析成聚焦理由") {
      assertTrue(at("/issues/7?review=reject").contains(Route.Single(Page.Issue(IssueId.wrap(7), reject = true))))
    },
    test("查询参数里认不出的值按没给处理；认不出的路径与编号是 None") {
      assertTrue(
        at("/issues?status=done,xx&owner=").contains(Route.issues(IssueQuery(List(IssueStatus.Done)))),
        at("/ai-assets?kind=nope").contains(Route.assets()),
        at("/issues/0").isEmpty,
        at("/issues/abc").isEmpty,
        at("/nope").isEmpty
      )
    },
    test("导航项：新会话不属于任何一项，详情页属于它的列表") {
      assertTrue(
        Route.newSession().section.isEmpty,
        Route.session(SessionId.wrap("s")).section.contains(Section.Sessions),
        Route.issue(IssueId.wrap(1)).section.contains(Section.Issues),
        Route.skill(None).section.contains(Section.Assets),
        Section.Assets.admin,
        !Section.Issues.admin
      )
    },
    test("接口地址由接口声明生成") {
      assertTrue(
        Http.href(SessionsApi.download)(FileId.wrap("a b")) == "/api/files/a%20b",
        Http.href(WorkspaceApi.workspaceFile)((AgentId.wrap("x"), "dir/f 1.txt")).startsWith("/api/agents/x/workspace/file?path=dir"),
        Http.href(SessionsApi.live.endpoint)(SessionId.wrap("s1")) == "/api/sessions/s1/live",
        Http.href(Api.changes.endpoint)(()) == "/api/events",
        // 正文里的附件图片按这个前缀认（下载地址改了形状时这里要跟着改）
        Markdown.FilesPath == "/api/files/"
      )
    }
  )
