package muliao.web.core

import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var
import muliao.contract.*
import org.scalajs.dom

import scala.scalajs.js

/** 会话分屏右侧打开的。 */
enum SessionPane:
  /** 新会话；`agent` 是预选的 agent（地址 `?agent=`） */
  case New(agent: Option[AgentId])
  case Open(id: SessionId)

/** agent 分屏右侧打开的。 */
enum AgentPane:
  case New
  case Open(id: AgentId)

/** AI 资产页的种类标签；`id` 是地址里的 `?kind=`。 */
enum AssetTab(val id: String, val label: String, val kinds: Set[AssetItemKind]):
  case Skill  extends AssetTab("skill", "Skill", Set(AssetItemKind.Skill))
  case Mcp    extends AssetTab("mcp", "MCP", Set(AssetItemKind.Mcp))
  case Secret extends AssetTab("secret", "Secret", Set(AssetItemKind.Secret))
  case Memory extends AssetTab("memory", "记忆", Set(AssetItemKind.Memory))
  /** agent.toml 与 access.toml */
  case Config extends AssetTab("config", "配置文件", Set(AssetItemKind.Agent, AssetItemKind.Access))

object AssetTab:
  def of(id: Option[String]): AssetTab = id.flatMap(i => values.find(_.id == i)).getOrElse(Skill)

/** 要编辑的 skill：名字与归属（平台或 agent 名；没给按名字找）。 */
final case class SkillRef(name: SkillName, owner: Option[String])

/** 整页显示的页面。 */
enum Page:
  case Notifications, Roles, Triggers, Settings, Pages, Usage
  /** 看板，带筛选（`?status=a,b&assignee=&owner=`） */
  case Issues(filter: IssueQuery)
  /** issue 详情；`reject`：从通知的「打回」点进来（`?review=reject`，Service Worker 加的），聚焦理由 */
  case Issue(id: IssueId, reject: Boolean)
  /** AI 资产（`?kind=&owner=`） */
  case Assets(tab: AssetTab, owner: Option[String])
  /** skill 编辑（`?name=&owner=`）；没有是新建 */
  case Skill(target: Option[SkillRef])
  /** agent 做的页面 */
  case Site(agent: AgentId, name: String)

/** 导航里的一项（左栏高亮、宽屏显示管理栏用）。 */
enum Section(val admin: Boolean):
  case Sessions      extends Section(false)
  case Issues        extends Section(false)
  case Pages         extends Section(false)
  case Notifications extends Section(false)
  case Agents        extends Section(true)
  case Triggers      extends Section(true)
  case Usage         extends Section(true)
  case Assets        extends Section(true)
  case Roles         extends Section(true)
  case Settings      extends Section(true)

/** 前端的页面，按布局分层：会话、agent 是列表与详情分屏，其余整页。地址与页面（含查询参数）一一对应：[[Route.parse]] 与 [[Route.url]] 互逆。 */
enum Route:
  case Sessions(pane: Option[SessionPane])
  case Agents(pane: Option[AgentPane])
  case Single(page: Page)

  def url: String = this match
    case Sessions(None)                         => "/"
    case Sessions(Some(SessionPane.New(agent))) => "/new" + Query.render("agent" -> agent.map(_.value))
    case Sessions(Some(SessionPane.Open(id)))   => s"/sessions/${Query.enc(id.value)}"
    case Agents(None)                           => "/agents"
    case Agents(Some(AgentPane.New))            => "/agents/new"
    case Agents(Some(AgentPane.Open(id)))       => s"/agents/${Query.enc(id.value)}"
    case Single(page) =>
      page match
        case Page.Notifications => "/notifications"
        case Page.Roles         => "/roles"
        case Page.Triggers      => "/triggers"
        case Page.Settings      => "/settings"
        case Page.Pages         => "/pages"
        case Page.Usage         => "/usage"
        case Page.Issues(f) =>
          "/issues" + Query.render(
            "status"   -> Option.when(f.status.nonEmpty)(f.status.map(_.label).mkString(",")),
            "assignee" -> f.assignee,
            "owner"    -> f.owner.map(_.value)
          )
        case Page.Issue(id, reject) => s"/issues/${id.value}" + Query.render("review" -> Option.when(reject)("reject"))
        case Page.Assets(tab, owner) => "/ai-assets" + Query.render("kind" -> Option.when(tab != AssetTab.Skill)(tab.id), "owner" -> owner)
        case Page.Skill(t)           => "/ai-assets/skill" + Query.render("name" -> t.map(_.name.value), "owner" -> t.flatMap(_.owner))
        case Page.Site(agent, name)  => s"/pages/${Query.enc(agent.value)}/${Query.enc(name)}"

  /** 属于导航里的哪一项（新会话不属于任何一项：它是居中的主按钮）。 */
  def section: Option[Section] = this match
    case Sessions(Some(SessionPane.New(_))) => None
    case Sessions(_)                        => Some(Section.Sessions)
    case Agents(_)                          => Some(Section.Agents)
    case Single(page) =>
      Some(page match
        case Page.Notifications            => Section.Notifications
        case Page.Roles                    => Section.Roles
        case Page.Triggers                 => Section.Triggers
        case Page.Settings                 => Section.Settings
        case Page.Pages | Page.Site(_, _)  => Section.Pages
        case Page.Usage                    => Section.Usage
        case Page.Issues(_) | Page.Issue(_, _) => Section.Issues
        case Page.Assets(_, _) | Page.Skill(_) => Section.Assets
      )

object Route:
  val home: Route                                       = Sessions(None)
  def newSession(agent: Option[AgentId] = None): Route  = Sessions(Some(SessionPane.New(agent)))
  def session(id: SessionId): Route                     = Sessions(Some(SessionPane.Open(id)))
  val agents: Route                                     = Agents(None)
  val agentNew: Route                                   = Agents(Some(AgentPane.New))
  def agent(id: AgentId): Route                         = Agents(Some(AgentPane.Open(id)))
  def issues(filter: IssueQuery = IssueQuery()): Route  = Single(Page.Issues(filter))
  def issue(id: IssueId): Route                         = Single(Page.Issue(id, reject = false))
  def assets(tab: AssetTab = AssetTab.Skill, owner: Option[String] = None): Route = Single(Page.Assets(tab, owner))
  def skill(target: Option[SkillRef]): Route            = Single(Page.Skill(target))

  /** 地址（路径 + 查询串，查询串可带或不带 `?`）→ 页面；认不出的是 None（外壳把它换成首页）。查询参数里认不出的值按没给处理。 */
  def parse(path: String, search: String): Option[Route] =
    val segs = path.split('/').toList.filter(_.nonEmpty).map(s => js.URIUtils.decodeURIComponent(s))
    val q    = Query.parse(search)
    def str(k: String): Option[String] = q.get(k).filter(_.nonEmpty)
    segs match
      case Nil                           => Some(home)
      case "new" :: Nil                  => Some(newSession(str("agent").flatMap(a => AgentId.make(a).toOption)))
      case "sessions" :: id :: Nil       => SessionId.make(id).toOption.map(session)
      case "notifications" :: Nil        => Some(Single(Page.Notifications))
      case "agents" :: Nil               => Some(agents)
      case "agents" :: "new" :: Nil      => Some(agentNew)
      case "agents" :: id :: Nil         => AgentId.make(id).toOption.map(agent)
      case "roles" :: Nil                => Some(Single(Page.Roles))
      case "triggers" :: Nil             => Some(Single(Page.Triggers))
      case "issues" :: Nil =>
        Some(issues(IssueQuery(
          status = str("status").toList.flatMap(_.split(',').toList).flatMap(IssueStatus.parse(_).toOption).distinct,
          assignee = str("assignee"),
          owner = str("owner").flatMap(o => AgentId.make(o).toOption)
        )))
      case "issues" :: id :: Nil =>
        id.toLongOption.flatMap(n => IssueId.make(n).toOption).map(i => Single(Page.Issue(i, reject = str("review").contains("reject"))))
      case "settings" :: Nil             => Some(Single(Page.Settings))
      case "ai-assets" :: Nil            => Some(assets(AssetTab.of(str("kind")), str("owner")))
      case "ai-assets" :: "skill" :: Nil => Some(skill(str("name").flatMap(n => SkillName.make(n).toOption).map(SkillRef(_, str("owner")))))
      case "pages" :: Nil                => Some(Single(Page.Pages))
      case "pages" :: a :: n :: Nil      => AgentId.make(a).toOption.map(id => Single(Page.Site(id, n)))
      case "usage" :: Nil                => Some(Single(Page.Usage))
      case _                             => None

/** 地址的查询串：解析与生成（与浏览器 URLSearchParams 的写法一致：`+` 是空格）。 */
object Query:
  def enc(s: String): String = js.URIUtils.encodeURIComponent(s)

  private def dec(s: String): String = js.URIUtils.decodeURIComponent(s.replace("+", " "))

  def parse(search: String): Map[String, String] =
    search.stripPrefix("?").split('&').toList.filter(_.nonEmpty).map { kv =>
      kv.split("=", 2) match
        case Array(k, v) => dec(k) -> dec(v)
        case Array(k)    => dec(k) -> ""
    }.toMap

  /** 只写给了值的参数；一个都没有是空串。 */
  def render(params: (String, Option[String])*): String =
    val present = params.collect { case (k, Some(v)) => s"${enc(k)}=${enc(v)}" }
    if present.isEmpty then "" else present.mkString("?", "&", "")

/** 当前地址与跳转。跳转时可带「从哪来」：目标页左上角的返回回到来源页，而不是它固定的上级页（从 issue 点进会话，返回回到 issue）。 */
object Router:
  final case class Location(path: String, search: String, from: Option[String]):
    def route: Option[Route] = Route.parse(path, search)
    def here: String         = path + search

  private def current(): Location =
    val st   = dom.window.history.state
    val from = if st == null || js.isUndefined(st) then None else st.asInstanceOf[js.Dynamic].from.asInstanceOf[js.UndefOr[String]].toOption
    Location(dom.window.location.pathname, dom.window.location.search, from)

  private val loc: Var[Location] = Var(current())

  val location: Signal[Location] = loc.signal
  def now: Location             = loc.now()

  dom.window.addEventListener("popstate", (_: dom.PopStateEvent) => loc.set(current()))

  private def stateOf(from: Option[String]): js.Any =
    from.fold[js.Any](null)(f => js.Dynamic.literal(from = f))

  /** 跳到站内地址；`withFrom` 时把当前页记为来源。 */
  def go(url: String, withFrom: Boolean = false): Unit =
    val from = Option.when(withFrom)(now.here)
    if url != now.here then
      dom.window.history.pushState(stateOf(from), "", url)
      loc.set(current())
    dom.window.scrollTo(0, 0)

  def go(route: Route): Unit = go(route.url)

  /** 换成另一个页面或改当前页的参数（筛选条件等），不压历史、保留来源。 */
  def update(route: Route): Unit =
    dom.window.history.replaceState(stateOf(now.from), "", route.url)
    loc.set(current())

  /** 返回按钮的目标：带了来源就回来源页，否则回固定的上级页。 */
  def back(fallback: Route): Unit = go(now.from.getOrElse(fallback.url))

  /** 站内地址：以单个 / 开头、不是后端接口与页面文件（/\x 会被浏览器当成协议相对地址，不算）。 */
  def isAppPath(href: String): Boolean =
    href.startsWith("/") && !href.startsWith("//") && !href.startsWith("/\\") && !href.startsWith("/api/") && !href.startsWith("/site/")
