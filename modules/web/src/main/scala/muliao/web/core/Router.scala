package muliao.web.core

import com.raquo.airstream.core.Signal
import com.raquo.airstream.state.Var
import org.scalajs.dom

import scala.scalajs.js

/** 前端的页面。地址与页面一一对应：[[Route.parse]] 与 [[Route.url]] 互逆（查询参数另见 [[Router.query]]）。 */
enum Route:
  case Sessions
  case NewSession
  case Session(id: String)
  case Notifications
  case Agents
  case AgentNew
  case Agent(id: String)
  case Roles
  case Triggers
  case Issues
  case Issue(id: Long)
  case Settings
  case Assets
  case SkillEdit
  case Pages
  case Page(agent: String, name: String)
  case Usage

  def url: String = this match
    case Sessions          => "/"
    case NewSession        => "/new"
    case Session(id)       => s"/sessions/${Http.enc(id)}"
    case Notifications     => "/notifications"
    case Agents            => "/agents"
    case AgentNew          => "/agents/new"
    case Agent(id)         => s"/agents/${Http.enc(id)}"
    case Roles             => "/roles"
    case Triggers          => "/triggers"
    case Issues            => "/issues"
    case Issue(id)         => s"/issues/$id"
    case Settings          => "/settings"
    case Assets            => "/ai-assets"
    case SkillEdit         => "/ai-assets/skill"
    case Pages             => "/pages"
    case Page(agent, name) => s"/pages/${Http.enc(agent)}/${Http.enc(name)}"
    case Usage             => "/usage"

object Route:
  /** 认不出的地址是 None（外壳把它换成首页）。 */
  def parse(path: String): Option[Route] =
    val segs = path.split('/').toList.filter(_.nonEmpty).map(s => js.URIUtils.decodeURIComponent(s))
    segs match
      case Nil                         => Some(Sessions)
      case "new" :: Nil                => Some(NewSession)
      case "sessions" :: id :: Nil     => Some(Session(id))
      case "notifications" :: Nil      => Some(Notifications)
      case "agents" :: Nil             => Some(Agents)
      case "agents" :: "new" :: Nil    => Some(AgentNew)
      case "agents" :: id :: Nil       => Some(Agent(id))
      case "roles" :: Nil              => Some(Roles)
      case "triggers" :: Nil           => Some(Triggers)
      case "issues" :: Nil             => Some(Issues)
      case "issues" :: id :: Nil       => id.toLongOption.map(Issue(_))
      case "settings" :: Nil           => Some(Settings)
      case "ai-assets" :: Nil          => Some(Assets)
      case "ai-assets" :: "skill" :: Nil => Some(SkillEdit)
      case "pages" :: Nil              => Some(Pages)
      case "pages" :: a :: n :: Nil    => Some(Page(a, n))
      case "usage" :: Nil              => Some(Usage)
      case _                           => None

  /** 页面属于管理菜单里的哪一项（左栏高亮、宽屏显示管理栏）。 */
  def adminItem(r: Route): Option[Route] = r match
    case Agents | AgentNew | Agent(_) => Some(Agents)
    case Triggers                     => Some(Triggers)
    case Usage                        => Some(Usage)
    case Assets | SkillEdit           => Some(Assets)
    case Roles                        => Some(Roles)
    case Settings                     => Some(Settings)
    case _                            => None

/** 当前地址与跳转。跳转时可带「从哪来」：目标页左上角的返回回到来源页，而不是它固定的上级页（从 issue 点进会话，返回回到 issue）。 */
object Router:
  final case class Location(path: String, search: String, from: Option[String]):
    def route: Option[Route] = Route.parse(path)
    def params: Map[String, String] =
      val p = new dom.URLSearchParams(search)
      p.asInstanceOf[js.Iterable[js.Tuple2[String, String]]].toSeq.map(t => t._1 -> t._2).toMap
    def here: String = path + search

  private def current(): Location =
    val st   = dom.window.history.state
    val from = if st == null || js.isUndefined(st) then None else st.asInstanceOf[js.Dynamic].from.asInstanceOf[js.UndefOr[String]].toOption
    Location(dom.window.location.pathname, dom.window.location.search, from)

  private val loc: Var[Location] = Var(current())

  val location: Signal[Location] = loc.signal
  def now: Location             = loc.now()

  /** 当前页的查询参数。 */
  val query: Signal[Map[String, String]] = loc.signal.map(_.params).distinct

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

  /** 换掉当前地址（改筛选条件等），不压历史。 */
  def replace(url: String): Unit =
    dom.window.history.replaceState(stateOf(now.from), "", url)
    loc.set(current())

  /** 改当前页的一个查询参数（None 去掉）。 */
  def setParam(key: String, value: Option[String]): Unit =
    val p = new dom.URLSearchParams(now.search)
    value.fold(p.delete(key))(v => p.set(key, v))
    val s = p.toString
    replace(now.path + (if s.isEmpty then "" else s"?$s"))

  /** 返回按钮的目标：带了来源就回来源页，否则回固定的上级页。 */
  def back(fallback: Route): Unit = go(now.from.getOrElse(fallback.url))

  /** 站内地址：以单个 / 开头、不是后端接口与页面文件（/\x 会被浏览器当成协议相对地址，不算）。 */
  def isAppPath(href: String): Boolean =
    href.startsWith("/") && !href.startsWith("//") && !href.startsWith("/\\") && !href.startsWith("/api/") && !href.startsWith("/site/")
