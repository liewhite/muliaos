package muliao.web.shell

import com.raquo.laminar.api.L.*
import muliao.contract.NotificationsApi
import muliao.web.core.*
import muliao.web.pages
import muliao.web.pwa.{Badge, Push, SwNavigate}
import muliao.web.ui.*
import org.scalajs.dom

import Http.ec

/** 导航的一项。 */
final case class NavItem(route: Route, label: String, icon: String)

object Nav:
  /** 首页导航（手机底栏、桌面左侧栏）：五项，新会话居中画成圆形主按钮。 */
  val activity: List[(NavItem, Boolean)] = List(
    NavItem(Route.Sessions, "会话", "MessageSquare")        -> false,
    NavItem(Route.Issues, "Issues", "KanbanSquare")         -> false,
    NavItem(Route.NewSession, "新会话", "Plus")              -> true,
    NavItem(Route.Pages, "页面", "LayoutDashboard")          -> false,
    NavItem(Route.Notifications, "通知", "Bell")             -> false
  )

  /** 管理菜单：配置类页面。 */
  val adminGroups: List[(String, List[NavItem])] = List(
    "运行"  -> List(NavItem(Route.Agents, "Agents", "Bot"), NavItem(Route.Triggers, "触发器", "Zap"), NavItem(Route.Usage, "Token 用量", "BarChart3")),
    "资源"  -> List(NavItem(Route.Assets, "AI 资产", "Boxes"), NavItem(Route.Roles, "角色", "ShieldCheck")),
    "本设备" -> List(NavItem(Route.Settings, "设置", "Settings"))
  )

  /** 导航项是否算「当前页」：会话是首页，它的子页（新会话除外）也算。 */
  def active(item: Route, at: Option[Route]): Boolean = (item, at) match
    case (Route.Sessions, Some(Route.Sessions | Route.Session(_))) => true
    case (Route.Issues, Some(Route.Issues | Route.Issue(_)))       => true
    case (Route.Pages, Some(Route.Pages | Route.Page(_, _)))       => true
    case (Route.Notifications, Some(Route.Notifications))          => true
    case (r, Some(x))                                              => Route.adminItem(x).contains(r)
    case _                                                         => false

  def adminNav(onNavigate: () => Unit): HtmlElement =
    navTag(
      aria.label := "管理",
      cls := "flex flex-col gap-4 p-3",
      adminGroups.map { (title, items) =>
        div(
          cls := "flex flex-col gap-0.5",
          div(cls := "px-3 pb-1 text-xs font-medium text-muted", title),
          items.map { it =>
            Ui.appLink(it.route.url, withFrom = false)(
              cls <-- Router.location.map(l => active(it.route, l.route)).map { on =>
                "flex items-center gap-3 rounded-lg px-3 py-2 text-sm transition-colors " +
                  (if on then "bg-accent/10 font-medium text-accent" else "hover:bg-surface-hover")
              },
              onClick --> (_ => onNavigate()),
              Icon(it.icon, "size-4 shrink-0"),
              span(cls := "truncate", it.label)
            )
          }
        )
      }
    )

object Shell:
  /** 页面怎么排：会话、Agents 在宽屏（xl）列表与详情分屏；其余整页。 */
  private enum Layout:
    case SessionsSplit, AgentsSplit, Single

  private def layoutOf(r: Route): Layout = r match
    case Route.Sessions | Route.NewSession | Route.Session(_) => Layout.SessionsSplit
    case Route.Agents | Route.AgentNew | Route.Agent(_)       => Layout.AgentsSplit
    case _                                                    => Layout.Single

  /** 列表 + 详情：宽屏左列表右详情；窄屏只显示其一，打开详情时列表隐藏。 */
  private def listDetail(list: HtmlElement, detail: Signal[Option[Route]], empty: String)(render: Route => HtmlElement): HtmlElement =
    div(
      cls := "flex h-full",
      div(
        cls <-- detail.map(d =>
          "h-full min-w-0 flex-col xl:flex xl:w-96 xl:flex-none xl:border-r xl:border-separator " + (if d.isDefined then "hidden" else "flex flex-1")
        ),
        list
      ),
      child <-- detail.map(_.isDefined).distinct.map {
        case false => div(cls := "hidden flex-1 items-center justify-center text-sm text-muted xl:flex", empty)
        case true =>
          div(
            cls := "h-full min-w-0 flex-1",
            // 每个详情一个独立的页面实例：切到另一个会话/agent 时草稿、附件、展开状态不带过去
            child <-- detail.map(_.get).distinct.map(render)
          )
      }
    )

  private def renderSingle(r: Route): HtmlElement = r match
    case Route.Notifications   => pages.notifications.NotificationsPage.view()
    case Route.Roles           => pages.agents.RolesPage.view()
    case Route.Triggers        => pages.triggers.TriggersPage.view()
    case Route.Issues          => pages.issues.IssuesPage.view()
    case Route.Issue(id)       => pages.issues.IssueView.view(id)
    case Route.Settings        => pages.settings.SettingsPage.view()
    case Route.Assets          => pages.assets.AssetsPage.view()
    case Route.SkillEdit       => pages.assets.SkillEditor.view()
    case Route.Pages           => pages.sites.SitePages.view()
    case Route.Page(a, n)      => pages.sites.SitePages.pageView(a, n)
    case Route.Usage           => pages.usage.UsagePage.view()
    case other                 => div(s"不该到这里：$other")

  private def content(route: Signal[Route]): HtmlElement =
    div(
      cls := "h-full",
      child <-- route.map(layoutOf).distinct.map {
        case Layout.SessionsSplit =>
          listDetail(pages.sessions.SessionList.view(), route.map { case Route.Sessions => None; case r => Some(r) }, "选择一个会话") {
            case Route.NewSession  => pages.sessions.NewSessionPage.view()
            case Route.Session(id) => pages.sessions.SessionView.view(id)
            case other             => div(s"不该到这里：$other")
          }
        case Layout.AgentsSplit =>
          listDetail(pages.agents.AgentList.view(), route.map { case Route.Agents => None; case r => Some(r) }, "选择一个 agent") {
            case Route.AgentNew  => pages.agents.AgentCreate.view()
            case Route.Agent(id) => pages.agents.AgentDetail.view(id)
            case other           => div(s"不该到这里：$other")
          }
        case Layout.Single =>
          // 每个页面（含每个 issue）一个独立实例；页面出错只影响自己
          div(cls := "h-full", child <-- route.distinct.map(renderSingle))
      }
    )

  /** 首页导航的一项；通知带未读角标。 */
  private def tabLink(item: NavItem, primary: Boolean, unread: Signal[Long], extra: String): HtmlElement =
    if primary then
      Ui.appLink(item.route.url, withFrom = false)(
        aria.label := item.label,
        title := item.label,
        cls := s"flex items-center justify-center $extra",
        span(
          cls := "flex size-11 items-center justify-center rounded-full bg-accent text-accent-foreground shadow-md transition-transform active:scale-95",
          Icon(item.icon, "size-6")
        )
      )
    else
      Ui.appLink(item.route.url, withFrom = false)(
        cls <-- Router.location.map(l => Nav.active(item.route, l.route)).map(on =>
          s"flex flex-col items-center justify-center gap-0.5 text-[11px] $extra " + (if on then "text-accent" else "text-muted")
        ),
        span(
          cls := "relative",
          Icon(item.icon, "size-5"),
          Option.when(item.route == Route.Notifications)(
            child.maybe <-- unread.map(n =>
              Option.when(n > 0)(
                span(
                  cls := "absolute -top-1.5 left-3 min-w-4 rounded-full bg-danger px-1 text-center text-[10px] leading-4 text-danger-foreground",
                  if n > 99 then "99+" else n.toString,
                  span(cls := "sr-only", "条未读")
                )
              )
            )
          )
        ),
        span(item.label)
      )

  /** 登录后的界面。手机：页面 + 底部导航，页面头部左上角打开管理抽屉。桌面（md 起）：左侧窄栏 + 页面；管理类页面在 lg 起多一列常驻的管理菜单。 */
  def view(): HtmlElement =
    val unreadLoad = Remote(NotificationsApi.list)(Some(1)).map(_.map(_.unread))
    val unread     = unreadLoad.map(_.data.getOrElse(0L))
    val route      = Router.location.map(_.route)
    div(
      cls := "flex h-full flex-col md:flex-row",
      // 认不出的地址回首页
      route.updates.filter(_.isEmpty) --> (_ => Router.replace("/")),
      onMountCallback(_ => if Router.now.route.isEmpty then Router.replace("/")),
      // 未读数跟着角标；推送到达时按服务端的数纠正
      unread --> (n => Badge.set(n)),
      Badge.pushed --> (_ => Events.refreshNow.emit(())),
      // 订阅可能被浏览器轮换：登录后与每次回到前台时同步一次
      onMountCallback(_ => syncPush()),
      documentEvents(_.onVisibilityChange) --> (_ => syncPush()),
      // 点通知后跳到 SW 留下的页面（含外壳挂上前就取到的那次）
      onMountUnmountCallbackWithState(_ => SwNavigate.handle(url => Router.go(url)), (_, stop) => stop()),
      navTag(
        aria.label := "动态",
        cls := "hidden w-16 shrink-0 flex-col items-center gap-1 border-r border-separator bg-surface pt-[calc(env(safe-area-inset-top)+0.5rem)] pb-2 md:flex",
        Ui.iconBtn("Menu", "管理菜单", size = Size.Md)(onClick --> (_ => Ui.adminMenuOpen.set(true))),
        div(cls := "my-1 h-px w-8 bg-separator"),
        Nav.activity.map((it, primary) => tabLink(it, primary, unread, "h-14 w-14 rounded-lg hover:bg-surface-hover"))
      ),
      child.maybe <-- route.map(_.flatMap(Route.adminItem).isDefined).distinct.map(admin =>
        Option.when(admin)(
          asideTag(
            cls := "hidden w-56 shrink-0 flex-col border-r border-separator bg-surface lg:flex",
            div(cls := "border-b border-separator px-6 pt-[calc(env(safe-area-inset-top)+0.5rem)] pb-2 text-lg leading-8 font-semibold", "管理"),
            div(cls := "min-h-0 flex-1 overflow-y-auto", Nav.adminNav(() => ()))
          )
        )
      ),
      div(
        cls := "flex min-h-0 min-w-0 flex-1 flex-col",
        div(cls := "min-h-0 flex-1", child <-- route.map(_.isDefined).distinct.map {
          case true  => content(route.map(_.getOrElse(Route.Sessions)))
          case false => div()
        }),
        navTag(
          aria.label := "动态",
          cls := "flex border-t border-separator bg-surface pb-[env(safe-area-inset-bottom)] md:hidden",
          Nav.activity.map((it, primary) => tabLink(it, primary, unread, "h-14 flex-1"))
        )
      ),
      // 管理抽屉：换了页面就收起
      Router.location.updates --> (_ => Ui.adminMenuOpen.set(false)),
      child.maybe <-- Ui.adminMenuOpen.signal.map(open =>
        Option.when(open)(
          Overlay.drawerLeft(() => Ui.adminMenuOpen.set(false), "管理菜单")(
            div(cls := "px-6 pt-4 pb-2 text-lg font-semibold", "管理"),
            div(cls := "min-h-0 flex-1 overflow-y-auto", Nav.adminNav(() => Ui.adminMenuOpen.set(false)))
          )
        )
      )
    )

  private def syncPush(): Unit =
    if dom.document.visibilityState == dom.VisibilityState.visible then
      Push.sync().failed.foreach(e => dom.console.error("push subscription sync failed", e.getMessage))

object Login:
  def view(): HtmlElement =
    val password = Var("")
    val action   = new Action
    def submit(): Unit =
      if password.now().nonEmpty then
        action.run(Auth.login(password.now()), leave = Some(() => Router.replace("/"))).foreach(_ => ())
    div(
      cls := "flex h-full items-center justify-center p-6",
      Ui.card(
        cls := "w-full max-w-sm",
        div(
          cls := "card__header items-center text-center",
          img(src := "/icon-192.png", alt := "", widthAttr := 64, heightAttr := 64, cls := "rounded-2xl"),
          div(cls := "card__title mt-2 text-xl", "Muliao")
        ),
        div(
          cls := "card__content",
          form(
            cls := "flex flex-col gap-4",
            onSubmit.preventDefault --> (_ => submit()),
            Fields.text("密码", password, inputType = "password", autoFocusOn = true),
            action.errorView(),
            button(
              tpe := "submit",
              cls := Ui.buttonCls() + " button--full-width",
              disabled <-- password.signal.map(_.isEmpty).combineWith(action.busy.signal).map(_ || _),
              child.maybe <-- action.busy.signal.map(b => Option.when(b)(Ui.spinner("spinner--sm spinner--current"))),
              "登录"
            )
          )
        )
      )
    )
