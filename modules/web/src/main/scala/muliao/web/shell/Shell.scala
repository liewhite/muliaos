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
    NavItem(Route.home, "会话", "MessageSquare")                     -> false,
    NavItem(Route.issues(), "Issues", "KanbanSquare")                -> false,
    NavItem(Route.newSession(), "新会话", "Plus")                     -> true,
    NavItem(Route.Single(Page.Pages), "页面", "LayoutDashboard")      -> false,
    NavItem(Route.Single(Page.Notifications), "通知", "Bell")         -> false
  )

  /** 管理菜单：配置类页面。 */
  val adminGroups: List[(String, List[NavItem])] = List(
    "运行"  -> List(NavItem(Route.agents, "Agents", "Bot"), NavItem(Route.Single(Page.Triggers), "触发器", "Zap"), NavItem(Route.Single(Page.Usage), "Token 用量", "BarChart3")),
    "资源"  -> List(NavItem(Route.assets(), "AI 资产", "Boxes"), NavItem(Route.Single(Page.Roles), "角色", "ShieldCheck")),
    "本设备" -> List(NavItem(Route.Single(Page.Settings), "设置", "Settings"))
  )

  /** 导航项是否算「当前页」：当前页与它属于导航里的同一项（新会话不属于任何一项）。 */
  def active(item: Route, at: Option[Route]): Boolean = item.section.isDefined && item.section == at.flatMap(_.section)

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
  /** 列表 + 详情：宽屏左列表右详情；窄屏只显示其一，打开详情时列表隐藏。 */
  private def listDetail[P](list: HtmlElement, detail: Signal[Option[P]], empty: String)(render: P => HtmlElement): HtmlElement =
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
            children <-- detail.map(_.toList).distinct.map(_.map(render))
          )
      }
    )

  /** 整页的页面。带参数的页面拿到参数的 Signal：改筛选条件不重建页面；issue 详情每个 issue 一个实例。 */
  private def single(page: Signal[Page]): Signal[HtmlElement] =
    page.splitMatchOne
      .handleCase { case Page.Notifications => () } (_ => pages.notifications.NotificationsPage.view())
      .handleCase { case Page.Roles => () } (_ => pages.agents.RolesPage.view())
      .handleCase { case Page.Triggers => () } (_ => pages.triggers.TriggersPage.view())
      .handleCase { case Page.Settings => () } (_ => pages.settings.SettingsPage.view())
      .handleCase { case Page.Pages => () } (_ => pages.sites.SitePages.view())
      .handleCase { case Page.Usage => () } (_ => pages.usage.UsagePage.view())
      .handleCase { case Page.Issues(f) => f } { f => pages.issues.IssuesPage.view(f) }
      .handleCase { case Page.Issue(id, reject) => (id, reject) } { at => div(cls := "h-full", child <-- at.distinct.map((id, reject) => pages.issues.IssueView.view(id, reject))) }
      .handleCase { case Page.Assets(tab, owner) => (tab, owner) } { at => pages.assets.AssetsPage.view(at) }
      .handleCase { case Page.Skill(t) => t } { t => pages.assets.SkillEditor.view(t) }
      .handleCase { case Page.Site(a, n) => (a, n) } { at => div(cls := "h-full", child <-- at.distinct.map((a, n) => pages.sites.SitePages.pageView(a, n))) }
      .toSignal

  private def content(route: Signal[Route]): HtmlElement =
    div(
      cls := "h-full",
      child <-- route.splitMatchOne
        .handleCase { case Route.Sessions(p) => p } { pane =>
          listDetail(pages.sessions.SessionList.view(), pane, "选择一个会话") {
            case SessionPane.New(agent) => pages.sessions.NewSessionPage.view(agent)
            case SessionPane.Open(id)   => pages.sessions.SessionView.view(id)
          }
        }
        .handleCase { case Route.Agents(p) => p } { pane =>
          listDetail(pages.agents.AgentList.view(), pane, "选择一个 agent") {
            case AgentPane.New      => pages.agents.AgentCreate.view()
            case AgentPane.Open(id) => pages.agents.AgentDetail.view(id)
          }
        }
        // 每个页面一个独立实例；页面出错只影响自己
        .handleCase { case Route.Single(p) => p } { page => div(cls := "h-full", child <-- single(page)) }
        .toSignal
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
          Option.when(item.route.section.contains(Section.Notifications))(
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
      route.updates.filter(_.isEmpty) --> (_ => Router.update(Route.home)),
      onMountCallback(_ => if Router.now.route.isEmpty then Router.update(Route.home)),
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
      child.maybe <-- route.map(_.flatMap(_.section).exists(_.admin)).distinct.map(admin =>
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
        // 全站共用的 agent 列表在这里一直订阅着；没拉到时各页的 agent 名字退回编号，在这里统一提示
        child.maybe <-- AgentsData.all.map {
          case Load.Failed(m) => Some(div(cls := "alert alert--danger m-2 text-sm", s"agent 列表没拉到，名字先显示编号：$m"))
          case _              => None
        },
        div(cls := "min-h-0 flex-1", child <-- route.map(_.isDefined).distinct.map {
          case true  => content(route.map(_.getOrElse(Route.home)))
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
        action.run(Auth.login(password.now()), leave = Some(() => Router.update(Route.home))).foreach(_ => ())
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
