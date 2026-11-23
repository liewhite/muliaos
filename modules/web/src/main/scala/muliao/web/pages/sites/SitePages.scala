package muliao.web.pages.sites

import com.raquo.laminar.api.L.*
import muliao.contract.{AgentId, PageShare, PageShareRequest, PageShareTtl, PageView, WorkspaceApi}
import org.scalajs.dom
import muliao.web.core.*
import muliao.web.core.Http.ec
import muliao.web.ui.*

/** 「页面」：agent 在工作目录 site/<名>/ 下写的网页（监控、数据展示）。列表是按 agent 分组的卡片网格、按最近更新排；点开在 iframe 里全屏显示。
  * 页面在沙箱里跑（服务端 CSP sandbox，不与界面同源），iframe 再加一层同样的 sandbox；地址是接口给的签名地址。
  */
object SitePages:
  /** 与服务端页面响应的 CSP sandbox（modules/workspace 的 pages/Pages.scala）一致：浏览器取交集，改就两处一起改。 */
  val Sandbox = "allow-scripts allow-forms allow-popups allow-modals allow-downloads"

  /** 按 agent 分组：页面与组都按最近更新排（最近更新的组在前）。 */
  def groupByAgent(pages: List[PageView]): List[(AgentId, List[PageView])] =
    pages.groupBy(_.agent).toList.map((a, ps) => a -> ps.sortBy(-_.modifiedAt)).sortBy((_, ps) => -ps.head.modifiedAt)

  private def load() = Remote(WorkspaceApi.listPages)(())

  def view(): HtmlElement =
    Ui.page("页面", wide = true)(
      div(
        cls := "mx-auto flex w-full max-w-7xl flex-col gap-4 px-4 py-3",
        Ui.loading(load()) { pages =>
          val groups = pages.map(groupByAgent)
          Seq(
            child.maybe <-- groups.map(g =>
              Option.when(g.isEmpty)(Ui.emptyHint("还没有页面：agent 在工作目录写 site/<页面名>/index.html 即出现在这里"))
            ),
            div(
              cls := "flex flex-col gap-4",
              children <-- groups.splitSeq(_._1) { g =>
                val ps = g.map(_._2)
                Ui.groupSection(g.now()._2.head.agentName, ps.map(_.size))(Ui.cardGrid(children <-- ps.splitSeq(_.page)(card)))
              }
            )
          )
        }
      )
    )

  /** 网格里的一张页面卡片，整张可点，打开页面。接口没有页面标题，标题用页面名。 */
  private def card(p: StrictSignal[PageView]): HtmlElement =
    val first = p.now()
    // 键是 agent 下的页面名，地址不会变
    Ui.appLink(Route.Single(Page.Site(first.agent, first.page)).url)(
      cls := "rounded-3xl outline-none focus-visible:ring-2 focus-visible:ring-accent",
      Ui.card(
        cls := "h-full gap-1 transition-shadow hover:shadow-md",
        div(cls := "truncate font-medium", child.text <-- p.map(_.page)),
        code(cls := "truncate font-mono text-xs text-muted", child.text <-- p.map(x => s"site/${x.page}")),
        div(cls := "text-xs text-muted", child.text <-- p.map(x => s"更新于 ${Format.relative(x.modifiedAt)}"))
      )
    )

  private given Shown[PageShareTtl] = {
    case PageShareTtl.Hour    => "1 小时"
    case PageShareTtl.Day     => "1 天"
    case PageShareTtl.Month   => "1 个月"
    case PageShareTtl.Forever => "永久"
  }

  /** 分享弹窗：选有效期生成不要登录的链接（服务端给站内路径，这里补上当前访问的源），显示到期时间、可复制。 */
  private def shareDialog(pv: PageView, onClose: () => Unit): HtmlElement =
    val act    = new Action
    val shared = Var(Option.empty[(PageShareTtl, PageShare)])
    def make(ttl: PageShareTtl): Unit =
      act.fire(Http.call(WorkspaceApi.sharePage)(PageShareRequest(pv.agent, pv.page, ttl)).map(s => shared.set(Some(ttl -> s))))
    Dialog(onClose, "分享页面", size = "md")(
      Dialog.header(Dialog.heading(s"分享 ${pv.page}")),
      Dialog.body(
        cls := "flex flex-col gap-3",
        p(cls := "text-sm text-muted", "拿到链接的人不用登录就能打开，到期后失效；到期前不能单独作废。"),
        div(
          cls := "flex flex-wrap gap-2",
          PageShareTtl.values.toList.map(ttl =>
            Ui.btn(Variant.Secondary, Size.Sm, disabledIf = act.busy.signal)(
              ttl.shown,
              dataAttr("selected") <-- shared.signal.map(_.exists(_._1 == ttl).toString),
              onClick --> (_ => make(ttl))
            )
          )
        ),
        child.maybe <-- shared.signal.map(_.map { (_, s) =>
          val full = dom.window.location.origin + s.url
          div(
            cls := "flex flex-col gap-2",
            input(cls := "input w-full font-mono text-xs", readOnly := true, value := full, onFocus --> (e => e.target.asInstanceOf[dom.html.Input].select())),
            div(
              cls := "flex items-center justify-between gap-2",
              span(cls := "text-xs text-muted", s.expiresAt.fold("永久有效")(t => s"${Format.time(t)} 到期")),
              Ui.btn(Variant.Primary, Size.Sm)(Icon("Copy"), "复制链接", onClick --> (_ => Ui.copyText(full, "链接")))
            )
          )
        }),
        act.errorView()
      )
    )

  def pageView(agent: AgentId, name: String): HtmlElement =
    val pages   = load()
    val page    = pages.map(_.data.flatMap(_.find(p => p.agent == agent && p.page == name)))
    // 刷新时递增：iframe 换一个重新加载
    val version = Var(0)
    // 正在分享的页面。弹窗不放在随页面数据重建的顶栏里：生成链接成功后界面会重拉数据，顶栏重建会把弹窗和刚生成的链接一起丢掉
    val sharing = Var(Option.empty[PageView])
    Ui.page(
      child.text <-- page.map(_.fold(name)(p => s"${p.page} · ${p.agentName}")),
      back = Some(Route.Single(Page.Pages)),
      wide = true,
      actions = child.maybe <-- page.map(_.map { p =>
        div(
          cls := "flex items-center gap-2",
          Ui.iconBtn("RefreshCw", "刷新", iconCls = "size-4")(onClick --> (_ => version.update(_ + 1))),
          Ui.iconBtn("Share2", "分享", iconCls = "size-4")(onClick --> (_ => sharing.set(Some(p)))),
          a(href := p.url, target := "_blank", rel := "noreferrer", aria.label := "新窗口打开", title := "新窗口打开", Icon("ExternalLink", "size-4"))
        )
      })
    )(
      child.maybe <-- sharing.signal.map(_.map(p => shareDialog(p, () => sharing.set(None)))),
      Ui.loading(pages) { list =>
        child <-- list.map(_.find(p => p.agent == agent && p.page == name)).distinct.combineWith(version.signal).map {
          case (Some(p), _) =>
            iframe(
              src := p.url,
              title := p.page,
              Ui.attr("sandbox") := Sandbox,
              Ui.attr("referrerpolicy") := "no-referrer",
              cls := "block h-full w-full border-0 bg-white"
            )
          case (None, _) => div(cls := "p-4", Ui.errorAlert("出错了", s"没有这个页面：$agent 的 site/$name"))
        }
      }
    )
