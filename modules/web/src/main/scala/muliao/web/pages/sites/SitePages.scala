package muliao.web.pages.sites

import com.raquo.laminar.api.L.*
import muliao.contract.{AgentId, PageView, WorkspaceApi}
import muliao.web.core.*
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
    Ui.appLink(Route.Page(first.agent.value, first.page).url)(
      cls := "rounded-3xl outline-none focus-visible:ring-2 focus-visible:ring-accent",
      div(
        cls := "card card--default h-full gap-1 transition-shadow hover:shadow-md",
        dataAttr("slot") := "card",
        div(cls := "truncate font-medium", child.text <-- p.map(_.page)),
        code(cls := "truncate font-mono text-xs text-muted", child.text <-- p.map(x => s"site/${x.page}")),
        div(cls := "text-xs text-muted", child.text <-- p.map(x => s"更新于 ${Ui.relativeTime(x.modifiedAt)}"))
      )
    )

  def pageView(agent: String, name: String): HtmlElement =
    val pages   = load()
    val page    = pages.map(_.data.flatMap(_.find(p => p.agent.value == agent && p.page == name)))
    // 刷新时递增：iframe 换一个重新加载
    val version = Var(0)
    Ui.page(
      child.text <-- page.map(_.fold(name)(p => s"${p.page} · ${p.agentName}")),
      back = Some(Route.Pages),
      wide = true,
      actions = child.maybe <-- page.map(_.map { p =>
        div(
          cls := "flex items-center gap-2",
          Ui.iconBtn("RefreshCw", "刷新", iconCls = "size-4")(onClick --> (_ => version.update(_ + 1))),
          a(href := p.url, target := "_blank", rel := "noreferrer", aria.label := "新窗口打开", title := "新窗口打开", Icon("ExternalLink", "size-4"))
        )
      })
    )(
      Ui.loading(pages) { list =>
        child <-- list.map(_.find(p => p.agent.value == agent && p.page == name)).distinct.combineWith(version.signal).map {
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
