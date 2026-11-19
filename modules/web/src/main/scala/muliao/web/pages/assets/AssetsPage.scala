package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** /ai-assets：全部 AI 资源，按种类（`?kind=`）与归属（`?owner=`）筛选。每项显示归属与被谁使用；没加载的文件与原因、access.toml 写坏的提示放在最上面。 */
object AssetsPage:
  /** 归属筛选：全部、平台或某个 agent 名。 */
  def ownerMatches(filter: Option[String], item: AssetItem): Boolean = filter.forall(_ == item.owner)

  def items(index: AssetIndex, tab: AssetTab, owner: Option[String]): List[AssetItem] =
    index.items.filter(i => tab.kinds(i.kind) && ownerMatches(owner, i))

  /** @param at 地址里的种类与归属（改它就是改地址） */
  def view(at: Signal[(AssetTab, Option[String])]): HtmlElement =
    val index  = Remote(ResourcesApi.listAssets)(())
    val agents = AgentsData.list
    val tab    = at.map(_._1).distinct
    val owner  = at.map(_._2).distinct
    val edits  = new EventBus[((AssetTab, Option[String])) => (AssetTab, Option[String])]
    Ui.page("AI 资产")(
      edits.events.withCurrentValueOf(at) --> ((edit, t, o) => { val (t2, o2) = edit((t, o)); Router.update(Route.assets(t2, o2)) }),
      p(
        cls := "text-sm text-muted",
        "每项 AI 资产都是资源目录下的文件，在谁的目录下就归谁（平台或某个 agent）。agent 对它能做什么按权限 ",
        code(cls := "font-mono", "动作:种类:名字"),
        " 算：动作是 use（用）与 write（改），主人两者都有，其余来自它的角色（每个 agent 都有 base）与自己的规则；admin 能改全部。"
      ),
      Ui.loading(index) { idx =>
        div(
          cls := "flex flex-col gap-3",
          child.maybe <-- idx.map(_.accessBroken.map(Ui.errorAlert("权限文件 access.toml 写坏了：整份不生效，沿用上一份有效的；修好前平台不再改它", _))),
          child.maybe <-- idx.map(i => Option.when(i.problems.nonEmpty)(Problems.alert(i.problems))),
          div(
            cls := "max-w-xs",
            AgentPicker[Option[String]](
              "归属",
              owner.map(Some(_)),
              o => edits.emit((t, _) => (t, o)),
              agents,
              a => Some(a.name.value),
              fixed = List(Opt(None, "全部"), Opt(Some(Owner.Platform), "平台")),
              archived = AgentPicker.Archived.Listed
            )
          ),
          Tabs[AssetTab](
            "资产种类",
            tab,
            t => edits.emit((_, o) => (t, o)),
            AssetTab.values.toList.map(t => t -> span(t.label, child.text <-- idx.combineWith(owner).map((i, o) => s"（${items(i, t, o).size}）")))
          ),
          // 每个种类一个独立的实例（切换种类时新建表单不带过去）
          child <-- tab.map(t => panel(t, idx.combineWith(owner).map((i, o) => items(i, t, o)), agents))
        )
      }
    )

  private def panel(t: AssetTab, list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement = t match
    case AssetTab.Skill  => SkillTab.view(list, agents)
    case AssetTab.Mcp    => McpTab.view(list, agents)
    case AssetTab.Secret => SecretTab.view(list, agents)
    case AssetTab.Memory => MemoryTab.view(list, agents)
    case AssetTab.Config => ConfigTab.view(list)

  /** 列表：空时提示。 */
  def listOf(list: Signal[List[AssetItem]], empty: String)(item: StrictSignal[AssetItem] => HtmlElement): HtmlElement =
    div(
      cls := "flex flex-col gap-2",
      child.maybe <-- list.map(l => Option.when(l.isEmpty)(Ui.emptyHint(empty))),
      children <-- list.splitSeq(i => (i.kind, i.owner, i.name))(item)
    )

  /** 删除一项资源（先确认）。 */
  def deleteButton(item: StrictSignal[AssetItem], body: String, act: Action): Button =
    Ui.iconBtn("Trash2", s"删除 ${item.now().name}")(
      onClick --> { _ =>
        val i = item.now()
        Confirm
          .ask(s"删除「${i.name}」？", Some(body), "删除", danger = true)
          .foreach(ok => if ok then i.kind.asset.foreach(k => act.fire(Http.call(ResourcesApi.deleteAsset)((k, i.name)))))
      }
    )
