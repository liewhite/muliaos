package muliao.web.pages.agents

import com.raquo.laminar.api.L.*
import muliao.contract.AgentSummary
import muliao.web.core.*
import muliao.web.ui.*

/** /agents 的列表：不分组的平铺在前，每个分组一段（默认展开），归档的放最后一段（默认折叠）；有搜索词时平铺搜索结果。 */
object AgentList:
  /** 列表的一段。 */
  private enum Section:
    case Plain
    case Group(name: String)
    case Archived

  private final case class Part(section: Section, agents: List[AgentSummary])

  private def parts(all: List[AgentSummary], query: String): List[Part] =
    if query.trim.nonEmpty then List(Part(Section.Plain, AgentsData.search(all, query)))
    else
      val g = AgentsData.group(all)
      (Part(Section.Plain, g.ungrouped) :: g.groups.map((n, as) => Part(Section.Group(n), as))) :+ Part(Section.Archived, g.archived)

  def view(): HtmlElement =
    val agents = AgentsData.all
    val query  = Var("")
    Ui.page(
      "Agents",
      actions = Ui.linkButton(Route.agentNew.url)(Icon("Plus"), "新建")
    )(
      Ui.loading(agents) { list =>
        val sections = list.combineWith(query.signal).map(parts)
        div(
          cls := "flex flex-col gap-4",
          Fields.search("搜索 agent", query, "搜索名称或描述"),
          child.maybe <-- list.combineWith(query.signal).map((all, q) =>
            if all.isEmpty then Some(Ui.emptyHint("还没有 agent"))
            else Option.when(q.trim.nonEmpty && AgentsData.search(all, q).isEmpty)(Ui.emptyHint("没有匹配的 agent"))
          ),
          children <-- sections.map(_.filter(_.agents.nonEmpty)).splitSeq(_.section)(part => section(part.now().section, part.map(_.agents)))
        )
      }
    )

  private def section(s: Section, agents: Signal[List[AgentSummary]]): HtmlElement = s match
    case Section.Plain     => cards(agents)
    case Section.Group(n)  => Ui.groupSection(n, agents.map(_.size))(cards(agents))
    case Section.Archived  => Ui.groupSection("已归档", agents.map(_.size), defaultExpanded = false)(cards(agents))

  private def cards(agents: Signal[List[AgentSummary]]): HtmlElement =
    div(cls := "flex flex-col gap-2", children <-- agents.splitSeq(_.id)(a => card(a)))

  /** 列表按编号复用元素，所以一张卡片的编号不变。 */
  private def card(agent: StrictSignal[AgentSummary]): HtmlElement =
    val url = Route.agent(agent.now().id).url
    Ui.linkCard(url, Router.location.map(_.path == url))(
      div(
        cls := "card__header",
        h3(cls := "card__title", child.text <-- agent.map(_.name.value)),
        p(
          cls := "card__description truncate",
          title <-- agent.map(_.description.getOrElse("")),
          child.text <-- agent.map(_.description.getOrElse("配置未加载，不能运行（原因见详情）"))
        )
      ),
      div(
        cls := "card__footer flex flex-wrap gap-2",
        child.maybe <-- agent.map(x => Option.when(x.isDefault)(Ui.chip("默认", Tone.Accent)())),
        child.maybe <-- agent.map(x => Option.when(AgentsData.archived(x))(Ui.chip("已归档", Tone.Warning)())),
        child <-- agent.map(_.model).map {
          case Some(m) => Ui.chip(Format.modelLabel(m), look = ChipLook.Secondary)()
          case None    => Ui.chip("配置未加载", Tone.Danger)()
        }
      )
    )
