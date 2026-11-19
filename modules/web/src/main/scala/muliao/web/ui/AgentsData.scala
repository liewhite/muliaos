package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.{AgentId, AgentState, AgentSummary, ResourcesApi}
import muliao.web.core.*

/** 多个页面共用的 agent 列表：显示名、分组、搜索。 */
object AgentsData:
  /** 全部 agent（含归档的），全站共用一份：登录后的外壳一直订阅着（失败时由外壳在顶上提示），随接口声明的主题刷新。 */
  val all: Signal[Load[List[AgentSummary]]] = Remote(ResourcesApi.listAgents)(())

  /** 只要名字、选项时用：还没拉到或拉取失败时为空（名字退回显示编号）。 */
  val list: Signal[List[AgentSummary]] = all.map(_.data.getOrElse(Nil))

  def archived(a: AgentSummary): Boolean = a.state == AgentState.Archived

  /** 列表页的分段：不分组的平铺在前，每个组一段（按组名），归档的单独一段；各段内保持接口给的顺序。 */
  final case class Groups(ungrouped: List[AgentSummary], groups: List[(String, List[AgentSummary])], archived: List[AgentSummary])

  def group(agents: List[AgentSummary]): Groups =
    val (arch, active) = agents.partition(archived)
    val (grouped, ungrouped) = active.partition(_.group.isDefined)
    Groups(ungrouped, grouped.groupBy(_.group.get).toList.sortBy(_._1), arch)

  /** 按名称或描述搜索（不区分大小写）；空搜索词返回空列表。 */
  def search(agents: List[AgentSummary], query: String): List[AgentSummary] =
    val q = query.trim.toLowerCase
    if q.isEmpty then Nil
    else agents.filter(a => a.name.value.toLowerCase.contains(q) || a.description.exists(_.toLowerCase.contains(q)))

  /** 按编号显示名字；还没加载完或已删除时显示编号。 */
  def label(agents: List[AgentSummary], id: AgentId): String = agents.find(_.id == id).fold(id.value)(_.name.value)

  /** 按名字找（资源归属、角色成员记的是名字）。 */
  def byName(agents: List[AgentSummary], name: String): Option[AgentSummary] = agents.find(_.name.value == name)

/** agent 名字的 chip，链到它的详情页；找不到（已删除、列表还没拉到）时只显示名字。 */
object AgentChip:
  def apply(name: String, tone: Tone = Tone.Accent): HtmlElement =
    span(
      child <-- AgentsData.list.map(AgentsData.byName(_, name)).map {
        case Some(a) => Ui.appLink(Route.agent(a.id).url)(Ui.chip(name, tone)())
        case None    => Ui.chip(name, tone)()
      }
    )
