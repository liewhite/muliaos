package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.{AgentId, AgentSummary, ResourcesApi}
import muliao.web.core.*

/** 多个页面共用的 agent 列表：显示名、分组、可选来派活的。 */
object AgentsData:
  /** 全部 agent（含归档的），随 agents 主题刷新。每次调用得到一份新的数据源，页面各自持有。 */
  def load(): Signal[Load[List[AgentSummary]]] = Remote("agents", "assets")(Http.call(ResourcesApi.listAgents)(()))  // 改 agent.toml（模型、分组）只发 assets

  def archived(a: AgentSummary): Boolean = a.state == "archived"

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

  /** 可以选来派活（新会话、指派、负责、触发器）的：不含归档的；`keep` 是当前已选的值，归档了也留着以便显示。 */
  def selectable(agents: List[AgentSummary], keep: Option[AgentId] = None): List[AgentSummary] =
    agents.filter(a => !archived(a) || keep.contains(a.id))

  /** 按编号显示名字；还没加载完或已删除时显示编号。 */
  def label(agents: List[AgentSummary], id: AgentId): String = agents.find(_.id == id).fold(id.value)(_.name.value)

  /** 下拉框的选项。 */
  def options(agents: List[AgentSummary]): List[Opt[AgentId]] =
    agents.map(a => Opt(a.id, a.name.value, a.description.map(_.linesIterator.nextOption().getOrElse(""))))
