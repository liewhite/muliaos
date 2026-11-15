package muliao.history

import muliao.resources.agents.Agents
import muliao.sessions.Contribution
import zio.*

/** 本领域接到会话领域的东西：给 agent 的工具。删 agent 时工作历史保留，所以不参与删除清理。 */
object HistoryLink:
  val contribution: URIO[History & Agents, Contribution] =
    for
      history <- ZIO.service[History]
      agents  <- ZIO.service[Agents]
    yield Contribution(tools = HistoryTools.all(history, agents))
