package muliao.board

import muliao.platform.GeneralSettings
import muliao.resources.AgentHook
import muliao.resources.agents.Agents
import muliao.sessions.Contribution
import zio.*

/** 本领域接到会话与资源领域的东西：给 agent 的工具、会话事件订阅（失败记动态、停滞时请表态）、删除 agent 时置空指派与负责。 */
object BoardLink:
  val contribution: URIO[Board & Agents & GeneralSettings, Contribution] =
    for
      board  <- ZIO.service[Board]
      agents <- ZIO.service[Agents]
      zone   <- ZIO.serviceWith[GeneralSettings](_.timezone)
    yield Contribution(
      tools = BoardTools.all(board, agents, zone),
      listener = Some(e => board.onSession(e)),
      agentHook = Some(AgentHook("看板", cleanup = board.forgetAgent))
    )
