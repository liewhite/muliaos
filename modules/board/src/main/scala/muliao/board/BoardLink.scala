package muliao.board

import muliao.contract.AgentId
import muliao.platform.{AppError, GeneralSettings}
import muliao.resources.{AgentHook, AgentHooks}
import muliao.resources.agents.Agents
import muliao.sessions.SessionHooks
import zio.*

/** 把本领域接到会话与资源领域：给 agent 的工具、会话事件订阅（失败记动态、停滞时请表态）、删除 agent 时置空指派与负责。
  * 组装时在 `Sessions.start` 之前调用 [[attach]]。
  */
object BoardLink:
  def attach: URIO[Board & SessionHooks & Agents & AgentHooks & GeneralSettings, Unit] =
    for
      board  <- ZIO.service[Board]
      agents <- ZIO.service[Agents]
      zone   <- ZIO.serviceWith[GeneralSettings](_.timezone)
      hooks  <- ZIO.service[SessionHooks]
      _      <- hooks.addTools(BoardTools.all(board, agents, zone))
      _      <- hooks.addListener(e => board.onSession(e))
      _ <- ZIO.serviceWithZIO[AgentHooks](_.register(new AgentHook:
             val name                                              = "看板"
             def busy(agent: AgentId): IO[AppError, Option[String]] = ZIO.none
             def cleanup(agent: AgentId): IO[AppError, Unit]      = board.forgetAgent(agent)
           ))
    yield ()
