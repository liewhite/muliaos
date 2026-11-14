package muliao.history

import muliao.resources.agents.Agents
import muliao.sessions.SessionHooks
import zio.*

/** 把本领域接到会话领域：给 agent 的工具。组装时在 `Sessions.start` 之前调用 [[attach]]。
  * 删 agent 时工作历史保留，所以不登记删除清理。
  */
object HistoryLink:
  def attach: URIO[History & SessionHooks & Agents, Unit] =
    for
      history <- ZIO.service[History]
      agents  <- ZIO.service[Agents]
      _       <- ZIO.serviceWithZIO[SessionHooks](_.addTools(HistoryTools.all(history, agents)))
    yield ()
