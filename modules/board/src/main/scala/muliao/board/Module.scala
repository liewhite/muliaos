package muliao.board

import muliao.platform.DomainModule

/** issue 看板：issue、依赖（无环）、时间线、投递到 issue 会话、停滞与请表态、审核、叫停。
  *
  * 给别的领域用的接口：
  *   - [[Board]]：读写 issue（工作历史的活动回顾也从这里读）；
  *   - [[BoardLink.attach]]：组装时接到会话与资源领域（工具、会话事件、删除 agent 的清理），要在 `Sessions.start` 之前；
  *   - [[BoardHttp]]：界面的接口；
  *   - 规则都在纯函数 [[Rules]] 里（谁在处理、投给谁、停滞、改动成哪些动态、交给用户、打回给谁、依赖成环）。
  *
  * 本领域自己的表结构迁移放在 `resources/db/board/`。
  */
object Module extends DomainModule:
  val name = "board"
