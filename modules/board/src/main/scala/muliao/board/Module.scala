package muliao.board

import muliao.platform.DomainModule

/** issue 看板：issue、依赖、时间线、投递到 issue 会话、停滞、审核。
  *
  * 本领域自己的表结构迁移放在 `resources/db/board/`。
  */
object Module extends DomainModule:
  val name = "board"
