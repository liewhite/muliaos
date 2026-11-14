package muliao.triggers

import muliao.platform.DomainModule

/** 触发器：定时、一次性、常驻流，前置检查、防重叠、脚本故障。
  *
  * 本领域自己的表结构迁移放在 `resources/db/triggers/`。
  */
object Module extends DomainModule:
  val name = "triggers"
