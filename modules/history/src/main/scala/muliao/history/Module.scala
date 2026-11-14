package muliao.history

import muliao.platform.DomainModule

/** 工作历史与活动回顾：只追加的工作记录、按天活动汇总、读会话时间线、用量统计。
  *
  * 本领域自己的表结构迁移放在 `resources/db/history/`。
  */
object Module extends DomainModule:
  val name = "history"
