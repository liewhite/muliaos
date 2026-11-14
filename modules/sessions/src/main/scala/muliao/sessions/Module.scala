package muliao.sessions

import muliao.platform.DomainModule

/** 会话与运行：消息、运行循环（请求模型 → 执行工具）、每步落库与续跑、压缩、委派 thread、等待与心跳、提问与审批。
  *
  * 本领域自己的表结构迁移放在 `resources/db/sessions/`。
  */
object Module extends DomainModule:
  val name = "sessions"
