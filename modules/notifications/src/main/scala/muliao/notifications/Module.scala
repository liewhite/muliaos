package muliao.notifications

import muliao.platform.DomainModule

/** 通知与推送：通知收件箱、未读、Web Push（VAPID）订阅与投递。
  *
  * 本领域自己的表结构迁移放在 `resources/db/notifications/`。
  */
object Module extends DomainModule:
  val name = "notifications"
