package muliao.notifications

import muliao.platform.DomainModule
import muliao.platform.config.ConfigSection

/** 通知与推送：通知收件箱（未读、按页面地址标已读、会话未读）、`notify_user`、会话事件转通知、Web Push（VAPID、aes128gcm）。
  *
  * 给别的领域用的接口：
  *   - [[Notifications]]：`notifyIn` 在调用方的写事务里发通知（看板交给用户、触发器告警等，与自己的改动同一事务提交），`notify` 自己开事务；
  *   - [[Notice]] / [[Target]]：通知的内容与点开去哪（会话、发送方给的页面、通知页）；
  *   - [[SessionLink]]：交给会话领域的东西（`contribution`：事件订阅、`notify_user`；`unread`：会话未读），组装入口在 `Sessions.start` 时交进去；
  *   - [[NotificationsHttp]]：界面的接口。
  *
  * 本领域自己的表结构迁移放在 `resources/db/notifications/`。
  */
object Module extends DomainModule:
  val name = "notifications"

  override val sections: List[ConfigSection[?]] = List(PushSettings.section)
