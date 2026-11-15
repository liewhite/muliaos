package muliao.notifications

import muliao.contract.*
import muliao.platform.server.{Endpoints, Route}
import zio.*

/** 本领域的 HTTP 接口：通知页、标已读、推送公钥、登记订阅、测试推送。 */
final case class NotificationsHttp(notifications: Notifications):
  import Endpoints.*

  def routes: List[Route] =
    List(
      NotificationsApi.list.secured(limit => notifications.inbox(limit.getOrElse(Notifications.DefaultLimit))),
      NotificationsApi.readThrough.secured(r => notifications.readThrough(r.through)),
      NotificationsApi.readPage.secured(r => notifications.readPage(r.url)),
      NotificationsApi.key.secured(_ => ZIO.succeed(PushKey(notifications.publicKey))),
      NotificationsApi.subscribe.secured(notifications.subscribe),
      NotificationsApi.test.secured(_ => notifications.sendTest.unit)
    )
