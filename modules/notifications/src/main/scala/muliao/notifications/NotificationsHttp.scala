package muliao.notifications

import muliao.contract.*
import muliao.platform.server.{ApiEndpoint, Auth, Endpoints}
import zio.*

/** 本领域的 HTTP 接口：通知页、标已读、推送公钥、登记订阅、测试推送。 */
final case class NotificationsHttp(notifications: Notifications):
  import Endpoints.*

  def endpoints(auth: Auth): List[ApiEndpoint] =
    List(
      NotificationsApi.list.secured(auth)(_ => limit => notifications.inbox(limit.getOrElse(Notifications.DefaultLimit))),
      NotificationsApi.readThrough.secured(auth)(_ => r => notifications.readThrough(r.through)),
      NotificationsApi.readPage.secured(auth)(_ => r => notifications.readPage(r.url)),
      NotificationsApi.key.secured(auth)(_ => _ => ZIO.succeed(PushKey(notifications.publicKey))),
      NotificationsApi.subscribe.secured(auth)(_ => notifications.subscribe),
      NotificationsApi.test.secured(auth)(_ => _ => notifications.sendTest.unit)
    )

object NotificationsHttp:
  val layer: URLayer[Notifications, NotificationsHttp] = ZLayer.derive[NotificationsHttp]
