package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- 通知收件箱 ----

/** 一条通知。
  * @param url     点开去的页面（前端路由）；系统通知没有，点开看通知页
  * @param session 通知关于的会话（打开它即已读、随它删除）
  */
final case class NotificationView(
    id: NotificationId,
    title: String,
    body: String,
    url: Option[String],
    session: Option[SessionId],
    unread: Boolean,
    createdAt: Long
) derives JsonCodec, Schema

/** 通知页：最新的若干条（新的在前）与全部未读数（应用角标）。 */
final case class Inbox(items: List[NotificationView], unread: Long) derives JsonCodec, Schema

/** 通知页：标到 `through` 这一条为止已读（之后才到的用户还没看到，保持未读）。 */
final case class ReadThrough(through: NotificationId) derives JsonCodec, Schema

/** 用户打开了页面 `url`（会话页、issue 页……）：点开就是它的通知已读。 */
final case class ReadPage(url: String) derives JsonCodec, Schema

// ---- Web Push ----

/** 浏览器订阅推送时用的服务器公钥（VAPID，base64url 编码的未压缩点）。 */
final case class PushKey(publicKey: String) derives JsonCodec, Schema

/** 浏览器 `PushSubscription.toJSON()` 的 keys。 */
final case class PushKeys(p256dh: String, auth: String) derives JsonCodec, Schema

/** 本设备的推送订阅（与浏览器 `PushSubscription.toJSON()` 的形状一致，多余字段忽略）。 */
final case class PushSubscriptionIn(endpoint: String, keys: PushKeys) derives JsonCodec, Schema

object NotificationsApi:
  private val notifications = Api.secure.in("notifications")
  private val push          = Api.secure.in("push")

  val list: Endpoint[Option[String], Option[Int], ApiError, Inbox, Any] =
    notifications.get
      .in(query[Option[Int]]("limit"))
      .out(jsonBody[Inbox])
      .name("listNotifications")
      .description("通知页：最新的 `limit` 条（默认 100），新的在前，附全部未读数。")

  val readThrough: Endpoint[Option[String], ReadThrough, ApiError, Unit, Any] =
    notifications.post
      .in("read")
      .in(jsonBody[ReadThrough])
      .out(statusCode(StatusCode.NoContent))
      .name("readNotificationsThrough")
      .description("通知页：标到某条为止已读。")

  val readPage: Endpoint[Option[String], ReadPage, ApiError, Unit, Any] =
    notifications.post
      .in("read-page")
      .in(jsonBody[ReadPage])
      .out(statusCode(StatusCode.NoContent))
      .name("readPageNotifications")
      .description("打开了某个页面：点开就是这个页面的通知已读（会话页即该会话的未读消失）。")

  val key: Endpoint[Option[String], Unit, ApiError, PushKey, Any] =
    push.get.in("key").out(jsonBody[PushKey]).name("pushKey").description("订阅推送用的服务器公钥。")

  val subscribe: Endpoint[Option[String], PushSubscriptionIn, ApiError, Unit, Any] =
    push.post
      .in("subscriptions")
      .in(jsonBody[PushSubscriptionIn])
      .out(statusCode(StatusCode.NoContent))
      .name("subscribePush")
      .description("登记（或更新）本设备的推送订阅。")

  val test: Endpoint[Option[String], Unit, ApiError, Unit, Any] =
    push.post.in("test").out(statusCode(StatusCode.NoContent)).name("testPush").description("发一条测试通知到全部设备。")

  val all = List(list, readThrough, readPage, key, subscribe, test)
