package muliao.notifications.store

import io.getquill.*
import muliao.contract.{NotificationId, SessionId}
import muliao.platform.db.*
import muliao.platform.db.Sql.*

final case class NotificationRow(
    id: Long,
    title: String,
    body: String,
    sessionId: Option[SessionId],
    url: Option[String],
    unread: Boolean,
    createdAt: Long
)

final case class SubscriptionRow(endpoint: String, p256dh: String, auth: String, createdAt: Long)

/** 本领域的表的读写。只做存取，不含规则。 */
private[notifications] object Repo:
  private inline def notifications = quote(querySchema[NotificationRow]("notification"))
  private inline def subscriptions = quote(querySchema[SubscriptionRow]("push_subscription"))

  def insert(title: String, body: String, session: Option[SessionId], url: Option[String], at: Long): Tx[NotificationId] =
    Sql
      .run(
        notifications
          .insertValue(lift(NotificationRow(0L, title, body, session, url, true, at)))
          .returningGenerated(_.id)
      )
      .map(NotificationId.wrap)
      .orInternal

  def get(id: NotificationId): Read[Option[NotificationRow]] =
    Sql.run(notifications.filter(_.id == lift(id.value))).map(_.headOption).orInternal

  def latest(limit: Int): Read[List[NotificationRow]] =
    Sql.run(notifications.sortBy(_.id)(using Ord.desc).take(lift(limit))).orInternal

  def unreadCount: Read[Long] =
    Sql.run(notifications.filter(_.unread).size).orInternal

  def unreadSessions(ids: List[SessionId]): Read[List[SessionId]] =
    Sql.run(notifications.filter(n => n.unread && n.sessionId.exists(s => liftQuery(ids).contains(s))).map(_.sessionId).distinct)
      .map(_.flatten)
      .orInternal

  def readThrough(id: NotificationId): Tx[Long] =
    Sql.run(notifications.filter(n => n.unread && n.id <= lift(id.value)).update(_.unread -> false)).orInternal

  def readUrl(url: String): Tx[Long] =
    Sql.run(notifications.filter(n => n.unread && n.url.exists(_ == lift(url))).update(_.unread -> false)).orInternal

  def deleteOfSession(id: SessionId): Tx[Long] =
    Sql.run(notifications.filter(_.sessionId.exists(_ == lift(id))).delete).orInternal

  // ---- 推送订阅 ----

  def subscribe(row: SubscriptionRow): Tx[Unit] =
    (Sql.run(subscriptions.filter(_.endpoint == lift(row.endpoint)).delete) *> Sql.run(subscriptions.insertValue(lift(row)))).unit.orInternal

  def subscription(endpoint: String): Read[Option[SubscriptionRow]] =
    Sql.run(subscriptions.filter(_.endpoint == lift(endpoint))).map(_.headOption).orInternal

  def endpoints: Read[List[String]] =
    Sql.run(subscriptions.map(_.endpoint)).orInternal

  def unsubscribe(endpoint: String): Tx[Long] =
    Sql.run(subscriptions.filter(_.endpoint == lift(endpoint)).delete).orInternal
