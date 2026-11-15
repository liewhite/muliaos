package muliao.notifications

import muliao.contract.*
import muliao.notifications.push.*
import muliao.notifications.store.{NotificationRow, Repo, SubscriptionRow}
import muliao.platform.{AppError, GeneralSettings, Retry}
import muliao.platform.Retry.retryTransient
import muliao.platform.db.{Database, Tx}
import muliao.platform.events.ChangeBus
import muliao.platform.http.OutboundHttp
import zio.*
import zio.json.*

/** 通知收件箱与推送：给用户发通知、通知页与未读、设备的推送订阅。
  *
  * 写下通知的事务提交后，广播主题 `notifications`（界面刷新通知页、未读数与会话列表）并推到每台设备：每条通知、每台设备独立投递，
  * 暂时失败只重试这一条，按平台的退避策略（约 30 分钟）；404/410 说明订阅失效，删掉它。重试只在内存里：服务恰好在重试期间重启，这条
  * 推送就不发了，通知本身和未读都还在。每次尝试前重读通知与订阅：通知随会话删了、设备失效被删了就不再推；角标取发出那一刻的未读数。
  */
trait Notifications:
  /** 在调用方的写事务里发一条通知（别的领域与自己的改动同一事务提交，如 issue 交给用户）；提交后才广播与推送。 */
  def notifyIn(n: Notice): Tx[NotificationId]

  /** 自己开事务发一条通知。 */
  def notify(n: Notice): IO[AppError, NotificationId]

  /** 通知页：最新的 `limit` 条（新的在前）与全部未读数。 */
  def inbox(limit: Int): IO[AppError, Inbox]

  /** 标到 `id` 这条为止已读（通知页；之后才到的保持未读）。 */
  def readThrough(id: NotificationId): IO[AppError, Unit]

  /** 用户打开了页面 `url`：点开就是它的通知已读。 */
  def readPage(url: String): IO[AppError, Unit]

  /** 这些会话里未读的：有指向它的未读通知。 */
  def unreadSessions(ids: List[SessionId]): IO[AppError, Set[SessionId]]

  /** 会话删了：指向它的通知一并删除。 */
  def forgetSession(id: SessionId): IO[AppError, Unit]

  /** 浏览器订阅推送用的服务器公钥。 */
  def publicKey: String

  /** 登记（或更新）一台设备的推送订阅。 */
  def subscribe(s: PushSubscriptionIn): IO[AppError, Unit]

  /** 测试通知（系统通知，点开看通知页）。 */
  def sendTest: IO[AppError, NotificationId]

object Notifications:
  /** 通知页不给条数时取多少条。 */
  val DefaultLimit = 100

  /** 推送服务限制载荷约 4KB（加密后），标题、正文截断留足余量（中文一个字三字节）。 */
  val TitleMax = 120
  val BodyMax  = 600

  /** 推到设备上的内容（Service Worker 的契约）：点开打开 `url`（也是通知的分组），`badge` 是发出那一刻的未读数（应用角标）。 */
  final case class Payload(title: String, body: String, url: String, badge: Long) derives JsonCodec

  def payload(n: NotificationRow, unread: Long): Payload =
    Payload(clip(n.title, TitleMax), clip(n.body, BodyMax), n.url.getOrElse("/notifications"), unread)

  def view(r: NotificationRow): NotificationView =
    NotificationView(NotificationId.wrap(r.id), r.title, r.body, r.url, r.sessionId, r.unread, r.createdAt)

  val layer: ZLayer[Database & ChangeBus & OutboundHttp & PushSettings & GeneralSettings, AppError, Notifications] =
    ZLayer.scoped(
      for
        settings <- ZIO.service[PushSettings]
        general  <- ZIO.service[GeneralSettings]
        keys     <- VapidKeys.loadOrCreate(settings.vapidKeyFile.getOrElse(general.dataDir.resolve("vapid_private.pem")))
        http     <- ZIO.service[OutboundHttp]
        db       <- ZIO.service[Database]
        bus      <- ZIO.service[ChangeBus]
        n        <- make(db, bus, PushGateway.live(http, keys, settings.vapidSubject.value), keys.publicKeyB64, Retry.transient)
      yield n
    )

  /** 投递在调用方的 scope 里进行（scope 结束即放弃进行中的重试）。 */
  def make(db: Database, bus: ChangeBus, gateway: PushGateway, publicKey: String, retry: Retry.Policy): URIO[Scope, Notifications] =
    ZIO.scope.map(scope => Live(db, bus, Pusher(db, gateway, retry, scope), publicKey))

  /** 一条通知、一台设备的一次尝试的结局（暂时失败不在这里：它以 [[AppError.Unavailable]] 交给退避重试）。`host`：推送服务主机名（日志用）。 */
  private enum Attempt:
    case Sent
    /** 通知或订阅已不在了 */
    case Dropped
    case Gone(host: String, status: Int)
    case Rejected(host: String, reason: String)

  private final case class Pusher(db: Database, gateway: PushGateway, retry: Retry.Policy, scope: Scope):
    def deliver(id: NotificationId): UIO[Unit] =
      db.read(Repo.endpoints)
        .flatMap(ZIO.foreachDiscard(_)(e => deliverTo(id, e).forkIn(scope)))
        .catchAll(e => ZIO.logError(s"推送通知 ${id.value} 时读不到设备：${e.message}"))

    /** 每次尝试前重读通知、订阅与未读数。 */
    private def attempt(id: NotificationId, endpoint: String): IO[AppError, Attempt] =
      db.read(Repo.get(id).zip(Repo.subscription(endpoint)).zip(Repo.unreadCount)).flatMap {
        case (Some(n), Some(s), unread) =>
          Device.parse(s.endpoint, s.p256dh, s.auth) match
            case Left(why) => ZIO.succeed(Attempt.Rejected("?", s"库里的订阅坏了：$why"))
            case Right(d) =>
              gateway
                .send(d, payload(n, unread).toJson.getBytes(java.nio.charset.StandardCharsets.UTF_8))
                .as(Attempt.Sent)
                .catchAll {
                  case PushFailure.Transient(r, after) => ZIO.fail(AppError.Unavailable(s"推到 ${d.host}：$r", None, after))
                  case PushFailure.Gone(code)          => ZIO.succeed(Attempt.Gone(d.host, code))
                  case PushFailure.Rejected(r)         => ZIO.succeed(Attempt.Rejected(d.host, r))
                }
        case _ => ZIO.succeed(Attempt.Dropped)
      }

    private def deliverTo(id: NotificationId, endpoint: String): UIO[Unit] =
      attempt(id, endpoint)
        .retryTransient(retry, s"推送通知 ${id.value}")
        .flatMap {
          case Attempt.Sent | Attempt.Dropped => ZIO.unit
          case Attempt.Gone(host, code) =>
            ZIO.logInfo(s"$host 的推送订阅已失效（HTTP $code），删除") *> db.write(Repo.unsubscribe(endpoint)).unit
          case Attempt.Rejected(host, r) => ZIO.logError(s"推送通知 ${id.value} 到 $host 被拒：$r")
        }
        .catchAll(e => ZIO.logError(s"推送通知 ${id.value} 最终失败：${e.message}"))

  private final case class Live(db: Database, bus: ChangeBus, pusher: Pusher, publicKey: String) extends Notifications:

    def notifyIn(n: Notice): Tx[NotificationId] =
      for
        now <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        id  <- Repo.insert(n.title, n.body, n.target.session, n.target.url, now)
        _   <- bus.publishAfterCommit(Topic.Notifications)
        _   <- muliao.platform.db.TxHooks.afterCommit(pusher.deliver(id))
      yield id

    def notify(n: Notice): IO[AppError, NotificationId] = db.write(notifyIn(n))

    def inbox(limit: Int): IO[AppError, Inbox] =
      db.read(Repo.latest(limit.max(1)).zip(Repo.unreadCount)).map((rows, unread) => Inbox(rows.map(view), unread))

    private def mark(tx: Tx[Long]): IO[AppError, Unit] =
      db.write(tx.tap(n => ZIO.when(n > 0)(bus.publishAfterCommit(Topic.Notifications)))).unit

    def readThrough(id: NotificationId): IO[AppError, Unit] = mark(Repo.readThrough(id))
    def readPage(url: String): IO[AppError, Unit]           = mark(Repo.readUrl(url))

    def unreadSessions(ids: List[SessionId]): IO[AppError, Set[SessionId]] =
      if ids.isEmpty then ZIO.succeed(Set.empty) else db.read(Repo.unreadSessions(ids)).map(_.toSet)

    def forgetSession(id: SessionId): IO[AppError, Unit] = mark(Repo.deleteOfSession(id))

    def subscribe(s: PushSubscriptionIn): IO[AppError, Unit] =
      for
        d   <- ZIO.fromEither(Device.parse(s.endpoint, s.keys.p256dh, s.keys.auth)).mapError(e => AppError.invalid(s"推送订阅不对：$e"))
        now <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        _   <- db.write(Repo.subscribe(SubscriptionRow(s.endpoint, s.keys.p256dh, s.keys.auth, now)))
        _   <- ZIO.logInfo(s"登记了 ${d.host} 的推送订阅")
      yield ()

    def sendTest: IO[AppError, NotificationId] = notify(Notice("Muliao", "推送通知已开启。", Target.Inbox))
