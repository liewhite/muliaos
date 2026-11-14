package muliao.web.pwa

import com.raquo.laminar.api.L.*
import muliao.contract.{NotificationsApi, PushKeys, PushSubscriptionIn}
import muliao.web.core.*
import org.scalajs.dom

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

import Http.ec

/** 本设备的推送状态：有通知权限不等于在接收推送（退出登录时会取消订阅），所以「已授权」再分成有没有订阅。 */
enum PushState:
  case Unsupported
  /** iOS 只有「添加到主屏幕」后以独立模式打开时才有推送 */
  case NeedsInstall
  case Default
  case Denied
  case Unsubscribed
  case Subscribed

object Push:
  private def nav = dom.window.navigator.asInstanceOf[js.Dynamic]
  private def hasSw: Boolean = !js.isUndefined(nav.serviceWorker)

  private def isIos: Boolean = "iPad|iPhone|iPod".r.findFirstIn(dom.window.navigator.userAgent).isDefined
  private def isStandalone: Boolean =
    dom.window.matchMedia("(display-mode: standalone)").matches || nav.standalone.asInstanceOf[js.UndefOr[Boolean]].contains(true)

  private enum Permission:
    case Unsupported, NeedsInstall, Default, Denied, Granted

  private def permission: Permission =
    if !hasSw then Permission.Unsupported
    else if js.isUndefined(dom.window.asInstanceOf[js.Dynamic].PushManager) then
      if isIos && !isStandalone then Permission.NeedsInstall else Permission.Unsupported
    else
      js.Dynamic.global.Notification.permission.asInstanceOf[String] match
        case "granted" => Permission.Granted
        case "denied"  => Permission.Denied
        case _         => Permission.Default

  /** 本设备当前的订阅。不用 serviceWorker.ready：注册失败时它永远不返回。 */
  private def currentSubscription(): Future[Option[js.Dynamic]] =
    nav.serviceWorker.getRegistration().asInstanceOf[js.Promise[js.UndefOr[js.Dynamic]]].toFuture.flatMap { reg =>
      reg.toOption match
        case None    => Future.successful(None)
        case Some(r) => r.pushManager.getSubscription().asInstanceOf[js.Promise[js.Dynamic]].toFuture.map(s => Option(s).filterNot(js.isUndefined))
    }

  def state(): Future[PushState] = permission match
    case Permission.Unsupported  => Future.successful(PushState.Unsupported)
    case Permission.NeedsInstall => Future.successful(PushState.NeedsInstall)
    case Permission.Default      => Future.successful(PushState.Default)
    case Permission.Denied       => Future.successful(PushState.Denied)
    case Permission.Granted      => currentSubscription().map(s => if s.isDefined then PushState.Subscribed else PushState.Unsubscribed)

  def registerServiceWorker(): Unit =
    if hasSw then
      nav.serviceWorker.register("/sw.js").asInstanceOf[js.Promise[js.Any]].toFuture.failed.foreach(e => dom.console.error("service worker registration failed", e.getMessage))

  private def base64UrlToBytes(value: String): Uint8Array =
    val padded = (value + "=" * ((4 - value.length % 4) % 4)).replace('-', '+').replace('_', '/')
    val raw    = dom.window.atob(padded)
    val bytes  = new Uint8Array(raw.length)
    raw.indices.foreach(i => bytes(i) = raw.charAt(i).toShort)
    bytes

  private def body(sub: js.Dynamic): PushSubscriptionIn =
    val keys = sub.toJSON().keys
    if js.isUndefined(keys) || js.isUndefined(keys.p256dh) || js.isUndefined(keys.auth) then throw new IllegalStateException("推送订阅缺加密密钥")
    PushSubscriptionIn(sub.endpoint.asInstanceOf[String], PushKeys(keys.p256dh.asInstanceOf[String], keys.auth.asInstanceOf[String]))

  /** 开启本设备推送。必须在用户点击里调用（iOS 要求）。 */
  def enable(): Future[Unit] =
    for
      perm <- js.Dynamic.global.Notification.requestPermission().asInstanceOf[js.Promise[String]].toFuture
      _    <- if perm == "granted" then Future.unit else Future.failed(new IllegalStateException(s"通知权限未授予（$perm）"))
      reg  <- nav.serviceWorker.getRegistration().asInstanceOf[js.Promise[js.UndefOr[js.Dynamic]]].toFuture
      r    <- reg.toOption.fold(Future.failed[js.Dynamic](new IllegalStateException("Service Worker 没有注册成功，刷新页面后再试")))(Future.successful)
      key  <- Http.call(NotificationsApi.key)(())
      existing <- r.pushManager.getSubscription().asInstanceOf[js.Promise[js.Dynamic]].toFuture
      sub <-
        if existing != null && !js.isUndefined(existing) then Future.successful(existing)
        else
          r.pushManager
            .subscribe(js.Dynamic.literal(userVisibleOnly = true, applicationServerKey = base64UrlToBytes(key.publicKey)))
            .asInstanceOf[js.Promise[js.Dynamic]]
            .toFuture
      _ <- Http.call(NotificationsApi.subscribe)(body(sub))
    yield ()

  /** 取消本设备的推送订阅（退出登录时）。 */
  def unsubscribe(): Future[Unit] = permission match
    case Permission.Granted => currentSubscription().flatMap(_.fold(Future.unit)(s => s.unsubscribe().asInstanceOf[js.Promise[js.Any]].toFuture.map(_ => ())))
    case _                  => Future.unit

  /** 已授权时把当前订阅同步到服务端（订阅可能被浏览器轮换）。 */
  def sync(): Future[Unit] = permission match
    case Permission.Granted => currentSubscription().flatMap(_.fold(Future.unit)(s => Http.call(NotificationsApi.subscribe)(body(s))))
    case _                  => Future.unit

/** 点通知后的跳转。Service Worker（sw.js 的 open()）把要打开的地址写进 Cache Storage，在 BroadcastChannel 上叫开着的页面来取，再聚焦窗口
  * 或打开新窗口。页面在加载、回到前台、获得焦点、收到广播时都去取，取走即删：iOS 主屏应用开着时 SW 可能列不出它的窗口，挂起或冷启动时消息
  * 可能送不到，只靠消息会表现为点通知只打开应用、不跳过去。取到的地址在登录后的外壳挂上前先存着，挂上时补上（只留最近一次）。
  */
object SwNavigate:
  /** 与 sw.js 的 PENDING_CACHE 一致 */
  private val PendingCache = "muliao-pending-navigation"
  /** 点通知之后这么久还没取走的不再跳 */
  val PendingMaxAgeMs = 2 * 60000

  private var pending: Option[String]           = None
  private var handler: Option[String => Unit]    = None

  /** Service Worker 发来的消息的 type（没有就是空串）。 */
  def msgType(e: dom.MessageEvent): String =
    val d = e.data.asInstanceOf[js.Dynamic]
    if d == null || js.isUndefined(d) then "" else d.selectDynamic("type").asInstanceOf[js.UndefOr[String]].getOrElse("")

  private def go(h: String => Unit, url: String): Unit =
    if s"${dom.window.location.pathname}${dom.window.location.search}" != url then h(url)

  private def deliver(url: String): Unit = handler match
    case Some(h) => go(h, url)
    case None    => pending = Some(url)

  /** 取走 SW 留下的待跳转地址：全部删掉，只认最新的一条；几处同时来取时只有删掉了自己看到的最新一条的那次算数。 */
  private def takePending(): Future[Option[String]] =
    val caches = dom.window.asInstanceOf[js.Dynamic].caches
    for
      cache <- caches.open(PendingCache).asInstanceOf[js.Promise[js.Dynamic]].toFuture
      keys  <- cache.keys().asInstanceOf[js.Promise[js.Array[js.Dynamic]]].toFuture
      seen <- Future.sequence(keys.toList.map { k =>
                cache.`match`(k).asInstanceOf[js.Promise[js.UndefOr[js.Dynamic]]].toFuture.flatMap { r =>
                  r.toOption match
                    case None => Future.successful(None)
                    case Some(resp) =>
                      resp.json().asInstanceOf[js.Promise[js.Dynamic]].toFuture.map(j => Some((k, j.url.asInstanceOf[String], j.at.asInstanceOf[Double])))
                }
              })
      entries = seen.flatten
      result <-
        if entries.isEmpty then Future.successful(None)
        else
          val newest = entries.maxBy(_._3)
          Future.sequence(entries.map(e => cache.delete(e._1).asInstanceOf[js.Promise[Boolean]].toFuture)).map { deleted =>
            val mine = deleted(entries.indexOf(newest))
            Option.when(mine && js.Date.now() - newest._3 <= PendingMaxAgeMs)(newest._2)
          }
    yield result

  /** 只有用户看得见的页面去取：桌面多标签时后台标签也收得到广播，不能让它抢走。 */
  private def check(): Unit =
    if dom.document.visibilityState == dom.VisibilityState.visible then
      takePending().onComplete {
        case scala.util.Success(Some(url)) => deliver(url)
        case scala.util.Success(None)      => ()
        case scala.util.Failure(e)         => dom.console.error("reading the pending navigation failed", e.getMessage)
      }

  def listen(): Unit =
    // 没有 SW（非安全上下文）就没有推送，也没有 Cache Storage
    if !js.isUndefined(dom.window.navigator.asInstanceOf[js.Dynamic].serviceWorker) then
      val ch = js.Dynamic.newInstance(js.Dynamic.global.BroadcastChannel)(PendingCache)
      ch.addEventListener("message", ((e: dom.MessageEvent) => if msgType(e) == "navigate" then check()): js.Function1[dom.MessageEvent, Unit])
      dom.document.addEventListener("visibilitychange", (_: dom.Event) => check())
      dom.window.addEventListener("focus", (_: dom.Event) => check())
      check()

  /** 登录后的外壳挂上时调用：接收跳转请求，并补上挂上前到达的那一次。 */
  def handle(h: String => Unit): () => Unit =
    handler = Some(h)
    pending.foreach { url =>
      pending = None
      go(h, url)
    }
    () => if handler.contains(h) then handler = None

/** 应用图标角标：跟着服务端的未读数。 */
object Badge:
  def set(count: Long): Unit =
    val nav = dom.window.navigator.asInstanceOf[js.Dynamic]
    if !js.isUndefined(nav.setAppBadge) then
      val p = if count > 0 then nav.setAppBadge(count.toDouble) else nav.clearAppBadge()
      p.asInstanceOf[js.Promise[Unit]].toFuture.failed.foreach(e => dom.console.error("app badge update failed", e.getMessage))

  /** 推送到达后 SW 发来 {type: 'pushed'}：按服务端的数纠正角标。 */
  val pushed: EventStream[Unit] =
    val sw = dom.window.navigator.asInstanceOf[js.Dynamic].serviceWorker
    if js.isUndefined(sw) then EventStream.empty
    else
      EventStream.fromCustomSource[Unit](
        shouldStart = _ => true,
        start = (fire, _, _, _) =>
          sw.onmessage = ((e: dom.MessageEvent) => if SwNavigate.msgType(e) == "pushed" then fire(())): js.Function1[dom.MessageEvent, Unit],
        stop = _ => sw.onmessage = null
      )
