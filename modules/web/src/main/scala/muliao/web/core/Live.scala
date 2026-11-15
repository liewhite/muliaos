package muliao.web.core

import com.raquo.airstream.core.EventStream
import muliao.contract.Feed
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers.{SetIntervalHandle, SetTimeoutHandle, clearInterval, clearTimeout, setInterval, setTimeout}

/** 一条实时连接（SSE）给出的东西：`Ready` 是订阅建好了（第一次连上与每次重连都有，页面据此重新拉取），`Data` 是一条业务事件
  * （看不懂的丢掉：服务端比页面新时，重连后整页会重拉）。
  */
enum Frame[+E]:
  case Ready
  case Data(event: E)

/** SSE 连接：有人订阅时连上，没人订阅时断开（Airstream 的流本身就是这样起停的）。
  *
  *   - 服务端在订阅建好后发 `ready`，之后每 N 秒发 `ping`（data 带 N）。超过两个周期再加 5 秒没收到任何帧，就当连接已死、丢掉重连：
  *     手机切后台、代理断链时浏览器常常察觉不到。非 200 回应会让 EventSource 永久关闭，也由看门狗重建。
  *   - 页面隐藏超过一分钟断开，回到前台重连（重连后的 ready 让页面拉一次，隐藏期间错过的变化都在里面）。
  */
object Live:
  private val WatchdogMs     = 5000
  private val HiddenCloseMs  = 60000

  /** 连上 contract 里声明的一条实时连接。 */
  def frames[I, E](feed: Feed[I, E])(in: I): EventStream[Frame[E]] =
    val url = Http.href(feed.endpoint)(in)
    var conn: Option[Connection] = None
    EventStream.fromCustomSource[Frame[String]](
      shouldStart = _ => true,
      start = (fire, _, _, _) =>
        val c = Connection(url, feed.event, fire)
        conn = Some(c)
        c.open(),
      stop = _ =>
        conn.foreach(_.close())
        conn = None
    ).collectOpt {
      case Frame.Ready   => Some(Frame.Ready)
      case Frame.Data(j) => feed.codec.decoder.decodeJson(j).toOption.map(Frame.Data(_))
    }

  private final class Connection(url: String, eventName: String, fire: Frame[String] => Unit):
    private var source: Option[dom.EventSource] = None
    private var lastFrameAt                     = 0.0
    private var intervalSeconds: Option[Int]    = None
    private var watchdog: Option[SetIntervalHandle] = None
    private var hiddenTimer: Option[SetTimeoutHandle] = None

    private val onVisibility: js.Function1[dom.Event, Unit] = _ =>
      hiddenTimer.foreach(clearTimeout)
      hiddenTimer = None
      if dom.document.visibilityState == dom.VisibilityState.visible then
        if source.isEmpty then connect()
      else hiddenTimer = Some(setTimeout(HiddenCloseMs.toDouble)(disconnect()))

    def open(): Unit =
      dom.document.addEventListener("visibilitychange", onVisibility)
      watchdog = Some(setInterval(WatchdogMs.toDouble)(check()))
      if dom.document.visibilityState == dom.VisibilityState.visible then connect()

    def close(): Unit =
      dom.document.removeEventListener("visibilitychange", onVisibility)
      watchdog.foreach(clearInterval)
      hiddenTimer.foreach(clearTimeout)
      watchdog = None
      hiddenTimer = None
      disconnect()

    private def connect(): Unit =
      val s = new dom.EventSource(url)
      lastFrameAt = js.Date.now()
      val beat: js.Function1[dom.MessageEvent, Unit] = e =>
        lastFrameAt = js.Date.now()
        intervalSeconds = js.JSON.parse(e.data.asInstanceOf[String]).intervalSeconds.asInstanceOf[js.UndefOr[Int]].toOption
      s.addEventListener("ready", (e: dom.MessageEvent) => { beat(e); fire(Frame.Ready) })
      s.addEventListener("ping", beat)
      s.addEventListener(eventName, (e: dom.MessageEvent) => { lastFrameAt = js.Date.now(); fire(Frame.Data(e.data.asInstanceOf[String])) })
      // 关闭的原因可能是登录过期（401）：用一次普通请求走统一的 401 处理
      s.onerror = _ => if s.readyState == dom.EventSource.CLOSED then Auth.recheck()
      source = Some(s)

    private def disconnect(): Unit =
      source.foreach(_.close())
      source = None

    /** 连接已死就重建；页面在前台却没有连接（漏了回到前台的事件）也连上。 */
    private def check(): Unit = source match
      case None =>
        if dom.document.visibilityState == dom.VisibilityState.visible then connect()
      case Some(s) =>
        val closed = s.readyState == dom.EventSource.CLOSED
        val stale  = intervalSeconds.exists(n => js.Date.now() - lastFrameAt > (2 * n + 5) * 1000.0)
        if closed || stale then
          disconnect()
          connect()
