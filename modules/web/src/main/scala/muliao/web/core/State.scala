package muliao.web.core

import com.raquo.airstream.core.{EventStream, Signal}
import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.state.Var
import muliao.contract.{AuthApi, Change, ErrorKind}
import zio.json.*

import scala.concurrent.Future
import scala.util.{Failure as TryFailure, Success}

/** 登录状态：启动时问一次服务端，之后任何接口回 401 就回到「没登录」。 */
enum AuthState:
  case Checking
  /** 连不上服务端（不是没登录）：整页报错 */
  case Unreachable(message: String)
  case SignedIn
  case SignedOut

object Auth:
  import Http.ec

  val state: Var[AuthState] = Var(AuthState.Checking)

  def check(): Unit =
    Http.call(AuthApi.session)(()).onComplete {
      case Success(_) => state.set(AuthState.SignedIn)
      case TryFailure(t) =>
        Http.failureOf(t) match
          case Failure.Api(e) if e.kind == ErrorKind.Unauthorized => state.set(AuthState.SignedOut)
          case f                                                  => state.set(AuthState.Unreachable(f.message))
    }

  /** 实时连接被服务端关了：可能是登录过期，用一次普通请求走统一的 401 处理。 */
  def recheck(): Unit = Http.call(AuthApi.session)(()).failed.foreach(_ => ())

  def login(password: String): Future[Unit] =
    Http.postStatus("/api/auth/login", Some(muliao.contract.LoginRequest(password).toJson)).map(_ => state.set(AuthState.SignedIn))

  def logout(): Future[Unit] = Http.postStatus("/api/auth/logout", None).map(_ => state.set(AuthState.SignedOut))

  Http.unauthorized.events.foreach(_ => state.set(AuthState.SignedOut))(using com.raquo.airstream.ownership.ManualOwner())

/** 全站的「数据变了」：一条全局 SSE 连接，登录后的外壳一直订阅着。 */
object Events:
  val frames: EventStream[Frame] = Live.frames("/api/events", "change")

  /** 连上了（含重连）：断线期间可能错过变化，正在显示的数据都要重新拉取。 */
  val ready: EventStream[Unit] = frames.collect { case Frame.Ready => () }

  /** 变了的主题（agents、assets、sessions、issues、triggers、notifications、history）。 */
  val changes: EventStream[String] = frames.collect { case Frame.Data(json) => json.fromJson[Change].toOption.map(_.topic.value) }.collect {
    case Some(t) => t
  }

  /** 用户刚做完一个操作：正在显示的数据都重新拉一次（不等 SSE 带来变化）。 */
  val refreshNow: EventBus[Unit] = new EventBus[Unit]

/** 一份从服务端拉来的数据的状态。 */
enum Load[+A]:
  case Pending
  case Ready(value: A)
  /** 第一次就没拉到 */
  case Failed(message: String)
  /** 重新拉取失败：照常显示之前的内容，顶上提示 */
  case Stale(value: A, error: String)

  def data: Option[A] = this match
    case Ready(d)    => Some(d)
    case Stale(d, _) => Some(d)
    case _           => None

object Remote:
  import Http.ec

  /** 一份随主题变化自动重新拉取的数据：挂上页面时拉，连上（含重连）、列出的主题变了、用户刚做完操作时再拉。
    * 只在被页面用着时活动（Airstream 的 Signal 无人订阅即停）；数据库是唯一事实来源，这里只是显示。
    */
  def apply[A](topics: String*)(fetch: => Future[A]): Signal[Load[A]] =
    triggered(EventStream.merge(Events.changes.filter(topics.contains).mapToUnit, Events.ready, Events.refreshNow.events))(fetch)

  /** 只在页面挂上与用户操作后拉取（不随主题刷新）。 */
  def once[A](fetch: => Future[A]): Signal[Load[A]] = triggered(Events.refreshNow.events)(fetch)

  def triggered[A](more: EventStream[Unit])(fetch: => Future[A]): Signal[Load[A]] =
    EventStream
      .merge(EventStream.unit(emitOnce = true), more.throttle(300))
      .flatMapSwitch(_ => EventStream.fromFuture(fetch.transform(Success(_))))
      .scanLeft(Load.Pending: Load[A]) {
        case (_, Success(a))                  => Load.Ready(a)
        case (Load.Ready(old), TryFailure(t)) => Load.Stale(old, Http.messageOf(t))
        case (Load.Stale(old, _), TryFailure(t)) => Load.Stale(old, Http.messageOf(t))
        case (_, TryFailure(t))               => Load.Failed(Http.messageOf(t))
      }
