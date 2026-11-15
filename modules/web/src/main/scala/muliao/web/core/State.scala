package muliao.web.core

import com.raquo.airstream.core.{EventStream, Signal}
import com.raquo.airstream.eventbus.EventBus
import com.raquo.airstream.state.Var
import muliao.contract.{Api, ApiError, AuthApi, Change, ErrorKind, Topic, refreshTopics}
import sttp.tapir.Endpoint
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
  val frames: EventStream[Frame[Change]] = Live.frames(Api.changes)(())

  /** 连上了（含重连）：断线期间可能错过变化，正在显示的数据都要重新拉取。 */
  val ready: EventStream[Unit] = frames.collect { case Frame.Ready => () }

  /** 变了的主题。 */
  val changes: EventStream[Topic] = frames.collect { case Frame.Data(c) => c.topic }

  /** 用户刚做完一个操作：正在显示的数据都重新拉一次（不等 SSE 带来变化）。 */
  val refreshNow: EventBus[Unit] = new EventBus[Unit]

/** 一份从服务端拉来的数据的状态。 */
enum Load[+A]:
  case Pending
  /** 第一次就没拉到 */
  case Failed(message: String)
  /** 拿到了数据；`stale`：之后重新拉取失败的原因（照常显示这份内容，顶上提示） */
  case Loaded(value: A, stale: Option[String] = None)

  def data: Option[A] = this match
    case Loaded(d, _) => Some(d)
    case _            => None

  def map[B](f: A => B): Load[B] = this match
    case Pending      => Pending
    case Failed(m)    => Failed(m)
    case Loaded(d, s) => Loaded(f(d), s)

object Remote:
  import Http.ec

  type Read[I, O] = Endpoint[Option[String], I, ApiError, O, Any]

  /** 从接口拉来、自动重拉的数据：挂上页面时拉，连上（含重连）、接口声明的主题（`refreshedBy`）变了、用户刚做完操作时再拉。
    * 只在被页面用着时活动（Airstream 的 Signal 无人订阅即停）；数据库是唯一事实来源，这里只是显示。
    */
  def apply[I, O](e: Read[I, O])(in: I): Signal[Load[O]] = following(e)(Http.call(e)(in))

  /** 同一个接口、请求自己组织（如连续翻页）：随 `e` 声明的主题重拉，`more` 发出时也重拉（如「显示更多」、手动刷新）。 */
  def following[A](e: Read[?, ?], more: EventStream[Unit] = EventStream.empty)(fetch: => Future[A]): Signal[Load[A]] =
    val topics = e.refreshTopics
    triggered(EventStream.merge(Events.changes.filter(topics.contains).mapToUnit, Events.ready, Events.refreshNow.events, more))(fetch)

  /** 只在挂上与 `more` 发出时拉（会话页按自己的实时事件重拉用它）。 */
  def triggered[A](more: EventStream[Unit])(fetch: => Future[A]): Signal[Load[A]] =
    EventStream
      .merge(EventStream.unit(emitOnce = true), more.throttle(300))
      .flatMapSwitch(_ => EventStream.fromFuture(fetch.transform(Success(_))))
      .scanLeft(Load.Pending: Load[A]) {
        case (_, Success(a))                      => Load.Loaded(a)
        case (Load.Loaded(old, _), TryFailure(t)) => Load.Loaded(old, Some(Http.messageOf(t)))
        case (_, TryFailure(t))                   => Load.Failed(Http.messageOf(t))
      }
