package muliao.platform.server

import muliao.contract.Beat
import muliao.platform.Build
import sttp.model.sse.ServerSentEvent
import sttp.tapir.ztapir.ZioServerSentEvents
import zio.*
import zio.json.*
import zio.stream.*

/** 实时连接（SSE）的统一帧格式，前端据此判断「连上了」与「连接还活着」：
  *   - `ready`：订阅建好之后的第一条（data 为 [[Beat]]：心跳间隔与程序版本）。前端收到它才重新拉取数据：
  *     早于订阅的变化不会漏（响应头先于订阅到达，按连接建立就拉取可能落在订阅之前）。
  *   - `ping`：每 N 秒一条（data 同上）。手机切后台、代理断链时浏览器常常察觉不到连接已死，前端超过两个周期没收到任何帧就重连。
  *   - 业务事件由调用方给出事件名（`change`、`live`）。
  */
object Sse:
  val PingSeconds = 25

  private val beat: String = Beat(PingSeconds, Build.version).toJson

  /** @param subscribed 在 scope 里建好订阅，得到业务事件流 */
  def frames(subscribed: URIO[Scope, ZStream[Any, Throwable, ServerSentEvent]]): ZStream[Any, Throwable, Byte] =
    val events = ZStream.unwrapScoped(subscribed.map(s => ZStream.succeed(ServerSentEvent(data = Some(beat), eventType = Some("ready"))) ++ s))
    val pings  = ZStream.tick(PingSeconds.seconds).drop(1).as(ServerSentEvent(data = Some(beat), eventType = Some("ping")))
    ZioServerSentEvents.serialiseSSEToBytes(events.merge(pings, ZStream.HaltStrategy.Left))
