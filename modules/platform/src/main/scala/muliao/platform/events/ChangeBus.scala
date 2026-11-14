package muliao.platform.events

import muliao.contract.{Change, Topic}
import muliao.platform.db.TxHooks
import zio.*
import zio.stream.*

/** 「数据变了」广播：写库的一方在提交后发一条主题，订阅方（界面的 SSE 连接）据此重新拉取。
  *
  * 只是加速刷新，不保证送达：订阅方处理不过来时丢旧留新，界面重连后会整页重拉。
  */
trait ChangeBus:
  def publish(topic: Topic): UIO[Unit]

  /** 在写事务里登记：提交成功后才广播。 */
  final def publishAfterCommit(topic: Topic): URIO[TxHooks, Unit] = TxHooks.afterCommit(publish(topic))

  /** 订阅：返回时已订上（之后发布的都收得到），scope 结束退订。缓冲满了丢最旧的；同一批里重复的主题合并。 */
  def subscribe: URIO[Scope, ZStream[Any, Nothing, Change]]

object ChangeBus:
  val layer: ULayer[ChangeBus] = ZLayer.fromZIO(Hub.sliding[Change](1024).map(Live(_)))

  private final case class Live(hub: Hub[Change]) extends ChangeBus:
    def publish(topic: Topic): UIO[Unit] = hub.publish(Change(topic)).unit

    def subscribe: URIO[Scope, ZStream[Any, Nothing, Change]] =
      hub.subscribe.map(q => ZStream.fromQueue(q).chunks.flatMap(c => ZStream.fromIterable(c.distinct)))
