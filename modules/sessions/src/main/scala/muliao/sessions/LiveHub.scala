package muliao.sessions

import muliao.contract.SessionId
import muliao.models.llm.{Growth, ReplyPart}
import zio.*
import zio.stream.*

/** 会话页的实时事件。只是加速：丢了不影响正确性，页面重新拉取即可。 */
enum LiveEvent:
  /** 开始生成新的一条回复（清空正在生成的） */
  case Begin
  /** 第 `index` 段开始，或整段换成新的 */
  case Part(index: Int, part: ReplyPart)
  /** 第 `index` 段长了一截 */
  case Grew(index: Int, growth: Growth, text: String)
  /** 库里的时间线或状态变了：重新拉取 */
  case Changed

/** 各会话正在生成的回复与实时事件。 */
final class LiveHub private (hub: Hub[(SessionId, LiveEvent)], current: Ref[Map[SessionId, Chunk[ReplyPart]]]):

  def publish(session: SessionId, e: LiveEvent): UIO[Unit] =
    current.update(_.updated(session, Chunk.empty)).when(e == LiveEvent.Begin) *> hub.publish(session -> e).unit

  /** 记下正在生成的回复到目前为止的各段（供中途打开页面的订阅者先拿到）。 */
  def track(session: SessionId, parts: Chunk[ReplyPart]): UIO[Unit] = current.update(_.updated(session, parts))

  /** 生成结束（存进库了，或放弃了）：不再有正在生成的。 */
  def done(session: SessionId): UIO[Unit] = current.update(_ - session) *> publish(session, LiveEvent.Changed)

  /** 订阅一个会话：先给正在生成的部分（没有为 None），再给之后的事件。 */
  def subscribe(session: SessionId): URIO[Scope, (Option[Chunk[ReplyPart]], ZStream[Any, Nothing, LiveEvent])] =
    for
      q    <- hub.subscribe
      snap <- current.get.map(_.get(session))
    yield (snap, ZStream.fromQueue(q).collect { case (`session`, e) => e })

object LiveHub:
  val layer: ULayer[LiveHub] = ZLayer(
    for
      h <- Hub.sliding[(SessionId, LiveEvent)](4096)
      c <- Ref.make(Map.empty[SessionId, Chunk[ReplyPart]])
    yield LiveHub(h, c)
  )
