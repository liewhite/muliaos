package muliao.resources.vault

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import zio.*

/** 「一段时间」的授权按「哪个 agent 用哪个 secret」记：A 拿到的授权 B 用不了。 */
final case class Slot(agent: AgentId, key: String)

/** 有调用开始等某个 passkey secret 的授权（发通知用）。 */
final case class Asked(slot: Slot, purpose: AskPurpose)

/** passkey secret 的授权：服务端解不开这类 secret，要用时等用户用 passkey 解开交回明文。
  * 每次调用各自一个请求（带会话与要跑的命令，用户看着它批）：「一次」与「拒绝」只结掉这一个；「一段时间」把这个 agent 这个 secret 记成已授权，
  * 同时结掉它全部在等的请求，到期前再用不再问（到期的在下次读写状态时删掉）。调用结束（含被中断）时它的请求随之撤下。全在内存里，重启清空。
  */
trait Grants:
  /** 取明文：有生效的授权直接给；否则登记一个请求，等到用户答复。 */
  def obtain(slot: Slot, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String]

  /** 等授权的请求与生效中的授权（不含明文）。 */
  def view: UIO[(List[SecretAsk], List[SecretGrant])]

  /** 答复一个请求；它已经没了（调用已结束或已答复）报 NotFound。 */
  def answer(id: Long, decision: AskDecision): IO[AppError, Unit]

  /** 收回一段时间的授权。 */
  def revoke(slot: Slot): UIO[Unit]

  /** 订阅新出现的请求（发通知用）：返回时已订上，scope 结束退订。 */
  def asked: URIO[Scope, Dequeue[Asked]]

object Grants:
  val layer: URLayer[ChangeBus, Grants] =
    ZLayer {
      for
        bus   <- ZIO.service[ChangeBus]
        state <- Ref.make(State(Map.empty, Map.empty, 0L))
        hub   <- Hub.sliding[Asked](256)
      yield Live(bus, state, hub)
    }

  /** 明文：toString 不带值，免得被意外打进日志。 */
  final class Plain(val value: String):
    override def toString: String = "Plain(***)"

  /** 一个在等的请求。 */
  private final case class Waiting(slot: Slot, reply: Promise[AppError, Plain], purpose: AskPurpose, box: PasskeyBox, since: Long)

  /** 一段时间的授权，到 `until`（epoch 毫秒）为止。 */
  private final case class Granted(plain: Plain, until: Long)

  private final case class State(asks: Map[Long, Waiting], grants: Map[Slot, Granted], lastId: Long):
    /** 去掉到期的授权：每次读写状态时顺手做，明文不留到下次之后（不另起定时器）。 */
    def at(t: Long): State = copy(grants = grants.filter((_, g) => g.until > t))

  private final case class Live(bus: ChangeBus, state: Ref[State], hub: Hub[Asked]) extends Grants:
    private val now = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    def obtain(slot: Slot, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String] =
      ZIO
        .acquireReleaseWith(register(slot, purpose, box))(withdraw) {
          case Left(plain) => ZIO.succeed(plain)
          case Right((_, reply)) =>
            bus.publish(Topic.Vault) *> hub.publish(Asked(slot, purpose)) *> reply.await
        }
        .map(_.value)

    /** 有生效的授权就拿到明文；否则登记一个请求。 */
    private def register(slot: Slot, purpose: AskPurpose, box: PasskeyBox): UIO[Either[Plain, (Long, Promise[AppError, Plain])]] =
      for
        t     <- now
        reply <- Promise.make[AppError, Plain]
        got <- state.modify { s0 =>
                 val s = s0.at(t)
                 s.grants.get(slot) match
                   case Some(g) => (Left(g.plain), s)
                   case None =>
                     val id = s.lastId + 1
                     (Right(id -> reply), s.copy(asks = s.asks.updated(id, Waiting(slot, reply, purpose, box, t)), lastId = id))
               }
      yield got

    /** 调用结束（拿到、被拒绝或被中断）：撤下它的请求。 */
    private def withdraw(got: Either[Plain, (Long, Promise[AppError, Plain])]): UIO[Unit] =
      ZIO.foreachDiscard(got.toOption) { (id, _) =>
        state.modify(s => (s.asks.contains(id), s.copy(asks = s.asks - id))).flatMap(bus.publish(Topic.Vault).when(_))
      }

    def view: UIO[(List[SecretAsk], List[SecretGrant])] =
      for
        t <- now
        s <- state.updateAndGet(_.at(t))
      yield
        val asks   = s.asks.toList.sortBy(_._1).map((id, w) => SecretAsk(id, w.slot.agent, w.slot.key, w.purpose, w.since, w.box))
        val grants = s.grants.toList.sortBy((k, _) => (k.agent.value, k.key)).map((k, g) => SecretGrant(k.agent, k.key, g.until))
        (asks, grants)

    def answer(id: Long, decision: AskDecision): IO[AppError, Unit] =
      for
        t <- now
        settled <- state.modify { s0 =>
                     val s = s0.at(t)
                     s.asks.get(id) match
                       case None => (None, s)
                       case Some(w) =>
                         decision match
                           case AskDecision.Once(v) => (Some(List(w.reply -> Exit.succeed(Plain(v)))), s.copy(asks = s.asks - id))
                           case AskDecision.Deny =>
                             (Some(List(w.reply -> Exit.fail(AppError.forbidden("用户拒绝了这次使用 passkey secret 的请求")))), s.copy(asks = s.asks - id))
                           case AskDecision.For(v, minutes) =>
                             // 授权覆盖这个 agent 这个 secret 的全部请求
                             val p            = Plain(v)
                             val (mine, rest) = s.asks.partition((_, x) => x.slot == w.slot)
                             (Some(mine.values.toList.map(_.reply -> Exit.succeed(p))), State(rest, s.grants.updated(w.slot, Granted(p, t + minutes.toLong * 60_000)), s.lastId))
                   }
        replies <- ZIO.fromOption(settled).orElseFail(AppError.notFound("这个请求已经没有了（调用已结束或已答复）"))
        _       <- ZIO.foreachDiscard(replies)((r, e) => r.done(e))
        _       <- bus.publish(Topic.Vault)
      yield ()

    def revoke(slot: Slot): UIO[Unit] =
      state.update(s => s.copy(grants = s.grants - slot)) *> bus.publish(Topic.Vault)

    def asked: URIO[Scope, Dequeue[Asked]] = hub.subscribe
