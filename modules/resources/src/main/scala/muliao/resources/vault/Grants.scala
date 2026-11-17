package muliao.resources.vault

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import zio.*

/** 授权按「哪个 agent 用哪个 secret」记：A 拿到的授权 B 用不了。 */
final case class Slot(agent: AgentId, key: String)

/** 有 agent 开始等某个 passkey secret 的授权（同一个 agent、同一个 secret 已在等的不再发）。 */
final case class Asked(slot: Slot, use: SecretUse, purpose: AskPurpose)

/** passkey secret 的授权：服务端解不开这类 secret，要用时等用户在界面上用 passkey 解开交回明文。
  * 同一 (agent, secret) 的调用合成一个请求；「一次」只交给答复时在等的调用，「一段时间」到期前同一 agent 再用不再问（到期的在下次读写状态时删掉）。
  * 等着的调用被中断（会话被停）不撤请求：请求留到用户答复或重启，届时交出的明文没人取就丢掉。全在内存里，重启清空。
  */
trait Grants:
  /** 取明文：有生效的授权直接给；否则登记（或加入）等授权的请求，等到用户答复。 */
  def obtain(slot: Slot, use: SecretUse, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String]

  /** 等授权的请求与生效中的授权（不含明文）。 */
  def view: UIO[(List[SecretAsk], List[SecretGrant])]

  /** 答复一个等授权的请求；没有这个请求（已答复过）报 NotFound。 */
  def answer(slot: Slot, decision: AskDecision): IO[AppError, Unit]

  /** 收回一段时间的授权。 */
  def revoke(slot: Slot): UIO[Unit]

  /** 订阅新出现的等授权请求（发通知用）：返回时已订上，scope 结束退订。 */
  def asked: URIO[Scope, Dequeue[Asked]]

object Grants:
  val layer: URLayer[ChangeBus, Grants] =
    ZLayer {
      for
        bus   <- ZIO.service[ChangeBus]
        state <- Ref.make(Map.empty[Slot, Entry])
        hub   <- Hub.sliding[Asked](256)
      yield Live(bus, state, hub)
    }

  /** 明文：toString 不带值，免得被意外打进日志。 */
  final class Plain(val value: String):
    override def toString: String = "Plain(***)"

  /** 一个 (agent, secret) 现在的状态。 */
  private enum Entry:
    /** 有调用在等这个答复 */
    case Waiting(reply: Promise[AppError, Plain], use: SecretUse, purpose: AskPurpose, box: PasskeyBox, since: Long)
    /** 一段时间的授权，到 `until`（epoch 毫秒）为止 */
    case Granted(plain: Plain, until: Long)

  private final case class Live(bus: ChangeBus, state: Ref[Map[Slot, Entry]], hub: Hub[Asked]) extends Grants:
    private val now = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    def obtain(slot: Slot, use: SecretUse, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String] =
      for
        t     <- now
        fresh <- Promise.make[AppError, Plain]
        found <- state.modify { all =>
                   val m = live(all, t)
                   m.get(slot) match
                     case Some(Entry.Granted(p, _)) => (Right(p), m)
                     case Some(w: Entry.Waiting)    => (Left(w.reply), m)
                     case None                      => (Left(fresh), m.updated(slot, Entry.Waiting(fresh, use, purpose, box, t)))
                 }
        plain <- found match
                   case Right(p) => ZIO.succeed(p)
                   case Left(reply) =>
                     (bus.publish(Topic.Vault) *> hub.publish(Asked(slot, use, purpose))).when(reply eq fresh) *> reply.await
      yield plain.value

    def view: UIO[(List[SecretAsk], List[SecretGrant])] =
      for
        t <- now
        m <- state.updateAndGet(live(_, t))
      yield
        val sorted = m.toList.sortBy((s, _) => (s.agent.value, s.key))
        val asks   = sorted.collect { case (s, w: Entry.Waiting) => SecretAsk(s.agent, s.key, w.use, w.purpose, w.since, w.box) }
        val grants = sorted.collect { case (s, Entry.Granted(_, until)) => SecretGrant(s.agent, s.key, until) }
        (asks.sortBy(_.since), grants)

    def answer(slot: Slot, decision: AskDecision): IO[AppError, Unit] =
      for
        t <- now
        (result, keep) = outcome(decision, t)
        found <- state.modify { all =>
                   val m = live(all, t)
                   m.get(slot) match
                     case Some(w: Entry.Waiting) => (Some(w.reply), keep.fold(m - slot)(g => m.updated(slot, g)))
                     case _                      => (None, m)
                 }
        reply <- ZIO.fromOption(found).orElseFail(AppError.notFound(s"${slot.key} 的这个请求已经没有了（已答复过）"))
        _     <- reply.done(result)
        _     <- bus.publish(Topic.Vault)
      yield ()

    /** 答复交给等着的调用什么、之后留下什么。 */
    private def outcome(d: AskDecision, t: Long): (Exit[AppError, Plain], Option[Entry]) = d match
      case AskDecision.Once(v) => (Exit.succeed(Plain(v)), None)
      case AskDecision.For(v, minutes) =>
        val p = Plain(v)
        (Exit.succeed(p), Some(Entry.Granted(p, t + minutes.toLong * 60_000)))
      case AskDecision.Deny => (Exit.fail(AppError.forbidden("用户拒绝了这次使用 passkey secret 的请求")), None)

    /** 去掉到期的授权：每次读写状态时顺手做，明文不留到下次之后（不另起定时器）。 */
    private def live(m: Map[Slot, Entry], t: Long): Map[Slot, Entry] =
      m.filter {
        case (_, Entry.Granted(_, until)) => until > t
        case _                            => true
      }

    def revoke(slot: Slot): UIO[Unit] =
      state.update { m =>
        m.get(slot) match
          case Some(_: Entry.Granted) => m - slot
          case _                      => m
      } *> bus.publish(Topic.Vault)

    def asked: URIO[Scope, Dequeue[Asked]] = hub.subscribe
