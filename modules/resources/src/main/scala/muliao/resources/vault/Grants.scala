package muliao.resources.vault

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import zio.*

/** 授权按「哪个 agent 用哪个 secret」记：A 拿到的授权 B 用不了。 */
final case class Slot(agent: AgentId, key: String)

/** 有 agent 开始等某个 passkey secret 的授权（同一个 agent、同一个 secret 正在等的不再发）。 */
final case class Asked(slot: Slot, use: SecretUse)

/** passkey secret 的授权：服务端解不开这类 secret，要用时等用户在界面上用 passkey 解开交回明文。
  * 「一次」只交给这时正在等的调用；「一段时间」到期前同一 agent 再用不再问（到期的在下次读写状态时删掉）。全在内存里：服务重启全部清空，等着的调用随运行一起中断。
  */
trait Grants:
  /** 取明文：有生效的授权直接给；否则登记（或加入）一个等授权的请求，等到用户答复。被中断时退出等待，没人等了请求随之撤下。 */
  def obtain(slot: Slot, use: SecretUse, box: PasskeyBox): IO[AppError, String]

  /** 等授权的请求与生效中的授权（不含明文）。 */
  def view: UIO[(List[SecretAsk], List[SecretGrant])]

  /** 答复一个等授权的请求；请求已经没了（等的调用都中断了、或已答复过）报 NotFound。 */
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
    /** 有调用在等：`waiters` 个调用在等同一个答复 */
    case Waiting(reply: Promise[AppError, Plain], waiters: Int, use: SecretUse, box: PasskeyBox, since: Long)
    /** 一段时间的授权，到 `until`（epoch 毫秒）为止 */
    case Granted(plain: Plain, until: Long)

  /** 这次取明文是直接拿到，还是要等（`fresh`：是新登记的请求）。 */
  private enum Step:
    case Ready(plain: Plain)
    case Wait(reply: Promise[AppError, Plain], fresh: Boolean)

  private final case class Live(bus: ChangeBus, state: Ref[Map[Slot, Entry]], hub: Hub[Asked]) extends Grants:
    private val now = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    def obtain(slot: Slot, use: SecretUse, box: PasskeyBox): IO[AppError, String] =
      ZIO.uninterruptibleMask { restore =>
        for
          t     <- now
          reply <- Promise.make[AppError, Plain]
          step <- state.modify { all =>
                    val m = live(all, t)
                    m.get(slot) match
                      case Some(Entry.Granted(p, until)) if until > t => (Step.Ready(p), m)
                      case Some(w: Entry.Waiting) => (Step.Wait(w.reply, fresh = false), m.updated(slot, w.copy(waiters = w.waiters + 1)))
                      case _ => (Step.Wait(reply, fresh = true), m.updated(slot, Entry.Waiting(reply, 1, use, box, t)))
                  }
          plain <- step match
                     case Step.Ready(p) => ZIO.succeed(p)
                     case Step.Wait(r, fresh) =>
                       (bus.publish(Topic.Vault) *> hub.publish(Asked(slot, use))).when(fresh) *>
                         restore(r.await).onInterrupt(leave(slot, r))
        yield plain.value
      }

    /** 一个等着的调用被中断了：没人等了就撤下请求。 */
    private def leave(slot: Slot, reply: Promise[AppError, Plain]): UIO[Unit] =
      state.modify { m =>
        m.get(slot) match
          case Some(w: Entry.Waiting) if w.reply eq reply =>
            if w.waiters <= 1 then (true, m - slot) else (false, m.updated(slot, w.copy(waiters = w.waiters - 1)))
          case _ => (false, m)
      }.flatMap(bus.publish(Topic.Vault).when(_)).unit

    def view: UIO[(List[SecretAsk], List[SecretGrant])] =
      for
        t <- now
        m <- state.updateAndGet(live(_, t))
      yield
        val sorted = m.toList.sortBy((s, _) => (s.agent.value, s.key))
        val asks = sorted.collect { case (s, w: Entry.Waiting) => SecretAsk(s.agent, s.key, w.use, w.since, w.box) }
        val grants = sorted.collect { case (s, Entry.Granted(_, until)) => SecretGrant(s.agent, s.key, until) }
        (asks.sortBy(_.since), grants)

    def answer(slot: Slot, decision: AskDecision): IO[AppError, Unit] =
      for
        t <- now
        settle <- ZIO.fromEither(outcome(decision, t))
        found <- state.modify { all =>
                   val m = live(all, t)
                   m.get(slot) match
                     case Some(w: Entry.Waiting) =>
                       (Some(w.reply), settle.keep.fold(m - slot)(g => m.updated(slot, g)))
                     case _ => (None, m)
                 }
        reply <- ZIO.fromOption(found).orElseFail(AppError.notFound(s"${slot.key} 的这个请求已经没有了（等它的调用已中断或已答复）"))
        _ <- reply.done(settle.result)
        _ <- bus.publish(Topic.Vault)
      yield ()

    /** 答复的结果：交给等着的调用什么、之后留下什么。 */
    private final case class Settle(result: Exit[AppError, Plain], keep: Option[Entry.Granted])

    private def outcome(d: AskDecision, t: Long): Either[AppError, Settle] = d match
      case AskDecision.Once(v) => plain(v).map(p => Settle(Exit.succeed(p), None))
      case AskDecision.For(v, minutes) =>
        if minutes <= 0 then Left(AppError.invalid("授权时长要大于 0 分钟"))
        else plain(v).map(p => Settle(Exit.succeed(p), Some(new Entry.Granted(p, t + minutes.toLong * 60_000))))
      case AskDecision.Deny =>
        Right(Settle(Exit.fail(AppError.forbidden("用户拒绝了这次使用 passkey secret 的请求")), None))

    private def plain(v: String): Either[AppError, Plain] =
      Either.cond(v.nonEmpty, Plain(v), AppError.invalid("解出来的值是空的"))

    /** 去掉到期的授权：每次读写状态时顺手做，明文不留到下次之后（不另起定时器）。 */
    private def live(m: Map[Slot, Entry], t: Long): Map[Slot, Entry] =
      m.filter {
        case (_, Entry.Granted(_, until)) => until > t
        case _                            => true
      }

    def revoke(slot: Slot): UIO[Unit] =
      state.modify { m =>
        m.get(slot) match
          case Some(_: Entry.Granted) => (true, m - slot)
          case _                      => (false, m)
      }.flatMap(bus.publish(Topic.Vault).when(_)).unit

    def asked: URIO[Scope, Dequeue[Asked]] = hub.subscribe
