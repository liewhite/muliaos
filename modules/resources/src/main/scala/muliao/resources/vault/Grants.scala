package muliao.resources.vault

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import zio.*

/** 「一段时间」的授权按「哪个 agent 用哪个 secret」记：A 拿到的授权 B 用不了。 */
final case class Slot(agent: AgentId, key: String)

/** 有调用开始等某个 passkey secret 的授权（发通知用）。 */
final case class Asked(slot: Slot, want: AskWant, purpose: AskPurpose)

/** passkey secret 的授权：服务端解不开这类 secret，要用时等用户用 passkey 在浏览器里解开。
  * 每次调用各自一个请求（带会话与要跑的命令，用户看着它批），要的是两种之一（[[AskWant]]）：
  *   - 明文（[[obtain]]）：「一次」与「拒绝」只结掉这一个；「一段时间」把这个 agent 这个 secret 记成已授权，同时结掉它全部在等的取明文请求，
  *     到期前再用不再问（到期的在下次读写状态时删掉）。
  *   - 签名（[[sign]]，#938）：签名钥匙在浏览器里签好交回结果，明文不出浏览器；每次都要批，不看也不留「一段时间」的授权。
  * 调用结束（含被中断）时它的请求随之撤下。全在内存里，重启清空。
  */
trait Grants:
  /** 取明文：有生效的授权直接给；否则登记一个请求，等到用户答复。 */
  def obtain(slot: Slot, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String]

  /** 请用户用这个签名钥匙签一份内容，等到用户答复；返回签好的结果。 */
  def sign(slot: Slot, content: SignContent, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String]

  /** 等授权的请求与生效中的授权（不含明文）。 */
  def view: UIO[(List[SecretAsk], List[SecretGrant])]

  /** 答复一个请求；它已经没了（调用已结束或已答复）报 NotFound，答复与请求的种类不符报 Invalid。 */
  def answer(id: AskId, decision: AskDecision): IO[AppError, Unit]

  /** 收回一段时间的授权。 */
  def revoke(slot: Slot): UIO[Unit]

  /** 订阅新出现的请求（发通知用）：返回时已订上，scope 结束退订。 */
  def asked: URIO[Scope, Dequeue[Asked]]

object Grants:
  val layer: URLayer[ChangeBus, Grants] =
    ZLayer {
      for
        bus   <- ZIO.service[ChangeBus]
        state <- Ref.make(State(Map.empty, Map.empty))
        hub   <- Hub.sliding[Asked](256)
      yield Live(bus, state, hub)
    }

  /** 明文：toString 不带值，免得被意外打进日志。 */
  final class Plain(val value: String):
    override def toString: String = "Plain(***)"

  /** 一个请求在等什么、答复交给谁：种类与答复的类型绑在一起，答错种类在类型上对不上。 */
  private enum Need:
    case Value(reply: Promise[AppError, Plain])
    case Sign(content: SignContent, reply: Promise[AppError, String])

    def want: AskWant = this match
      case Value(_)   => AskWant.Reveal
      case Sign(c, _) => AskWant.Sign(c)

    def deny: UIO[Unit] =
      val e = AppError.forbidden("用户拒绝了这次使用 passkey secret 的请求")
      this match
        case Value(r)   => r.fail(e).unit
        case Sign(_, r) => r.fail(e).unit

  /** 一个在等的请求。 */
  private final case class Waiting(slot: Slot, need: Need, purpose: AskPurpose, box: PasskeyBox, since: Long)

  /** 一段时间的授权，到 `until`（epoch 毫秒）为止。 */
  private final case class Granted(plain: Plain, until: Long)

  private final case class State(asks: Map[AskId, Waiting], grants: Map[Slot, Granted]):
    /** 去掉到期的授权：每次读写状态时顺手做，明文不留到下次之后（不另起定时器）。 */
    def at(t: Long): State = copy(grants = grants.filter((_, g) => g.until > t))

  private final case class Live(bus: ChangeBus, state: Ref[State], hub: Hub[Asked]) extends Grants:
    private val now = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    def obtain(slot: Slot, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String] =
      for
        reply <- Promise.make[AppError, Plain]
        got   <- ask(slot, Need.Value(reply), purpose, box, s => s.grants.get(slot).map(_.plain))(reply.await)
      yield got.value

    def sign(slot: Slot, content: SignContent, purpose: AskPurpose, box: PasskeyBox): IO[AppError, String] =
      for
        reply <- Promise.make[AppError, String]
        out   <- ask(slot, Need.Sign(content, reply), purpose, box, _ => None)(reply.await)
      yield out

    /** 登记一个请求并等答复（`ready` 给出不用问就有的结果）；结束时（拿到、被拒绝或被中断）撤下请求。 */
    private def ask[A](slot: Slot, need: Need, purpose: AskPurpose, box: PasskeyBox, ready: State => Option[A])(await: IO[AppError, A]): IO[AppError, A] =
      ZIO.acquireReleaseWith(register(slot, need, purpose, box, ready))(withdraw) {
        case Left(a) => ZIO.succeed(a)
        case Right(_) =>
          bus.publish(Topic.Vault) *> hub.publish(Asked(slot, need.want, purpose)) *> await
      }

    /** 现成就有的结果；否则登记一个请求、给出它的编号。 */
    private def register[A](slot: Slot, need: Need, purpose: AskPurpose, box: PasskeyBox, ready: State => Option[A]): UIO[Either[A, AskId]] =
      for
        t  <- now
        id <- Random.nextUUID.map(u => AskId.wrap(u.toString)) // 全局唯一：界面按它复用卡片，重启后不能撞上旧的
        got <- state.modify { s0 =>
                 val s = s0.at(t)
                 ready(s) match
                   case Some(a) => (Left(a), s)
                   case None    => (Right(id), s.copy(asks = s.asks.updated(id, Waiting(slot, need, purpose, box, t))))
               }
      yield got

    private def withdraw[A](got: Either[A, AskId]): UIO[Unit] =
      ZIO.foreachDiscard(got.toOption) { id =>
        state.update(s => s.copy(asks = s.asks - id)) *> bus.publish(Topic.Vault)
      }

    def view: UIO[(List[SecretAsk], List[SecretGrant])] =
      for
        t <- now
        s <- state.updateAndGet(_.at(t))
      yield
        val asks =
          s.asks.toList.sortBy((id, w) => (w.since, id.value)).map((id, w) => SecretAsk(id, w.slot.agent, w.slot.key, w.need.want, w.purpose, w.since, w.box))
        val grants = s.grants.toList.sortBy((k, _) => (k.agent.value, k.key)).map((k, g) => SecretGrant(k.agent, k.key, g.until))
        (asks, grants)

    def answer(id: AskId, decision: AskDecision): IO[AppError, Unit] =
      for
        t <- now
        settled <- state.modify { s0 =>
                     val s = s0.at(t)
                     s.asks.get(id) match
                       case None    => (Left(AppError.notFound("这个请求已经没有了（调用已结束或已答复）")), s)
                       case Some(w) => settle(s, id, w, decision, t)
                   }
        replies <- ZIO.fromEither(settled)
        _       <- replies
        _       <- bus.publish(Topic.Vault)
      yield ()

    /** 按答复结掉请求：新状态与要交出的结果。 */
    private def settle(s: State, id: AskId, w: Waiting, decision: AskDecision, t: Long): (Either[AppError, UIO[Unit]], State) =
      val done = s.copy(asks = s.asks - id)
      (w.need, decision) match
        case (need, AskDecision.Deny)                    => (Right(need.deny), done)
        case (Need.Value(r), AskDecision.Once(v))        => (Right(r.succeed(Plain(v)).unit), done)
        case (Need.Sign(_, r), AskDecision.Signed(c))    => (Right(r.succeed(c).unit), done)
        case (Need.Value(_), AskDecision.For(v, minutes)) =>
          // 授权覆盖这个 agent 这个 secret 的全部取明文请求
          val p    = Plain(v)
          val mine = s.asks.collect { case (k, Waiting(w.slot, Need.Value(r), _, _, _)) => k -> r }
          (Right(ZIO.foreachDiscard(mine.values)(_.succeed(p))), State(s.asks -- mine.keys, s.grants.updated(w.slot, Granted(p, t + minutes.toLong * 60_000))))
        case (need, _) =>
          val kind = need match
            case _: Need.Value => "取明文"
            case _: Need.Sign  => "签名"
          (Left(AppError.invalid(s"答复与请求的种类（$kind）不符")), s)

    def revoke(slot: Slot): UIO[Unit] =
      state.update(s => s.copy(grants = s.grants - slot)) *> bus.publish(Topic.Vault)

    def asked: URIO[Scope, Dequeue[Asked]] = hub.subscribe
