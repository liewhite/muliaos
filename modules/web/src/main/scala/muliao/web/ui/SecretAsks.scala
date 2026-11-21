package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*

import scala.concurrent.Future

import Http.ec

/** passkey secret 等授权的请求（#915）做成待办卡片（[[Todo]]）：显示哪个 agent 要拿哪个 secret 跑什么，允许一次、一段时间或拒绝。
  * 允许时浏览器用 passkey 解开，把明文交回服务端（只在内存里）。签名请求（SSH 证书 #938、链上交易与消息 #1092）逐行显示从内容本身解出的样子（[[Signing.rows]]），
  * 签好只交回结果。会话页显示这个会话的，Secret 页与通知页显示全部。
  */
object SecretAsks:
  /** 「一段时间」的几档（分钟）。 */
  private val Spans = List(60 -> "1 小时", 8 * 60 -> "8 小时", 24 * 60 -> "24 小时")

  /** 全部在等的请求（Secret 页）：按编号保留元素，别的请求进来刷新时，正在做的授权不被打断。 */
  def list(asks: Signal[List[SecretAsk]]): HtmlElement =
    div(cls := "flex flex-col gap-2", children <-- asks.splitSeq(_.id)(s => card(s.now(), away(s.now()))))

  /** 这个会话里在等的请求。 */
  def forSession(id: SessionId): HtmlElement =
    val asks = Remote(VaultApi.vaultState)(()).map(_.data.toList.flatMap(_.asks).filter(_.purpose.session.contains(id)))
    div(cls := "flex flex-col gap-2", children <-- asks.splitSeq(_.id)(s => card(s.now(), Todo.Place.InSession)))

  def away(a: SecretAsk): Todo.Place = Todo.Place.Away(a.agent, a.purpose.session)

  def card(a: SecretAsk, place: Todo.Place): HtmlElement =
    def reply(d: => Future[AskDecision]): () => Future[Unit] = () => d.flatMap(x => Http.call(VaultApi.answerAsk)(AskAnswer(a.id, x)))
    val deny = Todo.Choice("拒绝", reply(Future.successful(AskDecision.Deny)))
    val key  = code(cls := "font-mono", a.key)
    a.want match
      case AskWant.Reveal =>
        def answer(d: String => AskDecision) = reply(Passkey.reveal(a.key, a.box).map(d))
        Todo.card(
          Todo.Kind.Secret,
          place,
          Seq(span("要用 passkey secret"), key),
          Some(a.since),
          pre(cls := Ui.CodeBlock + " max-h-60 overflow-auto", a.purpose.what),
          (deny :: Spans.map((m, label) => Todo.Choice(s"允许 $label", answer(AskDecision.For(_, m)), hint = "这个 agent 用这个 secret 在这段时间内不再问，正在等的一并放行"))) :+
            Todo.Choice("允许一次", answer(AskDecision.Once(_)), primary = true)
        )
      case AskWant.Sign(content) =>
        Todo.card(
          Todo.Kind.Secret,
          place,
          Seq(span(s"要用密钥${Signing.title(content)}"), key),
          Some(a.since),
          div(
            cls := "grid grid-cols-[auto_minmax(0,1fr)] gap-x-3 gap-y-1 text-sm",
            Signing.rows(content).flatMap((k, v) =>
              List(
                span(cls := "text-muted whitespace-nowrap", k),
                if v.contains('\n') || v.length > 80 then pre(cls := Ui.CodeBlock + " max-h-60 overflow-auto whitespace-pre-wrap break-all text-xs", v)
                else code(cls := "font-mono break-all", v)
              )
            )
          ),
          List(
            deny,
            Todo.Choice("签名", reply(Passkey.sign(a.key, a.box, content).map(AskDecision.Signed(_))), primary = true, hint = "用 passkey 解开密钥、在这个页面里按上面的内容签好交回，私钥不离开浏览器")
          )
        )
