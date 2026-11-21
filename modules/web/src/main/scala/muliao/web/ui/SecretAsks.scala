package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*

import Http.ec

/** passkey secret 等授权的请求（#915）：显示哪个 agent 要拿哪个 secret 跑什么，允许一次、一段时间或拒绝。
  * 允许时浏览器用 passkey 解开，把明文交回服务端（只在内存里）。签名请求（SSH 证书 #938、链上交易与消息 #1092）逐行显示从内容本身解出的样子（[[Signing.rows]]），
  * 签好只交回结果。会话页显示这个会话的，Secret 页显示全部。
  */
object SecretAsks:
  /** 「一段时间」的几档（分钟）。 */
  private val Spans = List(60 -> "1 小时", 8 * 60 -> "8 小时", 24 * 60 -> "24 小时")

  /** 一串请求：按编号保留元素，别的请求进来刷新时，正在做的授权不被打断。 */
  def list(asks: Signal[List[SecretAsk]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(cls := "flex flex-col gap-2", children <-- asks.splitSeq(_.id)(s => card(s.now(), agents)))

  /** 这个会话里在等的请求。 */
  def forSession(id: SessionId): HtmlElement =
    val state = Remote(VaultApi.vaultState)(())
    list(state.map(_.data.toList.flatMap(_.asks).filter(_.purpose.session.contains(id))), AgentsData.list)

  def card(a: SecretAsk, agents: Signal[List[AgentSummary]]): HtmlElement =
    val act = new Action
    def deny = Ui.btn(Variant.Secondary, Size.Sm, disabledIf = act.busy.signal)(
      "拒绝",
      onClick --> (_ => act.fire(Http.call(VaultApi.answerAsk)(AskAnswer(a.id, AskDecision.Deny))))
    )
    def reply(d: scala.concurrent.Future[AskDecision]): Unit = act.fire(d.flatMap(x => Http.call(VaultApi.answerAsk)(AskAnswer(a.id, x))))
    val (head, body, buttons) = a.want match
      case AskWant.Reveal =>
        def answer(d: String => AskDecision): Unit = reply(Passkey.reveal(a.key, a.box).map(d))
        (
          "要用 passkey secret",
          pre(cls := Ui.CodeBlock + " max-h-60 overflow-auto", a.purpose.what),
          List(deny) ++ Spans.map((m, label) => Ui.btn(Variant.Secondary, Size.Sm, disabledIf = act.busy.signal)(title := "这个 agent 用这个 secret 在这段时间内不再问，正在等的一并放行", s"允许 $label", onClick --> (_ => answer(AskDecision.For(_, m))))) :+
            Ui.btn(Variant.Primary, Size.Sm, pending = act.busy.signal)("允许一次", onClick --> (_ => answer(AskDecision.Once(_))))
        )
      case AskWant.Sign(content) =>
        (
          s"要用密钥${Signing.title(content)}",
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
            Ui.btn(Variant.Primary, Size.Sm, pending = act.busy.signal)(
              title := "用 passkey 解开密钥、在这个页面里按上面的内容签好交回，私钥不离开浏览器",
              "签名",
              onClick --> (_ => reply(Passkey.sign(a.key, a.box, content).map(AskDecision.Signed(_))))
            )
          )
        )
    div(
      cls := "flex flex-col gap-2 self-stretch rounded-xl border border-warning/40 bg-surface p-3 shadow-sm",
      div(
        cls := "flex flex-wrap items-center gap-1.5 text-sm",
        Icon("KeyRound", "size-4 shrink-0 text-warning"),
        child.text <-- agents.map(as => s"${AgentsData.label(as, a.agent)} $head"),
        code(cls := "font-mono", a.key),
        span(cls := "text-muted", Format.relative(a.since))
      ),
      body,
      act.errorView(),
      div(cls := "flex flex-wrap justify-end gap-2", buttons)
    )
