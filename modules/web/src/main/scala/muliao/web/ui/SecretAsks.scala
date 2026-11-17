package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*

import Http.ec

/** passkey secret 等授权的请求（#915）：显示哪个 agent 要拿哪个 secret 跑什么，允许一次、一段时间或拒绝。
  * 允许时浏览器用 passkey 解开，把明文交回服务端（只在内存里）。会话页显示这个会话的，Secret 页显示全部。
  */
object SecretAsks:
  /** 「一段时间」的几档（分钟）。 */
  private val Spans = List(60 -> "1 小时", 8 * 60 -> "8 小时", 24 * 60 -> "24 小时")

  /** 一串请求：按 (agent, secret, 时间) 保留元素，别的请求进来刷新时，正在做的授权不被打断。 */
  def list(asks: Signal[List[SecretAsk]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(cls := "flex flex-col gap-2", children <-- asks.splitSeq(a => (a.agent, a.key, a.since))(s => card(s.now(), agents)))

  /** 这个会话里在等的请求。 */
  def forSession(id: SessionId): HtmlElement =
    val state = Remote(VaultApi.vaultState)(())
    list(state.map(_.data.toList.flatMap(_.asks).filter(_.purpose.session.contains(id))), AgentsData.list)

  def card(a: SecretAsk, agents: Signal[List[AgentSummary]]): HtmlElement =
    val act = new Action
    def answer(d: String => AskDecision): Unit =
      act.fire(Passkey.open(a.key, a.box).flatMap(v => Http.call(VaultApi.answerAsk)(AskAnswer(a.agent, a.key, d(v)))))
    div(
      cls := "flex flex-col gap-2 self-stretch rounded-xl border border-warning/40 bg-surface p-3 shadow-sm",
      div(
        cls := "flex flex-wrap items-center gap-1.5 text-sm",
        Icon("KeyRound", "size-4 shrink-0 text-warning"),
        child.text <-- agents.map(as => s"${AgentsData.label(as, a.agent)} 要用 passkey secret"),
        code(cls := "font-mono", a.key),
        span(cls := "text-muted", s"跑${a.use.shown}（${Ui.relativeTime(a.since)}）")
      ),
      pre(cls := Ui.CodeBlock + " max-h-60 overflow-auto", a.purpose.what),
      act.errorView(),
      div(
        cls := "flex flex-wrap justify-end gap-2",
        Ui.btn(Variant.Secondary, Size.Sm, disabledIf = act.busy.signal)(
          "拒绝",
          onClick --> (_ => act.fire(Http.call(VaultApi.answerAsk)(AskAnswer(a.agent, a.key, AskDecision.Deny))))
        ),
        Spans.map((m, label) => Ui.btn(Variant.Secondary, Size.Sm, disabledIf = act.busy.signal)(s"允许 $label", onClick --> (_ => answer(AskDecision.For(_, m))))),
        Ui.btn(Variant.Primary, Size.Sm, pending = act.busy.signal)("允许一次", onClick --> (_ => answer(AskDecision.Once(_))))
      )
    )
