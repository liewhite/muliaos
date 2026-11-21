package muliao.web.pages.notifications

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.pages.sessions.CallAsks
import muliao.web.ui.*

/** 通知页顶上的「待你处理」（#1097）：所有会话里在等用户的调用（批准、提问）与 passkey secret 的授权（交出明文、签名），在一处处理。
  * 待办是活的状态，各自的领域管着（会话、vault），处理掉就从各处消失；通知是历史记录，照旧列在下面。
  */
object Awaiting:
  /** 一件待办：会话里在等的调用，或 passkey secret 的授权请求。 */
  enum Item:
    case Call(c: AwaitingCall)
    case Secret(a: SecretAsk)

    def key: String = this match
      case Call(c)   => s"call:${c.entry}:${c.call.id.value}"
      case Secret(a) => s"secret:${a.id.value}"

    def since: Long = this match
      case Call(c)   => c.call.startedAt.getOrElse(0L)
      case Secret(a) => a.since

  private val calls = Remote(SessionsApi.awaiting)(())
  private val vault = Remote(VaultApi.vaultState)(())

  /** 全部待办，先等的在前；没拉到的一边先当作没有（原因见 [[problems]]）。外壳一直订阅着（导航角标）。 */
  val items: Signal[List[Item]] =
    calls.combineWith(vault).map((c, v) => (c.data.toList.flatten.map(Item.Call(_)) ++ v.data.toList.flatMap(_.asks).map(Item.Secret(_))).sortBy(_.since))

  /** 没拉到（或重拉失败）的原因。 */
  private val problems: Signal[List[String]] =
    def why(l: Load[?]): Option[String] = l match
      case Load.Failed(m)        => Some(m)
      case Load.Loaded(_, stale) => stale
      case Load.Pending          => None
    calls.combineWith(vault).map((c, v) => why(c).map("在等的调用：" + _).toList ++ why(v).map("secret 授权：" + _))

  private def card(i: Item): HtmlElement = i match
    case Item.Call(c) =>
      CallAsks
        .card(c.call, Todo.Place.Away(c.agent, Some(c.session)), in => Http.call(SessionsApi.answer)(CallAnswer(c.session, c.entry, c.call.id, in)))
        .getOrElse(div())
    case Item.Secret(a) => SecretAsks.card(a, SecretAsks.away(a))

  /** 有待办或有拉取问题时才出现。 */
  def view(): HtmlElement =
    div(
      cls := "flex flex-col gap-2",
      children <-- problems.map(_.map(m => div(cls := "alert alert--danger text-sm", s"待办没拉到：$m"))),
      child.maybe <-- items.map(_.size).distinct.map(n =>
        Option.when(n > 0)(div(cls := "flex items-center gap-1.5 pt-1 text-sm font-medium", Icon("Inbox", "size-4"), s"待你处理（$n）"))
      ),
      children <-- items.splitSeq(_.key)(s => card(s.now())),
      child.maybe <-- items.map(_.nonEmpty).distinct.map(has => Option.when(has)(div(cls := "pt-2 text-sm font-medium text-muted", "通知")))
    )
