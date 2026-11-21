package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.{AgentId, SessionId}
import muliao.web.core.*

import scala.concurrent.Future

/** 等用户处理的一件事的卡片（#1097）：工具调用的批准、agent 的提问、passkey secret 的授权（交出明文或签名）都用它。
  * 每种待办只给三样：头部（要做什么）、内容、几个选择（一个按钮和它产生答复的动作）；卡片的样子、忙碌与出错只写在这里。
  * passkey 不是单独的一种待办：要它的选择在自己的动作里先用 passkey 解开或签好再答复。会话页、Secret 页、通知页都用它。
  * 答复成功后正在显示的数据重拉，待办没了卡片就消失。
  */
object Todo:
  /** 卡片显示在哪：就在这个会话里（不重复写 agent、不给去会话的链接），或别处（通知页、Secret 页）。 */
  enum Place:
    case InSession
    /** `session`：在哪个会话里等（触发器等没有会话的为空） */
    case Away(agent: AgentId, session: Option[SessionId])

  /** 待办的类别：图标与颜色。 */
  enum Kind(val icon: String, val border: String, val tint: String):
    case Approval extends Kind("ShieldQuestion", "border-warning/40", "text-warning")
    case Question extends Kind("CircleHelp", "border-accent", "text-accent")
    case Secret   extends Kind("KeyRound", "border-warning/40", "text-warning")

  /** 一个选择：按钮文字、产生答复的动作；`primary` 是推荐的那个，`hint` 是按钮的说明，`enabled` 为假时不能点（如提问还没答完）。 */
  final case class Choice(label: String, run: () => Future[Any], primary: Boolean = false, hint: Option[String] = None, enabled: Signal[Boolean] = Val(true))

  /** @param what   要做什么（接在 agent 名后面）
    * @param since  开始等的时刻
    * @param anchor 页面内跳转用的元素 id
    */
  def card(kind: Kind, place: Place, what: Modifier[HtmlElement], since: Option[Long], body: Modifier[HtmlElement], choices: List[Choice], anchor: Option[String] = None): HtmlElement =
    val act = new Action
    val (who, link) = place match
      case Place.InSession => (None, None)
      case Place.Away(agent, session) =>
        (
          Some(span(cls := "font-medium", child.text <-- AgentsData.list.map(AgentsData.label(_, agent)))),
          session.map(s => Ui.appLink(Route.session(s).url)(cls := "ml-auto flex items-center gap-0.5 text-xs text-accent hover:underline", "去会话", Icon("ChevronRight", "size-3.5")))
        )
    div(
      anchor.map(idAttr := _),
      cls := s"flex scroll-mt-4 flex-col gap-2 self-stretch rounded-xl border ${kind.border} bg-surface p-3 shadow-sm",
      div(
        cls := "flex flex-wrap items-center gap-1.5 text-sm",
        Icon(kind.icon, s"size-4 shrink-0 ${kind.tint}"),
        who,
        what,
        since.map(t => span(cls := "text-muted", Format.relative(t))),
        link
      ),
      body,
      act.errorView(),
      div(
        cls := "flex flex-wrap justify-end gap-2",
        choices.map(c =>
          Ui.btn(if c.primary then Variant.Primary else Variant.Secondary, Size.Sm, disabledIf = act.busy.signal.combineWith(c.enabled).map(_ || !_))(
            c.hint.map(title := _),
            c.label,
            onClick --> (_ => act.fire(c.run()))
          )
        )
      )
    )
