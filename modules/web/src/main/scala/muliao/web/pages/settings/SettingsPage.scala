package muliao.web.pages.settings

import com.raquo.laminar.api.L.*
import muliao.contract.{NotificationsApi, ScriptGrantView, WorkspaceApi}
import muliao.web.core.*
import muliao.web.pwa.{Push, PushState}
import muliao.web.ui.*

import scala.scalajs.js

import Http.ec

object SettingsPage:
  /** 本设备推送状态的说明。 */
  def pushText(s: PushState): String = s match
    case PushState.Unsupported  => "此浏览器不支持推送通知。"
    case PushState.NeedsInstall => "iOS 需要先用 Safari「分享 → 添加到主屏幕」，再从主屏幕图标打开本应用，才能开启推送。"
    case PushState.Default      => "尚未开启推送。"
    case PushState.Denied       => "通知权限已被拒绝，请在系统设置中为本应用开启通知。"
    case PushState.Unsubscribed => "本设备没有订阅推送（退出登录时会取消），重新开启即可。"
    case PushState.Subscribed   => "推送已开启。"

  /** 这个状态下能做什么：开启（或重新订阅）按钮的文字、能不能发测试通知。 */
  private enum PushActions:
    case Unavailable
    case Enable(label: String, canTest: Boolean)

  private def actionsOf(s: PushState): PushActions = s match
    case PushState.Default | PushState.Unsubscribed            => PushActions.Enable("开启推送", canTest = false)
    case PushState.Subscribed                                  => PushActions.Enable("重新订阅本设备", canTest = true)
    case PushState.Unsupported | PushState.NeedsInstall | PushState.Denied => PushActions.Unavailable

  def view(): HtmlElement =
    val act = new Action
    Ui.page("设置")(
      themeCard,
      pushCard(act),
      ScriptGrants.view(),
      act.errorView(),
      div(Ui.btn(Variant.DangerSoft, disabledIf = act.busy.signal)(Icon("LogOut"), "退出登录", onClick --> (_ => logout(act))))
    )

  private def logout(act: Action): Unit =
    Confirm.ask("退出登录？", confirmLabel = "退出").foreach { ok =>
      if ok then
        // 服务端会话失效才是退出的核心，先做；退出后锁屏上不该再出现 agent 的回复：再取消本设备的订阅，失败只提示、不挡住退出
        act.fire(Auth.logout().map { _ =>
          Push.unsubscribe().failed.foreach(e => Toast.danger(s"取消本设备的推送失败：${Http.messageOf(e)}"))
        })
    }

  /** 深浅色：存在本设备，各设备各设各的。 */
  private def themeCard: HtmlElement =
    Ui.card(
      div(
        cls := "card__header",
        div(cls := "card__title flex items-center gap-2", Icon("SunMoon"), "主题"),
        div(cls := "card__description", "只对本设备生效。")
      ),
      div(cls := "card__content", Fields.segmented("主题", Theme.choice.signal, Theme.choice.set, Theme.Choice.values.toList))
    )

  private def pushCard(act: Action): HtmlElement =
    // None：还在查询本设备的订阅
    val state = Var(Option.empty[PushState])
    def refresh(): Unit =
      Push.state().onComplete {
        case scala.util.Success(s) => state.set(Some(s))
        case scala.util.Failure(e) => act.error.set(Some(s"读取推送状态失败：${Http.messageOf(e)}"))
      }
    Ui.card(
      onMountCallback(_ => refresh()),
      div(
        cls := "card__header",
        div(cls := "card__title flex items-center gap-2", Icon("BellRing"), "推送通知"),
        div(cls := "card__description", child.text <-- state.signal.map(_.fold("正在读取推送状态…")(pushText)))
      ),
      child.maybe <-- state.signal.map(_.map(actionsOf)).map {
        case Some(PushActions.Enable(label, canTest)) =>
          Some(
            div(
              cls := "card__footer flex gap-2",
              Ui.btn(size = Size.Sm, pending = act.busy.signal)(label, onClick --> (_ => act.run(Push.enable()).foreach(_ => refresh()))),
              Option.when(canTest)(
                Ui.btn(Variant.Secondary, Size.Sm, disabledIf = act.busy.signal)(
                  "发送测试通知",
                  onClick --> (_ => act.fire(Http.call(NotificationsApi.test)(()).map(_ => Toast.success("测试通知已发出"))))
                )
              )
            )
          )
        case _ => None
      }
    )

/** skill 脚本的「始终允许」：在审批处点「始终允许」记下的，逐条可撤销（撤销后同一调用重新要批准）。 */
object ScriptGrants:
  def view(): HtmlElement =
    val grants = Remote(WorkspaceApi.listGrants)(())
    val act    = new Action
    Ui.card(
      div(
        cls := "card__header",
        div(cls := "card__title flex items-center gap-2", Icon("ShieldCheck"), "始终允许的脚本"),
        div(cls := "card__description", "同一个 agent 以同样的参数和 secret 运行同一个脚本、且 skill 里除 SKILL.md 外的文件没变时，不再问你。")
      ),
      div(
        cls := "card__content",
        act.errorView(),
        Ui.loading(grants) { list =>
          Seq(
            child.maybe <-- list.map(l => Option.when(l.isEmpty)(p(cls := "text-sm text-muted", "没有"))),
            div(cls := "flex flex-col divide-y divide-border", children <-- list.splitSeq(_.id)(g => row(g.now(), act)))
          )
        }
      )
    )

  private def revoke(g: ScriptGrantView, act: Action): Unit =
    Confirm.ask("撤销这条始终允许？", confirmLabel = "撤销").foreach(ok => if ok then act.fire(Http.call(WorkspaceApi.revokeGrant)(g.id)))

  private def row(g: ScriptGrantView, act: Action): HtmlElement =
    div(
      cls := "flex items-start gap-2 py-2",
      div(
        cls := "flex min-w-0 flex-1 flex-col gap-0.5 text-sm",
        div(
          cls := "flex flex-wrap items-center gap-1.5",
          Ui.chip(g.agentName, Tone.Accent)(),
          span(cls := "break-all font-mono", s"${g.skill}/${g.script}"),
          Option.when(!g.current)(Ui.chip("skill 已改，不再生效", Tone.Warning)())
        ),
        span(
          cls := "break-all font-mono text-xs",
          if g.args.isEmpty then "（无参数）" else g.args.map(a => js.JSON.stringify(a)).mkString(" "),
          Option.when(g.secrets.nonEmpty)(s" · secrets: ${g.secrets.mkString(", ")}")
        ),
        span(cls := "text-xs text-muted", s"${Format.time(g.createdAt)} 允许")
      ),
      Ui.iconBtn("Trash2", "撤销", iconCls = "size-4", disabledIf = act.busy.signal)(onClick --> (_ => revoke(g, act)))
    )
