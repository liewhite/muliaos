package muliao.web

import com.raquo.laminar.api.L.*
import muliao.web.core.*
import muliao.web.pwa.{Push, SwNavigate}
import muliao.web.shell.{Login, Shell}
import muliao.web.ui.{Confirm, Toast, Ui}
import org.scalajs.dom

/** 前端入口：按登录状态显示登录页或登录后的外壳；全局的确认弹窗与提示挂在根部。 */
object Main:
  def main(args: Array[String]): Unit =
    followSystemTheme()
    SwNavigate.listen()
    Push.registerServiceWorker()
    Auth.check()
    renderOnDomContentLoaded(dom.document.getElementById("app"), app)

  private def app: HtmlElement =
    div(
      cls := "h-full",
      child <-- Auth.state.signal.map {
        case AuthState.Checking      => div()
        case AuthState.Unreachable(m) => div(cls := "p-4", Ui.errorAlert("无法连接服务端", m))
        case AuthState.SignedOut     => Login.view()
        case AuthState.SignedIn      => Shell.view()
      },
      Confirm.view,
      Toast.view
    )

  /** 主题跟随系统深浅色（index.html 里的内联脚本负责首屏，避免闪烁）。 */
  private def followSystemTheme(): Unit =
    val mq = dom.window.matchMedia("(prefers-color-scheme: dark)")
    def apply(): Unit =
      val t    = if mq.matches then "dark" else "light"
      val root = dom.document.documentElement
      root.classList.remove("dark")
      root.classList.remove("light")
      root.classList.add(t)
      root.setAttribute("data-theme", t)
    mq.addEventListener("change", (_: dom.Event) => apply())
    apply()
