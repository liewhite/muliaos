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
    Theme.start()
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
