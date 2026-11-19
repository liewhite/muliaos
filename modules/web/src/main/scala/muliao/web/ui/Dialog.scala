package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*

/** 居中的弹窗（HeroUI 的 Modal 外观：modal__backdrop / modal__dialog …），带右上角关闭按钮；详情弹窗与确认弹窗（[[Confirm]]）都用它。
  * 点背景不关：误触会丢掉编辑中的草稿；只用 Esc 或关闭按钮。
  */
object Dialog:
  /** @param size HeroUI 的尺寸后缀（xs、sm、md、lg、full）
    * @param dialogCls 追加到对话框上的类（如 `sm:max-w-3xl`）
    */
  def apply(onClose: () => Unit, labelText: String, size: String = "lg", dialogCls: String = "")(content: Modifier[HtmlElement]*): HtmlElement =
    div(
      // HeroUI 的层级是 100000，会盖住 Toast（z-100）与下拉弹层（z-50）：压到它们下面。
      // 详情里又弹出确认时同层：确认弹窗挂在应用根部、在 DOM 里靠后，压在上面
      cls := "modal__backdrop modal__backdrop--opaque z-40",
      dataAttr("slot") := "modal-backdrop",
      // 外层不经 React Aria，视口高度变量由这里给
      styleAttr := "--visual-viewport-height:100dvh",
      documentEvents(_.onKeyDown.filter(_.key == "Escape")) --> (_ => onClose()),
      div(
        cls := "modal__container",
        dataAttr("slot") := "modal-container",
        dataAttr("placement") := "auto",
        sectionTag(
          cls := s"modal__dialog modal__dialog--scroll-inside modal__dialog--$size pointer-events-auto $dialogCls".trim,
          dataAttr("slot") := "modal-dialog",
          role := "dialog",
          Ui.attr("aria-modal") := "true",
          aria.label := labelText,
          content,
          button(
            tpe := "button",
            cls := "close-button close-button--default modal__close-trigger",
            dataAttr("slot") := "modal-close-trigger",
            aria.label := "关闭",
            onClick --> (_ => onClose()),
            Icon("X", "size-4")
          )
        )
      )
    )

  def header(mods: Modifier[HtmlElement]*): HtmlElement = div(cls := "modal__header", dataAttr("slot") := "modal-header", mods)

  def heading(mods: Modifier[HtmlElement]*): HtmlElement = h2(cls := "modal__heading", dataAttr("slot") := "modal-heading", mods)

  def body(mods: Modifier[HtmlElement]*): HtmlElement =
    div(cls := "modal__body modal__body--scroll-inside", dataAttr("slot") := "modal-body", mods)

  def footer(mods: Modifier[HtmlElement]*): HtmlElement = div(cls := "modal__footer", dataAttr("slot") := "modal-footer", mods)
