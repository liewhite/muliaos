package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.timers.setTimeout

/** 全局唯一的确认弹窗：页面用 `Confirm.ask(...)` 以 Future 询问用户。新的询问、换了页面都让还没回答的旧询问以「取消」结束。 */
object Confirm:
  final case class Ask(title: String, body: Option[String], confirmLabel: String, danger: Boolean, answer: Promise[Boolean])

  private val current: Var[Option[Ask]] = Var(None)

  def ask(title: String, body: Option[String] = None, confirmLabel: String = "确定", danger: Boolean = false): Future[Boolean] =
    settle(false)
    val p = Promise[Boolean]()
    current.set(Some(Ask(title, body, confirmLabel, danger, p)))
    p.future

  private def settle(ok: Boolean): Unit =
    current.now().foreach(_.answer.trySuccess(ok))
    current.set(None)

  /** 挂在应用根部。 */
  val view: HtmlElement = div(
    // 弹窗属于发起它的页面：页面切走即取消
    Router.location.map(_.path).updates --> (_ => settle(false)),
    child.maybe <-- current.signal.map(_.map { a =>
      Overlay.modal(onDismiss = () => settle(false))(
        div(
          cls := "flex items-center gap-3",
          span(
            cls := (if a.danger then "flex size-9 items-center justify-center rounded-full bg-danger/10 text-danger"
                    else "flex size-9 items-center justify-center rounded-full bg-accent/10 text-accent"),
            Icon(if a.danger then "CircleAlert" else "Info", "size-5")
          ),
          h2(cls := "text-base font-semibold", a.title)
        ),
        a.body.map(b => p(cls := "mt-3 text-sm text-muted whitespace-pre-wrap break-words", b)),
        div(
          cls := "mt-5 flex justify-end gap-2",
          Ui.btn(Variant.Tertiary)("取消", onClick --> (_ => settle(false))),
          Ui.btn(if a.danger then Variant.Danger else Variant.Primary)(a.confirmLabel, onClick --> (_ => settle(true)))
        )
      )
    })
  )

/** 顶部弹出的短提示。 */
object Toast:
  private final case class Item(id: Int, tone: Tone, text: String)
  private val items = Var(List.empty[Item])
  private var seq   = 0

  private def show(tone: Tone, text: String): Unit =
    seq += 1
    val id = seq
    items.update(_ :+ Item(id, tone, text))
    setTimeout(4000)(items.update(_.filterNot(_.id == id))): Unit

  def success(text: String): Unit = show(Tone.Success, text)
  def danger(text: String): Unit  = show(Tone.Danger, text)
  def info(text: String): Unit    = show(Tone.Default, text)

  val view: HtmlElement = div(
    cls := "pointer-events-none fixed top-[calc(env(safe-area-inset-top)+1rem)] left-1/2 z-[100] flex w-[calc(100vw-2rem)] max-w-md -translate-x-1/2 flex-col gap-2",
    role := "status",
    children <-- items.signal.splitSeq(_.id)(sig =>
      val it = sig.now()
      div(
        cls := (it.tone match
          case Tone.Danger  => "pointer-events-auto rounded-xl bg-overlay px-4 py-3 text-sm shadow-lg ring-1 ring-danger/40 text-danger"
          case Tone.Success => "pointer-events-auto rounded-xl bg-overlay px-4 py-3 text-sm shadow-lg ring-1 ring-success/40"
          case _            => "pointer-events-auto rounded-xl bg-overlay px-4 py-3 text-sm shadow-lg ring-1 ring-separator"),
        it.text
      )
    )
  )

/** 弹层的外框：模态框（居中）与抽屉（从左侧滑出）。点背景或按 Esc 关闭。 */
object Overlay:
  def modal(onDismiss: () => Unit, size: String = "max-w-sm")(content: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "fixed inset-0 z-50 flex items-center justify-center bg-black/40 p-4",
      onClick.filter(e => e.target == e.currentTarget) --> (_ => onDismiss()),
      documentEvents(_.onKeyDown.filter(_.key == "Escape")) --> (_ => onDismiss()),
      div(cls := s"w-full $size rounded-2xl bg-overlay p-5 shadow-xl", role := "dialog", Ui.attr("aria-modal") := "true", content)
    )

  def drawerLeft(onDismiss: () => Unit, labelText: String)(content: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "fixed inset-0 z-50 bg-black/40",
      onClick.filter(e => e.target == e.currentTarget) --> (_ => onDismiss()),
      documentEvents(_.onKeyDown.filter(_.key == "Escape")) --> (_ => onDismiss()),
      div(
        cls := "absolute inset-y-0 left-0 flex w-72 max-w-[85vw] flex-col bg-overlay pt-[env(safe-area-inset-top)] pb-[env(safe-area-inset-bottom)] shadow-xl",
        role := "dialog",
        aria.label := labelText,
        content
      )
    )

  /** 底部弹出的面板（手机上的提问、附件预览等）。 */
  def sheet(onDismiss: () => Unit, labelText: String)(content: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "fixed inset-0 z-50 flex items-end justify-center bg-black/40 md:items-center",
      onClick.filter(e => e.target == e.currentTarget) --> (_ => onDismiss()),
      documentEvents(_.onKeyDown.filter(_.key == "Escape")) --> (_ => onDismiss()),
      div(
        cls := "max-h-[85vh] w-full overflow-y-auto rounded-t-2xl bg-overlay p-4 pb-[calc(env(safe-area-inset-bottom)+1rem)] shadow-xl md:max-w-lg md:rounded-2xl",
        role := "dialog",
        aria.label := labelText,
        content
      )
    )

