package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*
import org.scalajs.dom

/** 选项：`id` 是值，`label` 是显示的字，`description` 是小字说明，`prefix` 显示在标签前（如色点）。 */
final case class Opt[T](id: T, label: String, description: Option[String] = None, prefix: Option[() => HtmlElement] = None)

object Opt:
  /** 枚举的选项：显示文字取自它的 [[Shown]]。 */
  def of[T: Shown](values: List[T]): List[Opt[T]] = values.map(v => Opt(v, v.shown))

/** 表单字段。表单都放在卡片或底部栏（surface 背景）上，统一用 secondary 变体（primary 的背景与 surface 同色看不出边界）。 */
object Fields:
  private var counter = 0
  private def nextId(): String =
    counter += 1
    s"f$counter"

  private def labelled(labelText: String, hint: Option[Modifier[HtmlElement]], fieldId: String)(control: HtmlElement): HtmlElement =
    div(
      cls := "flex w-full flex-col gap-1",
      dataAttr("slot") := "text-field",
      label(cls := "label", dataAttr("slot") := "label", forId := fieldId, labelText),
      control,
      hint.map(h => div(cls := "description", dataAttr("slot") := "description", h))
    )

  /** 单行输入，值在 `value` 里（外部改 `value` 也会更新输入框）。 */
  def text(
      labelText: String,
      value: Var[String],
      hint: Option[Modifier[HtmlElement]] = None,
      inputType: String = "text",
      placeholderText: String = "",
      mono: Boolean = false,
      autoFocusOn: Boolean = false,
      disabledIf: Signal[Boolean] = Val(false)
  ): HtmlElement =
    val id = nextId()
    labelled(labelText, hint, id)(
      input(
        idAttr := id,
        tpe := inputType,
        cls := "input input--secondary input--full-width" + (if mono then " font-mono" else ""),
        placeholder := placeholderText,
        autoFocus := autoFocusOn,
        disabled <-- disabledIf,
        controlled(Laminar.value <-- value.signal, onInput.mapToValue --> value)
      )
    )

  /** 多行输入。`onSubmit` 给出时 ⌘/Ctrl+Enter 调用它，Enter 照常换行。 */
  def area(
      labelText: String,
      value: Var[String],
      hint: Option[Modifier[HtmlElement]] = None,
      rowsN: Int = 4,
      mono: Boolean = false,
      autoFocusOn: Boolean = false,
      onSubmit: Option[() => Unit] = None,
      disabledIf: Signal[Boolean] = Val(false)
  ): HtmlElement =
    val id = nextId()
    labelled(labelText, hint, id)(
      textArea(
        idAttr := id,
        cls := "textarea textarea--secondary textarea--full-width" + (if mono then " font-mono text-sm" else ""),
        rows := rowsN,
        autoFocus := autoFocusOn,
        disabled <-- disabledIf,
        onSubmit.map(_ => placeholder := SendKey.hint),
        onSubmit.map(f => onKeyDown.filter(SendKey.isSend) --> { e => e.preventDefault(); f() }),
        controlled(Laminar.value <-- value.signal, onInput.mapToValue --> value)
      )
    )

  /** 下拉单选（HeroUI 的 Select 外观：按钮 + 弹出的选项列表，选项可带说明）。 */
  def select[T](
      labelText: String,
      value: Signal[Option[T]],
      onPick: T => Unit,
      options: Signal[List[Opt[T]]],
      hint: Option[Modifier[HtmlElement]] = None,
      placeholderText: String = "请选择",
      disabledIf: Signal[Boolean] = Val(false)
  ): HtmlElement =
    val open = Var(false)
    val id   = nextId()
    val current = value.combineWith(options).map((v, os) => v.flatMap(x => os.find(_.id == x)))
    def optionBody(o: Opt[T]) =
      div(
        cls := "flex flex-col text-left",
        span(cls := "flex items-center gap-2", o.prefix.map(_()), o.label),
        o.description.map(d => span(cls := "text-xs text-muted", d))
      )
    div(
      cls := "select select--full-width select--secondary relative",
      dataAttr("slot") := "select",
      span(cls := "label", dataAttr("slot") := "label", idAttr := s"$id-l", labelText),
      button(
        tpe := "button",
        cls := "select__trigger select__trigger--full-width",
        dataAttr("slot") := "select-trigger",
        aria.labelledBy := s"$id-l",
        Ui.attr("aria-haspopup") := "listbox",
        aria.expanded <-- open.signal,
        dataAttr("open") <-- open.signal.map(_.toString),
        disabled <-- disabledIf,
        onClick --> (_ => open.update(!_)),
        span(
          cls := "select__value",
          dataAttr("slot") := "select-value",
          dataAttr("placeholder") <-- current.map(_.isEmpty.toString),
          child <-- current.map {
            case Some(o) => optionBody(o)
            case None    => span(placeholderText)
          }
        ),
        Icon("ChevronDown", "select__indicator size-4")
      ),
      hint.map(h => div(cls := "description", dataAttr("slot") := "description", h)),
      child.maybe <-- open.signal.map(o =>
        Option.when(o)(
          div(
            cls := "absolute left-0 right-0 top-full z-50 mt-1 max-h-72 overflow-y-auto rounded-xl bg-overlay p-1 shadow-lg ring-1 ring-separator",
            role := "listbox",
            // 点到外面就收起
            documentEvents(_.onClick.filter(e => !e.target.asInstanceOf[dom.Element].closest(s"[data-sel='$id']").isInstanceOf[dom.Element])) --> (_ => open.set(false)),
            children <-- options.combineWith(value).map((os, v) =>
              os.map { o =>
                button(
                  tpe := "button",
                  role := "option",
                  cls := "flex w-full items-center gap-2 rounded-lg px-2 py-1.5 text-sm hover:bg-default",
                  aria.selected := v.contains(o.id),
                  onClick --> { _ => onPick(o.id); open.set(false) },
                  div(cls := "flex-1", optionBody(o)),
                  Option.when(v.contains(o.id))(Icon("Check", "size-4 text-accent"))
                )
              }
            )
          )
        )
      ),
      dataAttr("sel") := id
    )

  /** 单选切换（列表筛选等）。`showLabel` 时显示可见标签，否则 `labelText` 只作读屏名称。 */
  def segmented[T: Shown](labelText: String, value: Signal[T], onPick: T => Unit, options: List[T], showLabel: Boolean = false): HtmlElement =
    div(
      cls := "flex flex-col gap-1",
      Option.when(showLabel)(span(cls := "text-sm font-medium", labelText)),
      div(
        cls := "toggle-button-group toggle-button-group--horizontal",
        role := "radiogroup",
        aria.label := labelText,
        options.map { id =>
          button(
            tpe := "button",
            cls := "toggle-button toggle-button--default toggle-button--sm",
            role := "radio",
            aria.checked <-- value.map(v => (v == id).toString),
            dataAttr("selected") <-- value.map(v => (v == id).toString),
            onClick --> (_ => onPick(id)),
            id.shown
          )
        }
      )
    )

  /** 开关。`labelText` 描述它控制什么（读屏名称），状态由开关本身表达。 */
  def switch(labelText: String, checked: Signal[Boolean], onToggle: Boolean => Unit, disabledIf: Signal[Boolean] = Val(false), showLabel: Boolean = false): HtmlElement =
    div(
      cls := "switch switch--sm",
      dataAttr("slot") := "switch",
      label(
        cls := "switch__content",
        dataAttr("slot") := "switch-content",
        input(
          tpe := "checkbox",
          role := "switch",
          cls := "sr-only",
          Laminar.checked <-- checked,
          disabled <-- disabledIf,
          onChange.mapToChecked --> onToggle
        ),
        span(
          cls := "switch__control",
          dataAttr("slot") := "switch-control",
          dataAttr("selected") <-- checked.map(_.toString),
          span(cls := "switch__thumb", dataAttr("slot") := "switch-thumb", dataAttr("selected") <-- checked.map(_.toString))
        ),
        span(cls := (if showLabel then "label text-sm" else "label sr-only"), dataAttr("slot") := "label", labelText)
      ),
      dataAttr("selected") <-- checked.map(_.toString)
    )

  /** 复选框。 */
  def checkbox(labelText: String, checked: Signal[Boolean], onToggle: Boolean => Unit): HtmlElement =
    label(
      cls := "flex items-center gap-2 text-sm",
      input(tpe := "checkbox", cls := "size-4 accent-[var(--accent)]", Laminar.checked <-- checked, onChange.mapToChecked --> onToggle),
      labelText
    )

  private object Laminar:
    val value   = com.raquo.laminar.api.L.value
    val checked = com.raquo.laminar.api.L.checked

/** 发送快捷键：⌘+Enter（Mac）/ Ctrl+Enter；Enter 换行。 */
object SendKey:
  private val isMac = dom.window.navigator.platform.toLowerCase.contains("mac") || dom.window.navigator.userAgent.contains("iPhone")
  val hint: String  = if isMac then "⌘↩ 发送" else "Ctrl+↩ 发送"
  def isSend(e: dom.KeyboardEvent): Boolean =
    e.key == "Enter" && (e.metaKey || e.ctrlKey) && !e.isComposing
