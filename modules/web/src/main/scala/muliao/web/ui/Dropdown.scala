package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*
import org.scalajs.dom

/** 下拉的壳（HeroUI 的 Select 外观）：标签、显示当前值的按钮、点开的弹层；点到外面、按 Esc 或窗口改尺寸时收起。
  * 下拉单选 [[Fields.select]] 和选 agent 的 [[AgentPicker]] 共用，各自只给弹层里的内容。
  * 弹层挂在 body 上、fixed 定位跟着按钮（页面滚动时跟着走）：不被卡片、折叠区的 overflow 裁掉，也不被它们的层叠上下文
  * 压在后面的卡片下；下方放不下时翻到上方。
  */
object Dropdown:
  private var counter = 0
  private val Gap     = 4.0

  /** @param shown 按钮上显示的当前值，None 时显示 `placeholderText`
    * @param panelCls 弹层的宽度等附加类；弹层是竖排的 flex，高度不超过可用空间，内容里能滚动的那块要带 `min-h-0`
    * @param panel 弹层内容，每次打开新建一份（搜索词等随之清空）；参数是收起弹层的函数
    */
  def apply(
      labelText: String,
      shown: Signal[Option[HtmlElement]],
      placeholderText: String,
      hint: Option[Modifier[HtmlElement]],
      disabledIf: Signal[Boolean],
      panelCls: String
  )(panel: (() => Unit) => Seq[Modifier[HtmlElement]]): HtmlElement =
    counter += 1
    val id   = s"dd$counter"
    val open = Var(false)
    // 冒泡经过本组件的最近一次点击：document 收到的点击不是它就是点在外面。
    // 不按目标元素的祖先判断：点弹层里的分组会重画列表，被点的元素已离开 DOM。
    var inside: Option[dom.Event]          = None
    var trigger: Option[dom.html.Element]  = None
    var floating: Option[dom.html.Element] = None
    var portal: Option[(dom.Element, RootNode)] = None
    def reposition(): Unit =
      for t <- trigger; el <- floating do place(t, el)
    // 弹层里的点击和按键不冒泡经过组件（它挂在 body 上），弹层自己也记一份
    val ownEvents: Seq[Modifier[HtmlElement]] = Seq(
      onClick --> (e => inside = Some(e)),
      onKeyDown.filter(_.key == "Escape") --> (_ => open.set(false))
    )
    def floatingPanel(): HtmlElement =
      div(
        cls := s"fixed z-50 flex flex-col overflow-hidden rounded-xl bg-overlay shadow-lg ring-1 ring-separator $panelCls",
        ownEvents,
        onMountUnmountCallback(ctx => { floating = Some(ctx.thisNode.ref); reposition() }, _ => floating = None),
        documentEvents(_.onClick).filter(e => !inside.contains(e)) --> (_ => open.set(false)),
        windowEvents(_.onResize) --> (_ => open.set(false)),
        // 任一层滚动都可能挪动按钮；弹层自己里面的滚动不用管
        documentEvents(_.onScroll.useCapture).filter(e => !floating.exists(_.contains(e.target.asInstanceOf[dom.Node]))) --> (_ => reposition()),
        panel(() => open.set(false))
      )
    def show(on: Boolean): Unit =
      portal.foreach { (holder, root) =>
        root.unmount()
        holder.remove()
      }
      portal = Option.when(on) {
        val holder = dom.document.createElement("div")
        dom.document.body.appendChild(holder)
        (holder, render(holder, floatingPanel()))
      }
    div(
      cls := "select select--full-width select--secondary relative",
      dataAttr("slot") := "select",
      ownEvents,
      open.signal.updates --> show,
      onUnmountCallback(_ => { open.set(false); show(false) }),
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
        onMountUnmountCallback(ctx => trigger = Some(ctx.thisNode.ref), _ => trigger = None),
        span(
          cls := "select__value flex min-w-0 items-center gap-2",
          dataAttr("slot") := "select-value",
          dataAttr("placeholder") <-- shown.map(_.isEmpty.toString),
          child <-- shown.map(_.getOrElse(span(cls := "text-muted", placeholderText)))
        ),
        Icon("ChevronDown", "select__indicator size-4")
      ),
      hint.map(h => div(cls := "description", dataAttr("slot") := "description", h))
    )

  /** 放在按钮下方（下方放不下且上方更宽裕时放上方），左对齐按钮、至少和它一样宽、右边不出屏，高度不超过那一侧的空间。 */
  private def place(trigger: dom.html.Element, el: dom.html.Element): Unit =
    val r     = trigger.getBoundingClientRect()
    val vw    = dom.window.innerWidth
    val vh    = dom.window.innerHeight
    val below = vh - r.bottom - Gap * 2
    val above = r.top - Gap * 2
    val st    = el.style
    st.minWidth = s"${r.width}px"
    st.left = s"${math.max(Gap, math.min(r.left, vw - el.offsetWidth - Gap))}px"
    val down = below >= math.min(el.scrollHeight.toDouble, 320) || below >= above
    if down then
      st.top = s"${r.bottom + Gap}px"
      st.bottom = "auto"
      st.maxHeight = s"${below}px"
    else
      st.top = "auto"
      st.bottom = s"${vh - r.top + Gap}px"
      st.maxHeight = s"${above}px"
