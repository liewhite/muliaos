package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*

/** 虚线框、默认收起的一块（会话里的工具调用、事件、等待中的后台任务）：标题行整行可点，右边的箭头随展开翻转；内容只在展开时挂上。 */
object Fold:
  /** 展开/收起的箭头：展开时朝上。 */
  def chevron(open: Signal[Boolean], extra: String = "shrink-0"): Modifier[HtmlElement] =
    child <-- open.map(o => Icon("ChevronDown", s"size-4 $extra transition-transform " + (if o then "rotate-180" else "")))

  /** @param open 展开状态由调用方持有（会话页按条目记住，刷新重画不收起）
    * @param lead 放在展开按钮之前、自己可以点的东西（如指向 thread 的链接、图标）
    * @param extra 外框的附加类（宽度、对齐）
    */
  def dashed(open: Var[Boolean], title: Modifier[HtmlElement], lead: Modifier[HtmlElement] = emptyMod, extra: String = "")(
      body: => Modifier[HtmlElement]
  ): HtmlElement =
    div(
      cls := s"rounded-xl border border-dashed border-separator px-3 py-1.5 text-sm text-muted $extra",
      div(
        cls := "flex items-center gap-2",
        lead,
        button(
          tpe := "button",
          cls := "flex min-w-0 flex-1 items-center gap-2 text-left",
          aria.expanded <-- open.signal,
          onClick --> (_ => open.update(!_)),
          title,
          chevron(open.signal)
        )
      ),
      child.maybe <-- open.signal.map(x => Option.when(x)(div(cls := "pt-1", body)))
    )
