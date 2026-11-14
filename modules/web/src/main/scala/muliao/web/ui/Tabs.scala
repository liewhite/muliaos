package muliao.web.ui

import com.raquo.laminar.api.L.*

/** 标签页（HeroUI Tabs 的 secondary 外观：一排标签 + 选中项下的指示条）。只画标签栏；面板由调用方按选中值显示。 */
object Tabs:
  /** @param tabs 每个标签的值与显示内容（可带随数据变化的计数） */
  def apply[T](labelText: String, value: Signal[T], onPick: T => Unit, tabs: List[(T, Modifier[HtmlElement])]): HtmlElement =
    div(
      cls := "tabs tabs--secondary",
      dataAttr("slot") := "tabs",
      dataAttr("orientation") := "horizontal",
      div(
        cls := "tabs__list-container overflow-x-auto",
        dataAttr("slot") := "tabs-list-container",
        div(
          cls := "tabs__list",
          dataAttr("slot") := "tabs-list",
          role := "tablist",
          aria.label := labelText,
          dataAttr("orientation") := "horizontal",
          tabs.map { (id, label) =>
            val selected = value.map(_ == id)
            div(
              cls := "tabs__tab whitespace-nowrap",
              dataAttr("slot") := "tabs-tab",
              role := "tab",
              tabIndex <-- selected.map(s => if s then 0 else -1),
              aria.selected <-- selected,
              dataAttr("selected") <-- selected.map(_.toString),
              onClick --> (_ => onPick(id)),
              onKeyDown.filter(e => e.key == "Enter" || e.key == " ") --> { e => e.preventDefault(); onPick(id) },
              label,
              child.maybe <-- selected.map(s => Option.when(s)(div(cls := "tabs__indicator", dataAttr("slot") := "tabs-indicator")))
            )
          }
        )
      )
    )
