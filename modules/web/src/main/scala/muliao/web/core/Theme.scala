package muliao.web.core

import com.raquo.laminar.api.L.{unsafeWindowOwner, Var}
import org.scalajs.dom

/** 界面深浅色：用户在设置里选（存本设备 localStorage，各设备各设各的），选「跟随系统」时随系统切换。
  * index.html 的内联脚本在首屏前按同一个存储键先设一次（避免闪烁），与这里的 [[Choice]] 名称、[[Mode]] 的类名与颜色保持一致。
  */
object Theme:
  /** 实际生效的深浅色：根元素上的类名与 `data-theme`、浏览器顶栏颜色（theme-color）。 */
  enum Mode(val cssName: String, val barColor: String):
    case Light extends Mode("light", "#ffffff")
    case Dark  extends Mode("dark", "#181f38")

  /** 用户的选择；`System` 由系统偏好决定实际的 [[Mode]]。 */
  enum Choice(val label: String) extends Shown.Self:
    case System extends Choice("跟随系统")
    case Light  extends Choice("浅色")
    case Dark   extends Choice("深色")

    def resolve(systemDark: Boolean): Mode = this match
      case System => if systemDark then Mode.Dark else Mode.Light
      case Light  => Mode.Light
      case Dark   => Mode.Dark

  /** 存储键与 index.html 内联脚本里的一致。 */
  val choice: Var[Choice] = Stored.choice("muliao.theme", Choice.values.toList, Choice.System)

  /** 启动时调用一次：按选择与系统偏好设主题，二者任一变化都重设。 */
  def start(): Unit =
    val mq         = dom.window.matchMedia("(prefers-color-scheme: dark)")
    val systemDark = Var(mq.matches)
    mq.addEventListener("change", (_: dom.Event) => systemDark.set(mq.matches))
    // 订阅随页面存活，不需要取消
    val _ = choice.signal
      .combineWith(systemDark.signal)
      .map(_.resolve(_))
      .foreach(apply)(using unsafeWindowOwner)

  private def apply(m: Mode): Unit =
    val root = dom.document.documentElement
    Mode.values.foreach(x => root.classList.remove(x.cssName))
    root.classList.add(m.cssName)
    root.setAttribute("data-theme", m.cssName)
    dom.document.querySelector("meta[name=theme-color]").setAttribute("content", m.barColor)
