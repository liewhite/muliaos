package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*
import org.scalajs.dom

import scala.concurrent.Future
import scala.scalajs.js
import scala.util.{Failure as TryFailure, Success}

import Http.ec

/** 按钮的样式（HeroUI 的 button 类）。 */
enum Variant(val cls: String):
  case Primary   extends Variant("button--primary")
  case Secondary extends Variant("button--secondary")
  case Tertiary  extends Variant("button--tertiary")
  case Outline   extends Variant("button--outline")
  case Ghost     extends Variant("button--ghost")
  case Danger    extends Variant("button--danger")
  case DangerSoft extends Variant("button--danger-soft")

enum Size(val suffix: String):
  case Sm extends Size("sm")
  case Md extends Size("md")
  case Lg extends Size("lg")

/** 状态色（Chip、Alert）。 */
enum Tone(val name: String):
  case Default extends Tone("default")
  case Accent  extends Tone("accent")
  case Success extends Tone("success")
  case Warning extends Tone("warning")
  case Danger  extends Tone("danger")

/** Chip 的底色：按状态色的浅底，或中性的灰底（标签、分类）。 */
enum ChipLook(val cls: String):
  case Soft      extends ChipLook("chip--soft")
  case Secondary extends ChipLook("chip--secondary")

object Ui:
  /** 代码/日志块的统一样式。 */
  val CodeBlock = "my-1 max-h-80 overflow-auto whitespace-pre-wrap break-all rounded-lg bg-surface-secondary p-2 font-mono text-xs text-foreground"

  def buttonCls(variant: Variant = Variant.Primary, size: Size = Size.Md, iconOnly: Boolean = false): String =
    s"button button--${size.suffix} ${variant.cls}" + (if iconOnly then " button--icon-only" else "")

  /** 按钮。`pending` 时转圈并禁用。 */
  def btn(
      variant: Variant = Variant.Primary,
      size: Size = Size.Md,
      iconOnly: Boolean = false,
      pending: Signal[Boolean] = Val(false),
      label: String = "",
      disabledIf: Signal[Boolean] = Val(false)
  )(mods: Modifier[Button]*): Button =
    button(
      tpe := "button",
      cls := buttonCls(variant, size, iconOnly),
      dataAttr("slot") := "button",
      Option.when(label.nonEmpty)(aria.label := label),
      Option.when(label.nonEmpty && iconOnly)(title := label),
      disabled <-- pending.combineWith(disabledIf).map(_ || _),
      dataAttr("pending") <-- pending.map(_.toString),
      child.maybe <-- pending.map(p => Option.when(p)(spinner("spinner--sm spinner--current"))),
      mods
    )

  def iconBtn(
      icon: String,
      label: String,
      variant: Variant = Variant.Ghost,
      size: Size = Size.Sm,
      iconCls: String = "size-5",
      disabledIf: Signal[Boolean] = Val(false)
  )(mods: Modifier[Button]*): Button =
    btn(variant, size, iconOnly = true, label = label, disabledIf = disabledIf)(Icon(icon, iconCls), mods)

  /** 站内链接：走前端路由；`withFrom` 时目标页的返回回到这里。 */
  def appLink(url: String, withFrom: Boolean = true)(mods: Modifier[Anchor]*): Anchor =
    a(
      href := url,
      onClick.filter(e => !(e.metaKey || e.ctrlKey || e.shiftKey || e.button != 0)).preventDefault --> (_ => Router.go(url, withFrom)),
      mods
    )

  def linkButton(url: String, variant: Variant = Variant.Primary, size: Size = Size.Sm)(mods: Modifier[Anchor]*): Anchor =
    appLink(url, withFrom = false)(cls := buttonCls(variant, size), mods)

  def spinner(extra: String = ""): HtmlElement =
    span(cls := s"spinner $extra".trim, role := "status", aria.label := "加载中",
      // HeroUI 的转圈由 CSS 画在这个元素上
      svgSpinner)

  private def svgSpinner: SvgElement =
    import com.raquo.laminar.api.L.svg as s
    s.svg(
      s.cls := "size-full",
      s.viewBox := "0 0 24 24",
      s.fill := "none",
      s.circle(s.cx := "12", s.cy := "12", s.r := "10", s.stroke := "currentColor", s.strokeWidth := "3", s.opacity := "0.25"),
      s.path(s.d := "M22 12a10 10 0 0 1-10 10", s.stroke := "currentColor", s.strokeWidth := "3", s.strokeLineCap := "round")
    )

  def card(mods: Modifier[HtmlElement]*): HtmlElement =
    div(cls := "card card--default", dataAttr("slot") := "card", mods)

  /** 整张可点、打开详情的列表卡片；`active`（右侧正打开的就是它）时加强调色环。 */
  def linkCard(url: String, active: Signal[Boolean])(mods: Modifier[HtmlElement]*): Anchor =
    appLink(url, withFrom = false)(
      cls := "block",
      card(
        cls := "transition-colors hover:bg-surface-hover",
        cls <-- active.map(on => if on then "ring-2 ring-accent" else ""),
        mods
      )
    )

  def chip(text: String, tone: Tone = Tone.Default, look: ChipLook = ChipLook.Soft, size: Size = Size.Sm)(mods: Modifier[HtmlElement]*): HtmlElement =
    span(
      cls := s"chip chip--${tone.name} chip--${size.suffix} ${look.cls}",
      span(cls := "chip__label", text),
      mods
    )

  /** 行内的一小段代码（编号、cron、引用）：灰底等宽字。 */
  def inlineCode(mods: Modifier[HtmlElement]*): HtmlElement =
    code(cls := "rounded bg-surface-secondary px-1.5 py-0.5 font-mono", mods)

  // ---- 页面布局 ----

  /** 打开管理菜单（抽屉）。由外壳设置。 */
  val adminMenuOpen: Var[Boolean] = Var(false)

  /** 一页：头部（手机上的管理菜单按钮、返回、标题、操作）+ 滚动的内容 + 固定在底部的 footer。
    * @param wide 内容不限宽、不加内边距（如横向滚动的看板），由页面自己排版
    */
  def page(
      title: Modifier[HtmlElement],
      back: Option[Route] = None,
      /** 返回的上级页随数据而定时（如 thread 回发起会话）：给出取上级页的函数，代替 `back` */
      backTo: Option[() => Route] = None,
      actions: Modifier[HtmlElement] = emptyMod,
      footer: Modifier[HtmlElement] = emptyMod,
      wide: Boolean = false,
      main: Modifier[HtmlElement] = emptyMod
  )(content: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "flex h-full flex-col",
      headerTag(
        cls := "flex items-center gap-2 border-b border-separator bg-surface px-3 pt-[calc(env(safe-area-inset-top)+0.5rem)] pb-2",
        // 手机上每个页面左上角都有管理菜单（二级页面也是，在返回前）；桌面端左侧栏已有同一个按钮
        iconBtn("Menu", "管理菜单")(cls := "md:hidden", onClick --> (_ => adminMenuOpen.set(true))),
        back.map(b => () => b).orElse(backTo).map(b => iconBtn("ChevronLeft", "返回")(onClick --> (_ => Router.back(b())))),
        h1(cls := "min-w-0 flex-1 truncate text-lg font-semibold", title),
        div(cls := "flex items-center gap-2", actions)
      ),
      mainTag(
        cls := "min-h-0 flex-1 overflow-y-auto",
        main,
        if wide then content else div(cls := "mx-auto flex w-full max-w-3xl flex-col gap-3 px-4 py-3", content)
      ),
      footer
    )

  /** 卡片网格：按容器宽度自动排列，宽屏一行 3–4 张。 */
  def cardGrid(mods: Modifier[HtmlElement]*): HtmlElement =
    div(cls := "grid grid-cols-[repeat(auto-fill,minmax(17rem,1fr))] gap-3", mods)

  private def indicator(expanded: Signal[Boolean], extra: String): SvgElement =
    import com.raquo.laminar.api.L.svg as s
    s.svg(
      s.cls := s"disclosure__indicator $extra",
      s.viewBox := "0 0 16 16",
      s.fill := "none",
      s.width := "16",
      s.height := "16",
      s.svgAttr("data-expanded", com.raquo.laminar.codecs.Codec.stringAsIs, None) <-- expanded.map(_.toString),
      s.path(
        s.d := "M2.97 5.47a.75.75 0 0 1 1.06 0L8 9.44l3.97-3.97a.75.75 0 1 1 1.06 1.06l-4.5 4.5a.75.75 0 0 1-1.06 0l-4.5-4.5a.75.75 0 0 1 0-1.06",
        s.fill := "currentColor"
      )
    )

  /** 可折叠的一块。内容只在展开时挂上（展开前不拉数据）。 */
  def disclosure(
      expanded: Var[Boolean],
      triggerCls: String,
      bodyCls: String,
      indicatorCls: String = "ml-auto",
      indicatorFirst: Boolean = false
  )(trigger: Modifier[Button]*)(body: => Modifier[HtmlElement]): HtmlElement =
    val ind = indicator(expanded.signal, indicatorCls)
    div(
      cls := "disclosure",
      h3(
        cls := "disclosure__heading",
        button(
          tpe := "button",
          cls := s"disclosure__trigger $triggerCls",
          aria.expanded <-- expanded.signal,
          onClick --> (_ => expanded.update(!_)),
          if indicatorFirst then Seq[Modifier[Button]](ind, trigger) else Seq[Modifier[Button]](trigger, ind)
        )
      ),
      child.maybe <-- expanded.signal.map(e =>
        Option.when(e)(div(cls := "disclosure__content", dataAttr("expanded") := "true", styleAttr := "height:auto", div(cls := s"disclosure__body $bodyCls", body)))
      )
    )

  /** 列表里可折叠的一组（一个 agent 的触发器、一个分组的 agent）：组头靠左，强调色竖条 + 加粗大字。 */
  def groupSection(name: String, count: Signal[Int], defaultExpanded: Boolean = true)(body: => Modifier[HtmlElement]): HtmlElement =
    disclosure(Var(defaultExpanded), "flex w-full items-center gap-2 border-l-4 border-accent py-1 pl-2 text-base font-semibold", "pt-2", "ms-0", indicatorFirst = true)(
      span(cls := "truncate", name),
      span(cls := "chip chip--default chip--sm chip--soft", span(cls := "chip__label", child.text <-- count.map(_.toString)))
    )(body)

  /** 可折叠的一张卡片（「新建 xxx」表单、「已结束」的列表）：默认收起，内容展开时才挂上。 */
  def collapsibleCard(title: String, icon: String = "Plus")(body: => Modifier[HtmlElement]): HtmlElement =
    card(disclosure(Var(false), "flex w-full items-center gap-2 px-1 font-medium", "flex flex-col gap-4 pt-4")(Icon(icon), title)(body))

  // ---- 状态展示 ----

  /** 提示框：图标、标题、说明。 */
  def alert(tone: Tone, icon: String, title: String)(description: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := s"alert alert--${tone.name}",
      role := "alert",
      div(cls := "alert__indicator", Icon(icon, "size-5")),
      div(cls := "alert__content", div(cls := "alert__title", title), div(cls := "alert__description", description))
    )

  def errorAlert(title: String, message: String): HtmlElement =
    alert(Tone.Danger, "CircleAlert", title)(cls := "whitespace-pre-wrap break-words", message)

  def errorAlert(error: Signal[Option[String]], title: String = "出错了"): Modifier[HtmlElement] =
    child.maybe <-- error.map(_.map(errorAlert(title, _)))

  def emptyHint(mods: Modifier[HtmlElement]*): HtmlElement = p(cls := "py-10 text-center text-sm text-muted", mods)

  def centeredSpinner: HtmlElement = div(cls := "flex justify-center py-10", spinner())

  /** 显示一份远端数据：首次加载转圈，首次失败整块报错；已有数据时重新拉取失败照常显示，顶上提示。
    * `render` 只调用一次，拿到的是数据的 Signal：数据刷新不重建 DOM（输入框、展开状态不丢）。
    */
  def loading[A](state: Signal[Load[A]])(render: Signal[A] => Modifier[HtmlElement]): Modifier[HtmlElement] =
    Seq(
      child.maybe <-- state.map {
        case Load.Loaded(_, Some(m)) => Some(errorAlert("重新加载失败，显示的是之前的内容", m))
        case _                       => None
      },
      child <-- state.map(_.data).splitOption(data => div(cls := "contents", render(data))).someOrElse(
        div(cls := "contents", child <-- state.map {
          case Load.Failed(m) => errorAlert("加载失败", m)
          case _              => centeredSpinner
        })
      )
    )

  /** 任意 HTML 属性（字符串值）。 */
  def attr(name: String): HtmlAttr[String] = htmlAttr(name, com.raquo.laminar.codecs.Codec.stringAsIs)

  /** 复制到剪贴板。剪贴板只在 HTTPS 等安全上下文可用；不可用时如实报错。 */
  def copyText(text: String, what: String): Unit =
    val clip = dom.window.navigator.asInstanceOf[js.Dynamic].clipboard
    if js.isUndefined(clip) then Toast.danger(s"当前页面无法访问剪贴板（需要 HTTPS），请手动选中${what}复制")
    else
      clip.writeText(text).asInstanceOf[js.Promise[Unit]].toFuture.onComplete {
        case Success(_)  => Toast.success(s"已复制$what")
        case TryFailure(e) => Toast.danger(s"复制失败：${e.getMessage}")
      }

/** 一个用户操作：执行中 busy，失败记在 error（由 errorAlert 显示），或按 `toast` 提示；成功后让正在显示的数据重新拉取。 */
final class Action:
  val busy: Var[Boolean]          = Var(false)
  val error: Var[Option[String]] = Var(None)

  /** @param toast 失败时用 toast 提示「<toast>失败：<原因>」，不记进 error（没有放 errorAlert 的位置时用）
    * @param leave 成功后离开这个页面（如删掉了它展示的东西）
    */
  def run(action: => Future[Any], toast: Option[String] = None, leave: Option[() => Unit] = None): Future[Boolean] =
    busy.set(true)
    if toast.isEmpty then error.set(None)
    action.transform { r =>
      busy.set(false)
      r match
        case Success(_) =>
          leave match
            case Some(go) => go()
            case None     => Events.refreshNow.emit(())
          Success(true)
        case TryFailure(t) =>
          toast match
            case Some(what) => Toast.danger(s"${what}失败：${Http.messageOf(t)}")
            case None       => error.set(Some(Http.messageOf(t)))
          Success(false)
    }

  /** 同 `run`，不关心结果（按钮的点击）。 */
  def fire(action: => Future[Any], toast: Option[String] = None, leave: Option[() => Unit] = None): Unit =
    run(action, toast, leave): Unit

  def errorView(title: String = "出错了"): Modifier[HtmlElement] = Ui.errorAlert(error.signal, title)
