package muliao.web.pages.issues

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.ui.*

import IssueModel.*

/** issue 的小部件：状态圆点、负责项目的色点与胶囊、负责项目与指派人的下拉框（看板与 issue 页共用）。 */
object IssueParts:
  def statusDot(s: IssueStatus): HtmlElement =
    span(cls := s"inline-block size-3 shrink-0 rounded-full border-2 ${statusColor(s)}", aria.hidden := true)

  /** 负责项目的色点；不着色时是灰点。 */
  def ownerDot(hue: Option[Int]): HtmlElement = hue match
    case Some(h) => span(cls := "owner-color inline-block size-2.5 shrink-0 rounded-full bg-(--owner-fg)", styleAttr := ownerStyle(h), aria.hidden := true)
    case None    => span(cls := "inline-block size-2.5 shrink-0 rounded-full bg-muted", aria.hidden := true)

  /** 负责项目的胶囊：浅色底 + owner 色文字。 */
  def ownerChip(hue: Option[Int], name: String): HtmlElement =
    span(
      hue match
        case Some(h) => Seq(cls := "owner-color inline-flex items-center gap-1.5 rounded-full bg-(--owner-bg) px-2 py-0.5 text-xs font-medium text-(--owner-fg)", styleAttr := ownerStyle(h))
        case None    => Seq(cls := "inline-flex items-center gap-1.5 rounded-full bg-default px-2 py-0.5 text-xs font-medium text-muted")
      ,
      ownerDot(hue),
      name
    )

  /** 左侧色条用 owner 色的卡片类与样式。 */
  def ownerBar(hue: Option[Int]): Seq[Modifier[HtmlElement]] = hue match
    case Some(h) => Seq(cls := "owner-color border-l-(--owner-fg)", styleAttr := ownerStyle(h))
    case None    => Seq(cls := "border-l-transparent")

  /** 选负责项目时每个 agent 前的色点。 */
  val ownerPrefix: Option[(List[AgentSummary], AgentSummary) => HtmlElement] = Some((all, a) => ownerDot(ownerHue(all, Some(a.id))))

  /** 指派人里 agent 以外的两项。归档的 agent 不能被指派（选择器不列，当前指派人归档了仍显示在按钮上）。 */
  val assigneeFixed: List[Opt[AssigneeChoice]] = List(Opt(AssigneeChoice.Nobody, "未指派"), Opt(AssigneeChoice.Me, "我"))

  def statusOptions: List[Opt[IssueStatus]] = Opt.of(Statuses)

  def priorityOptions: List[Opt[Priority]] = Opt.of(Priorities)

  /** 依赖的 issue 编号输入框，写了不是编号的内容时下面提示（解析见 [[IssueModel.parseRefs]]）。 */
  def depsField(labelText: String, deps: Var[String]): Modifier[HtmlElement] =
    Seq(
      Fields.text(labelText, deps),
      child.maybe <-- deps.signal.map(parseRefs).map(r => Option.when(r.isEmpty)(p(cls := "text-xs text-danger", "只能填 issue 编号，如 #12, #15")))
    )

  /** 折叠起来的 issue 的展开 / 收起（看板的列、列表的组）；不超过 FoldLimit 个时不显示。 */
  def foldToggle(total: Signal[Int], expanded: Var[Boolean]): Modifier[HtmlElement] =
    child.maybe <-- total.combineWith(expanded.signal).map((n, e) =>
      Option.when(n > FoldLimit)(
        button(
          tpe := "button",
          cls := "self-start px-1 py-1 text-xs text-muted hover:text-foreground",
          onClick --> (_ => expanded.update(!_)),
          if e then "收起" else s"展开其余 ${n - FoldLimit} 个"
        )
      )
    )

