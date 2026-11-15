package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

/** 资源页各部分共用的小组件：提交按钮、归属下拉、归属与使用者、转移归属、记忆编辑。 */
object AssetUi:
  val Platform = "platform"

  /** 主按钮：执行中转圈，`enabled` 为假时禁用。 */
  def submit(text: String, pending: Signal[Boolean], enabled: Signal[Boolean] = Val(true), variant: Variant = Variant.Primary, size: Size = Size.Md)(
      mods: Modifier[Button]*
  ): Button =
    button(
      tpe := "button",
      cls := Ui.buttonCls(variant, size),
      dataAttr("slot") := "button",
      disabled <-- pending.combineWith(enabled).map((p, e) => p || !e),
      dataAttr("pending") <-- pending.map(_.toString),
      child.maybe <-- pending.map(p => Option.when(p)(Ui.spinner("spinner--sm spinner--current"))),
      text,
      mods
    )

  def ownerLabel(owner: String): String = if owner == Platform then "平台" else owner

  /** 归属下拉：平台或某个 agent。`withArchived`：也列出归档的（转移时资源可能正归它们）。 */
  def ownerSelect(value: Signal[String], onPick: String => Unit, agents: Signal[List[AgentSummary]], withArchived: Boolean = false): HtmlElement =
    Fields.select[String](
      "归属（owner）",
      value.map(Some(_)),
      onPick,
      agents.map(as =>
        Opt(Platform, "平台") :: as
          .filter(a => withArchived || !AgentsData.archived(a))
          .map(a => Opt(a.name.value, if AgentsData.archived(a) then s"${a.name.value}（已归档）" else a.name.value))
      ),
      hint = Some(child.text <-- value.map {
        case Platform => "归平台的资源要收录进某个角色（如 base）才有 agent 能用"
        case _        => "这个 agent 自己总能用；要给别的 agent，把它收录进角色"
      })
    )

  /** agent 名链到它的详情页（找不到编号时只显示名字）。 */
  def agentChip(name: String, agents: Signal[List[AgentSummary]], tone: Tone = Tone.Accent): HtmlElement =
    span(
      child <-- agents.map(_.find(_.name.value == name)).map {
        case Some(a) => Ui.appLink(Route.Agent(a.id.value).url)(Ui.chip(name, tone)())
        case None    => Ui.chip(name, tone)()
      }
    )

  /** 能转移归属的种类。 */
  def transferable(kind: AssetItemKind): Boolean = kind.asset.exists(k => k == AssetKind.Skill || k == AssetKind.Mcp || k == AssetKind.Secret)

  /** 资源归谁、被谁用；能转的可以转移归属（挪文件，内容不变）。 */
  def access(item: Signal[AssetItem], agents: Signal[List[AgentSummary]]): HtmlElement =
    val transferring = Var(false)
    div(
      cls := "flex flex-col gap-2",
      div(
        cls := "flex flex-wrap items-center gap-1.5 text-xs",
        child <-- item.map(_.owner).distinct.map {
          case Platform => span(cls := "chip chip--default chip--sm chip--secondary", "平台")
          case o        => agentChip(o, agents)
        },
        child <-- item.map(_.usedBy).distinct.map {
          case Nil => Ui.chip("没有 agent 在用", Tone.Warning)()
          case us =>
            span(cls := "contents", span(cls := "text-muted", "被这些 agent 使用"), us.map(u => agentChip(u.value, agents, Tone.Default)))
        },
        child.maybe <-- item.map(i => transferable(i.kind)).distinct.map(t =>
          Option.when(t)(
            button(
              tpe := "button",
              cls := Ui.buttonCls(Variant.Ghost, Size.Sm) + " h-6 px-2 text-xs text-accent",
              onClick --> (_ => transferring.update(!_)),
              child.text <-- transferring.signal.map(if _ then "取消转移" else "转移归属")
            )
          )
        )
      ),
      child.maybe <-- transferring.signal.map(on => Option.when(on)(transferForm(item, agents, () => transferring.set(false))))
    )

  private def transferForm(item: Signal[AssetItem], agents: Signal[List[AgentSummary]], done: () => Unit): HtmlElement =
    val target = Var(Option.empty[String])
    val act    = new Action
    val now    = Var(Option.empty[AssetItem])
    div(
      cls := "flex flex-col gap-2 rounded-lg border border-border p-3",
      item --> (i => now.set(Some(i))),
      ownerSelect(target.signal.combineWith(item).map((t, i) => t.getOrElse(i.owner)), o => target.set(Some(o)), agents, withArchived = true),
      div(
        submit("转移", act.busy.signal, target.signal.combineWith(item).map((t, i) => t.exists(_ != i.owner)), size = Size.Sm)(
          onClick --> { _ =>
            for i <- now.now(); to <- target.now() do
              i.kind.asset.foreach(k => act.run(Http.call(ResourcesApi.transferAsset)((k, i.name, AssetTransfer(to)))).foreach(ok => if ok then done())(using Http.ec))
          }
        )
      ),
      act.errorView()
    )

/** 记忆的编辑：字数提示（上限由服务端配置，超了保存会被拒绝并说明）。 */
object MemoryEditor:
  /** 服务端默认的记忆上限；实际上限以服务端配置为准（接口没给出）。 */
  val DefaultLimit = 4000

  /** 服务端按规整后的字符数算；这里按首尾去空白后的字符（码点）数估计。 */
  def length(text: String): Int =
    val t = text.trim
    t.codePointCount(0, t.length)

  def view(path: String, name: String, hint: String): HtmlElement =
    FileEditor.view(
      path,
      s"记忆 $name",
      hint = content =>
        Seq(
          span(
            cls <-- content.map(c => if length(c) > DefaultLimit then "text-danger" else ""),
            child.text <-- content.map(c => s"${length(c)} 字（每块上限默认 $DefaultLimit 字，超出时保存会被拒绝）。")
          ),
          span(hint)
        ),
      saveLabel = "保存记忆"
    )
