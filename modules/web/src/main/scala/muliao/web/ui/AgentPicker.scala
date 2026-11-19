package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*
import muliao.contract.AgentSummary

/** 选 agent 的下拉（所有选 agent 的地方都用它）：一级是顶部的固定项（如「平台」「我」「全部」）、各分组（点开展开，
  * 当前值所在的组默认展开）和未分组的 agent；有搜索词时平铺按名称/描述匹配到的项并标出分组。
  * 值的类型由调用方定：agent 编号、agent 名，或「我」「未指派」与 agent 合在一起的选择；`key` 给出每个 agent 对应的值。
  */
object AgentPicker:
  /** 归档的 agent 怎么列。 */
  enum Archived:
    /** 不列（派活、定归属）：当前值归档了仍显示在按钮上，由服务端在使用时拒绝并说明。 */
    case Hidden

    /** 单独一组「已归档」列在最后（筛选、转移归属：它们可能正持有东西）。 */
    case Listed

  /** @param fixed 列在最前的非 agent 选项
    * @param prefix 每个 agent 名字前的小标记（如 owner 色点），参数是全部 agent 与这一个
    */
  def apply[T](
      labelText: String,
      value: Signal[Option[T]],
      onPick: T => Unit,
      agents: Signal[List[AgentSummary]],
      key: AgentSummary => T,
      fixed: List[Opt[T]] = Nil,
      archived: Archived = Archived.Hidden,
      prefix: Option[(List[AgentSummary], AgentSummary) => HtmlElement] = None,
      hint: Option[Modifier[HtmlElement]] = None,
      placeholderText: String = "选择 agent",
      disabledIf: Signal[Boolean] = Val(false)
  ): HtmlElement =
    // 当前值对应的 agent：在全部（含归档的）里找，归档了也显示得出
    val chosen = agents.combineWith(value).map((all, v) => all.find(a => v.contains(key(a))))
    val shown = value.combineWith(chosen, agents).map((v, c, all) =>
      fixed.find(o => v.contains(o.id)).map(Fields.optionBody).orElse(c.map(agentLine(all, _, prefix, showGroup = true, firstLine = false)))
    )
    Dropdown(labelText, shown, placeholderText, hint, disabledIf, "w-[min(28rem,90vw)]") { close =>
      val query   = Var("")
      // 点过的组；当前值所在的组默认展开，点它是收起
      val toggled = Var(Set.empty[String])
      val current = chosen.map(_.flatMap(folderOf))
      def pick(x: T): Unit =
        onPick(x)
        close()
      def agentItem(all: List[AgentSummary], a: AgentSummary, showGroup: Boolean, selected: Option[T]): HtmlElement =
        val on = selected.contains(key(a))
        button(
          tpe := "button",
          role := "option",
          title := a.description.getOrElse(""),
          cls := "flex w-full items-start gap-2 rounded-lg px-2 py-1.5 text-left text-sm hover:bg-default",
          aria.selected := on,
          onClick --> (_ => pick(key(a))),
          agentLine(all, a, prefix, showGroup, firstLine = true).amend(cls := "flex-1"),
          Option.when(on)(Icon("Check", "size-4 shrink-0 text-accent"))
        )
      def folder(name: String, members: List[AgentSummary], isOpen: Boolean, all: List[AgentSummary], sel: Option[T]): List[HtmlElement] =
        button(
          tpe := "button",
          cls := "flex w-full items-center gap-2 rounded-lg px-2 py-1.5 text-left text-sm hover:bg-default",
          aria.expanded := isOpen,
          onClick --> (_ => toggled.update(s => if s(name) then s - name else s + name)),
          Icon(if name == ArchivedGroup then "Archive" else "Folder", "size-4 shrink-0 text-muted"),
          span(cls := "flex-1 truncate", name),
          span(cls := "text-xs text-muted", members.size.toString),
          Icon(if isOpen then "ChevronDown" else "ChevronRight", "size-4 text-muted")
        ) :: (if isOpen then members.map(a => agentItem(all, a, showGroup = false, sel).amend(cls := "pl-8")) else Nil)
      Seq(
        div(
          cls := "relative p-2",
          Icon("Search", "pointer-events-none absolute top-1/2 left-5 size-4 -translate-y-1/2 text-muted"),
          input(
            tpe := "search",
            cls := "input input--secondary input--full-width pl-9",
            placeholder := "搜索名称或描述",
            aria.label := s"搜索 $labelText",
            onMountFocus,
            controlled(com.raquo.laminar.api.L.value <-- query.signal, onInput.mapToValue --> query)
          )
        ),
        div(
          cls := "max-h-96 min-h-0 overflow-y-auto p-1",
          role := "listbox",
          children <-- agents.combineWith(query.signal, toggled.signal, current, value).map { (all, q, tog, cur, sel) =>
            def isOpen(name: String) = tog(name) != cur.contains(name)
            val fixedItems = (os: List[Opt[T]]) => os.map(o => Fields.optionButton(o, sel.contains(o.id), () => pick(o.id)))
            val g          = AgentsData.group(all)
            val arch       = archived match
              case Archived.Hidden => Nil
              case Archived.Listed => g.archived
            if q.trim.nonEmpty then
              val qq    = q.trim.toLowerCase
              val found = fixedItems(fixed.filter(_.label.toLowerCase.contains(qq))) ++
                AgentsData.search(g.ungrouped ++ g.groups.flatMap(_._2) ++ arch, q).map(agentItem(all, _, showGroup = true, sel))
              if found.isEmpty then List(div(cls := "px-3 py-4 text-center text-sm text-muted", "没有匹配的项"))
              else found
            else
              fixedItems(fixed) ++
                g.groups.flatMap((name, as) => folder(name, as, isOpen(name), all, sel)) ++
                g.ungrouped.map(agentItem(all, _, showGroup = false, sel)) ++
                Option.when(arch.nonEmpty)(folder(ArchivedGroup, arch, isOpen(ArchivedGroup), all, sel)).toList.flatten
          }
        )
      )
    }

  private val ArchivedGroup = "已归档"

  /** agent 在列表里属于哪个可折叠的组：归档的在「已归档」，未分组的不在组里。 */
  private def folderOf(a: AgentSummary): Option[String] =
    if AgentsData.archived(a) then Some(ArchivedGroup) else a.group

  /** 一个 agent 的显示：前缀、名字、分组（`showGroup`）、归档标记，`firstLine` 时下面带描述的第一行。 */
  private def agentLine(
      all: List[AgentSummary],
      a: AgentSummary,
      prefix: Option[(List[AgentSummary], AgentSummary) => HtmlElement],
      showGroup: Boolean,
      firstLine: Boolean
  ): HtmlElement =
    div(
      cls := "min-w-0",
      div(
        cls := "flex min-w-0 items-center gap-2",
        prefix.map(_(all, a)),
        span(cls := "truncate font-medium", a.name.value),
        Option.when(showGroup)(a.group.map(g => span(cls := "shrink-0 text-xs text-muted", g))).flatten,
        Option.when(AgentsData.archived(a))(span(cls := "shrink-0 text-xs text-muted", ArchivedGroup))
      ),
      Option.when(firstLine)(a.description.map(d => div(cls := "truncate text-xs text-muted", d.linesIterator.nextOption().getOrElse("")))).flatten
    )
