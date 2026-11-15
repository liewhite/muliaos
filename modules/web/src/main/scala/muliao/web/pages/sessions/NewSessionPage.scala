package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** 选 agent：一级是各分组（点开展开）和未分组的 agent；有搜索词时平铺按名称/描述匹配到的 agent 并标出分组。
  * 菜单只列可选的（不含归档的）；当前值归档了仍显示在按钮上，由服务端在使用时拒绝并说明。
  */
object AgentPicker:
  def apply(labelText: String, agents: Signal[List[AgentSummary]], selected: Signal[Option[AgentId]], onPick: AgentId => Unit, placeholderText: String = "选择 agent"): HtmlElement =
    val open     = Var(false)
    val query    = Var("")
    val expanded = Var(Set.empty[String])
    val current  = agents.combineWith(selected).map((as, v) => v.flatMap(id => as.find(_.id == id)))
    def choose(id: AgentId): Unit =
      onPick(id)
      query.set("")
      open.set(false)
    def item(a: AgentSummary, showGroup: Boolean, selected: Option[AgentId]): HtmlElement =
      button(
        tpe := "button",
        role := "option",
        title := a.description.getOrElse(""),
        cls := "flex w-full items-start gap-2 rounded-lg px-2 py-1.5 text-left text-sm hover:bg-default",
        aria.selected := selected.contains(a.id),
        onClick --> (_ => choose(a.id)),
        div(
          cls := "min-w-0 flex-1",
          div(cls := "flex items-center gap-2", span(cls := "truncate font-medium", a.name.value), Option.when(showGroup)(a.group.map(g => span(cls := "shrink-0 text-xs text-muted", g))).flatten),
          a.description.map(d => div(cls := "truncate text-xs text-muted", d.linesIterator.nextOption().getOrElse("")))
        ),
        Option.when(selected.contains(a.id))(Icon("Check", "size-4 shrink-0 text-accent"))
      )
    div(
      cls := "select select--full-width select--secondary relative",
      span(cls := "label", labelText),
      button(
        tpe := "button",
        cls := "select__trigger select__trigger--full-width",
        aria.expanded <-- open.signal,
        onClick --> { e => e.stopPropagation(); open.update(!_) },
        span(
          cls := "select__value flex min-w-0 items-center gap-2",
          child <-- current.map {
            case Some(a) => span(cls := "flex min-w-0 items-center gap-2", span(cls := "truncate", a.name.value), a.group.map(g => span(cls := "shrink-0 text-xs text-muted", g)))
            case None    => span(cls := "text-muted", placeholderText)
          }
        ),
        Icon("ChevronDown", "select__indicator size-4")
      ),
      child.maybe <-- open.signal.map(o =>
        Option.when(o)(
          div(
            cls := "absolute left-0 top-full z-50 mt-1 flex w-[min(28rem,90vw)] flex-col rounded-xl bg-overlay shadow-lg ring-1 ring-separator",
            // 点到外面就收起：弹层里的点击不冒泡到 document（点分组会重画列表，被点的元素已离开 DOM，不能再按祖先判断）
            onClick --> (_.stopPropagation()),
            documentEvents(_.onClick) --> (_ => open.set(false)),
            div(
              cls := "relative p-2",
              Icon("Search", "pointer-events-none absolute top-1/2 left-5 size-4 -translate-y-1/2 text-muted"),
              input(
                tpe := "search",
                cls := "input input--secondary input--full-width pl-9",
                placeholder := "搜索名称或描述",
                aria.label := "搜索 agent",
                onMountFocus,
                controlled(value <-- query.signal, onInput.mapToValue --> query)
              )
            ),
            div(
              cls := "max-h-96 overflow-y-auto p-1",
              role := "listbox",
              children <-- agents.combineWith(query.signal, expanded.signal, selected).map { (all, q, exp, sel) =>
                val active = AgentsData.selectable(all)
                if q.trim.nonEmpty then
                  AgentsData.search(active, q) match
                    case Nil   => List(div(cls := "px-3 py-4 text-center text-sm text-muted", "没有匹配的 agent"))
                    case found => found.map(item(_, showGroup = true, sel))
                else
                  val g = AgentsData.group(active)
                  g.groups.flatMap { (name, as) =>
                    val isOpen = exp(name)
                    button(
                      tpe := "button",
                      cls := "flex w-full items-center gap-2 rounded-lg px-2 py-1.5 text-left text-sm hover:bg-default",
                      aria.expanded := isOpen,
                      onClick --> (_ => expanded.update(s => if s(name) then s - name else s + name)),
                      Icon("Folder", "size-4 shrink-0 text-muted"),
                      span(cls := "flex-1 truncate", name),
                      span(cls := "text-xs text-muted", as.size.toString),
                      Icon(if isOpen then "ChevronDown" else "ChevronRight", "size-4 text-muted")
                    ) :: (if isOpen then as.map(a => item(a, showGroup = false, sel).amend(cls := "pl-8")) else Nil)
                  } ++ g.ungrouped.map(item(_, showGroup = false, sel))
              }
            )
          )
        )
      )
    )

/** 新会话页：选 agent（地址 `?agent=` 或默认 agent）、写消息、附件、语音，开始后跳到会话页。 */
object NewSessionPage:
  /** @param preset 预选的 agent（地址 `?agent=`） */
  def view(preset: Option[AgentId]): HtmlElement =
    val agents  = AgentsData.all
    val chosen  = Var(preset)
    val prompt  = Var("")
    val uploads = new Uploads
    val action  = new Action
    // 新会话还要选 agent：转写结果先填进输入框，由用户点「开始」
    val voice   = new Voice(text => prompt.update(p => Model.appendTranscript(p, text)))
    val list    = AgentsData.list
    val agent   = chosen.signal.combineWith(list).map((c, as) => c.orElse(as.find(_.isDefault).map(_.id)))
    val canStart = agent.combineWith(prompt.signal, uploads.ids, uploads.blocking).map((a, p, ids, b) => a.isDefined && (p.trim.nonEmpty || ids.nonEmpty) && !b)
    val canNow = Var(false)
    val agentNow = Var(Option.empty[AgentId])
    def start(): Unit =
      // 快捷键不经过按钮的禁用状态
      if !action.busy.now() && canNow.now() then
        agentNow.now().foreach { a =>
          action
            .run(
              Http.call(SessionsApi.create)(NewSession(a, prompt.now().trim, uploads.done.map(_._2.id))).map(c => Router.update(Route.session(c.id))),
              leave = Some(() => ())
            )
            .foreach(_ => ())
        }
    Ui.page("新会话", back = Some(Route.home))(
      canStart --> canNow,
      agent --> agentNow,
      Ui.loading(agents) { data =>
        div(
          cls := "contents",
          child <-- data.map(_.isEmpty).distinct.map {
            case true =>
              div(cls := "flex flex-col items-center gap-3", Ui.emptyHint("还没有 agent。"), Ui.linkButton(Route.agentNew.url)("创建 agent"))
            case false =>
              Ui.card(
                div(
                  cls := "card__content flex flex-col gap-4",
                  AgentPicker("Agent", data, agent, a => chosen.set(Some(a))),
                  div(uploads.onPasteFiles, Fields.area("消息", prompt, rowsN = 6, autoFocusOn = true, onSubmit = Some(() => start()))),
                  uploads.pendingView,
                  action.errorView(),
                  Ui.errorAlert(voice.error.signal),
                  div(
                    cls := "flex items-center gap-2",
                    uploads.attachButton(action.busy.signal),
                    voice.button(action.busy.signal),
                    Ui.btn(pending = action.busy.signal, disabledIf = canStart.map(!_))("开始", onClick --> (_ => start()))
                  )
                )
              )
          }
        )
      }
    )
