package muliao.web.pages.agents

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.pages.assets.MemoryEditor
import muliao.web.ui.*

import scala.concurrent.Future

import Http.ec

/** /agents/<编号>：配置、最终能用的工具与资源、记忆、工作历史、工作目录、后台任务。 */
object AgentDetail:
  private enum Tab(val label: String):
    case Config  extends Tab("配置")
    case Grants  extends Tab("资源")
    case Memory  extends Tab("记忆")
    case History extends Tab("历史")
    case Files   extends Tab("文件")
    case Jobs    extends Tab("后台任务")

  def view(id: String): HtmlElement =
    val agentId = AgentId.wrap(id)
    val detail  = Remote("agents", "assets")(Http.call(ResourcesApi.getAgent)(agentId))
    val tab     = Var(Tab.Config)
    // 打开过的标签页一直挂着（切走再回来，正在改的表单还在）
    val visited = Var(Set[Tab](Tab.Config))
    val name    = detail.map(_.data.fold(id)(_.summary.name.value))
    Ui.page(
      child.text <-- name,
      back = Some(Route.Agents),
      actions = child.maybe <-- detail.map(_.data.filterNot(d => AgentsData.archived(d.summary)).map(_ =>
        Ui.linkButton(s"${Route.NewSession.url}?agent=${Http.enc(id)}", Variant.Secondary)(Icon("MessageSquarePlus"), "对话")
      ))
    )(
      Ui.loading(detail) { d =>
        div(
          cls := "flex flex-col gap-3",
          Tabs[Tab]("agent 详情", tab.signal, t => { tab.set(t); visited.update(_ + t) }, Tab.values.toList.map(t => t -> t.label)),
          Tab.values.toList.map { t =>
            child.maybe <-- visited.signal.map(_.contains(t)).distinct.map(v =>
              Option.when(v)(
                div(
                  cls <-- tab.signal.map(cur => if cur == t then "flex flex-col gap-3" else "hidden"),
                  role := "tabpanel",
                  panel(t, agentId, d)
                )
              )
            )
          }
        )
      }
    )

  private def panel(t: Tab, id: AgentId, d: Signal[AgentDetail]): HtmlElement = t match
    case Tab.Config  => config(id, d)
    case Tab.Grants  => grants(d)
    case Tab.Memory  => memory(d)
    case Tab.History => AgentHistory.view(id)
    case Tab.Files   => FileBrowser.view(workspace(id), "工作目录", "工作目录是空的")
    case Tab.Jobs    => AgentJobs.view(id)

  def workspace(id: AgentId): DirSource = new DirSource:
    def list(path: String): Future[DirContent] =
      Http.call(WorkspaceApi.listDir)((id, Option(path).filter(_.nonEmpty))).map(l =>
        DirContent(l.entries.map(e => FileEntry(e.name, e.dir, Option.when(!e.dir)(e.size), Some(e.modifiedAt))), l.truncated)
      )
    def read(path: String): Future[Array[Byte]] = Http.call(WorkspaceApi.workspaceFile)((id, path)).map(_._3)
    def url(path: String): String               = s"/api/agents/${Http.enc(id.value)}/workspace/file?path=${Http.enc(path)}"

  // ---- 配置 ----

  private def config(id: AgentId, d: Signal[AgentDetail]): HtmlElement =
    val act = new Action
    def update(patch: AgentUpdate, done: String): Unit =
      act.run(Http.call(ResourcesApi.updateAgent)((id, patch))).foreach(ok => if ok then Toast.success(done))
    def name = d.map(_.summary.name.value)
    val now  = Var(Option.empty[AgentDetail])
    def current: AgentDetail = now.now().get

    def toggleArchived(): Unit =
      val on = !AgentsData.archived(current.summary)
      val ask =
        if on then Confirm.ask(s"归档 agent「${current.summary.name.value}」？", Some("归档后不能对话、委派、指派 issue，它的触发器不触发；会话、记忆等数据都保留，取消归档即恢复。"), "归档")
        else Future.successful(true)
      ask.foreach(ok => if ok then update(AgentUpdate(archived = Some(on)), if on then "已归档" else "已取消归档"))

    def remove(): Unit =
      Confirm
        .ask(
          s"删除 agent「${current.summary.name.value}」？",
          Some("它的会话、触发器、工作目录和私有资源都会被删除（被别人用着的资源转给平台），无法恢复；用量与工作历史保留。它忙时不能删。"),
          "删除",
          danger = true
        )
        .foreach(ok => if ok then act.run(Http.call(ResourcesApi.deleteAgent)(id), leave = Some(() => Router.go(Route.Agents))): Unit)

    div(
      cls := "flex flex-col gap-3",
      d --> (x => now.set(Some(x))),
      Ui.errorAlert(d.map(_.profileError), "配置未加载，不能运行"),
      Ui.errorAlert(d.map(_.accessBroken), "权限文件 access.toml 写坏了（沿用上一份有效的）"),
      child.maybe <-- d.map(_.problems).map(ps => Option.when(ps.nonEmpty)(problemsView(ps))),
      Ui.card(
        div(
          cls := "card__content flex flex-col gap-2 text-sm",
          div(
            cls := "flex flex-wrap items-center gap-2",
            span(cls := "text-muted", "编号"),
            code(cls := "select-all rounded bg-surface-secondary px-1.5 py-0.5 font-mono", id.value),
            Ui.btn(Variant.Ghost, Size.Sm)(Icon("Copy"), "复制", onClick --> (_ => Ui.copyText(id.value, "编号")))
          ),
          div(
            cls := "text-muted",
            child <-- name.map(n => Ui.appLink(s"${Route.Assets.url}?owner=${Http.enc(n)}")(cls := "text-accent", "它名下的 AI 资产"))
          ),
          div(cls := "break-all text-xs text-muted", "工作目录 ", span(cls := "font-mono", child.text <-- d.map(_.workspace))),
          div(
            cls := "flex flex-wrap items-center gap-2",
            span(cls := "text-muted", child.text <-- d.map(x => if x.summary.isDefault then "系统默认 agent：新会话默认选中它" else "不是默认 agent")),
            Ui.btn(Variant.Ghost, Size.Sm, pending = act.busy.signal)(
              Icon("Star"),
              child.text <-- d.map(x => if x.summary.isDefault then "取消默认" else "设为默认"),
              onClick --> { _ =>
                val on = !current.summary.isDefault
                update(AgentUpdate(isDefault = Some(on)), if on then "已设为默认 agent" else "已取消默认")
              }
            )
          ),
          div(
            cls := "flex flex-wrap items-center gap-2",
            span(cls := "text-muted", child.text <-- d.map(x => if AgentsData.archived(x.summary) then "已归档：不能对话、委派、指派，触发器不触发" else "未归档")),
            Ui.btn(Variant.Ghost, Size.Sm, pending = act.busy.signal)(
              child <-- d.map(x => if AgentsData.archived(x.summary) then Icon("ArchiveRestore") else Icon("Archive")),
              child.text <-- d.map(x => if AgentsData.archived(x.summary) then "取消归档" else "归档"),
              onClick --> (_ => toggleArchived())
            )
          )
        )
      ),
      Ui.card(
        div(
          cls := "card__content flex flex-col gap-3",
          child <-- d.map(_.profileError.isDefined).distinct.map {
            case true  => p(cls := "text-sm text-danger", "配置没加载，修好它的 agent.toml 后才能在这里改（可在 AI 资产 · 配置文件里改）")
            case false => editForm(id, d)
          }
        )
      ),
      act.errorView(),
      div(Ui.btn(Variant.DangerSoft, pending = act.busy.signal)(Icon("Trash2"), "删除 agent", onClick --> (_ => remove())))
    )

  private def editForm(id: AgentId, d: Signal[AgentDetail]): HtmlElement =
    val thinkings = Remote.once(Http.call(ResourcesApi.agentDefaults)(()).map(_.thinking))
    Draft.form(d, AgentForm.of) { draft =>
      val act = new Action
      def save(): Unit =
        act
          .run(Http.call(ResourcesApi.updateAgent)((id, AgentForm.patch(draft))).map { saved =>
            draft.saved(AgentForm.of(saved))
            Toast.success("已保存")
          })
          .foreach(_ => ())
      div(
        cls := "flex flex-col gap-4",
        Ui.loading(thinkings)(ts => child <-- ts.distinct.map(t => div(cls := "flex flex-col gap-4", AgentForm.fields(Some(id), draft.value, t)))),
        act.errorView(),
        div(
          cls := "flex flex-wrap gap-2",
          button(
            tpe := "button",
            cls := Ui.buttonCls(),
            disabled <-- draft.dirty.combineWith(act.busy.signal).map((dirty, b) => !dirty || b),
            child.maybe <-- act.busy.signal.map(b => Option.when(b)(Ui.spinner("spinner--sm spinner--current"))),
            "保存",
            onClick --> (_ => save())
          ),
          child.maybe <-- draft.dirty.map(dirty => Option.when(dirty)(Ui.btn(Variant.Ghost)("放弃修改", onClick --> (_ => draft.discard()))))
        )
      )
    }

  def problemsView(ps: List[ProblemView]): HtmlElement =
    div(
      cls := "alert alert--warning",
      role := "alert",
      div(cls := "alert__indicator", Icon("TriangleAlert", "size-5")),
      div(
        cls := "alert__content",
        div(cls := "alert__title", s"有 ${ps.size} 个资源文件没加载"),
        div(
          cls := "alert__description flex flex-col gap-1",
          ps.map(p => div(span(cls := "font-mono break-all", p.path), "：", p.reason))
        )
      )
    )

  // ---- 资源 ----

  private def grants(d: Signal[AgentDetail]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      p(
        cls := "text-sm text-muted",
        "它运行时能用的工具与资源，以及凭哪条规则（「用」与「改」各一条）。改分发在「配置」，改内容在 ",
        Ui.appLink(Route.Assets.url)(cls := "text-accent", "AI 资产"),
        "。"
      ),
      child.maybe <-- d.map(x =>
        Option.when(x.writablePaths.nonEmpty)(
          Ui.card(div(cls := "card__content text-sm", span(cls := "text-muted", "还能写这些目录："), span(cls := "font-mono break-all", x.writablePaths.mkString("、"))))
        )
      ),
      Grants.full(d.map(_.tools), d.map(_.assets))
    )

  // ---- 记忆 ----

  private def memory(d: Signal[AgentDetail]): HtmlElement =
    div(
      cls := "flex flex-col gap-4",
      // 改名后私有记忆的文件跟着挪到新名字的目录
      child <-- d.map(_.summary.name.value).distinct.map(n =>
        MemoryEditor.view(s"$n/memory.md", n, "agent 每次运行都会看到它能用的全部记忆，也会自行修改。")
      ),
      div(
        cls := "flex flex-col gap-1 text-sm text-muted",
        child.text <-- d.map(x => x.assets.filter(_.kind == AssetKind.Memory).map(_.name) match
          case Nil => "它没有别的记忆可用。"
          case ms  => s"它还能用这些记忆：${ms.mkString("、")}。"
        ),
        div("全局与共享记忆在 ", Ui.appLink(s"${Route.Assets.url}?kind=memory")(cls := "text-accent", "AI 资产 · 记忆"), " 里改。")
      )
    )
