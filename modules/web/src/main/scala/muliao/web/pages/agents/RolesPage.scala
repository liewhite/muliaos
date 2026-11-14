package muliao.web.pages.agents

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** /roles：角色把工具与资源的权限打包分给 agent。内置角色（base、admin）不能删、不能改名，只能改说明和额外规则；自建的可增删改。 */
object RolesPage:
  /** 角色表单的值。 */
  private final case class RoleForm(description: String, allow: List[String], deny: List[String])

  private object RoleForm:
    def of(r: RoleView): RoleForm = RoleForm(r.description, r.allow, r.deny)

  def view(): HtmlElement =
    val roles      = Remote("agents", "assets")(Http.call(ResourcesApi.listRoles)(()))
    val agents     = AgentsData.load().map(_.data.getOrElse(Nil))
    val candidates = Rules.candidates()
    // 建好一个后换一份空表单
    val formKey = Var(0)
    Ui.page("角色")(
      p(
        cls := "text-sm text-muted",
        "角色把工具与 AI 资源（skill、MCP、secret、记忆）的权限打包分给 agent：写 动作:种类:名字（动作 use 用、write 改，可用通配符），禁止优先。base 每个 agent 都有，收录进 base 的人人可用；其余角色在 agent 配置里勾选，从它的下一次运行起生效。新规则至少要匹配一个现有的工具或资源。"
      ),
      Ui.collapsibleForm("新建角色")(child <-- formKey.signal.map(_ => createForm(candidates, () => formKey.update(_ + 1)))),
      Ui.loading(roles)(list =>
        div(
          cls := "flex flex-col gap-2",
          child.maybe <-- list.map(l => Option.when(l.isEmpty)(Ui.emptyHint("没有角色"))),
          children <-- list.splitSeq(_.name)(r => card(r, agents, candidates))
        )
      )
    )

  private def fields(value: Var[RoleForm], candidates: Signal[List[String]]): Modifier[HtmlElement] =
    Seq(
      Fields.text("说明", value.zoom(_.description)((f, d) => f.copy(description = d))),
      Rules.field(
        "收录（允许）",
        "动作:种类:名字，可用通配符，如 use:skill:relay-host、use:secret:RELAY_*、*:memory:trading（成员能用也能改）",
        value.signal.map(_.allow),
        a => value.update(_.copy(allow = a)),
        candidates,
        showMatches = true
      ),
      Rules.field("禁止", "优先于所有允许（包括别的角色给的）", value.signal.map(_.deny), d => value.update(_.copy(deny = d)), candidates, showMatches = true)
    )

  private def createForm(candidates: Signal[List[String]], created: () => Unit): HtmlElement =
    val name  = Var("")
    val value = Var(RoleForm("", Nil, Nil))
    val act   = new Action
    def save(): Unit =
      RoleName.make(name.now().trim) match
        case Left(e) => act.error.set(Some(s"名称$e"))
        case Right(n) =>
          val v = value.now()
          act.run(Http.call(ResourcesApi.putRole)((n, RoleWrite(v.description.trim, v.allow, v.deny)))).foreach(ok => if ok then created())
    div(
      cls := "flex flex-col gap-3",
      Fields.text("名称", name, hint = Some("小写字母、数字、- 和 _"), mono = true),
      fields(value, candidates),
      act.errorView(),
      div(Ui.btn(pending = act.busy.signal)("保存", onClick --> (_ => save())))
    )

  private def secondaryChip(text: String): HtmlElement =
    span(cls := "chip chip--default chip--sm chip--secondary font-mono", text)

  private def card(r: StrictSignal[RoleView], agents: Signal[List[AgentSummary]], candidates: Signal[List[String]]): HtmlElement =
    val name    = r.now().name
    val editing = Var(false)
    val act     = new Action
    def remove(): Unit =
      Confirm
        .ask(s"删除角色「${name.value}」？", Some("删掉的角色也从各 agent 身上去掉，它们从下一次运行起失去这些权限。"), "删除", danger = true)
        .foreach(ok => if ok then act.run(Http.call(ResourcesApi.deleteRole)(name)): Unit)
    Ui.card(
      div(
        cls := "card__header",
        div(
          cls := "card__title flex flex-wrap items-center gap-2",
          span(cls := "font-mono", name.value),
          child.maybe <-- r.map(x => Option.when(x.builtin)(secondaryChip("内置")))
        ),
        p(cls := "card__description", child.text <-- r.map(_.description))
      ),
      div(
        cls := "card__content flex flex-col gap-3",
        div(
          cls := "flex flex-col gap-1 text-sm",
          div(
            cls := "flex flex-wrap items-center gap-1.5",
            span(cls := "text-muted", "收录"),
            children <-- r.map(_.allow).map {
              case Nil => List(span(cls := "text-muted", "—"))
              case as  => as.map(secondaryChip)
            }
          ),
          // 收录规则现在匹配到的（服务端逐条算）
          child.maybe <-- Rules.checks(r.map(_.allow)).map(_.values.flatMap(_.matches).toList.distinct.sorted).map(ms =>
            Option.when(ms.nonEmpty)(
              div(
                cls := "flex flex-wrap items-center gap-1.5",
                span(cls := "text-muted", s"匹配到（${ms.size}）"),
                ms.map(m => span(cls := "chip chip--default chip--sm chip--soft font-mono", m))
              )
            )
          ),
          child.maybe <-- r.map(_.deny).map(ds =>
            Option.when(ds.nonEmpty)(
              div(cls := "flex flex-wrap items-center gap-1.5", span(cls := "text-muted", "禁止"), ds.map(d => Ui.chip(d, Tone.Danger)(cls := "font-mono")))
            )
          ),
          div(
            cls := "flex flex-wrap items-center gap-1.5",
            span(cls := "text-muted", "分给"),
            children <-- r.map(x => (x.name.value, x.members)).combineWith(agents).map {
              case ("base", _, _) => List(span(cls := "text-muted", "每个 agent"))
              case (_, Nil, _)    => List(span(cls := "text-muted", "还没有 agent（在 agent 配置里勾选）"))
              case (_, ms, as) =>
                ms.map(m =>
                  as.find(_.name == m) match
                    case Some(a) => Ui.appLink(Route.Agent(a.id.value).url)(Ui.chip(m.value, Tone.Accent)())
                    case None    => Ui.chip(m.value, Tone.Accent)()
                )
            }
          )
        ),
        child.maybe <-- editing.signal.map(on => Option.when(on)(editForm(name, r, candidates, () => editing.set(false)))),
        act.errorView()
      ),
      div(
        cls := "card__footer flex gap-2",
        Ui.btn(Variant.Ghost, Size.Sm)(Icon("Pencil"), child.text <-- editing.signal.map(if _ then "取消" else "编辑"), onClick --> (_ => editing.update(!_))),
        child.maybe <-- r.map(x => Option.when(!x.builtin)(Ui.btn(Variant.Ghost, Size.Sm, pending = act.busy.signal)(Icon("Trash2"), "删除", onClick --> (_ => remove()))))
      )
    )

  /** 编辑：接口收整份，所以没改的字段用服务端最新的值，不盖掉期间别处改的。 */
  private def editForm(name: RoleName, r: Signal[RoleView], candidates: Signal[List[String]], done: () => Unit): HtmlElement =
    Draft.form(r, RoleForm.of) { draft =>
      val act = new Action
      def save(): Unit =
        val write = RoleWrite(draft.merged(_.description.trim), draft.merged(_.allow), draft.merged(_.deny))
        act.run(Http.call(ResourcesApi.putRole)((name, write))).foreach(ok => if ok then done())
      div(
        cls := "flex flex-col gap-3 border-t border-separator pt-3",
        fields(draft.value, candidates),
        act.errorView(),
        div(Ui.btn(pending = act.busy.signal)("保存", onClick --> (_ => save())))
      )
    }
