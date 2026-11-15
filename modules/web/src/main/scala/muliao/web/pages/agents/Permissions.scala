package muliao.web.pages.agents

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*


/** 资源种类在界面上的名字。 */
extension (k: AssetKind)
  def shownName: String = k match
    case AssetKind.Tool   => "工具"
    case AssetKind.Skill  => "Skill"
    case AssetKind.Mcp    => "MCP"
    case AssetKind.Secret => "Secret"
    case AssetKind.Memory => "记忆"

/** 权限规则（`动作:种类:名字`）的输入、逐条校验，以及按规则算出的「能用什么」。规则语法与匹配只在服务端，这里只显示。 */
object Rules:
  /** 补全候选：每种的 `use:<种类>:*`、每个工具与资源的 `use:<种类>:<名字>`。工具名取自规则 `use:tool:*` 匹配到的。 */
  def candidates(): Signal[List[String]] =
    val tools  = Remote(ResourcesApi.checkRules)(RulesRequest(List("use:tool:*"))).map(_.map(_.flatMap(_.matches)))
    val assets = Remote(ResourcesApi.listAssets)(()).map(_.map(_.items))
    tools.combineWith(assets).map { (t, a) =>
      val wild  = AssetKind.values.toList.map(k => s"use:${k.label}:*")
      val named = t.data.getOrElse(Nil).map(n => s"use:tool:$n") ++
        a.data.getOrElse(Nil).flatMap(i => i.kind.asset.map(k => s"use:${k.label}:${i.name}"))
      (wild ++ named).distinct
    }

  /** 逐条校验（随规则变化、稍等片刻再查）。 */
  def checks(rules: Signal[List[String]]): Signal[Map[String, RuleCheck]] =
    rules.distinct
      .composeUpdates(_.debounce(300))
      .flatMapSwitch {
        case Nil   => Val(Map.empty[String, RuleCheck])
        case rules => Http.stream(Http.call(ResourcesApi.checkRules)(RulesRequest(rules))).collect { case scala.util.Success(cs) => cs.map(c => c.rule -> c).toMap }.toSignal(Map.empty)
      }

  private var counter = 0

  /** 一组规则：已有的是可删的标签（带校验结果），输入框从候选里补全，也可以直接写通配符，回车加入。
    * @param showMatches 每条下面列出它匹配到的工具与资源（角色页）；否则只在标签上标出错与没匹配到
    */
  def field(
      labelText: String,
      hint: String,
      value: Signal[List[String]],
      update: List[String] => Unit,
      candidates: Signal[List[String]],
      showMatches: Boolean = false
  ): HtmlElement =
    counter += 1
    val listId = s"rules-$counter"
    val text   = Var("")
    val current = Var(List.empty[String])
    def add(): Unit =
      val r = text.now().trim
      if r.nonEmpty && !current.now().contains(r) then update(current.now() :+ r)
      text.set("")
    val checked = checks(value)
    div(
      cls := "flex flex-col gap-1.5",
      value --> current,
      div(
        cls := "flex w-full flex-col gap-1",
        label(cls := "label", forId := s"$listId-in", labelText),
        div(
          cls := "flex gap-2",
          input(
            idAttr := s"$listId-in",
            cls := "input input--secondary input--full-width font-mono",
            placeholder := "动作:种类:名字，回车加入",
            Ui.attr("list") := listId,
            Ui.attr("autocomplete") := "off",
            controlled(com.raquo.laminar.api.L.value <-- text.signal, onInput.mapToValue --> text),
            onKeyDown.filter(e => e.key == "Enter" && !e.isComposing) --> { e => e.preventDefault(); add() }
          ),
          Ui.btn(Variant.Secondary, Size.Md, iconOnly = true, label = s"加入$labelText")(Icon("Plus"), onClick --> (_ => add()))
        ),
        htmlTag("datalist")(
          idAttr := listId,
          children <-- candidates.combineWith(value).map((cs, v) => cs.filterNot(v.contains).map(c => option(com.raquo.laminar.api.L.value := c)))
        ),
        div(cls := "description", hint)
      ),
      div(
        cls := "flex flex-col gap-1",
        children <-- value.splitSeq(identity)(r => ruleRow(r.now(), checked.map(_.get(r.now())), showMatches, () => update(current.now().filterNot(_ == r.now()))))
      )
    )

  private def ruleRow(rule: String, check: Signal[Option[RuleCheck]], showMatches: Boolean, remove: () => Unit): HtmlElement =
    div(
      cls := "flex flex-col gap-0.5",
      div(
        cls := "flex flex-wrap items-center gap-1.5",
        span(
          cls <-- check.map(c => "chip chip--sm font-mono " + toneOf(c)),
          span(cls := "chip__label", rule),
          button(
            tpe := "button",
            cls := "ms-1 inline-flex size-4 items-center justify-center rounded-full hover:bg-default",
            aria.label := s"去掉 $rule",
            onClick --> (_ => remove()),
            Icon("X", "size-3")
          )
        ),
        child.maybe <-- check.map(_.map {
          case RuleCheck(_, Some(err), _) => span(cls := "text-xs text-danger", err)
          case RuleCheck(_, None, Nil)    => span(cls := "text-xs text-warning", "没有匹配到任何工具或资源")
          case RuleCheck(_, None, ms)     => span(cls := "text-xs text-muted", s"匹配 ${ms.size} 项")
        })
      ),
      Option.when(showMatches)(
        child.maybe <-- check.map(_.filter(c => c.error.isEmpty && c.matches.nonEmpty).map(c => div(cls := "ps-2 font-mono text-xs break-all text-muted", c.matches.mkString("、"))))
      )
    )

  private def toneOf(c: Option[RuleCheck]): String = c match
    case Some(RuleCheck(_, Some(_), _)) => "chip--danger chip--soft"
    case Some(RuleCheck(_, None, Nil))  => "chip--warning chip--soft"
    case _                              => "chip--default chip--secondary"

/** 能用的工具与资源（服务端按与运行时同一套规则算出，带凭哪条规则）。 */
object Grants:
  private def secondaryChip(mods: Modifier[HtmlElement]*): HtmlElement =
    span(cls := "chip chip--default chip--sm chip--secondary", mods)

  /** 精简版（预览）：资源直接列出，工具（较多）折叠。 */
  def compact(result: Signal[PreviewResult], what: String): HtmlElement =
    div(
      cls := "flex flex-col gap-2",
      div(cls := "text-sm text-muted", child.text <-- result.map(r => s"${what}的 skill、MCP、secret、记忆（${r.assets.size}）")),
      div(
        cls := "flex flex-wrap gap-1.5",
        children <-- result.map(r =>
          if r.assets.isEmpty then List(span(cls := "text-sm text-muted", "没有 skill、MCP、secret、记忆"))
          else
            r.assets.map(g =>
              Ui.appLink(s"${Route.Assets.url}?kind=${g.kind.label}")(
                title := s"用：${g.use}" + g.write.fold("")(w => s"\n改：$w"),
                secondaryChip(span(cls := "text-muted", g.kind.shownName), " ", span(cls := "font-mono", g.name))
              )
            )
        )
      ),
      Ui.disclosure(Var(false), "flex items-center gap-1 text-sm text-muted", "flex flex-col gap-1 pt-2 text-sm", "ms-0", indicatorFirst = true)(
        child.text <-- result.map(r => s"${what}的工具（${r.tools.size}）")
      )(
        children <-- result.map(r =>
          if r.tools.isEmpty then List(span(cls := "text-muted", "没有工具"))
          else r.tools.map(t => div(cls := "flex flex-wrap items-center gap-2", span(cls := "font-mono", t.name), span(cls := "text-xs text-muted", t.use)))
        )
      ),
      child.maybe <-- result.map(r =>
        Option.when(r.writablePaths.nonEmpty)(div(cls := "text-sm text-muted", "还能写这些目录：", span(cls := "font-mono break-all", r.writablePaths.mkString("、"))))
      )
    )

  /** 完整版（agent 详情的「资源」）：按种类成组，每项列出用、改各凭哪条规则。 */
  def full(tools: Signal[List[GrantView]], assets: Signal[List[GrantView]]): HtmlElement =
    val kinds = AssetKind.values.toList.filterNot(_ == AssetKind.Tool) :+ AssetKind.Tool
    div(
      cls := "flex flex-col gap-3",
      kinds.map { k =>
        val items = (if k == AssetKind.Tool then tools else assets.map(_.filter(_.kind == k))).map(_.sortBy(_.name))
        Ui.groupSection(k.shownName, items.map(_.size), defaultExpanded = k != AssetKind.Tool)(
          div(
            cls := "flex flex-col",
            child.maybe <-- items.map(is => Option.when(is.isEmpty)(p(cls := "py-2 text-sm text-muted", s"没有${k.shownName}"))),
            children <-- items.splitSeq(_.item)(g => row(g))
          )
        )
      }
    )

  private def row(g: Signal[GrantView]): HtmlElement =
    div(
      cls := "flex flex-col gap-1 border-b border-separator py-2 last:border-b-0",
      div(
        cls := "flex flex-wrap items-center gap-1.5",
        span(cls := "me-1 font-mono text-sm font-medium", child.text <-- g.map(_.name)),
        child.maybe <-- g.map(_.owner.map(o => Ui.chip(if o == "platform" then "平台" else o, if o == "platform" then Tone.Default else Tone.Accent)()))
      ),
      div(cls := "text-xs text-muted", "用：", child.text <-- g.map(_.use)),
      child.maybe <-- g.map(_.write.map(w => div(cls := "text-xs text-warning", s"改：$w")))
    )

/** agent 的角色开关、自己的允许/禁止规则，以及按它们算出的最终工具与资源（新建与编辑共用；`agent` 为空是还没建的）。 */
object PermissionsInput:
  final case class Value(roles: List[RoleName], allow: List[String], deny: List[String])

  def view(agent: Option[AgentId], value: Signal[Value], onChange: (Value => Value) => Unit): HtmlElement =
    val roles      = Remote(ResourcesApi.listRoles)(())
    val lastOk     = Var(Option.empty[PreviewResult])
    val error      = Var(Option.empty[String])
    val candidates = Rules.candidates()
    val preview: Signal[Option[Either[String, PreviewResult]]] =
      value.distinct
        .composeUpdates(_.debounce(300))
        .flatMapSwitch(v =>
          Http.stream(Http.call(ResourcesApi.previewAgent)(PreviewRequest(agent, v.roles, v.allow, v.deny)))
            .map(r => Some(r.toEither.left.map(Http.messageOf)))
            .toSignal(None)
        )
    div(
      cls := "flex flex-col gap-3",
      div(cls := "text-sm font-medium", "角色与规则"),
      div(
        cls := "text-xs text-muted",
        "规则是 动作:种类:名字，动作 use（用）或 write（改）。能做的 = 它自己名下的（use 与 write）+ base（每个 agent 都有）+ 选中的角色 + 下面的允许，禁止优先（不拦它自己名下的）。admin 是全部工具并能改任何资源。"
      ),
      Ui.loading(roles) { list =>
        div(
          cls := "grid grid-cols-1 gap-2 sm:grid-cols-2",
          children <-- list.map(_.filterNot(_.name.value == "base")).splitSeq(_.name)(r =>
            val role = r.now().name
            val name = role.value
            div(
              cls := "flex items-center gap-2 text-sm",
              Fields.switch(
                name,
                value.map(_.roles.contains(role)),
                on => onChange(v => v.copy(roles = if on then (v.roles :+ role).distinct else v.roles.filterNot(_ == role)))
              ),
              span(cls := "font-mono", name),
              span(cls := "truncate text-xs text-muted", child.text <-- r.map(_.description))
            )
          )
        )
      },
      div(
        cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
        Rules.field(
          "额外允许",
          "只给这个 agent 的，如 use:skill:relay-host、use:tool:issue_*、*:memory:trading",
          value.map(_.allow),
          a => onChange(_.copy(allow = a)),
          candidates
        ),
        Rules.field("禁止", "优先于所有角色与允许，如 use:tool:trigger_*", value.map(_.deny), d => onChange(_.copy(deny = d)), candidates)
      ),
      // 最近一次算成功的结果一直显示（改规则时不闪），算错了在上面提示
      preview --> {
        case Some(Right(r)) => lastOk.set(Some(r)); error.set(None)
        case Some(Left(e))  => error.set(Some(e))
        case None           => ()
      },
      Ui.errorAlert(error.signal, "权限配置有误"),
      child <-- lastOk.signal.map(_.isDefined).distinct.map {
        case false => Ui.centeredSpinner
        case true  => Grants.compact(lastOk.signal.map(_.get), "最终能用")
      }
    )
