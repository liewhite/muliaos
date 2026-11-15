package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** /ai-assets：全部 AI 资源，按种类（`?kind=`）与归属（`?owner=`）筛选。每项显示归属与被谁使用；没加载的文件与原因、access.toml 写坏的提示放在最上面。 */
object AssetsPage:
  /** 种类标签页；`id` 是地址里的 `?kind=`。 */
  enum KindTab(val id: String, val label: String, val kinds: Set[AssetItemKind]):
    case Skill  extends KindTab("skill", "Skill", Set(AssetItemKind.Skill))
    case Mcp    extends KindTab("mcp", "MCP", Set(AssetItemKind.Mcp))
    case Secret extends KindTab("secret", "Secret", Set(AssetItemKind.Secret))
    case Memory extends KindTab("memory", "记忆", Set(AssetItemKind.Memory))
    /** agent.toml 与 access.toml */
    case Config extends KindTab("config", "配置文件", Set(AssetItemKind.Agent, AssetItemKind.Access))

  object KindTab:
    def of(id: Option[String]): KindTab = id.flatMap(i => values.find(_.id == i)).getOrElse(Skill)

  /** 归属筛选：全部、平台或某个 agent 名。 */
  def ownerMatches(filter: Option[String], item: AssetItem): Boolean = filter.forall(_ == item.owner)

  def items(index: AssetIndex, tab: KindTab, owner: Option[String]): List[AssetItem] =
    index.items.filter(i => tab.kinds(i.kind) && ownerMatches(owner, i))

  def view(): HtmlElement =
    val index  = Remote("assets", "agents")(Http.call(ResourcesApi.listAssets)(()))
    val agents = AgentsData.load().map(_.data.getOrElse(Nil))
    val tab    = Router.query.map(q => KindTab.of(q.get("kind"))).distinct
    val owner  = Router.query.map(_.get("owner").filter(_ != "all")).distinct
    Ui.page("AI 资产")(
      p(
        cls := "text-sm text-muted",
        "每项 AI 资产都是资源目录下的文件，在谁的目录下就归谁（平台或某个 agent）。agent 对它能做什么按权限 ",
        code(cls := "font-mono", "动作:种类:名字"),
        " 算：动作是 use（用）与 write（改），主人两者都有，其余来自它的角色（每个 agent 都有 base）与自己的规则；admin 能改全部。"
      ),
      Ui.loading(index) { idx =>
        div(
          cls := "flex flex-col gap-3",
          child.maybe <-- idx.map(_.accessBroken.map(Ui.errorAlert("权限文件 access.toml 写坏了：整份不生效，沿用上一份有效的；修好前平台不再改它", _))),
          child.maybe <-- idx.map(i => Option.when(i.problems.nonEmpty)(problems(i.problems))),
          div(
            cls := "max-w-xs",
            Fields.select[String](
              "归属",
              owner.map(o => Some(o.getOrElse("all"))),
              o => Router.setParam("owner", Option.when(o != "all")(o)),
              agents.map(as => Opt("all", "全部") :: Opt(AssetUi.Platform, "平台") :: as.map(a => Opt(a.name.value, a.name.value)))
            )
          ),
          Tabs[KindTab](
            "资产种类",
            tab,
            t => Router.setParam("kind", Some(t.id)),
            KindTab.values.toList.map(t => t -> span(t.label, child.text <-- idx.combineWith(owner).map((i, o) => s"（${items(i, t, o).size}）")))
          ),
          // 每个种类一个独立的实例（切换种类时新建表单不带过去）
          child <-- tab.map(t => panel(t, idx.combineWith(owner).map((i, o) => items(i, t, o)), agents))
        )
      }
    )

  private def problems(ps: List[ProblemView]): HtmlElement =
    div(
      cls := "alert alert--warning",
      role := "alert",
      div(cls := "alert__indicator", Icon("TriangleAlert", "size-5")),
      div(
        cls := "alert__content",
        div(cls := "alert__title", s"有 ${ps.size} 个资源文件没加载"),
        div(
          cls := "alert__description flex flex-col gap-1",
          ps.map(p => div(span(cls := "font-mono break-all", p.path), p.owner.fold("")(o => s"（${AssetUi.ownerLabel(o)}）"), "：", p.reason))
        )
      )
    )

  private def panel(t: KindTab, list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement = t match
    case KindTab.Skill  => SkillTab.view(list, agents)
    case KindTab.Mcp    => McpTab.view(list, agents)
    case KindTab.Secret => SecretTab.view(list, agents)
    case KindTab.Memory => MemoryTab.view(list, agents)
    case KindTab.Config => ConfigTab.view(list)

  /** 列表：空时提示。 */
  def listOf(list: Signal[List[AssetItem]], empty: String)(item: StrictSignal[AssetItem] => HtmlElement): HtmlElement =
    div(
      cls := "flex flex-col gap-2",
      child.maybe <-- list.map(l => Option.when(l.isEmpty)(Ui.emptyHint(empty))),
      children <-- list.splitSeq(i => (i.kind, i.owner, i.name))(item)
    )

  /** 删除一项资源（先确认）。 */
  def deleteButton(item: StrictSignal[AssetItem], body: String, act: Action): Button =
    Ui.iconBtn("Trash2", s"删除 ${item.now().name}")(
      onClick --> { _ =>
        val i = item.now()
        Confirm
          .ask(s"删除「${i.name}」？", Some(body), "删除", danger = true)
          .foreach(ok => if ok then i.kind.asset.foreach(k => act.run(Http.call(ResourcesApi.deleteAsset)((k, i.name))): Unit))
      }
    )

/** Skill：点进编辑页。 */
object SkillTab:
  def view(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      div(Ui.linkButton(Route.SkillEdit.url, Variant.Secondary)(Icon("Plus"), "新建 skill")),
      AssetsPage.listOf(list, "没有 skill") { s =>
        val url = SkillEditor.url(s.now().name, s.now().owner)
        Ui.card(
          div(
            cls := "card__header",
            Ui.appLink(url)(cls := "hover:underline", h3(cls := "card__title font-mono text-base", s.now().name)),
            p(cls := "card__description", child.text <-- s.map(_.description.getOrElse("")))
          ),
          div(cls := "card__content", AssetUi.access(s, agents))
        )
      }
    )

/** MCP server：启用/停用、编辑配置（JSON 文件）、新建。 */
object McpTab:
  import zio.json.*
  import zio.json.ast.Json

  /** 把配置文件里的 enabled 改掉（其余原样）。 */
  def setEnabled(content: String, on: Boolean): Either[String, String] =
    content.fromJson[Json].flatMap(_.asObject.toRight("配置不是 JSON 对象")).map { o =>
      val fields = o.fields.filterNot(_._1 == "enabled") :+ ("enabled" -> Json.Bool(on))
      Json.Obj(fields*).toJsonPretty + "\n"
    }

  enum Transport(val id: String, val label: String):
    case Stdio extends Transport("stdio", "stdio（本地命令）")
    case HttpT extends Transport("http", "HTTP / SSE")

  /** 每行一个 KEY=VALUE。 */
  def parsePairs(text: String): Either[String, List[(String, String)]] =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).toList.foldRight[Either[String, List[(String, String)]]](Right(Nil)) { (line, acc) =>
      line.indexOf('=') match
        case i if i > 0 => acc.map((line.substring(0, i).trim -> line.substring(i + 1).trim) :: _)
        case _          => Left(s"无法解析「$line」，格式应为 KEY=VALUE")
    }

  final case class NewMcp(owner: String, name: String, transport: Transport, command: String, args: String, url: String, pairs: String)

  /** 新建的配置文件内容。 */
  def render(m: NewMcp): Either[String, String] =
    parsePairs(m.pairs).map { kv =>
      val obj = Json.Obj(kv.map((k, v) => k -> Json.Str(v))*)
      val fields = m.transport match
        case Transport.Stdio =>
          List(
            "transport" -> Json.Str("stdio"),
            "command"   -> Json.Str(m.command.trim),
            "args"      -> Json.Arr(m.args.linesIterator.map(_.trim).filter(_.nonEmpty).map(Json.Str(_)).toSeq*),
            "env"       -> obj
          )
        case Transport.HttpT => List("transport" -> Json.Str("http"), "url" -> Json.Str(m.url.trim), "headers" -> obj)
      Json.Obj(fields*).toJsonPretty + "\n"
    }

  def view(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      Ui.collapsibleForm("添加 MCP server")(form(agents)),
      AssetsPage.listOf(list, "没有 MCP server")(item(_, agents))
    )

  private def form(agents: Signal[List[AgentSummary]]): Modifier[HtmlElement] =
    val v   = Var(NewMcp(AssetUi.Platform, "", Transport.Stdio, "", "", "", ""))
    val act = new Action
    def text(get: NewMcp => String, set: (NewMcp, String) => NewMcp) = v.zoom(get)(set)
    def save(): Unit =
      val m = v.now()
      render(m) match
        case Left(e) => act.error.set(Some(e))
        case Right(content) =>
          act
            .run(Http.call(ResourcesApi.writeAssetFile)(AssetFileWrite(s"${m.owner}/mcp/${m.name.trim}.json", content, None)))
            .foreach(ok => if ok then { Toast.success(s"已保存 ${m.name}"); v.set(NewMcp(m.owner, "", m.transport, "", "", "", "")) })
    Seq(
      p(cls := "text-sm text-muted", "也可以直接在对话里把 MCP 信息告诉 agent，让它在自己的资源目录里写 mcp/<名字>.json。"),
      Fields.text("名称", text(_.name, (m, s) => m.copy(name = s)), hint = Some("工具名会以「名称_」为前缀"), mono = true),
      AssetUi.ownerSelect(v.signal.map(_.owner), o => v.update(_.copy(owner = o)), agents),
      Fields.segmented[Transport]("传输", v.signal.map(_.transport), t => v.update(_.copy(transport = t)), Transport.values.toList.map(t => t -> t.label), showLabel = true),
      child <-- v.signal.map(_.transport).distinct.map {
        case Transport.Stdio =>
          div(
            cls := "flex flex-col gap-4",
            Fields.text("命令", text(_.command, (m, s) => m.copy(command = s)), placeholderText = "npx", mono = true),
            Fields.area("参数（每行一个）", text(_.args, (m, s) => m.copy(args = s)), rowsN = 3, mono = true)
          )
        case Transport.HttpT => Fields.text("URL", text(_.url, (m, s) => m.copy(url = s)), mono = true)
      },
      Fields.area(
        "环境变量 / 请求头（KEY=VALUE，每行一个）",
        text(_.pairs, (m, s) => m.copy(pairs = s)),
        hint = Some("stdio 是环境变量，HTTP 是请求头。secret 请先存入 Secret 标签，再用 ${vault:KEY} 引用"),
        rowsN = 3,
        mono = true
      ),
      act.errorView(),
      div(AssetUi.submit("保存", act.busy.signal, v.signal.map(_.name.trim.nonEmpty))(onClick --> (_ => save())))
    )

  private def item(s: StrictSignal[AssetItem], agents: Signal[List[AgentSummary]]): HtmlElement =
    val act     = new Action
    val editing = Var(false)
    def toggle(on: Boolean): Unit =
      val path = s.now().path
      act
        .run(
          Http.call(ResourcesApi.readAssetFile)(path).flatMap { f =>
            setEnabled(f.content, on).fold(
              e => scala.concurrent.Future.failed(FailureException(Failure.Transport(e))),
              c => Http.call(ResourcesApi.writeAssetFile)(AssetFileWrite(path, c, f.fingerprint))
            )
          },
          toast = Some(if on then "启用" else "停用")
        )
        .foreach(_ => ())
    Ui.card(
      div(
        cls := "card__header flex flex-row items-start gap-3",
        div(
          cls := "min-w-0 flex-1",
          h3(cls := "card__title font-mono text-base", s.now().name),
          div(cls := "mt-1.5", AssetUi.access(s, agents))
        ),
        Fields.switch(s"启用 MCP server「${s.now().name}」", s.map(_.enabled.getOrElse(true)), toggle, disabledIf = act.busy.signal)
      ),
      div(
        cls := "card__content flex flex-col gap-2",
        child.maybe <-- editing.signal.map(on => Option.when(on)(FileEditor.view(s.now().path, "配置（JSON）", rows = 10)))
      ),
      div(
        cls := "card__footer flex gap-2",
        Ui.btn(Variant.Ghost, Size.Sm)(Icon("Pencil"), child.text <-- editing.signal.map(if _ then "收起配置" else "编辑配置"), onClick --> (_ => editing.update(!_))),
        AssetsPage.deleteButton(s, "用它的 agent 从下一次运行起就没有这些工具了。", act)
      )
    )

/** Secret：值加密保存，只能设不能看。 */
object SecretTab:
  final case class SecretForm(key: String, value: String, description: String, allowShell: Option[Boolean], owner: String)

  def view(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      p(
        cls := "text-sm text-muted",
        "agent 通过 key 引用：MCP 配置里写 ",
        code(cls := "font-mono", "${vault:KEY}"),
        "，shell 里用 ",
        code(cls := "font-mono", "run_shell(secrets=[KEY])"),
        " 注入同名环境变量；不允许 shell 的只能用 ",
        code(cls := "font-mono", "skill_run(secrets=[KEY])"),
        " 经 skill 里的脚本注入。值只能设，不能看。"
      ),
      Ui.collapsibleForm("新建或改值")(form(list, agents)),
      AssetsPage.listOf(list, "没有 secret")(item(_))
    )

  private def form(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): Modifier[HtmlElement] =
    val v        = Var(SecretForm("", "", "", None, AssetUi.Platform))
    val act      = new Action
    val existing = v.signal.map(_.key.trim).combineWith(list).map((k, l) => l.find(_.name == k))
    def text(get: SecretForm => String, set: (SecretForm, String) => SecretForm) = v.zoom(get)(set)
    def save(): Unit =
      val f = v.now()
      act
        .run(
          Http.call(ResourcesApi.putSecret)(
            (f.key.trim, SecretWrite(Some(f.value), Option(f.description.trim).filter(_.nonEmpty), f.allowShell, Some(f.owner)))
          )
        )
        .foreach(ok => if ok then { Toast.success(s"已保存 ${f.key.trim}"); v.set(SecretForm("", "", "", None, f.owner)) })
    Seq(
      Fields.text("Key", text(_.key, (f, s) => f.copy(key = s)), hint = Some("已有的同名 secret 改值与说明，归属不变"), placeholderText = "SLACK_TOKEN", mono = true),
      Fields.text("值", text(_.value, (f, s) => f.copy(value = s)), inputType = "password"),
      Fields.text("说明", text(_.description, (f, s) => f.copy(description = s))),
      div(
        cls := "flex flex-row items-center gap-2 text-sm",
        // 没动过开关：已有的保持原值、新建的允许（只改值时不把不允许的悄悄放开）
        Fields.switch("允许 shell 使用", v.signal.combineWith(existing).map((f, e) => f.allowShell.orElse(e.flatMap(_.allowShell)).getOrElse(true)), on => v.update(_.copy(allowShell = Some(on)))),
        span("允许 shell 使用"),
        span(cls := "text-xs text-muted", "关掉后只能经 skill 脚本（skill_run）和 MCP 配置用")
      ),
      child.maybe <-- existing.map(_.isEmpty).distinct.map(isNew => Option.when(isNew)(AssetUi.ownerSelect(v.signal.map(_.owner), o => v.update(_.copy(owner = o)), agents))),
      act.errorView(),
      div(AssetUi.submit("保存", act.busy.signal, v.signal.map(f => f.key.trim.nonEmpty && f.value.nonEmpty))(onClick --> (_ => save())))
    )

  private def item(s: StrictSignal[AssetItem]): HtmlElement =
    val act      = new Action
    val setting  = Var(false)
    val newValue = Var("")
    val key      = s.now().name
    def put(w: SecretWrite, done: String): Unit =
      act.run(Http.call(ResourcesApi.putSecret)((key, w))).foreach(ok => if ok then { Toast.success(done); setting.set(false); newValue.set("") })
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-2",
        div(
          cls := "flex flex-row items-center gap-3",
          div(
            cls := "min-w-0 flex-1",
            div(
              cls := "flex flex-row items-center gap-2",
              span(cls := "font-mono font-medium", key),
              child.maybe <-- s.map(i => Option.when(i.allowShell.contains(false))(Ui.chip("仅 skill 脚本")()))
            ),
            div(cls := "truncate text-xs text-muted", child.text <-- s.map(_.description.filter(_.nonEmpty).getOrElse("无说明")))
          ),
          Fields.switch(
            s"允许 shell 使用 $key",
            s.map(_.allowShell.getOrElse(true)),
            on => put(SecretWrite(allowShell = Some(on)), if on then "已允许 shell 使用" else "已改为仅 skill 脚本可用"),
            disabledIf = act.busy.signal
          ),
          Ui.iconBtn("KeyRound", s"改 $key 的值")(onClick --> (_ => setting.update(!_))),
          AssetsPage.deleteButton(s, "引用它的 MCP 与命令会失败。", act)
        ),
        AssetUi.access(s, AgentsData.load().map(_.data.getOrElse(Nil))),
        child.maybe <-- setting.signal.map(on =>
          Option.when(on)(
            div(
              cls := "flex items-end gap-2",
              div(cls := "flex-1", Fields.text("新的值", newValue, inputType = "password", autoFocusOn = true)),
              AssetUi.submit("保存", act.busy.signal, newValue.signal.map(_.nonEmpty))(onClick --> (_ => put(SecretWrite(value = Some(newValue.now())), "已改值")))
            )
          )
        ),
        act.errorView()
      )
    )

/** 记忆：全局、共享（平台的）与各 agent 的私有记忆。 */
object MemoryTab:
  def view(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      p(
        cls := "text-sm text-muted",
        "agent 能用（use）的每一块记忆每次运行都全文放进指令，能改（write）的直接改它的文件。私有记忆只属于它的 agent；共享记忆在角色里写 ",
        code(cls := "font-mono", "*:memory:名字"),
        " 分给一组 agent（成员能用也能改），只读写 ",
        code(cls := "font-mono", "use:memory:名字"),
        "。全局记忆总是存在、人人可读。"
      ),
      Ui.collapsibleForm("新建共享记忆")(form()),
      AssetsPage.listOf(list, "没有记忆")(item(_, agents))
    )

  private def form(): Modifier[HtmlElement] =
    val name = Var("")
    val act  = new Action
    def save(): Unit =
      val n = name.now().trim
      act
        .run(Http.call(ResourcesApi.writeAssetFile)(AssetFileWrite(s"${AssetUi.Platform}/memory/$n.md", "", None)))
        .foreach(ok => if ok then { Toast.success(s"已新建记忆 $n"); name.set("") })
    Seq(
      Fields.text("名字", name, hint = Some("小写字母、数字、- 和 _"), placeholderText = "trading", mono = true),
      act.errorView(),
      div(AssetUi.submit("新建", act.busy.signal, name.signal.map(_.trim.nonEmpty))(onClick --> (_ => save())))
    )

  /** 能删的：平台的共享记忆（全局记忆与私有记忆不能删）。 */
  def deletable(i: AssetItem): Boolean = i.owner == AssetUi.Platform && i.name != "global"

  private def item(s: StrictSignal[AssetItem], agents: Signal[List[AgentSummary]]): HtmlElement =
    val open = Var(false)
    val act  = new Action
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-2",
        div(
          cls := "flex flex-row items-center gap-3",
          Ui.iconBtn("ChevronRight", "编辑")(
            aria.expanded <-- open.signal,
            cls <-- open.signal.map(o => if o then "[&_svg]:rotate-90" else ""),
            onClick --> (_ => open.update(!_))
          ),
          div(
            cls := "min-w-0 flex-1",
            div(cls := "font-mono font-medium", s.now().name),
            div(cls := "truncate font-mono text-xs text-muted", s.now().path)
          ),
          Option.when(deletable(s.now()))(AssetsPage.deleteButton(s, "用它的 agent 从下一次运行起就看不到这些内容了。", act))
        ),
        AssetUi.access(s, agents),
        act.errorView(),
        child.maybe <-- open.signal.map(o =>
          Option.when(o)(MemoryEditor.view(s.now().path, s.now().name, "能用它的 agent 每次运行都会看到全文；能改它的 agent 也会直接改它的文件。"))
        )
      )
    )

/** 配置文件：各 agent 的 agent.toml 与平台的 access.toml（角色与各 agent 的权限），可直接编辑。 */
object ConfigTab:
  def view(list: Signal[List[AssetItem]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      p(
        cls := "text-sm text-muted",
        "agent 的配置和权限也是普通文件，平时在 agent 页与角色页里改；这里可以直接改原文（如修好写坏的文件）。保存前服务端会校验格式。"
      ),
      AssetsPage.listOf(list, "没有配置文件") { s =>
        val open = Var(false)
        Ui.card(
          div(
            cls := "card__content flex flex-col gap-2",
            div(
              cls := "flex flex-row items-center gap-3",
              Ui.iconBtn("ChevronRight", "编辑")(
                aria.expanded <-- open.signal,
                cls <-- open.signal.map(o => if o then "[&_svg]:rotate-90" else ""),
                onClick --> (_ => open.update(!_))
              ),
              div(
                cls := "min-w-0 flex-1",
                div(cls := "font-mono font-medium", s.now().path),
                div(cls := "truncate text-xs text-muted", child.text <-- s.map(_.description.getOrElse("")))
              )
            ),
            child.maybe <-- open.signal.map(o => Option.when(o)(FileEditor.view(s.now().path, s.now().path, rows = 12)))
          )
        )
      }
    )
