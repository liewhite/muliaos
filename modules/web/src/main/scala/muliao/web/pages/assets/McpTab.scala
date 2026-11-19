package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

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

  enum Transport(val id: String, val label: String) extends Shown.Self:
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
      Ui.collapsibleCard("添加 MCP server")(form(agents)),
      AssetsPage.listOf(list, "没有 MCP server")(item(_, agents))
    )

  private def form(agents: Signal[List[AgentSummary]]): Modifier[HtmlElement] =
    val v   = Var(NewMcp(Owner.Platform, "", Transport.Stdio, "", "", "", ""))
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
      Fields.segmented[Transport]("传输", v.signal.map(_.transport), t => v.update(_.copy(transport = t)), Transport.values.toList, showLabel = true),
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
      div(Ui.btn(pending = act.busy.signal, disabledIf = v.signal.map(_.name.trim.isEmpty))("保存", onClick --> (_ => save())))
    )

  private def item(s: StrictSignal[AssetItem], agents: Signal[List[AgentSummary]]): HtmlElement =
    val act     = new Action
    val editing = Var(false)
    def toggle(on: Boolean): Unit =
      val path = s.now().path
      act
        .fire(
          Http.call(ResourcesApi.readAssetFile)(path).flatMap { f =>
            setEnabled(f.content, on).fold(
              e => scala.concurrent.Future.failed(FailureException(Failure.Transport(e))),
              c => Http.call(ResourcesApi.writeAssetFile)(AssetFileWrite(path, c, f.fingerprint))
            )
          },
          toast = Some(if on then "启用" else "停用")
        )
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
