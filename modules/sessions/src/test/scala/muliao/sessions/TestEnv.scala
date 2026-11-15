package muliao.sessions

import muliao.contract.*
import muliao.models.llm.*
import muliao.models.mcp.{Connected, Launch, McpConnector, McpServerSpec, SecretLookup}
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.*
import muliao.resources.access.AccessEditor
import muliao.resources.agents.{Agents, NewAgent}
import muliao.resources.tree.AssetStore
import muliao.resources.vault.{Fernet, Vault}
import muliao.toolkit.*
import zio.*
import zio.json.ast.Json
import zio.schema.{DeriveSchema, Schema}
import zio.stream.*

import java.nio.file.{Files as JFiles, Path}

/** 端到端测试用的完整环境：真的数据库、资源目录、会话领域，假的模型与 MCP。 */
object TestEnv:

  final case class Dirs(base: Path):
    val assets: Path     = base.resolve("assets")
    val data: Path       = base.resolve("data")
    val workspaces: Path = data.resolve("workspaces")

  def dirs: UIO[Dirs] = ZIO.succeed {
    val d = Dirs(JFiles.createTempDirectory("muliao-sessions"))
    JFiles.createDirectories(d.assets)
    JFiles.createDirectories(d.workspaces)
    d
  }

  val vaultKey: String = Fernet.generateKey()

  // ---- 假模型 ----

  /** 模型对一次请求的回应。 */
  enum Script:
    case Say(parts: List[ReplyPart], usage: Usage = Usage(100, 10, 0, 0))
    /** 先吐出这些段，然后一直不结束 */
    case Hang(parts: List[ReplyPart] = Nil)
    case Fail(e: AppError)

  def text(t: String): ReplyPart                       = ReplyPart.Text(t)
  def call(id: String, name: String, args: String): ReplyPart = ReplyPart.ToolCall(id, name, args)

  final class FakeModels(val handler: Ref[ModelRequest => Script], val seen: Ref[Chunk[ModelRequest]]) extends Models:
    val defaultModel: ModelRef   = ModelRef("fake", "m")
    val providers: Chunk[String] = Chunk("fake")
    def problem(model: String): Option[String] = Option.unless(model.startsWith("fake:"))("只有 fake")
    def listModels(provider: String): IO[AppError, Chunk[String]] = ZIO.succeed(Chunk("m"))

    def request(model: ModelRef, req: ModelRequest): ZStream[Any, AppError, ReplyEvent] =
      ZStream.unwrap(
        seen.update(_ :+ req) *> handler.get.map(_(req)).map {
          case Script.Say(parts, usage) =>
            val ps = Chunk.fromIterable(parts)
            ZStream.fromIterable(ps.zipWithIndex.map((p, i) => ReplyEvent.Started(i, p))) ++
              ZStream.succeed(ReplyEvent.Finished(Reply(ps, Protocol.Anthropic, model.model, None, Some("end_turn"), usage)))
          case Script.Hang(parts) =>
            ZStream.fromIterable(parts.zipWithIndex.map((p, i) => ReplyEvent.Started(i, p))) ++ ZStream.never
          case Script.Fail(e) => ZStream.fail(e)
        }
      )

    def setHandler(f: ModelRequest => Script): UIO[Unit] = handler.set(f)
    def requests: UIO[Chunk[ModelRequest]]               = seen.get

  def fakeModels(f: ModelRequest => Script): UIO[FakeModels] =
    Ref.make(f).zip(Ref.make(Chunk.empty[ModelRequest])).map((h, s) => FakeModels(h, s))

  val noMcp: McpConnector = new McpConnector:
    def connectAll(servers: Chunk[McpServerSpec], secrets: SecretLookup, launch: Launch): ZIO[Scope, Nothing, Connected] =
      ZIO.succeed(Connected(Chunk.empty, Chunk.empty))
    def probe(server: McpServerSpec, secrets: SecretLookup, launch: Launch): IO[AppError, Chunk[String]] = ZIO.succeed(Chunk.empty)

  // ---- 测试工具 ----

  final case class EchoIn(text: String)
  given Schema[EchoIn] = DeriveSchema.gen[EchoIn]

  final case class SecretIn(@sensitive value: String)
  given Schema[SecretIn] = DeriveSchema.gen[SecretIn]

  final case class NoIn()
  given Schema[NoIn] = DeriveSchema.gen[NoIn]

  /** 测试里能观察、控制的工具。`gate` 没放行前 `block` 一直卡着。 */
  final class Probe(val gate: Promise[Nothing, Unit], val secrets: Ref[List[String]], val approvals: Ref[List[Boolean]])

  def probe: UIO[Probe] =
    for
      g <- Promise.make[Nothing, Unit]
      s <- Ref.make(List.empty[String])
      a <- Ref.make(List.empty[Boolean])
    yield Probe(g, s, a)

  def tools(p: Probe): List[Tool[RunContext]] = List(
    Tool[RunContext, EchoIn]("echo", "原样返回", Access.Read)((in, _) => ZIO.succeed(ToolOutcome.text(s"echo:${in.text}"))),
    Tool[RunContext, NoIn]("block", "卡住直到放行", Access.Write, Scheduling.Ordered)((_, _) => p.gate.await.as(ToolOutcome.text("放行了"))),
    Tool[RunContext, SecretIn]("keep_secret", "收一个秘密", Access.Write)((in, _) => p.secrets.update(_ :+ in.value).as(ToolOutcome.text("收到"))),
    Tool[RunContext, NoIn]("approve_me", "要批准", Access.Write, Scheduling.Ordered)((_, _) =>
      ZIO.succeed(ToolOutcome.Suspended(Pause.Approval(ApproverName.wrap("test-approval"), "跑测试脚本", Json.Obj("x" -> Json.Num(1)))))
    )
  )

  def approval(p: Probe): ApprovalHandler = new ApprovalHandler:
    val name = ApproverName.wrap("test-approval")
    def approved(payload: Json, always: Boolean, ctx: RunContext): IO[ToolFailure, ToolOutcome] =
      p.approvals.update(_ :+ always).as(ToolOutcome.text(s"批准后执行了，always=$always"))

  /** `more`：下游领域测试时一并登记的工具名。 */
  def toolCatalog(more: List[String] = Nil): ToolCatalog = ToolCatalog.fixed(
    (List("echo", "keep_secret", "block", "approve_me") ++ Sessions.toolNames ++ more).map(ToolInfo(_, Access.Write))
  )

  val modelCheck: ModelCheck = new ModelCheck:
    val defaultModel                         = "fake:m"
    def check(model: String): Option[String] = Option.unless(model.startsWith("fake:"))("只有 fake")

  type Env = Sessions & SessionHooks & Agents & Database & AgentHooks & ChangeBus & Files

  /** 在 `d` 上起一套环境（同一个 `d` 起第二套就是模拟重启）。 */
  /** `more` / `moreTools`：下游领域测试时一并执行的迁移、一并登记的工具名。 */
  def layer(
      d: Dirs,
      models: FakeModels,
      settings: SessionSettings = SessionSettings(),
      more: List[muliao.platform.db.Migrations] = Nil,
      moreTools: List[String] = Nil
  ): ZLayer[Any, AppError, Env] =
    val paths = ResourcePaths(d.assets, d.workspaces, d.data.resolve("resources"), Nil)
    ZLayer.make[Env](
      ZLayer.succeed(paths),
      Database.layer(Database.Settings(d.data.resolve("t.db")), List(muliao.resources.Module.migrations, muliao.sessions.Module.migrations) ++ more),
      ChangeBus.layer,
      AssetStore.layer(AssetStore.Settings(paths.assets, paths.lastGoodAccess, 100.millis)),
      ZLayer.succeed(ResourcesSettings(writableDirs = Nil)),
      ZLayer.succeed(VaultSettings(vaultKey)),
      ZLayer.succeed(toolCatalog(moreTools)),
      ZLayer.succeed(modelCheck),
      AgentHooks.layer,
      AccessEditor.layer,
      Agents.layer,
      Resources.layer,
      Vault.layer,
      ZLayer.succeed[Models](models),
      ZLayer.succeed(noMcp),
      ZLayer.succeed(settings),
      ZLayer.succeed(SessionPaths(d.data.resolve("files"))),
      ZLayer.succeed(GeneralSettings(d.data, d.assets)),
      SessionHooks.layer,
      LiveHub.layer,
      Files.layer,
      Sessions.layer
    )

  /** 建一个 agent。 */
  def agent(name: String): ZIO[Agents, AppError, AgentId] =
    ZIO.serviceWithZIO[Agents](_.create(NewAgent(name, s"测试 agent $name", Some("fake:m")))).map(_.id)

  /** 接好测试工具与批准执行者并启动。 */
  def start(p: Probe): ZIO[Sessions & SessionHooks, AppError, Unit] =
    for
      hooks <- ZIO.service[SessionHooks]
      _     <- hooks.addTools(tools(p))
      _     <- hooks.addApproval(approval(p))
      _     <- ZIO.serviceWithZIO[Sessions](_.start)
    yield ()

  /** 记下全部会话事件。 */
  def recordEvents: ZIO[SessionHooks, Nothing, Ref[List[SessionEvent]]] =
    for
      ref <- Ref.make(List.empty[SessionEvent])
      _   <- ZIO.serviceWithZIO[SessionHooks](_.addListener(e => ref.update(_ :+ e)))
    yield ref

  /** 每 20 毫秒看一次，直到满足（最多 10 秒）。 */
  def eventually[R, A](what: String)(get: ZIO[R, AppError, A])(ok: A => Boolean): ZIO[R, AppError, A] =
    def loop(left: Int): ZIO[R, AppError, A] =
      get.flatMap { a =>
        if ok(a) then ZIO.succeed(a)
        else if left <= 0 then ZIO.fail(AppError.Internal(s"等不到：$what（最后是 $a）"))
        else Clock.sleep(20.millis) *> loop(left - 1)
      }
    loop(500)

  def stateOf(id: SessionId): ZIO[Sessions, AppError, WorkState] = ZIO.serviceWithZIO[Sessions](_.state(id))

  def idle(id: SessionId): ZIO[Sessions, AppError, WorkState] = eventually(s"会话 ${id.value} 空闲")(stateOf(id))(_ == WorkState.Idle)

  def timeline(id: SessionId): ZIO[Sessions, AppError, List[TimelineItem]] = ZIO.serviceWithZIO[Sessions](_.timeline(id))

  /** 用户新开一个会话。 */
  def userSession(agent: AgentId, text: String): ZIO[Sessions, AppError, SessionId] =
    ZIO
      .serviceWithZIO[Sessions](_.deliver(Address.Fresh(agent, Origin.User, Titles.of(text)), Letter(Source.User, text, replyTo = Some(ReplyTo.User))))
      .someOrFail(AppError.Internal("没投进"))
      .map(_.session)

  def say(id: SessionId, text: String): ZIO[Sessions, AppError, Unit] =
    ZIO.serviceWithZIO[Sessions](_.deliver(Address.To(id), Letter(Source.User, text, replyTo = Some(ReplyTo.User)))).unit

  /** 一次请求里全部用户方文字（含工具结果）。 */
  def userTexts(req: ModelRequest): List[String] =
    req.messages.toList.flatMap {
      case Message.User(ps) =>
        ps.toList.flatMap {
          case UserPart.Text(t)                => List(t)
          case UserPart.ToolResult(_, c, _)    => c.toList.collect { case ToolContent.Text(t) => t }
          case _                               => Nil
        }
      case _ => Nil
    }

  def lastUserText(req: ModelRequest): String = userTexts(req).lastOption.getOrElse("")

  /** 请求来自哪个 agent（看固定说明）。 */
  def isAgent(req: ModelRequest, name: String): Boolean = req.system.contains(s"agent「$name」")

  /** 测试里读时间线条目的简写：种类、正文、来源标签、回复各段等（取不到的是空）。 */
    extension (t: TimelineItem)
      def kind: String = t.entry match
        case _: EntryView.Context    => "context"
        case _: EntryView.Letter     => "letter"
        case _: EntryView.Reply      => "reply"
        case _: EntryView.Compaction => "compaction"
        case _: EntryView.Notice     => "notice"
      def text: Option[String] = t.entry match
        case EntryView.Context(x)            => Some(x)
        case EntryView.Letter(_, _, x, _, _) => Some(x)
        case EntryView.Compaction(x)         => Some(x)
        case EntryView.Notice(_, x)          => Some(x)
        case _: EntryView.Reply              => None
      def source: Option[String] = t.entry match
        case EntryView.Letter(s, _, _, _, _) => Some(s.label)
        case _                               => None
      def files: List[FileView] = t.entry match
        case EntryView.Letter(_, _, _, fs, _) => fs
        case _                                => Nil
      def checkIn: Boolean = t.entry match
        case EntryView.Letter(_, _, _, _, c) => c
        case _                               => false
      def parts: List[PartView] = t.entry match
        case EntryView.Reply(ps, _, _, _) => ps
        case _                            => Nil
      def interrupted: Boolean = t.entry match
        case EntryView.Reply(_, _, _, i) => i
        case _                           => false
      def noticeKind: Option[String] = t.entry match
        case EntryView.Notice(k, _) => Some(k.label)
        case _                      => None

    extension (p: PartView)
      def text: Option[String] = p match
        case PartView.Text(x)     => Some(x)
        case PartView.Thinking(x) => Some(x)
        case PartView.Call(_)     => None
      def call: Option[CallView] = p match
        case PartView.Call(c) => Some(c)
        case _                => None

    extension (v: LiveView)
      def parts: List[PartView] = v match
        case LiveView.Snapshot(ps) => ps
        case _                     => Nil

