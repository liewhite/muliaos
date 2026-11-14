package muliao.workspace

import muliao.contract.*
import muliao.models.llm.Models
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.*
import muliao.resources.access.AccessEditor
import muliao.resources.agents.Agents
import muliao.resources.guard.AssetGuard
import muliao.resources.tree.AssetStore
import muliao.resources.vault.Vault
import muliao.sessions.{Files, LiveHub, Origin, RunContext, SessionHooks, SessionPaths, SessionSettings, Sessions}
import muliao.sessions.TestEnv as S
import muliao.toolkit.{Access, ToolOutcome, ToolContent, ToolFailure}
import muliao.workspace.pages.PageSigner
import zio.*
import zio.json.ast.Json

import java.nio.file.{Files as JFiles, Path}

/** 工作区领域的测试环境：真的库、资源目录、会话领域与本领域，假的模型。 */
object TestEnv:

  val signer: PageSigner = PageSigner.fromSecret("s" * 32)

  val toolCatalog: ToolCatalog = ToolCatalog.fixed(Sessions.toolNames.map(ToolInfo(_, Access.Write)) ++ Workspace.toolInfos)

  type Env = Workspace.Services & Sessions & SessionHooks & Agents & Database & AgentHooks & ChangeBus & Files & Resources & AssetStore &
    ResourcePaths & Vault

  def layer(d: S.Dirs, models: S.FakeModels): ZLayer[Any, AppError, Env] =
    val paths = ResourcePaths(d.assets, d.workspaces, d.data.resolve("resources"), List(d.data.resolve("private")))
    ZLayer.make[Env](
      ZLayer.succeed(paths),
      Database.layer(
        Database.Settings(d.data.resolve("t.db")),
        List(muliao.resources.Module.migrations, muliao.sessions.Module.migrations, muliao.workspace.Module.migrations)
      ),
      ChangeBus.layer,
      AssetStore.layer(AssetStore.Settings(paths.assets, paths.lastGoodAccess, 100.millis)),
      ZLayer.succeed(ResourcesSettings(writableDirs = Nil)),
      ZLayer.succeed(VaultSettings(S.vaultKey)),
      ZLayer.succeed(toolCatalog),
      ZLayer.succeed(S.modelCheck),
      AgentHooks.layer,
      AccessEditor.layer,
      Agents.layer,
      Resources.layer,
      Vault.layer,
      AssetGuard.layer,
      ZLayer.succeed[Models](models),
      ZLayer.succeed(S.noMcp),
      ZLayer.succeed(SessionSettings()),
      ZLayer.succeed(SessionPaths(d.data.resolve("files"))),
      ZLayer.succeed(GeneralSettings(d.data, d.assets)),
      SessionHooks.layer,
      LiveHub.layer,
      Files.layer,
      Sessions.layer,
      ZLayer.succeed(signer),
      Workspace.layer
    )

  /** 起会话领域并收尾后台任务（组装入口的启动顺序）。 */
  val start: ZIO[Sessions & muliao.workspace.jobs.Jobs, AppError, Unit] =
    ZIO.serviceWithZIO[Sessions](_.start) *> ZIO.serviceWithZIO[muliao.workspace.jobs.Jobs](_.recover).unit

  /** 直接调用工具用的上下文（不经模型）。 */
  def context(agent: AgentId, run: Long = 1L, session: String = "direct"): ZIO[Resources & Agents & ResourcePaths, AppError, RunContext] =
    for
      res   <- ZIO.serviceWithZIO[Resources](_.forRun(agent))
      paths <- ZIO.service[ResourcePaths]
      dir    = paths.workspace(agent)
      _     <- ZIO.attemptBlocking(JFiles.createDirectories(dir)).orDie
    yield RunContext(agent, res.agent.name, SessionId.wrap(session), RunId.wrap(run), CallId.wrap("c"), Origin.User, dir, res, userWaiting = true)

  /** 直接调用一个工具：成功时返回结果的文字（或 `[image:类型]`），失败时返回失败。 */
  def call(name: String, args: String, ctx: RunContext): ZIO[WorkspaceTools, Nothing, Either[ToolFailure, ToolOutcome]] =
    ZIO.serviceWithZIO[WorkspaceTools] { ts =>
      val tool = ts.tools.find(_.spec.name == name).getOrElse(throw IllegalArgumentException(s"没有工具 $name"))
      val json = zio.json.ast.Json.decoder.decodeJson(args).fold(e => throw IllegalArgumentException(e), identity)
      tool.invoke(json, ctx).either
    }

  def text(r: Either[ToolFailure, ToolOutcome]): String = r match
    case Right(ToolOutcome.Completed(cs)) =>
      cs.map {
        case ToolContent.Text(t)     => t
        case ToolContent.Image(m, _) => s"[image:$m]"
      }.mkString
    case Right(ToolOutcome.Suspended(s))     => s"[suspended:$s]"
    case Left(ToolFailure.Rejected(m))       => s"[rejected] $m"
    case Left(ToolFailure.Unavailable(m, _)) => s"[unavailable] $m"
    case Left(ToolFailure.Aborted(m, _))     => s"[aborted] $m"

  def write(p: Path, content: String): UIO[Path] =
    ZIO.attemptBlocking { JFiles.createDirectories(p.getParent); JFiles.writeString(p, content) }.orDie

  def read(p: Path): UIO[String] = ZIO.attemptBlocking(JFiles.readString(p)).orDie

  def jsonStr(s: String): String = Json.Str(s).toString
