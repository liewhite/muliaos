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
import muliao.sessions.{Contribution, Files, LiveHub, Pause, RunContext, SessionConsole, SessionPaths, SessionSettings, Sessions}
import muliao.sessions.TestEnv as S
import muliao.toolkit.{ToolOutcome, ToolFailure}
import muliao.workspace.pages.PageSigner
import zio.*
import zio.json.ast.Json

import java.nio.file.{Files as JFiles, Path}

/** 工作区领域的测试环境：真的库、资源目录、会话领域与本领域，假的模型。 */
object TestEnv:

  val signer: PageSigner = PageSigner.fromSecret("s" * 32)

  val toolCatalog: ToolCatalog = S.toolCatalog(Workspace.specs)

  type Env = Workspace.Services & AgentExec & Sessions & SessionConsole & Agents & Database & ChangeBus & Files & Resources & AssetStore & ResourcePaths &
    Vault & S.EventLog

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
      LiveHub.layer,
      Files.layer,
      Sessions.layer,
      ZLayer.succeed(signer),
      AgentExec.layer,
      Workspace.layer,
      ZLayer(Ref.make(List.empty[muliao.sessions.SessionEvent]).map(S.EventLog(_)))
    )

  /** 按组装入口的启动顺序：会话（接上本领域，`more` 是别的领域的）→ agent → 收尾后台任务。 */
  def startWith(more: List[Contribution]): ZIO[Env, AppError, Unit] =
    for
      mine <- Workspace.contribution
      jobs <- Workspace.jobControl
      _    <- S.startWith(mine :: more, jobs)
      _    <- ZIO.serviceWithZIO[muliao.workspace.jobs.Jobs](_.recover)
    yield ()

  val start: ZIO[Env, AppError, Unit] = startWith(Nil)

  export S.context

  /** 直接调用一个工具：成功时返回结果的文字（或 `[image:类型]`），失败时返回失败。 */
  def call(name: String, args: String, ctx: RunContext): ZIO[Env, Nothing, Either[ToolFailure, ToolOutcome[Pause]]] =
    Workspace.contribution.flatMap { c =>
      val tool = c.tools.find(_.spec.name == name).getOrElse(throw IllegalArgumentException(s"没有工具 $name"))
      val json = zio.json.ast.Json.decoder.decodeJson(args).fold(e => throw IllegalArgumentException(e), identity)
      tool.invoke(json, ctx).either
    }

  def text(r: Either[ToolFailure, ToolOutcome[Pause]]): String = S.outcome(r)

  def write(p: Path, content: String): UIO[Path] =
    ZIO.attemptBlocking { JFiles.createDirectories(p.getParent); JFiles.writeString(p, content) }.orDie

  def read(p: Path): UIO[String] = ZIO.attemptBlocking(JFiles.readString(p)).orDie

  def jsonStr(s: String): String = Json.Str(s).toString
