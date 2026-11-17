package muliao.resources

import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.access.AccessEditor
import muliao.resources.agents.Agents
import muliao.resources.guard.AssetGuard
import muliao.resources.tree.AssetStore
import muliao.resources.vault.{Fernet, Vault}
import muliao.toolkit.Access
import zio.*

import java.nio.file.{Files, Path}

/** 测试用的完整领域环境：临时数据目录与资源目录。 */
object TestEnv:
  final case class Dirs(base: Path):
    val assets: Path     = base.resolve("assets")
    val data: Path       = base.resolve("data")
    val workspaces: Path = data.resolve("workspaces")
    val privateFile: Path = data.resolve("config.toml")

  val key: String = Fernet.generateKey()

  val tools = ToolCatalog(
    List(
      ToolInfo("read_file", Access.Read),
      ToolInfo("write_file", Access.Write),
      ToolInfo("run_shell", Access.Write),
      ToolInfo("trigger_create", Access.Write),
      ToolInfo("trigger_delete", Access.Write),
      ToolInfo("create_agent", Access.Admin),
      ToolInfo("delete_agent", Access.Admin)
    )
  )

  val models: ModelCheck = new ModelCheck:
    val defaultModel = "claude:test-model"
    def check(model: String): Option[String] =
      Option.unless(model.startsWith("claude:") || model.startsWith("deepseek:"))("没有这个服务商")

  /** 一个可控的「忙」登记：测试里改 busy 的内容。 */
  final class FakeHook(val busyFor: Ref[Map[muliao.contract.AgentId, String]], val cleaned: Ref[List[muliao.contract.AgentId]], val failOnce: Ref[Boolean]):
    val hook: AgentHook = AgentHook(
      "测试领域",
      cleanup = agent =>
        failOnce.getAndSet(false).flatMap(f =>
          if f then ZIO.fail(AppError.Internal("模拟清理到一半服务停了")) else cleaned.update(_ :+ agent)
        ),
      busy = agent => busyFor.get.map(_.get(agent))
    )

  type Env = Dirs & Database & ChangeBus & AssetStore & Agents & Resources & AssetGuard & Vault & muliao.resources.vault.Grants & FakeHook &
    ResourcePaths & ResourcesSettings & AccessEditor

  def prepared(setup: Dirs => Unit): ZLayer[Any, AppError, Env] =
    val dirs = ZLayer.fromZIO(ZIO.succeed {
      val d = Dirs(Files.createTempDirectory("muliao-res"))
      Files.createDirectories(d.assets)
      Files.createDirectories(d.workspaces)
      Files.writeString(d.privateFile, "secret = 1")
      setup(d)
      d
    })
    val hook = ZLayer.fromZIO(
      for
        b <- Ref.make(Map.empty[muliao.contract.AgentId, String])
        c <- Ref.make(List.empty[muliao.contract.AgentId])
        f <- Ref.make(false)
      yield FakeHook(b, c, f)
    )
    val paths = ZLayer.fromFunction((d: Dirs) => ResourcePaths(d.assets, d.workspaces, d.data.resolve("resources"), List(d.privateFile)))
    val db    = ZLayer.fromZIO(ZIO.service[Dirs]).flatMap(d => Database.layer(Database.Settings(d.get.data.resolve("t.db")), List(Module.migrations)))
    val store = ZLayer.fromZIO(ZIO.service[ResourcePaths]).flatMap(p => AssetStore.layer(AssetStore.Settings(p.get.assets, p.get.lastGoodAccess, 200.millis)))
    ZLayer.make[Env](
      dirs,
      hook,
      paths,
      db,
      ChangeBus.layer,
      store,
      ZLayer.succeed(ResourcesSettings(memoryLimit = 100, writableDirs = Nil)), // 临时目录在默认的常用可写目录里，测试里去掉
      ZLayer.succeed(VaultSettings(key)),
      ZLayer.succeed(tools),
      ZLayer.succeed(models),
      AccessEditor.layer,
      Agents.layer.tap(a => ZIO.serviceWithZIO[FakeHook](h => a.get.start(List(h.hook)))),
      Resources.layer,
      AssetGuard.layer,
      muliao.resources.vault.Grants.layer,
      Vault.layer
    )

  val layer: ZLayer[Any, AppError, Env] = prepared(_ => ())

  def write(p: Path, s: String): Unit =
    Files.createDirectories(p.getParent)
    Files.writeString(p, s): Unit

  /** 模拟重启：在同一份库与资源目录上另起一个 agent 管理并启动（接着做没做完的改名与删除）。 */
  def restartAgents: ZIO[Env, AppError, Agents] =
    ZIO.scoped(
      Agents.layer.build
        .provideSomeLayer[Env & Scope](ZLayer.succeed(models) ++ ZLayer.succeed(tools))
        .map(_.get[Agents])
        .tap(a => ZIO.serviceWithZIO[FakeHook](h => a.start(List(h.hook))))
    )
