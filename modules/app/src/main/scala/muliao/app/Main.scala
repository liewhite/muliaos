package muliao.app

import muliao.contract.{AuthApi, ModelsApi, ResourcesApi}
import muliao.platform.{AppError, GeneralSettings, InstanceLock}
import muliao.platform.config.ConfigFile
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.platform.http.HttpSettings
import muliao.platform.log.{LogSettings, Logging}
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import zio.*

import java.nio.file.Paths

/** 程序入口。
  *
  * {{{
  * muliao serve <配置文件>   起服务
  * muliao openapi            把接口描述（OpenAPI JSON）打到标准输出
  * }}}
  */
object Main extends ZIOAppDefault:

  val version: String = Option(getClass.getPackage.getImplementationVersion).getOrElse("dev")

  /** 平台各领域，按依赖顺序（迁移也按这个顺序跑）。 */
  private val domains = List(
    muliao.resources.Module,
    muliao.models.Module,
    muliao.sessions.Module,
    muliao.workspace.Module,
    muliao.notifications.Module,
    muliao.board.Module,
    muliao.triggers.Module,
    muliao.history.Module
  )

  /** 配置文件里允许出现的段。各领域有了自己的配置段就加在这里。 */
  private val sections: Set[String] =
    (List(
      GeneralSettings.section,
      LogSettings.section,
      HttpSettings.section,
      ServerSettings.section,
      muliao.resources.ResourcesSettings.section,
      muliao.resources.VaultSettings.section
    ) ++ muliao.models.Module.sections ++ muliao.models.Module.optionalSections).map(_.key).toSet

  /** 全部接口声明（导出接口描述用）。 */
  private val contractEndpoints = AuthApi.all ++ ResourcesApi.all ++ ModelsApi.all ++ List(HttpServer.events, HttpServer.openApiEndpoint)

  def run: ZIO[ZIOAppArgs, Any, Any] =
    getArgs.flatMap {
      case Chunk("serve", path) =>
        serve(path).foldZIO(e => Console.printLineError(s"启动失败：${e.message}").ignore *> exit(ExitCode.failure), ZIO.succeed(_))
      case Chunk("openapi") => Console.printLine(HttpServer.openApi(contractEndpoints, version))
      case _                => Console.printLineError("用法：muliao serve <配置文件> | muliao openapi").ignore *> exit(ExitCode(2))
    }

  private def serve(path: String): IO[AppError, Unit] =
    for
      file    <- ConfigFile.load(Paths.get(path), sections)
      general <- file.section(GeneralSettings.section)
      log     <- file.section(LogSettings.section)
      server  <- file.section(ServerSettings.section)
      _ <- ZIO
             .scoped(
               for
                 _    <- ZIO.logInfo(s"muliao $version 启动，配置 $path")
                 _    <- InstanceLock.acquire(general.dataDir)
                 env  <- Database.layer(Database.Settings(general.databaseFile), domains.map(_.migrations)).build
                 auth <- Auth.layer.build.provideSomeLayer[Scope](ZLayer.succeed(server)).map(_.get[Auth])
                 bus  <- ChangeBus.layer.build.map(_.get[ChangeBus])
                 _    <- ZIO.succeed(env.get[Database])
                 _ <- HttpServer
                        .serve(server, auth, bus, Nil, version)
                        .mapError(e => AppError.internal(s"HTTP 服务起不来：${e.getMessage}", e))
                 _ <- ZIO.never
               yield ()
             )
             .provideLayer(Logging.layer(log).mapError(AppError.invalid))
    yield ()
