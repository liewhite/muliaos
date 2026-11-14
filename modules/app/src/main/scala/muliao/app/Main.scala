package muliao.app

import muliao.contract.{AuthApi, BoardApi, HistoryApi, ModelsApi, NotificationsApi, ResourcesApi, SessionsApi, TriggersApi, WorkspaceApi}
import muliao.platform.{AppError, InstanceLock}
import muliao.platform.log.Logging
import muliao.platform.server.HttpServer
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

  /** 全部接口声明（导出接口描述用）。 */
  val contractEndpoints =
    AuthApi.all ++ ResourcesApi.all ++ ModelsApi.all ++ SessionsApi.all ++ WorkspaceApi.all ++ NotificationsApi.all ++ BoardApi.all ++
      TriggersApi.all ++ HistoryApi.all ++ List(HttpServer.events, HttpServer.openApiEndpoint)

  def run: ZIO[ZIOAppArgs, Any, Any] =
    getArgs.flatMap {
      case Chunk("serve", path) =>
        serve(path).foldZIO(e => Console.printLineError(s"启动失败：${e.message}").ignore *> exit(ExitCode.failure), ZIO.succeed(_))
      case Chunk("openapi") => Console.printLine(HttpServer.openApi(contractEndpoints, version))
      case _                => Console.printLineError("用法：muliao serve <配置文件> | muliao openapi").ignore *> exit(ExitCode(2))
    }

  private def serve(path: String): IO[AppError, Unit] =
    AppConfig.load(Paths.get(path)).flatMap { cfg =>
      ZIO
        .scoped(
          for
            _   <- ZIO.logInfo(s"muliao $version 启动，配置 ${cfg.path}")
            _   <- InstanceLock.acquire(cfg.general.dataDir)
            env <- Assembly.layer(cfg).build
            _   <- Assembly.boot(version).provideSomeEnvironment[Scope](_ ++ env)
            _   <- ZIO.addFinalizer(ZIO.logInfo("muliao 正在关停"))
            _   <- ZIO.never
          yield ()
        )
        .provideLayer(Logging.layer(cfg.log).mapError(AppError.invalid))
    }
