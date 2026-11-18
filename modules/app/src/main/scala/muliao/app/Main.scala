package muliao.app

import muliao.contract.{Api, AuthApi, BoardApi, HistoryApi, ModelsApi, NotificationsApi, ResourcesApi, SessionsApi, TriggersApi, VaultApi, WorkspaceApi}
import muliao.platform.{AppError, Build, InstanceLock}
import muliao.platform.log.Logging
import muliao.platform.server.{Endpoints, HttpServer}
import zio.*

import java.nio.file.Paths

/** 程序入口。
  *
  * {{{
  * muliao serve <配置文件>   起服务
  * muliao openapi            把接口描述（OpenAPI JSON）打到标准输出
  * muliao version            打印版本（打包时写进 jar 清单的 `<时间>-<commit>`，开发时是 dev）
  * }}}
  */
object Main extends ZIOAppDefault:

  /** 全部接口声明（导出接口描述用）。 */
  val contractEndpoints =
    AuthApi.all ++ ResourcesApi.all ++ VaultApi.all ++ ModelsApi.all ++ SessionsApi.all ++ WorkspaceApi.all ++ NotificationsApi.all ++ BoardApi.all ++
      TriggersApi.all ++ HistoryApi.all ++ List(Endpoints.stream(Api.changes), Endpoints.stream(SessionsApi.live), HttpServer.openApiEndpoint)

  def run: ZIO[ZIOAppArgs, Any, Any] =
    getArgs.flatMap {
      case Chunk("serve", path) =>
        serve(path).foldZIO(e => Console.printLineError(s"启动失败：${e.message}").ignore *> exit(ExitCode.failure), ZIO.succeed(_))
      case Chunk("openapi") => Console.printLine(HttpServer.openApi(contractEndpoints, Build.version))
      case Chunk("version") => Console.printLine(Build.version)
      case _                => Console.printLineError("用法：muliao serve <配置文件> | muliao openapi | muliao version").ignore *> exit(ExitCode(2))
    }

  private def serve(path: String): IO[AppError, Unit] =
    AppConfig.load(Paths.get(path)).flatMap { cfg =>
      ZIO
        .scoped(
          for
            _   <- ZIO.logInfo(s"muliao ${Build.version} 启动，pid ${ProcessHandle.current.pid}，配置 ${cfg.path}")
            _   <- InstanceLock.acquire(cfg.general.dataDir)
            env <- Assembly.layer(cfg).build
            _   <- Assembly.boot.provideSomeEnvironment[Scope](_ ++ env)
            _   <- ZIO.addFinalizer(ZIO.logInfo("muliao 正在关停"))
            _   <- ZIO.never
          yield ()
        )
        .provideLayer(Logging.layer(cfg.log).mapError(AppError.invalid))
    }
