package muliao.app

import muliao.board.{Board, BoardHttp, BoardLink, BoardTools}
import muliao.history.{History, HistoryHttp, HistoryLink, HistoryTools}
import muliao.models.ModelsHttp
import muliao.models.llm.{ModelSettings, Models}
import muliao.models.mcp.McpConnector
import muliao.models.speech.{TranscriptionSettings, Transcriber}
import muliao.models.web.{Web, WebSettings, WebTools}
import muliao.notifications.{Notifications, NotificationsHttp, SessionLink}
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.platform.http.{OutboundHttp, SystemProxy}
import muliao.platform.server.{Auth, HttpServer, Route, ServerSettings}
import muliao.resources.*
import muliao.resources.access.AccessEditor
import muliao.resources.agents.Agents
import muliao.resources.guard.AssetGuard
import muliao.resources.tools.AgentTools
import muliao.resources.tree.AssetStore
import muliao.resources.vault.Vault
import muliao.sessions.*
import muliao.toolkit.ToolSpec
import muliao.triggers.{Triggers, TriggersHttp}
import muliao.workspace.{Browse, Workspace, WorkspaceHttp}
import muliao.workspace.jobs.Jobs
import muliao.workspace.pages.{PageSigner, Pages}
import muliao.workspace.scripts.ScriptGrants
import zio.*

/** 组装入口：把全部领域装成一个服务（[[layer]]），再按固定顺序启动（[[boot]]）。
  *
  * 领域之间的接线都是声明式的：全部内置工具的目录由各领域静态的工具定义给出（[[toolSpecs]]）；下游领域交给会话领域的东西
  * （工具、事件订阅、批准后的执行者、对 agent 生命周期的参与）由各自的纯函数给出（[[contributions]]），连同必须接上的后台任务与未读
  * 在 `Sessions.start` 时一次交进去。
  *
  * 启动顺序：会话（定下端口、续跑）→ agent（定下生命周期参与方、接着删没删完的 agent）→ 收尾上次的后台任务 → 触发器 → HTTP 服务。
  * 关停按相反顺序：先断 HTTP（含实时连接），再停触发器与常驻命令，再结束后台任务（标被中断），最后是会话（在跑的运行下次接着跑）。
  * 全局记忆由资源目录在装配时确保存在。
  */
object Assembly:

  /** 平台各领域，按依赖顺序（迁移也按这个顺序跑）。 */
  val domains = List(
    muliao.resources.Module,
    muliao.models.Module,
    muliao.sessions.Module,
    muliao.workspace.Module,
    muliao.notifications.Module,
    muliao.board.Module,
    muliao.triggers.Module,
    muliao.history.Module
  )

  /** 全部内置工具的静态定义，每个领域一份（网页工具配了 `[web]` 才有）。MCP 工具按运行连接，不在其中。 */
  def toolSpecs(web: Boolean): List[ToolSpec] =
    Sessions.specs ++ Workspace.specs ++ SessionLink.specs ++ BoardTools.specs ++ Triggers.specs ++ HistoryTools.specs ++ AgentTools.specs ++
      (if web then WebTools.specs else Nil)

  /** 配了才有的外部服务。 */
  final case class Optional(web: Option[Web], transcriber: Option[Transcriber])

  type Services = Database & ChangeBus & Auth & ToolCatalog & AssetStore & AccessEditor & AssetGuard & ModelCheck & Agents & Resources & Vault &
    ResourcePaths & AgentExec & Models & Optional & Files & Sessions & SessionConsole & Workspace.Services & Notifications & Board & History & Triggers &
    ServerSettings & GeneralSettings

  /** 装配全部领域。`models`：模型接入（测试里换成假模型）。 */
  def layer(cfg: AppConfig, models: ZLayer[ModelSettings & OutboundHttp, AppError, Models] = Models.layer): ZLayer[Any, AppError, Services] =
    val paths = cfg.resourcePaths
    ZLayer.make[Services](
      cfg.layer,
      ZLayer.succeed(PageSigner.fromSecret(cfg.server.cookieSecret)),
      ZLayer.succeed(ToolCatalog.of(toolSpecs(cfg.web.isDefined))),
      // 平台基础
      SystemProxy.macOS(),
      OutboundHttp.layer.mapError(e => AppError.internal(s"出站 HTTP 客户端起不来：${e.getMessage}", e)),
      Database.layer(Database.Settings(cfg.general.databaseFile), domains.map(_.migrations)),
      ChangeBus.layer,
      Auth.layer,
      // 资源、agent 与权限
      AssetStore.layer(AssetStore.Settings(paths.assets, paths.lastGoodAccess)),
      ZLayer.fromFunction(modelCheck),
      AccessEditor.layer,
      Agents.layer,
      Resources.layer,
      Vault.layer,
      AssetGuard.layer,
      AgentExec.layer,
      // 模型接入与外部服务
      models,
      McpConnector.layer(),
      optional(cfg.web, cfg.transcription),
      // 会话与下游领域
      LiveHub.layer,
      Files.layer,
      Sessions.layer,
      Workspace.layer,
      Notifications.layer,
      Board.layer,
      History.layer,
      Triggers.layer
    )

  private def modelCheck(m: Models): ModelCheck = new ModelCheck:
    val defaultModel: String                 = m.defaultModel.toString
    def check(model: String): Option[String] = m.problem(model)

  private def optional(web: Option[WebSettings], tr: Option[TranscriptionSettings]): URLayer[OutboundHttp, Optional] =
    ZLayer.fromFunction((http: OutboundHttp) =>
      Optional(web.map(muliao.models.web.DeepSeekWeb(_, http)), tr.map(Transcriber.Dashscope(_, http)))
    )

  /** 各领域交给会话领域的东西，每个领域一行（会话领域自己的工具由它自己接上）。 */
  val contributions: URIO[Services, List[Contribution]] =
    ZIO.collectAll(
      List(
        Workspace.contribution,
        SessionLink.contribution,
        BoardLink.contribution,
        Triggers.contribution,
        HistoryLink.contribution,
        ZIO.environmentWith[Services](env =>
          Contribution(tools = AgentTools.all(env.get[Agents], env.get[Resources], env.get[AssetStore], env.get[Vault], env.get[ToolCatalog], env.get[ResourcePaths]))
        ),
        ZIO.serviceWith[Optional](opt => Contribution(tools = opt.web.toList.flatMap(WebTools.all(_).toList)))
      )
    )

  /** 按启动顺序把服务跑起来，返回 HTTP 实际监听的端口。服务随调用方的 scope 关停。 */
  def boot(version: String): ZIO[Services & Scope, AppError, Int] =
    for
      contribs <- contributions
      jobs     <- Workspace.jobControl
      unread   <- SessionLink.unread
      sessions <- ZIO.service[Sessions]
      _        <- sessions.start(SessionPorts(contribs, jobs, unread))
      _        <- ZIO.serviceWithZIO[Agents](_.start(sessions.agentHook :: contribs.flatMap(_.agentHook)))
      _        <- ZIO.serviceWithZIO[Jobs](_.recover)
      _        <- ZIO.serviceWithZIO[Triggers](_.start)
      port     <- serve(version)
    yield port

  private def serve(version: String): ZIO[Services & Scope, AppError, Int] =
    for
      env  <- ZIO.environment[Services]
      port <- HttpServer
                .serve(env.get[ServerSettings], env.get[Auth], env.get[ChangeBus], routes(env), version)
                .mapError(e => AppError.internal(s"HTTP 服务起不来：${e.getMessage}", e))
    yield port

  /** 各领域的接口，每个领域一行。 */
  private def routes(env: ZEnvironment[Services]): List[Route] =
    val opt = env.get[Optional]
    ResourcesHttp(
      env.get[Agents], env.get[Resources], env.get[AssetStore], env.get[AssetGuard], env.get[Vault], env.get[AccessEditor], env.get[ModelCheck],
      env.get[ToolCatalog], env.get[ResourcePaths]
    ).routes ++
      ModelsHttp.routes(env.get[Models], opt.transcriber) ++
      SessionsHttp(env.get[Sessions], env.get[SessionConsole], env.get[Files], env.get[Agents]).routes ++
      WorkspaceHttp(env.get[Browse], env.get[Jobs], env.get[Pages], env.get[ScriptGrants]).routes ++
      NotificationsHttp(env.get[Notifications]).routes ++
      BoardHttp(env.get[Board]).routes ++
      TriggersHttp(env.get[Triggers]).routes ++
      HistoryHttp(env.get[History]).routes
