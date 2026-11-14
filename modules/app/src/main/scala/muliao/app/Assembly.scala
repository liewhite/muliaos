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
import muliao.platform.server.{ApiEndpoint, Auth, HttpServer, ServerSettings}
import muliao.resources.*
import muliao.resources.access.AccessEditor
import muliao.resources.agents.Agents
import muliao.resources.guard.AssetGuard
import muliao.resources.tools.AgentTools
import muliao.resources.tree.AssetStore
import muliao.resources.vault.Vault
import muliao.sessions.*
import muliao.toolkit.Tool
import muliao.triggers.{Triggers, TriggersHttp}
import muliao.workspace.{Workspace, WorkspaceHttp}
import muliao.workspace.jobs.Jobs
import muliao.workspace.pages.PageSigner
import zio.*

import java.util.concurrent.atomic.AtomicReference

/** 组装入口：把全部领域装成一个服务（[[layer]]），再按固定顺序启动（[[boot]]）。
  *
  * 启动顺序：各领域接到会话领域（工具、事件、未读）→ 核对工具（重名、漏接即报错）并定下内置工具目录 → 接着删没删完的 agent →
  * 会话续跑 → 收尾上次的后台任务 → 触发器 → HTTP 服务。关停按相反顺序：先断 HTTP（含实时连接），再停触发器与常驻命令，
  * 再结束后台任务（标被中断），最后是会话（在跑的运行下次接着跑）。全局记忆由资源目录在装配时确保存在。
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

  /** 全部内置工具的目录：资源领域按它分发、校验规则。它的唯一来源是实际接进会话领域的工具，所以在 [[boot]] 里全部接好后才定下；
    * 定下之前只有界面、工具调用会读它，而它们都在 [[boot]] 之后才可能发生，提前读就是组装顺序错了。
    */
  final class Catalog extends ToolCatalog:
    private val ref = AtomicReference[Option[List[ToolInfo]]](None)
    def tools: List[ToolInfo] =
      ref.get.getOrElse(throw IllegalStateException("内置工具目录还没定下：组装入口的启动顺序错了"))
    private[app] def settle(ts: List[ToolInfo]): UIO[Unit] = ZIO.succeed(ref.set(Some(ts)))

  /** 配了才有的外部服务。 */
  final case class Optional(web: Option[Web], transcriber: Option[Transcriber])

  type Services = Database & ChangeBus & Auth & Catalog & AssetStore & Agents & AgentHooks & Resources & Vault & ResourcePaths & ResourcesHttp &
    Models & Optional & SessionHooks & Sessions & SessionsHttp & Workspace.Services & Notifications & NotificationsHttp & Board &
    BoardHttp & History & HistoryHttp & Triggers & TriggersHttp & ServerSettings & GeneralSettings

  /** 装配全部领域。`models`：模型接入（测试里换成假模型）。 */
  def layer(cfg: AppConfig, models: ZLayer[ModelSettings & OutboundHttp, AppError, Models] = Models.layer): ZLayer[Any, AppError, Services] =
    val paths = cfg.resourcePaths
    ZLayer.make[Services](
      ZLayer.succeed(cfg.general),
      ZLayer.succeed(cfg.server),
      ZLayer.succeed(cfg.http),
      ZLayer.succeed(cfg.resources),
      ZLayer.succeed(cfg.vault),
      ZLayer.succeed(cfg.models),
      ZLayer.succeed(cfg.sessions),
      ZLayer.succeed(cfg.push),
      ZLayer.succeed(cfg.triggers),
      ZLayer.succeed(paths),
      ZLayer.succeed(cfg.sessionPaths),
      ZLayer.succeed(PageSigner.fromSecret(cfg.server.cookieSecret)),
      // 平台基础
      SystemProxy.macOS(),
      OutboundHttp.layer.mapError(e => AppError.internal(s"出站 HTTP 客户端起不来：${e.getMessage}", e)),
      Database.layer(Database.Settings(cfg.general.databaseFile), domains.map(_.migrations)),
      ChangeBus.layer,
      Auth.layer,
      // 资源、agent 与权限
      ZLayer.succeed(Catalog()),
      AssetStore.layer(AssetStore.Settings(paths.assets, paths.lastGoodAccess)),
      ZLayer.fromFunction(modelCheck),
      AgentHooks.layer,
      AccessEditor.layer,
      Agents.layer,
      Resources.layer,
      Vault.layer,
      AssetGuard.layer,
      ResourcesHttp.layer,
      // 模型接入与外部服务
      models,
      McpConnector.layer,
      optional(cfg.web, cfg.transcription),
      // 会话与下游领域
      SessionHooks.layer,
      LiveHub.layer,
      Files.layer,
      Sessions.layer,
      SessionsHttp.layer,
      Workspace.layer,
      Notifications.layer,
      NotificationsHttp.layer,
      Board.layer,
      BoardHttp.layer,
      History.layer,
      HistoryHttp.layer,
      Triggers.layer,
      TriggersHttp.layer
    )

  private def modelCheck(m: Models): ModelCheck = new ModelCheck:
    val defaultModel: String                 = m.defaultModel.toString
    def check(model: String): Option[String] = m.problem(model)

  private def optional(web: Option[WebSettings], tr: Option[TranscriptionSettings]): URLayer[OutboundHttp, Optional] =
    ZLayer.fromFunction((http: OutboundHttp) =>
      Optional(web.map(muliao.models.web.DeepSeekWeb(_, http)), tr.map(Transcriber.Dashscope(_, http)))
    )

  /** 本组装应接进会话领域的工具名（不含 MCP 工具：它们按运行连接）。缺了或多了说明有领域没接上或接重了。 */
  private def expectedTools(agentTools: List[String], web: Boolean): Set[String] =
    (Sessions.toolNames ++ SessionLink.toolNames ++ BoardTools.toolNames ++ HistoryTools.toolNames ++ Workspace.toolInfos.map(_.name) ++
      Triggers.toolInfos.map(_.name) ++ agentTools ++ (if web then List("web_search", "web_fetch") else Nil)).toSet

  /** 核对实际接进来的工具：重名、缺、多都是组装错误，拒绝启动。 */
  def checkTools(tools: List[Tool[RunContext]], expected: Set[String]): Either[String, List[ToolInfo]] =
    val names   = tools.map(_.spec.name)
    val dup     = names.groupBy(identity).collect { case (n, ns) if ns.size > 1 => n }.toList.sorted
    val missing = (expected -- names).toList.sorted
    val extra   = (names.toSet -- expected).toList.sorted
    List(
      Option.when(dup.nonEmpty)(s"工具重名：${dup.mkString("、")}"),
      Option.when(missing.nonEmpty)(s"没接上的工具：${missing.mkString("、")}"),
      Option.when(extra.nonEmpty)(s"多出的工具：${extra.mkString("、")}")
    ).flatten match
      case Nil      => Right(tools.map(t => ToolInfo(t.spec.name, t.spec.access)))
      case problems => Left(s"组装错误：${problems.mkString("；")}")

  /** 按启动顺序把服务跑起来，返回 HTTP 实际监听的端口。服务随调用方的 scope 关停。 */
  def boot(version: String): ZIO[Services & Scope, AppError, Int] =
    for
      _       <- SessionLink.attach *> BoardLink.attach *> HistoryLink.attach
      hooks   <- ZIO.service[SessionHooks]
      opt     <- ZIO.service[Optional]
      agentTs <- ZIO.environmentWith[Services](env =>
                   AgentTools.all(env.get[Agents], env.get[Resources], env.get[AssetStore], env.get[Vault], env.get[Catalog], env.get[ResourcePaths])
                 )
      _       <- hooks.addTools(agentTs.map(SessionHooks.widen))
      _       <- hooks.addTools(opt.web.toList.flatMap(w => WebTools.all(w).toList))
      tools   <- hooks.tools
      infos   <- ZIO.fromEither(checkTools(tools, expectedTools(agentTs.map(_.spec.name), opt.web.isDefined))).mapError(AppError.Internal(_, None))
      _       <- ZIO.serviceWithZIO[Catalog](_.settle(infos))
      _       <- ZIO.logInfo(s"已接上 ${infos.size} 个内置工具")
      _       <- ZIO.serviceWithZIO[Agents](_.resumePending)
      _       <- ZIO.serviceWithZIO[Sessions](_.start)
      _       <- ZIO.serviceWithZIO[Jobs](_.recover)
      _       <- ZIO.serviceWithZIO[Triggers](_.start)
      port    <- serve(version)
    yield port

  private def serve(version: String): ZIO[Services & Scope, AppError, Int] =
    for
      auth     <- ZIO.service[Auth]
      settings <- ZIO.service[ServerSettings]
      bus      <- ZIO.service[ChangeBus]
      models   <- ZIO.service[Models]
      opt      <- ZIO.service[Optional]
      eps      <- endpoints(auth, models, opt)
      port     <- HttpServer.serve(settings, auth, bus, eps, version).mapError(e => AppError.internal(s"HTTP 服务起不来：${e.getMessage}", e))
    yield port

  private def endpoints(auth: Auth, models: Models, opt: Optional): URIO[Services, List[ApiEndpoint]] =
    ZIO.environmentWith[Services] { env =>
      env.get[ResourcesHttp].endpoints(auth) ++
        ModelsHttp.endpoints(auth, models, opt.transcriber) ++
        env.get[SessionsHttp].endpoints(auth) ++
        env.get[WorkspaceHttp].endpoints(auth) ++
        env.get[NotificationsHttp].endpoints(auth) ++
        env.get[BoardHttp].endpoints(auth) ++
        env.get[TriggersHttp].endpoints(auth) ++
        env.get[HistoryHttp].endpoints(auth)
    }
