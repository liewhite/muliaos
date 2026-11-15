package muliao.triggers

import muliao.contract.*
import muliao.models.llm.Models
import muliao.notifications.{Notifications, SessionLink}
import muliao.notifications.push.{Device, PushFailure, PushGateway}
import muliao.platform.{AppError, GeneralSettings, Retry}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.*
import muliao.resources.access.AccessEditor
import muliao.resources.agents.Agents
import muliao.resources.tree.AssetStore
import muliao.resources.vault.Vault
import muliao.sessions.{Files, LiveHub, SessionConsole, SessionPaths, SessionSettings, Sessions}
import muliao.sessions.TestEnv as S
import zio.*
import zio.test.Live

import java.nio.file.{Files as JFiles, Path}

/** 触发器领域的测试环境：真的库、资源目录、会话、通知与本领域，假的模型与推送服务。 */
object TestEnv:

  val noPush: PushGateway = new PushGateway:
    def send(device: Device, payload: Array[Byte]): IO[PushFailure, Unit] = ZIO.unit

  val toolCatalog: ToolCatalog = S.toolCatalog(SessionLink.specs ++ Triggers.specs)

  /** 测试用的节奏：重启、跑稳都很快。 */
  val quick: Tuning = Tuning(busyPoll = 100.millis, restartFirst = 50.millis, restartMax = 200.millis, stableAfter = 1.second)

  type Env = Triggers & TriggersHttp & Notifications & AgentExec & Sessions & SessionConsole & Agents & Database & ChangeBus & S.EventLog &
    Files & Resources & AssetStore & ResourcePaths & Vault

  /** `assetPoll`：资源目录轮询间隔；用测试时钟拨时间的测试调大，免得拨一次要跑成千上万次轮询。 */
  def layer(
      d: S.Dirs,
      models: S.FakeModels,
      settings: TriggerSettings = TriggerSettings(),
      tuning: Tuning = quick,
      assetPoll: Duration = 100.millis
  ): ZLayer[Any, AppError, Env] =
    val paths = ResourcePaths(d.assets, d.workspaces, d.data.resolve("resources"), List(d.data.resolve("private")))
    ZLayer.make[Env](
      ZLayer.succeed(paths),
      Database.layer(
        Database.Settings(d.data.resolve("t.db")),
        List(
          muliao.resources.Module.migrations,
          muliao.sessions.Module.migrations,
          muliao.notifications.Module.migrations,
          muliao.triggers.Module.migrations
        )
      ),
      ChangeBus.layer,
      AssetStore.layer(AssetStore.Settings(paths.assets, paths.lastGoodAccess, assetPoll)),
      ZLayer.succeed(ResourcesSettings(writableDirs = Nil)),
      ZLayer.succeed(VaultSettings(S.vaultKey)),
      ZLayer.succeed(toolCatalog),
      ZLayer.succeed(S.modelCheck),
      AccessEditor.layer,
      Agents.layer,
      Resources.layer,
      Vault.layer,
      ZLayer.succeed[Models](models),
      ZLayer.succeed(S.noMcp),
      ZLayer.succeed(SessionSettings()),
      ZLayer.succeed(SessionPaths(d.data.resolve("files"))),
      ZLayer.succeed(GeneralSettings(d.data, d.assets)),
      LiveHub.layer,
      Files.layer,
      Sessions.layer,
      AgentExec.layer,
      ZLayer.scoped(
        for
          db  <- ZIO.service[Database]
          bus <- ZIO.service[ChangeBus]
          n   <- Notifications.make(db, bus, noPush, "PUBLIC", Retry.Policy(10.millis, 40.millis, 200.millis))
        yield n
      ),
      ZLayer.succeed(settings),
      Triggers.layerWith(tuning),
      ZLayer.derive[TriggersHttp],
      ZLayer(Ref.make(List.empty[muliao.sessions.SessionEvent]).map(S.EventLog(_)))
    )

  /** 接好测试工具并按组装入口的顺序启动：会话 → 触发器。 */
  def start(p: S.Probe): ZIO[Env, AppError, Unit] =
    for
      t <- Triggers.contribution
      _ <- S.start(p, List(t))
      _ <- ZIO.serviceWithZIO[Triggers](_.start)
    yield ()

  // ---- 读状态（都用真实时钟等待，免得测试时钟下卡住） ----

  def triggers: URIO[Triggers, Triggers] = ZIO.service[Triggers]

  def byName(name: String): ZIO[Triggers, AppError, TriggerView] =
    triggers.flatMap(t => t.named(name, orFinished = true).flatMap(t.view))

  /** 这个 agent 由触发器开出的会话（旧的在前）。 */
  def triggerSessions(agent: AgentId): ZIO[SessionConsole, AppError, List[SessionSummary]] =
    ZIO.serviceWithZIO[SessionConsole](_.list(Some(agent), None, None, 200)).map(_.items.filter(_.origin == OriginKind.Trigger).sortBy(_.createdAt))

  def eventually[R, A](what: String)(get: ZIO[R, AppError, A])(ok: A => Boolean): ZIO[R, AppError, A] =
    Live.live(S.eventually(what)(get)(ok))

  /** 等一会儿（真实时钟），用来确认「没有再发生什么」。 */
  def pause(d: Duration): UIO[Unit] = Live.live(ZIO.sleep(d))

  def inbox: ZIO[Notifications, AppError, List[NotificationView]] = ZIO.serviceWithZIO[Notifications](_.inbox(100)).map(_.items)

  def stateOf(id: SessionId): ZIO[Sessions, AppError, WorkState] = ZIO.serviceWithZIO[Sessions](_.state(id))

  def workspaceOf(agent: AgentId): URIO[ResourcePaths, Path] = ZIO.serviceWith[ResourcePaths](_.workspace(agent))

  def write(p: Path, content: String): UIO[Unit] =
    ZIO.attemptBlocking { JFiles.createDirectories(p.getParent); JFiles.writeString(p, content) }.orDie.unit

  def read(p: Path): UIO[String] = ZIO.attemptBlocking(if JFiles.exists(p) then JFiles.readString(p) else "").orDie

  def schedule(name: String, agent: AgentId, cron: String, prompt: String = "定时任务", precheck: Option[PrecheckSpec] = None): NewTrigger =
    NewTrigger(name, agent, TriggerConfig.Schedule(cron), prompt, precheck = precheck)

  def streamOf(name: String, agent: AgentId, command: String, debounce: Double = 0.3, precheck: Option[PrecheckSpec] = None): NewTrigger =
    NewTrigger(name, agent, TriggerConfig.Stream(command, Nil, debounce), "处理事件", precheck = precheck)

  /** 不经工具直接用到的类型，免得测试文件逐个 import。 */
  export muliao.sessions.TestEnv.{Script, dirs, fakeModels, probe, text, call, lastUserText, userTexts, agent, context, outcome}
