package muliao.app

import muliao.models.llm.ModelSettings
import muliao.models.speech.TranscriptionSettings
import muliao.models.web.WebSettings
import muliao.notifications.PushSettings
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.config.{ConfigFile, ConfigSection}
import muliao.platform.http.HttpSettings
import muliao.platform.log.LogSettings
import muliao.platform.server.ServerSettings
import muliao.resources.{ResourcePaths, ResourcesSettings, VaultSettings}
import muliao.sessions.{SessionPaths, SessionSettings}
import muliao.triggers.TriggerSettings
import zio.*

import java.nio.file.Path

/** 整个配置文件读成的各段。可选段（网页工具、语音转写）没配就是 None，对应功能不提供。 */
final case class AppConfig(
    path: Path,
    general: GeneralSettings,
    log: LogSettings,
    server: ServerSettings,
    http: HttpSettings,
    resources: ResourcesSettings,
    vault: VaultSettings,
    models: ModelSettings,
    web: Option[WebSettings],
    transcription: Option[TranscriptionSettings],
    sessions: SessionSettings,
    push: PushSettings,
    triggers: TriggerSettings
):
  /** 数据目录下各领域用到的位置。 */
  def vapidKeyFile: Path = push.vapidKeyFile.getOrElse(general.dataDir.resolve("vapid_private.pem"))

  def resourcePaths: ResourcePaths =
    ResourcePaths(
      assets = general.assetsDir,
      workspaces = general.dataDir.resolve("workspaces"),
      state = general.dataDir.resolve("resources"),
      // 平台私有文件：配置（含密码、密钥）、备份、推送私钥
      privatePaths = List(path, general.dataDir.resolve("backups"), vapidKeyFile)
    )

  def sessionPaths: SessionPaths = SessionPaths(general.dataDir.resolve("files"))

  /** 各领域要的配置值与位置。 */
  def layer: ULayer[
    GeneralSettings & ServerSettings & HttpSettings & ResourcesSettings & VaultSettings & ModelSettings & SessionSettings & PushSettings &
      TriggerSettings & ResourcePaths & SessionPaths
  ] =
    ZLayer.succeedEnvironment(
      ZEnvironment(general, server, http, resources, vault)
        .add(models)
        .add(sessions)
        .add(push)
        .add(triggers)
        .add(resourcePaths)
        .add(sessionPaths)
    )

object AppConfig:

  /** 平台自己的段（领域的段由各领域的 `Module` 声明）。 */
  private val platformSections: List[ConfigSection[?]] =
    List(GeneralSettings.section, LogSettings.section, ServerSettings.section, HttpSettings.section)

  /** 配置文件里允许出现的顶层段。 */
  val sectionNames: Set[String] =
    (platformSections ++ Assembly.domains.flatMap(m => m.sections ++ m.optionalSections)).map(_.key).toSet

  def load(path: Path): IO[AppError, AppConfig] =
    for
      f     <- ConfigFile.load(path.toAbsolutePath, sectionNames)
      gen   <- f.section(GeneralSettings.section)
      log   <- f.section(LogSettings.section)
      srv   <- f.section(ServerSettings.section)
      http  <- f.section(HttpSettings.section)
      res   <- f.section(ResourcesSettings.section)
      vault <- f.section(VaultSettings.section)
      mod   <- f.section(ModelSettings.section)
      web   <- f.optionalSection(WebSettings.section)
      tr    <- f.optionalSection(TranscriptionSettings.section)
      ses   <- f.section(SessionSettings.section)
      push  <- f.section(PushSettings.section)
      trig  <- f.section(TriggerSettings.section)
    yield AppConfig(path.toAbsolutePath, gen, log, srv, http, res, vault, mod, web, tr, ses, push, trig)
