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

object AppConfig:

  private val required: List[ConfigSection[?]] = List(
    GeneralSettings.section,
    LogSettings.section,
    ServerSettings.section,
    HttpSettings.section,
    ResourcesSettings.section,
    VaultSettings.section,
    ModelSettings.section,
    SessionSettings.section,
    PushSettings.section,
    TriggerSettings.section
  )

  private val optional: List[ConfigSection[?]] = List(WebSettings.section, TranscriptionSettings.section)

  /** 配置文件里允许出现的顶层段。 */
  val sectionNames: Set[String] = (required ++ optional).map(_.key).toSet

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
