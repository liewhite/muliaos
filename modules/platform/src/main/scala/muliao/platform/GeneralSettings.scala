package muliao.platform

import muliao.platform.config.ConfigCodecs.given
import muliao.platform.config.ConfigSection
import zio.json.*

import java.nio.file.Path
import java.time.ZoneId

/** 配置文件 `[general]` 段。
  * @param dataDir   数据目录（数据库、工作目录、附件、日志等）
  * @param assetsDir 资源目录（记忆、skill、MCP、secret、agent 配置与权限规则）
  * @param timezone  按天统计、定时触发用的时区，如 `Asia/Shanghai`
  * @param offsiteBackupDir 异地备份目录（如 iCloud Drive 里的一个文件夹）：平台不往里写，只把它当平台私有文件，备份脚本从这里读路径
  */
@jsonMemberNames(SnakeCase)
final case class GeneralSettings(
    dataDir: Path,
    assetsDir: Path,
    timezone: ZoneId = ZoneId.of("Asia/Shanghai"),
    offsiteBackupDir: Option[Path] = None
) derives JsonCodec:
  def databaseFile: Path = dataDir.resolve("muliao.db")

object GeneralSettings:
  val section: ConfigSection[GeneralSettings] = ConfigSection("general")
