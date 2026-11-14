package muliao.platform.server

import muliao.platform.config.ConfigCodecs.given
import muliao.platform.config.ConfigSection
import zio.json.*

import java.nio.file.Path

/** 配置文件 `[server]` 段。
  * @param host          监听地址
  * @param port          监听端口
  * @param publicUrl     对外地址（经反向代理访问时的 https 地址，通知里的链接用它）
  * @param password      登录密码（改了它，所有已登录设备失效）
  * @param cookieSecret  签登录 cookie 的密钥，至少 32 个字符
  * @param webDir        前端文件目录；不写则用打包在程序里的前端（开发时指向构建输出）
  */
@jsonMemberNames(SnakeCase)
final case class ServerSettings(
    host: String = "127.0.0.1",
    port: Int,
    publicUrl: Option[String] = None,
    password: String,
    cookieSecret: String,
    webDir: Option[Path] = None
) derives JsonCodec:
  override def toString: String = s"ServerSettings($host:$port, publicUrl=$publicUrl, webDir=$webDir)"

object ServerSettings:
  val section: ConfigSection[ServerSettings] = ConfigSection("server")
