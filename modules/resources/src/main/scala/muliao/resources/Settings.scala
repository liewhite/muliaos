package muliao.resources

import muliao.contract.AgentId
import muliao.platform.AppError
import muliao.platform.config.ConfigCodecs.given
import muliao.platform.config.ConfigSection
import muliao.toolkit.Access
import zio.*
import zio.json.*

import java.nio.file.Path

/** 配置文件 `[resources]` 段。
  * @param memoryLimit  每块记忆的长度上限（规整后的字符数）
  * @param writableDirs 每个 agent 的命令都能写的常用目录（临时、缓存、工具链），`~/` 开头的按用户主目录展开
  */
@jsonMemberNames(SnakeCase)
final case class ResourcesSettings(
    memoryLimit: Int = 4000,
    writableDirs: List[Path] = ResourcesSettings.defaultWritable
) derives JsonCodec

object ResourcesSettings:
  val section: ConfigSection[ResourcesSettings] = ConfigSection("resources")

  private def home(p: String): Path = muliao.platform.config.ConfigCodecs.expandHome(p)

  val defaultWritable: List[Path] = List(
    Path.of("/tmp"),
    Path.of("/private/tmp"),
    Path.of("/private/var/folders"),
    home("~/Library/Caches"),
    home("~/.cache"),
    home("~/.npm"),
    home("~/.cargo"),
    home("~/.rustup"),
    home("~/go"),
    home("~/.local/share/uv"),
    home("~/.ivy2"),
    home("~/.sbt"),
    home("~/.m2"),
    home("~/.gradle"),
    home("~/.bun"),
    home("~/.pnpm-store")
  )

/** 配置文件 `[vault]` 段：secret 加密用的主密钥（Fernet 密钥，URL-safe base64 的 32 字节）。 */
final case class VaultSettings(key: String) derives JsonCodec:
  override def toString: String = "VaultSettings(***)"

object VaultSettings:
  val section: ConfigSection[VaultSettings] = ConfigSection("vault")

/** 本领域用到的目录。
  * @param assets       资源目录
  * @param workspaces   各 agent 工作目录的上级（`<workspaces>/<agent 编号>`）
  * @param state        本领域自己的状态文件（如上一份有效的 access.toml）
  * @param privatePaths 平台私有文件（配置、备份、推送私钥等），非 admin 的命令读写都不行
  */
final case class ResourcePaths(assets: Path, workspaces: Path, state: Path, privatePaths: List[Path]):
  def workspace(id: AgentId): Path = workspaces.resolve(id.value)
  def lastGoodAccess: Path         = state.resolve("access.last-good.toml")

/** 模型配置（由模型领域在组装时提供）：默认模型、某个模型写法能不能跑。 */
trait ModelCheck:
  def defaultModel: String
  /** 不能跑时说明原因。 */
  def check(model: String): Option[String]

/** 平台的全部内置工具（由组装入口提供）：名字与类别，用于按规则分发和校验新规则。 */
final case class ToolInfo(name: String, access: Access)

trait ToolCatalog:
  def tools: List[ToolInfo]

object ToolCatalog:
  def fixed(ts: List[ToolInfo]): ToolCatalog = new ToolCatalog:
    val tools = ts

/** 别的领域对 agent 生命周期的参与：它忙不忙（忙时不能归档、删除），删除时清理自己的数据。
  * 下游领域实现后在组装时登记到 [[AgentHooks]]。
  */
trait AgentHook:
  /** 登记方的名字（日志与报错里用）。 */
  def name: String

  /** 忙时说明在忙什么（在运行、在等后台任务/委派/唤醒……），空闲为 None。 */
  def busy(agent: AgentId): IO[AppError, Option[String]]

  /** 删除 agent 时清理本领域的数据。必须可重复执行：删到一半重启后会再调一次。 */
  def cleanup(agent: AgentId): IO[AppError, Unit]

final class AgentHooks private (hooks: Ref[List[AgentHook]]):
  def register(h: AgentHook): UIO[Unit] = hooks.update(_ :+ h)
  def all: UIO[List[AgentHook]]          = hooks.get

object AgentHooks:
  val layer: ULayer[AgentHooks] = ZLayer(Ref.make(List.empty[AgentHook]).map(AgentHooks(_)))
