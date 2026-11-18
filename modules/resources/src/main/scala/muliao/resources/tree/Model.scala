package muliao.resources.tree

import muliao.contract.{AgentName, AssetKind, SkillName, Thinking}
import muliao.toolkit.McpTransport

import java.nio.file.Path

/** 资源归谁：文件在谁的目录下就归谁。 */
enum Owner:
  case Platform
  case Agent(name: AgentName)

  def label: String = this match
    case Platform    => Owner.PlatformDir
    case Agent(name) => name.value

object Owner:
  /** 资源目录下归平台的那个子目录名；agent 不能叫这个名字。 */
  val PlatformDir = "platform"

  /** [[Owner.label]] 的反向：资源目录下的一级目录名对应的主人（不查 agent 是否存在）。 */
  def ofLabel(label: String): Owner = if label == PlatformDir then Platform else Agent(AgentName.wrap(label))

/** `agent.toml`：agent 的可编辑配置（名字、归档、权限不在这里）。 */
final case class AgentProfile(description: String, model: String, thinking: Thinking, group: Option[String])

/** 没加载的文件和原因。`subject` 是能认出来的资源（种类、名字），用来判断告诉谁。 */
final case class Problem(path: Path, reason: String, owner: Option[Owner], subject: Option[(AssetKind, String)])

/** 记忆的范围：平台的（全局记忆与各共享记忆，按名字分发）或某 agent 私有的。 */
enum MemoryScope:
  case Shared(name: String)
  case Private(agent: AgentName)

final case class MemoryDoc(scope: MemoryScope, file: Path, content: String):
  def owner: Owner = scope match
    case MemoryScope.Shared(_)     => Owner.Platform
    case MemoryScope.Private(name) => Owner.Agent(name)

object MemoryDoc:
  val GlobalName = "global"

/** 一个 skill：目录 + SKILL.md 的 frontmatter。`approval` 是要用户批准才执行的脚本（相对 skill 目录）。 */
final case class SkillDoc(name: SkillName, owner: Owner, dir: Path, description: String, approval: Set[String]):
  def main: Path = dir.resolve(SkillDoc.Main)

object SkillDoc:
  val Main = "SKILL.md"

/** 一个 MCP 资源文件：连接方式与是否启用。 */
final case class McpConfig(transport: McpTransport, enabled: Boolean):
  /** 引用到的 secret 名。 */
  def secretRefs: Set[String] = transport.secretRefs

final case class McpDoc(name: String, owner: Owner, file: Path, config: McpConfig)

/** secret 的值怎么锁着：服务端主密钥（Fernet 密文，用时服务端自己解），用户的 passkey（服务端只存，用时等用户在浏览器里解开），
  * 或签名钥匙（passkey 加密的私钥，只能请用户在浏览器里签名，取不出明文，#938）。与 `allowShell`（解开后能注入到哪）互不相干。
  */
enum SecretLock:
  case Server(token: String)
  case Passkey(box: muliao.contract.PasskeyBox)
  case Signing(algorithm: muliao.contract.SigningAlgorithm, box: muliao.contract.PasskeyBox)

/** secret 文件的内容：说明、密文、能否注入自写命令。 */
final case class SecretFile(description: String, lock: SecretLock, allowShell: Boolean)

final case class SecretDoc(key: String, owner: Owner, file: Path, content: SecretFile)
