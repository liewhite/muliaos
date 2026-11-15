package muliao.resources.tree

import muliao.contract.{AgentName, AssetKind, SkillName, Thinking}

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

/** MCP server 的连接方式。字符串里可以写 `${vault:KEY}` 引用 secret。 */
enum McpTransport:
  case Stdio(command: String, args: List[String], env: Map[String, String])
  case Http(url: String, headers: Map[String, String])

final case class McpConfig(transport: McpTransport, enabled: Boolean):
  /** 引用到的 secret 名。 */
  def secretRefs: Set[String] =
    val texts = transport match
      case McpTransport.Stdio(c, args, env) => c :: args ++ env.values
      case McpTransport.Http(url, headers)  => url :: headers.values.toList
    texts.flatMap(t => McpConfig.VaultRef.findAllMatchIn(t).map(_.group(1))).toSet

object McpConfig:
  val VaultRef = """\$\{vault:([A-Za-z0-9_]+)\}""".r

final case class McpDoc(name: String, owner: Owner, file: Path, config: McpConfig)

/** secret 文件的内容：说明、密文、能否注入自写命令。 */
final case class SecretFile(description: String, ciphertext: String, allowShell: Boolean)

final case class SecretDoc(key: String, owner: Owner, file: Path, content: SecretFile)
