package muliao.resources.tree

import muliao.contract.{AgentName, AssetKind, RevealScope, SecretContent, SkillName}
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
final case class AgentProfile(description: String, model: String, group: Option[String])

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

/** secret 的值怎么保存，连同内容与能否取出明文（#1069）：服务端主密钥加密（Fernet 密文，用时服务端自己解）、用户的 passkey 加密
  * （服务端只存，用时等用户在浏览器里解开），或明文（#1093，只能是口令，见 [[muliao.contract.SecretValue]]）。
  * 签名在哪做由它推出：服务端的在服务端签，passkey 的在用户浏览器里签。
  */
enum SecretBody:
  case Server(token: String, override val content: SecretContent)
  case Passkey(box: muliao.contract.PasskeyBox, override val content: SecretContent)
  case Plain(text: String, scope: RevealScope)

  def content: SecretContent = this match
    case Server(_, c)  => c
    case Passkey(_, c) => c
    case Plain(_, s)   => SecretContent.Opaque(s)

  /** 界面上看到的（加密的不含值）。 */
  def view: muliao.contract.SecretView = this match
    case Server(_, c)  => muliao.contract.SecretView.Server(c)
    case Passkey(_, c) => muliao.contract.SecretView.Passkey(c)
    case Plain(t, s)   => muliao.contract.SecretView.Plain(t, s)

  /** 只改明文范围（保存方式、值、内容类型不变）；只能签名的没有范围可改。 */
  def withScope(s: RevealScope): Either[String, SecretBody] = this match
    case Server(t, c)  => c.withScope(s).map(Server(t, _))
    case Passkey(b, c) => c.withScope(s).map(Passkey(b, _))
    case Plain(t, _)   => Right(Plain(t, s))

  /** 手改文件不许动的部分：去掉说明以外能手改的（明文范围；明文保存的还有值），手改前后这部分要相同。 */
  def fixed: SecretBody = this match
    case Server(t, c)  => Server(t, c.unscoped)
    case Passkey(b, c) => Passkey(b, c.unscoped)
    case Plain(_, _)   => Plain("", RevealScope.Shell)

/** secret 文件的内容：说明与值（怎么保存、内容与能否取出明文）。 */
final case class SecretFile(description: String, body: SecretBody)

final case class SecretDoc(key: String, owner: Owner, file: Path, content: SecretFile)
