package muliao.resources.tree

import muliao.contract.SkillName
import muliao.resources.access.AccessConfig

import java.nio.file.Path

/** 一个资源文件解析出来的内容。 */
enum Parsed:
  case Access(config: AccessConfig)
  case Agent(profile: AgentProfile)
  case Memory(doc: MemoryDoc)
  case Skill(doc: SkillDoc)
  case Mcp(doc: McpDoc)
  case Secret(doc: SecretDoc)

/** 每种资源文件的规格（名字怎么写、内容怎么解析）只写在这里：扫描资源目录与写入口（[[muliao.resources.guard.AssetGuard]]）都经 [[load]]。 */
object FileSpec:

  /** 解析一个资源文件。名字不合规、内容读不懂时给出原因。
    * @param path   文件在磁盘上的路径（记进解析结果）
    * @param exists skill 里引用的文件（相对 skill 目录）在不在
    */
  def load(file: ParsedFile, path: Path, text: String, exists: String => Boolean): Either[String, Parsed] =
    file match
      case AssetPath.AccessFile       => AccessConfig.parse(text).map(Parsed.Access(_))
      case AssetPath.AgentFile(_)     => Formats.parseAgent(text).map(Parsed.Agent(_))
      case AssetPath.PrivateMemory(a) => Right(Parsed.Memory(MemoryDoc(MemoryScope.Private(a), path, text)))
      case AssetPath.SharedMemory(n) =>
        slug(n, "共享记忆名").map(_ => Parsed.Memory(MemoryDoc(MemoryScope.Shared(n), path, text)))
      case AssetPath.SkillMain(o, n) =>
        slug(n, "skill 名（目录名）")
          .flatMap(_ => Formats.parseSkill(text, n, exists))
          .map(m => Parsed.Skill(SkillDoc(SkillName.wrap(n), o, path.getParent, m.description, m.approval)))
      case AssetPath.Mcp(o, n) =>
        slug(n, "MCP 名").flatMap(_ => Formats.parseMcp(text)).map(c => Parsed.Mcp(McpDoc(n, o, path, c)))
      case AssetPath.Secret(o, k) =>
        Either
          .cond(Formats.validSecretKey(k), (), "secret 名只能用大写字母、数字和下划线，字母开头")
          .flatMap(_ => Formats.parseSecret(text))
          .map(s => Parsed.Secret(SecretDoc(k, o, path, s)))

  private def slug(name: String, what: String): Either[String, Unit] =
    Either.cond(Formats.validSlug(name), (), s"${what}只能用小写字母、数字和 -")
