package muliao.resources.tree

import muliao.contract.{AgentName, AssetKind}

import java.nio.file.Path

/** 资源目录里一个文件的位置意味着什么。资源目录的布局：
  * {{{
  * platform/access.toml              角色与各 agent 的权限
  * platform/memory/<名>.md           全局记忆 global 与共享记忆
  * <owner>/skills/<名>/SKILL.md …    skill（其余文件是附件）
  * <owner>/mcp/<名>.json             MCP server
  * <owner>/secrets/<KEY>.toml        secret
  * <agent>/agent.toml、memory.md     agent 配置与私有记忆
  * }}}
  * `<owner>` 是 `platform` 或 agent 名。`.` 开头的文件与目录不读也不校验。
  * 位置与路径的互换只在这里：路径 → 位置是 [[AssetPath.classify]]，资源文件 → 路径是 [[AssetPath.render]]。
  */
enum AssetPath:
  case AccessFile                                                    extends AssetPath, ParsedFile
  case SharedMemory(name: String)                                    extends AssetPath, ParsedFile
  case PrivateMemory(agent: AgentName)                               extends AssetPath, ParsedFile
  case AgentFile(agent: AgentName)                                   extends AssetPath, ParsedFile
  case SkillMain(owner: Owner, name: String)                         extends AssetPath, ParsedFile
  case SkillAttachment(owner: Owner, name: String, relative: String) extends AssetPath, ResourceFile
  case Mcp(owner: Owner, name: String)                               extends AssetPath, ParsedFile
  case Secret(owner: Owner, key: String)                             extends AssetPath, ParsedFile
  /** 目录本身（agent 目录、skills 目录、某个 skill 目录等），不是资源文件。 */
  case Directory(owner: Option[Owner])
  case Hidden
  case Misplaced

  def ownedBy: Option[Owner] = this match
    case AccessFile | SharedMemory(_)          => Some(Owner.Platform)
    case PrivateMemory(a)                      => Some(Owner.Agent(a))
    case AgentFile(a)                          => Some(Owner.Agent(a))
    case SkillMain(o, _)                       => Some(o)
    case SkillAttachment(o, _, _)              => Some(o)
    case Mcp(o, _)                             => Some(o)
    case Secret(o, _)                          => Some(o)
    case Directory(o)                          => o
    case Hidden | Misplaced                    => None

  /** 这个文件属于哪个按名字分发的资源。 */
  def subject: Option[(AssetKind, String)] = this match
    case SharedMemory(n)          => Some(AssetKind.Memory -> n)
    case SkillMain(_, n)          => Some(AssetKind.Skill -> n)
    case SkillAttachment(_, n, _) => Some(AssetKind.Skill -> n)
    case Mcp(_, n)                => Some(AssetKind.Mcp -> n)
    case Secret(_, k)             => Some(AssetKind.Secret -> k)
    case _                        => None

/** 资源目录里一个规定位置上的文件（[[AssetPath]] 里除目录、隐藏、放错位置以外的那些）。 */
sealed trait ResourceFile:
  self: AssetPath =>

  /** 不该让别人读到：新建时权限只给主人。 */
  def privateFile: Boolean = this match
    case AssetPath.Secret(_, _) => true
    case _                      => false

/** 内容由平台解析的资源文件（skill 附件以外的），规格见 [[FileSpec]]。 */
sealed trait ParsedFile extends ResourceFile:
  self: AssetPath =>

object AssetPath:

  /** 资源文件相对资源目录根的路径：`classify(render(f)) == f`。 */
  def render(f: ResourceFile): Path = f match
    case AccessFile                   => Path.of(Owner.PlatformDir, "access.toml")
    case SharedMemory(n)              => Path.of(Owner.PlatformDir, "memory", s"$n.md")
    case PrivateMemory(a)             => Path.of(a.value, "memory.md")
    case AgentFile(a)                 => Path.of(a.value, "agent.toml")
    case SkillMain(o, n)              => skillDir(o, n).resolve(SkillDoc.Main)
    case SkillAttachment(o, n, rel)   => skillDir(o, n).resolve(rel)
    case Mcp(o, n)                    => ownerDir(o).resolve("mcp").resolve(s"$n.json")
    case Secret(o, k)                 => ownerDir(o).resolve("secrets").resolve(s"$k.toml")

  /** 一项按名字分发的资源归 `owner` 时放在哪（skill 是目录）；共享记忆只能归平台、工具不是文件，其余为 None。 */
  def home(kind: AssetKind, name: String, owner: Owner): Option[Path] = kind match
    case AssetKind.Skill  => Some(skillDir(owner, name))
    case AssetKind.Mcp    => Some(render(Mcp(owner, name)))
    case AssetKind.Secret => Some(render(Secret(owner, key = name)))
    case AssetKind.Memory => Option.when(owner == Owner.Platform)(render(SharedMemory(name)))
    case AssetKind.Tool   => None

  /** 主人的目录（`platform` 或 agent 名）。 */
  def ownerDir(o: Owner): Path = Path.of(o.label)

  /** skill 的目录。 */
  def skillDir(o: Owner, name: String): Path = ownerDir(o).resolve("skills").resolve(name)

  /** 按相对资源目录根的路径分类。 */
  def classify(relative: Path): AssetPath =
    val parts = (0 until relative.getNameCount).map(relative.getName(_).toString).toList
    if parts.isEmpty then Directory(None)
    else if parts.exists(_.startsWith(".")) then Hidden
    else
      val owner = Owner.ofLabel(parts.head)
      (owner, parts.tail) match
        case (_, Nil)                                        => Directory(Some(owner))
        case (Owner.Platform, List("access.toml"))           => AccessFile
        case (Owner.Platform, List("memory"))                => Directory(Some(owner))
        case (Owner.Platform, List("memory", f)) if f.endsWith(".md") => SharedMemory(f.stripSuffix(".md"))
        case (Owner.Agent(a), List("memory.md"))             => PrivateMemory(a)
        case (Owner.Agent(a), List("agent.toml"))            => AgentFile(a)
        case (_, List("skills"))                             => Directory(Some(owner))
        case (_, List("skills", _))                          => Directory(Some(owner))
        case (_, "skills" :: n :: List(SkillDoc.Main))       => SkillMain(owner, n)
        case (_, "skills" :: n :: rest)                      => SkillAttachment(owner, n, rest.mkString("/"))
        case (_, List("mcp"))                                => Directory(Some(owner))
        case (_, List("mcp", f)) if f.endsWith(".json")      => Mcp(owner, f.stripSuffix(".json"))
        case (_, List("secrets"))                            => Directory(Some(owner))
        case (_, List("secrets", f)) if f.endsWith(".toml")  => Secret(owner, f.stripSuffix(".toml"))
        case _                                               => Misplaced

  /** 路径第一段对应的主人（资源目录根下的文件没有主人）。 */
  def ownerOf(relative: Path): Option[Owner] =
    Option.when(relative.getNameCount >= 2)(Owner.ofLabel(relative.getName(0).toString))

  /** 资源文件的规定位置（报错时提示用）。 */
  val layoutHint: String =
    "资源只能放在：platform/access.toml、platform/memory/<名>.md、<agent>/agent.toml、<agent>/memory.md、" +
      "<owner>/skills/<名>/…、<owner>/mcp/<名>.json、<owner>/secrets/<KEY>.toml（owner 是 platform 或 agent 名）"
