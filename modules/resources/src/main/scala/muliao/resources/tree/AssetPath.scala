package muliao.resources.tree

import muliao.contract.AgentName
import muliao.resources.access.Kind

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
  */
enum AssetPath:
  case AccessFile
  case SharedMemory(name: String)
  case PrivateMemory(agent: AgentName)
  case AgentFile(agent: AgentName)
  case SkillMain(owner: Owner, name: String)
  case SkillAttachment(owner: Owner, name: String, relative: String)
  case Mcp(owner: Owner, name: String)
  case Secret(owner: Owner, key: String)
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
  def subject: Option[(Kind, String)] = this match
    case SharedMemory(n)          => Some(Kind.Memory -> n)
    case SkillMain(_, n)          => Some(Kind.Skill -> n)
    case SkillAttachment(_, n, _) => Some(Kind.Skill -> n)
    case Mcp(_, n)                => Some(Kind.Mcp -> n)
    case Secret(_, k)             => Some(Kind.Secret -> k)
    case _                        => None

object AssetPath:

  /** 按相对资源目录根的路径分类。 */
  def classify(relative: Path): AssetPath =
    val parts = (0 until relative.getNameCount).map(relative.getName(_).toString).toList
    if parts.isEmpty then Directory(None)
    else if parts.exists(_.startsWith(".")) then Hidden
    else
      val owner = if parts.head == Owner.PlatformDir then Owner.Platform else Owner.Agent(AgentName.wrap(parts.head))
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
    if relative.getNameCount < 2 then None
    else
      relative.getName(0).toString match
        case Owner.PlatformDir => Some(Owner.Platform)
        case n                 => Some(Owner.Agent(AgentName.wrap(n)))

  /** 资源文件的规定位置（报错时提示用）。 */
  val layoutHint: String =
    "资源只能放在：platform/access.toml、platform/memory/<名>.md、<agent>/agent.toml、<agent>/memory.md、" +
      "<owner>/skills/<名>/…、<owner>/mcp/<名>.json、<owner>/secrets/<KEY>.toml（owner 是 platform 或 agent 名）"
