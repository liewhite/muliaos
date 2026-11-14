package muliao.resources.access

import muliao.contract.{AgentId, AgentName, RoleName}
import muliao.resources.tree.TomlFile

/** 一个角色：一组允许与禁止规则。 */
final case class RoleDef(description: String, allow: List[Rule], deny: List[Rule])

/** 某 agent 在 access.toml 里的部分：它的角色（base 之外）和自己的额外规则。 */
final case class AgentGrants(roles: List[RoleName], allow: List[Rule], deny: List[Rule]):
  def isEmpty: Boolean = roles.isEmpty && allow.isEmpty && deny.isEmpty

object AgentGrants:
  val empty: AgentGrants = AgentGrants(Nil, Nil, Nil)

/** `platform/access.toml` 的内容：角色定义，各 agent 的角色与规则。内置角色 base、admin 在这里只写可编辑的部分（说明与额外规则）。 */
final case class AccessConfig(roles: Map[RoleName, RoleDef], agents: Map[AgentId, AgentGrants]):

  def role(name: RoleName): Option[RoleDef] =
    roles.get(name).orElse(BuiltinRole.of(name).map(b => RoleDef(b.description, Nil, Nil)))

  def grantsOf(agent: AgentId): AgentGrants = agents.getOrElse(agent, AgentGrants.empty)

  /** 全部角色名（含没写进文件的内置角色）。 */
  def roleNames: List[RoleName] = (roles.keySet ++ BuiltinRole.values.map(_.roleName)).toList.sortBy(_.value)

  /** 文件里出现的所有规则原文。 */
  def allRules: Set[String] =
    (roles.values.flatMap(r => r.allow ++ r.deny) ++ agents.values.flatMap(g => g.allow ++ g.deny)).map(_.text).toSet

  /** 引用关系上的问题（引用了不存在的角色、不存在的 agent）。规则本身的语法在解析时已检查。 */
  def problems(liveAgents: Set[AgentId]): List[String] =
    val badRoles = agents.toList.flatMap { (id, g) =>
      g.roles.filterNot(r => roles.contains(r) || BuiltinRole.of(r).isDefined).map(r => s"[agents.${id.value}] 引用了不存在的角色 ${r.value}")
    }
    val badAgents = agents.keySet.filterNot(liveAgents).toList.map(id => s"[agents.${id.value}] 不是现有的 agent")
    (badRoles ++ badAgents).sorted

object AccessConfig:
  val empty: AccessConfig = AccessConfig(Map.empty, Map.empty)

  private val RoleNamePattern = "[A-Za-z0-9_-]{1,64}"

  def parse(text: String): Either[String, AccessConfig] =
    for
      top    <- TomlFile.parse(text).map(TomlFile.fields)
      _      <- top.only(Set("roles", "agents"))
      rolesT <- top.tables("roles")
      roles <- traverse(rolesT) { (name, f) =>
                 for
                   n    <- Either.cond(name.matches(RoleNamePattern), RoleName.wrap(name), s"角色名 `$name` 只能用字母、数字、- 和 _")
                   _    <- f.only(Set("description", "allow", "deny"))
                   desc <- f.string("description")
                   al   <- f.strings("allow").flatMap(rules(s"[roles.$name] allow"))
                   de   <- f.strings("deny").flatMap(rules(s"[roles.$name] deny"))
                 yield n -> RoleDef(desc.getOrElse(""), al, de)
               }
      agentsT <- top.tables("agents")
      agents <- traverse(agentsT) { (id, f) =>
                  for
                    _  <- f.only(Set("roles", "allow", "deny"))
                    rs <- f.strings("roles")
                    al <- f.strings("allow").flatMap(rules(s"[agents.$id] allow"))
                    de <- f.strings("deny").flatMap(rules(s"[agents.$id] deny"))
                  yield AgentId.wrap(id) -> AgentGrants(rs.map(RoleName.wrap), al, de)
                }
    yield AccessConfig(roles.toMap, agents.toMap)

  private def rules(where: String)(texts: List[String]): Either[String, List[Rule]] =
    traverse(texts)(t => Rule.parse(t).left.map(e => s"$where：$e"))

  private def traverse[A, B](xs: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    xs.foldLeft[Either[String, List[B]]](Right(Nil))((acc, a) => acc.flatMap(bs => f(a).map(bs :+ _)))

  /** 写成固定格式：角色按名字排序，agent 按名字排序并在表头后注明名字；空的 agent 段不写。 */
  def render(c: AccessConfig, names: AgentId => Option[AgentName]): String =
    def ruleLines(rs: List[Rule], key: String) = Option.when(rs.nonEmpty)(s"$key = ${TomlFile.strArray(rs.map(_.text))}")
    val roleBlocks = c.roles.toList.sortBy(_._1.value).map { (n, r) =>
      (List(s"[roles.${TomlFile.key(n.value)}]", s"description = ${TomlFile.str(r.description)}") ++
        ruleLines(r.allow, "allow") ++ ruleLines(r.deny, "deny")).mkString("\n")
    }
    val agentBlocks = c.agents.toList
      .filterNot(_._2.isEmpty)
      .sortBy((id, _) => (names(id).map(_.value).getOrElse("~"), id.value))
      .map { (id, g) =>
        val header = s"[agents.${TomlFile.key(id.value)}]" + names(id).fold("")(n => s" # ${n.value}")
        (List(header) ++ Option.when(g.roles.nonEmpty)(s"roles = ${TomlFile.strArray(g.roles.map(_.value))}") ++
          ruleLines(g.allow, "allow") ++ ruleLines(g.deny, "deny")).mkString("\n")
      }
    (roleBlocks ++ agentBlocks).mkString("", "\n\n", "\n")

/** 内置角色：base 人人都有（全部非管理类工具、全局记忆），admin 拥有一切（全部工具、能用能改全部资源）。 */
enum BuiltinRole(val name: String, val description: String):
  case Base  extends BuiltinRole("base", "每个 agent 都有：全部非管理类工具、全局记忆，加上这里列出的资源")
  case Admin extends BuiltinRole("admin", "全部工具，能用、能改全部资源")

  def roleName: RoleName = RoleName.wrap(name)

object BuiltinRole:
  def of(name: RoleName): Option[BuiltinRole] = values.find(_.name == name.value)
