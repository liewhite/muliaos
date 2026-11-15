package muliao.resources.access

import muliao.contract.{AgentId, AgentName, RoleName, AssetKind}
import muliao.resources.tree.Owner
import muliao.toolkit.Access

import java.nio.file.Path

/** 规则从哪来。 */
enum Source:
  case Role(name: RoleName)
  /** agent 自己在 access.toml 里的额外规则 */
  case Own

  def label: String = this match
    case Role(n) => s"角色 ${n.value}"
    case Own     => "自己的规则"

/** 为什么能用或能改。 */
enum Grant:
  /** 资源归它自己 */
  case Ownership
  /** base：全部非管理类工具 */
  case NonAdminTools
  /** base：全局记忆人人可读 */
  case GlobalMemory
  /** admin：全部工具，能用能改全部资源 */
  case Everything
  case ByRule(rule: Rule, source: Source)

  def label: String = this match
    case Ownership       => "自己的资源"
    case NonAdminTools   => "角色 base：全部非管理类工具"
    case GlobalMemory    => "角色 base：全局记忆"
    case Everything      => "角色 admin：全部"
    case ByRule(r, s)    => s"${s.label}：${r.text}"

enum Decision:
  case Allowed(by: Grant)
  case Denied(rule: Rule, source: Source)
  case NotGranted

  def allowed: Boolean = this match
    case Allowed(_) => true
    case _          => false

/** 一个 agent 的最终权限：base + 各角色 + 自己的规则，deny 优先（但不拦它自己的资源）。 */
final case class Permissions(
    agent: AgentId,
    name: AgentName,
    roles: List[RoleName],
    allow: List[(Rule, Source)],
    deny: List[(Rule, Source)]
):
  val isAdmin: Boolean = roles.contains(BuiltinRole.Admin.roleName)

  /** 能否对某工具（`toolAccess` 是它的类别）或某资源（主人 `owner`）做 `action`。 */
  def decide(action: Action, kind: AssetKind, name: String, owner: Owner, toolAccess: Option[Access] = None): Decision =
    if owner == Owner.Agent(this.name) && kind != AssetKind.Tool then Decision.Allowed(Grant.Ownership)
    else if kind == AssetKind.Memory && name == Permissions.GlobalMemory && action == Action.Use then Decision.Allowed(Grant.GlobalMemory)
    else
      deny.find(_._1.selects(action, kind, name)) match
        case Some((r, s)) => Decision.Denied(r, s)
        case None =>
          if isAdmin then Decision.Allowed(Grant.Everything)
          else if kind == AssetKind.Tool && action == Action.Use && toolAccess.exists(_ != Access.Admin) then Decision.Allowed(Grant.NonAdminTools)
          else allow.find(_._1.selects(action, kind, name)).fold(Decision.NotGranted)((r, s) => Decision.Allowed(Grant.ByRule(r, s)))

  def canUseTool(name: String, access: Access): Boolean =
    decide(Action.Use, AssetKind.Tool, name, Owner.Platform, Some(access)).allowed

  /** `write:path` 给的目录（被 deny 的 `write:path` 收回的在 [[deniedPaths]]）。 */
  def writablePaths: List[Path] = allow.flatMap((r, _) => Rule.pathOf(r)).distinct

  def deniedPaths: List[Path] = deny.flatMap((r, _) => Rule.pathOf(r)).distinct

object Permissions:
  val GlobalMemory = "global"

  def of(agent: AgentId, name: AgentName, access: AccessConfig): Permissions =
    val grants = access.grantsOf(agent)
    val roles  = (BuiltinRole.Base.roleName :: grants.roles).distinct
    val roleRules = roles.flatMap(r => access.role(r).map(d => (r, d)))
    Permissions(
      agent,
      name,
      roles,
      roleRules.flatMap((r, d) => d.allow.map(_ -> Source.Role(r))) ++ grants.allow.map(_ -> Source.Own),
      roleRules.flatMap((r, d) => d.deny.map(_ -> Source.Role(r))) ++ grants.deny.map(_ -> Source.Own)
    )
