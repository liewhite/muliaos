package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- agent ----

/** agent 列表里的一行。`state`：active / archived。 */
final case class AgentSummary(
    id: AgentId,
    name: AgentName,
    description: Option[String],
    group: Option[String],
    model: Option[String],
    state: String,
    isDefault: Boolean
) derives JsonCodec, Schema

/** 一项能用的工具或资源。`item` 形如 `skill:x`（工具就是工具名）；`use`/`write` 是凭哪条规则（没有写权限时 write 为空）。 */
final case class GrantView(item: String, kind: String, name: String, owner: Option[String], use: String, write: Option[String])
    derives JsonCodec, Schema

/** 没加载的资源文件。 */
final case class ProblemView(path: String, reason: String, owner: Option[String]) derives JsonCodec, Schema

final case class AgentDetail(
    summary: AgentSummary,
    thinking: Option[String],
    roles: List[String],
    allow: List[String],
    deny: List[String],
    tools: List[GrantView],
    assets: List[GrantView],
    writablePaths: List[String],
    workspace: String,
    assetsDir: String,
    problems: List[ProblemView],
    /** agent.toml 加载不了时的原因 */
    profileError: Option[String],
    /** access.toml 写坏了（沿用上一份）时的原因 */
    accessBroken: Option[String]
) derives JsonCodec, Schema

final case class AgentCreate(
    name: String,
    description: String,
    model: Option[String] = None,
    thinking: Option[String] = None,
    group: Option[String] = None,
    roles: List[String] = Nil,
    allow: List[String] = Nil,
    deny: List[String] = Nil
) derives JsonCodec, Schema

/** 只改给出的字段。`group` 给空串表示去掉分组。 */
final case class AgentUpdate(
    name: Option[String] = None,
    description: Option[String] = None,
    model: Option[String] = None,
    thinking: Option[String] = None,
    group: Option[String] = None,
    roles: Option[List[String]] = None,
    allow: Option[List[String]] = None,
    deny: Option[List[String]] = None,
    archived: Option[Boolean] = None,
    isDefault: Option[Boolean] = None
) derives JsonCodec, Schema

/** 新建或修改前预览最终权限。`agent` 给出时按这个 agent 算「自己的资源」。 */
final case class PreviewRequest(agent: Option[AgentId] = None, roles: List[String] = Nil, allow: List[String] = Nil, deny: List[String] = Nil)
    derives JsonCodec, Schema

final case class PreviewResult(tools: List[GrantView], assets: List[GrantView], writablePaths: List[String]) derives JsonCodec, Schema

final case class AgentDefaults(model: String, thinking: List[String]) derives JsonCodec, Schema

// ---- 角色 ----

final case class RoleView(name: RoleName, builtin: Boolean, description: String, allow: List[String], deny: List[String], members: List[AgentName])
    derives JsonCodec, Schema

final case class RoleWrite(description: String, allow: List[String] = Nil, deny: List[String] = Nil) derives JsonCodec, Schema

final case class RuleCheck(rule: String, error: Option[String], matches: List[String]) derives JsonCodec, Schema

final case class RulesRequest(rules: List[String]) derives JsonCodec, Schema

// ---- AI 资源 ----

/** 一项资源。`kind`：memory / skill / mcp / secret / agent（agent.toml）/ access；`owner`：platform 或 agent 名；`path` 相对资源目录。 */
final case class AssetItem(
    kind: String,
    name: String,
    owner: String,
    path: String,
    usedBy: List[AgentName],
    description: Option[String],
    enabled: Option[Boolean],
    allowShell: Option[Boolean]
) derives JsonCodec, Schema

final case class AssetIndex(root: String, items: List[AssetItem], problems: List[ProblemView], accessBroken: Option[String]) derives JsonCodec, Schema

/** 一个资源文件的内容与指纹（保存时带回指纹，期间被别人改了就冲突）。`path` 相对资源目录。 */
final case class AssetFile(path: String, content: String, fingerprint: Option[String]) derives JsonCodec, Schema

final case class AssetFileWrite(path: String, content: String, fingerprint: Option[String]) derives JsonCodec, Schema

final case class SkillFiles(name: String, owner: String, files: List[String]) derives JsonCodec, Schema

/** 设 secret（值只能设不能看）。`owner` 只在新建时有用，缺省归平台。 */
final case class SecretWrite(value: Option[String] = None, description: Option[String] = None, allowShell: Option[Boolean] = None, owner: Option[String] = None)
    derives JsonCodec, Schema

final case class AssetTransfer(to: String) derives JsonCodec, Schema

object ResourcesApi:
  private val agents = Api.secure.in("agents")
  private val roles  = Api.secure.in("roles")
  private val assets = Api.secure.in("assets")

  val listAgents = agents.get.out(jsonBody[List[AgentSummary]]).name("listAgents").description("现存的 agent（含归档的）。")

  val createAgent =
    agents.post.in(jsonBody[AgentCreate]).out(statusCode(StatusCode.Created)).out(jsonBody[AgentDetail]).name("createAgent")

  val getAgent = agents.get.in(path[AgentId]("id")).out(jsonBody[AgentDetail]).name("getAgent")

  val updateAgent =
    agents.patch.in(path[AgentId]("id")).in(jsonBody[AgentUpdate]).out(jsonBody[AgentDetail]).name("updateAgent")
      .description("只改给出的字段：改名、归档/取消归档（要空闲）、设为默认、配置、角色与规则。")

  val deleteAgent =
    agents.delete.in(path[AgentId]("id")).out(statusCode(StatusCode.NoContent)).name("deleteAgent")
      .description("不可撤销；它忙时拒绝（Conflict）。")

  val previewAgent =
    Api.secure.post.in("agents-preview").in(jsonBody[PreviewRequest]).out(jsonBody[PreviewResult]).name("previewAgent")

  val agentDefaults = Api.secure.get.in("agent-defaults").out(jsonBody[AgentDefaults]).name("agentDefaults")

  val listRoles = roles.get.out(jsonBody[List[RoleView]]).name("listRoles")

  val putRole =
    roles.put.in(path[RoleName]("name")).in(jsonBody[RoleWrite]).out(jsonBody[RoleView]).name("putRole")
      .description("建或改角色；内置角色（base、admin）只能改说明和额外规则。新规则至少要匹配一个现有的工具或资源。")

  val deleteRole =
    roles.delete.in(path[RoleName]("name")).out(statusCode(StatusCode.NoContent)).name("deleteRole")
      .description("内置角色不能删；删掉的角色也从各 agent 身上去掉。")

  val checkRules =
    Api.secure.post.in("rules-check").in(jsonBody[RulesRequest]).out(jsonBody[List[RuleCheck]]).name("checkRules")
      .description("逐条解析规则并列出它匹配到的工具与资源。")

  val listAssets = assets.get.out(jsonBody[AssetIndex]).name("listAssets")

  val readAssetFile =
    assets.get.in("file").in(query[String]("path")).out(jsonBody[AssetFile]).name("readAssetFile")

  val writeAssetFile =
    assets.put.in("file").in(jsonBody[AssetFileWrite]).out(jsonBody[AssetFile]).name("writeAssetFile")
      .description("写资源文件：先校验格式与位置；带回读到的指纹，期间被改过就 Conflict。")

  val skillFiles = assets.get.in("skills" / path[String]("name") / "files").out(jsonBody[SkillFiles]).name("skillFiles")

  val putSecret =
    assets.put.in("secrets" / path[String]("key")).in(jsonBody[SecretWrite]).out(statusCode(StatusCode.NoContent)).name("putSecret")

  val transferAsset =
    assets.post.in(path[String]("kind") / path[String]("name") / "transfer").in(jsonBody[AssetTransfer])
      .out(statusCode(StatusCode.NoContent)).name("transferAsset")
      .description("转移归属：把 skill、MCP 或 secret 挪到另一主人（platform 或 agent 名）的目录。")

  val deleteAsset =
    assets.delete.in(path[String]("kind") / path[String]("name")).out(statusCode(StatusCode.NoContent)).name("deleteAsset")
      .description("删除 skill、MCP、secret 或共享记忆（全局记忆不能删）。")

  val all = List(
    listAgents, createAgent, getAgent, updateAgent, deleteAgent, previewAgent, agentDefaults,
    listRoles, putRole, deleteRole, checkRules,
    listAssets, readAssetFile, writeAssetFile, skillFiles, putSecret, transferAsset, deleteAsset
  )
