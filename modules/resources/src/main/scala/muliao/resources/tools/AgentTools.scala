package muliao.resources.tools

import muliao.contract.{RoleName, AssetKind}
import muliao.platform.AppError
import muliao.resources.*
import muliao.resources.access.BuiltinRole
import muliao.contract.AgentState
import muliao.resources.agents.{AgentRecord, Agents, NewAgent}
import muliao.contract.Thinking
import muliao.resources.tree.{AssetPath, Owner}
import muliao.resources.vault.{Actor, Vault}
import muliao.toolkit.*
import zio.*
import zio.json.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{description, fieldName}

/** 本领域给 agent 的工具：list_agents、get_agent、create_agent、delete_agent、vault_set。 */
object AgentTools:

  final case class NoArgs()
  given Schema[NoArgs] = DeriveSchema.gen[NoArgs]

  final case class AgentRef(@description("agent 名") agent: String)
  given Schema[AgentRef] = DeriveSchema.gen[AgentRef]

  @description("新建一个长期存在的 agent")
  final case class CreateAgent(
      @description("显示名，唯一（不分大小写），不能叫 user") name: String,
      @description("它是干什么的；它自己看得到，别的 agent 按它决定把活交给谁") description: String,
      @description("`<服务商>:<模型>`；省略用默认模型") model: Option[String] = None,
      @description("思考档位：default 按模型默认，off 关闭，其余是档位") thinking: String = "default",
      @description("分组，如同一批做同类事的 agent") group: Option[String] = None,
      @description("base 之外的角色（get_agent 列出全部角色）") roles: List[String] = Nil,
      @description("额外允许的规则 `动作:种类:名字`，如 `use:skill:relay-host`、`use:secret:RELAY_*`、`write:path:~/projects/x`") allow: List[String] = Nil,
      @description("禁止的规则，优先于一切允许，如 `use:tool:trigger_*`") deny: List[String] = Nil
  )
  given Schema[CreateAgent] = DeriveSchema.gen[CreateAgent]

  @description("加密保存一个 secret")
  final case class VaultSet(
      @description("名字，如 `SLACK_TOKEN`：大写字母开头，大写字母、数字、下划线") key: String,
      @sensitive @description("值") value: String,
      @description("是什么、哪个账户或服务、和谁配对") description: String = "",
      @fieldName("allow_shell")
      @description("false：只能经 skill 脚本（skill_run）和 MCP 使用，不能注入自写命令。省略时新 secret 为 true，已有的不变")
      allowShell: Option[Boolean] = None
  )
  given Schema[VaultSet] = DeriveSchema.gen[VaultSet]

  // ---- 输出 ----

  final case class AgentLine(name: String, description: String) derives JsonEncoder
  final case class Group(group: Option[String], agents: List[AgentLine]) derives JsonEncoder

  final case class RoleLine(name: String, description: String, allow: List[String], deny: List[String]) derives JsonEncoder

  final case class AgentView(
      name: String,
      id: String,
      description: Option[String],
      group: Option[String],
      archived: Boolean,
      model: Option[String],
      thinking: Option[String],
      roles: List[String],
      allow: List[String],
      deny: List[String],
      tools: List[String],
      assets: List[String],
      @jsonField("available_roles") availableRoles: List[RoleLine],
      @jsonField("all_tools") allTools: List[String],
      problems: List[String]
  ) derives JsonEncoder

  final case class Created(name: String, id: String, workspace: String, assets: String) derives JsonEncoder

  private def json[A: JsonEncoder](a: A): ToolOutcome = ToolOutcome.text(a.toJsonPretty)

  private def fail(e: AppError): ToolFailure = ToolFailure.fromAppError(e)

  val ListAgentsDef = ToolDef[ToolContext, NoArgs](
    "list_agents",
    "列出全部 agent 和各自管什么（按分组）。委派前用它找该交给谁。归档的不列。",
    Access.Read,
    display = Display.JsonData
  )

  val GetAgentDef = ToolDef[ToolContext, AgentRef](
    "get_agent",
    "看一个 agent 的配置、最终能用的工具（tools）和资源（assets：`种类:名字`）、全部角色与全部内置工具。看自己的用自己的名字。",
    Access.Read,
    display = Display.JsonData
  )

  val CreateAgentDef = ToolDef[ToolContext, CreateAgent](
    "create_agent",
    "新建一个长期存在的 agent（有自己的记忆、skill、工作目录），例如以后会反复委派的专人。一次性的活委派给自己（delegate_task / ask_agent 用自己的名字），不用新建。",
    Access.Admin,
    Scheduling.Ordered,
    Display.JsonData
  )

  val DeleteAgentDef = ToolDef[ToolContext, AgentRef](
    "delete_agent",
    "删除另一个 agent：连同它的会话、记忆、私有资源（仍被别人用着的转给平台）、触发器和工作目录，不可撤销。它忙时拒绝。不能删自己。",
    Access.Admin,
    Scheduling.Ordered
  )

  val VaultSetDef = ToolDef[ToolContext, VaultSet](
    "vault_set",
    "把用户给的 secret 加密存进你的 `secrets/<KEY>.toml`（新 key 归你；改已有的要有改它的权限），值不进会话记录。之后用 `run_shell(secrets=[KEY])` / `skill_run(secrets=[KEY])` 当环境变量用，或在 MCP 配置里写 `${vault:KEY}`。",
    Access.Write,
    Scheduling.Ordered
  )

  /** 本领域工具的静态定义（进全部内置工具目录）。 */
  val specs: List[ToolSpec] = List(ListAgentsDef, GetAgentDef, CreateAgentDef, DeleteAgentDef, VaultSetDef).map(_.spec)

  def all(
      agents: Agents,
      resources: Resources,
      store: tree.AssetStore,
      vault: Vault,
      catalog: ToolCatalog,
      paths: ResourcePaths
  ): List[Tool[ToolContext]] =

    val listAgents = ListAgentsDef { (_, _) =>
      (for
        as   <- agents.list.map(_.filter(_.state == AgentState.Active))
        snap <- store.snapshot
      yield
        val lines = as.map(a => (snap.profiles.get(a.name).flatMap(_.group), AgentLine(a.name.value, snap.profiles.get(a.name).fold("（agent.toml 没加载）")(_.description))))
        val groups = lines.groupMap(_._1)(_._2).toList.sortBy((g, _) => (g.isDefined, g.getOrElse("")))
        json(groups.map((g, ls) => Group(g, ls.sortBy(_.name))))
      ).mapError(fail)
    }

    def view(a: AgentRecord): IO[AppError, AgentView] =
      for
        eff    <- resources.effective(a.id)
        snap   <- store.snapshot
        access <- store.access
        grants  = access.config.grantsOf(a.id)
        prof    = snap.profiles.get(a.name)
        roles   = access.config.roleNames.map(n => access.config.role(n).map(r => RoleLine(n.value, r.description, r.allow.map(_.text), r.deny.map(_.text))))
      yield AgentView(
        name = a.name.value,
        id = a.id.value,
        description = prof.map(_.description),
        group = prof.flatMap(_.group),
        archived = a.state == AgentState.Archived,
        model = prof.map(_.model),
        thinking = prof.map(_.thinking.label),
        roles = eff.permissions.roles.filterNot(_ == BuiltinRole.Base.roleName).map(_.value),
        allow = grants.allow.map(_.text),
        deny = grants.deny.map(_.text),
        tools = eff.tools.map(_.name).sorted,
        assets = eff.assets.map(g => s"${g.kind.label}:${g.name}") ++ List(s"${AssetKind.Memory.label}:${a.name.value}（私有）"),
        availableRoles = roles.flatten,
        allTools = catalog.tools.map(_.name).sorted,
        problems = snap.problems.filter(_.owner.contains(Owner.Agent(a.name))).map(p => s"${p.path}：${p.reason}")
      )

    val getAgent = GetAgentDef { (in, _) =>
      agents.resolve(in.agent).flatMap(view).map(json(_)).mapError(fail)
    }

    val createAgent = CreateAgentDef { (in, _) =>
      (for
        thinking <- ZIO.fromEither(Thinking.parse(in.thinking)).mapError(AppError.invalid)
        rec <- agents.create(NewAgent(in.name, in.description, in.model, thinking, in.group, in.roles.map(RoleName.wrap), in.allow, in.deny))
      yield json(Created(rec.name.value, rec.id.value, paths.workspace(rec.id).toString, paths.assets.resolve(AssetPath.ownerDir(Owner.Agent(rec.name))).toString)))
        .mapError(fail)
    }

    val deleteAgent = DeleteAgentDef { (in, ctx) =>
      (for
        a <- agents.resolve(in.agent)
        _ <- ZIO.when(a.id == ctx.agent)(ZIO.fail(AppError.invalid("不能删除自己")))
        _ <- agents.delete(a.id)
      yield ToolOutcome.text(s"已删除 agent ${a.name.value}。")).mapError(fail)
    }

    val vaultSet = VaultSetDef { (in, ctx) =>
      vault
        .set(Actor.Agent(ctx.agent), in.key, in.value, Some(in.description), in.allowShell, Owner.Platform)
        .map(created => ToolOutcome.text(if created then s"已新建 secret ${in.key}。" else s"已更新 secret ${in.key}。"))
        .mapError(fail)
    }

    List(listAgents, getAgent, createAgent, deleteAgent, vaultSet)
