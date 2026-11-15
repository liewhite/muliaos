package muliao.resources

import muliao.platform.DomainModule
import muliao.platform.config.ConfigSection

/** 资源、agent 与权限：记忆、skill、MCP 配置、secret（vault）、agent 配置、角色与分发规则、写沙箱可写范围。
  *
  * 给别的领域用的接口：
  *   - [[agents.Agents]]：agent 的身份与生命周期（`requireActive` 判断能不能接新工作）；
  *   - [[Resources]]：一次运行的资源快照 `forRun`、最终权限、可写范围 `writableScope`（就是写沙箱 `Sandbox.Confined`）、写不了时的说明；
  *   - [[ResourcePrompt]]：给模型的记忆、skill 索引段落与每次运行的资源提醒；
  *   - [[guard.AssetGuard]]：写文件的入口校验，得到 `Vetted` 才能写（资源文件的格式、位置、重名、记忆长度、并发覆盖）；
  *   - [[vault.Vault]]：secret 的存与解（命令注入、skill 脚本、MCP）；
  *   - [[AgentExec]]：以 agent 身份起命令的唯一入口（工作目录、写沙箱、secret），run_shell、skill_run、后台任务、触发器的前置检查与常驻命令都经它；
  *   - [[tree.AssetStore]]：资源目录快照与变化广播（主题 `assets`）；agent 变化广播主题 `agents`；
  *   - [[tools.AgentTools]]：list_agents、get_agent、create_agent、delete_agent、vault_set；
  *   - [[ResourcesHttp]]：界面的 agent、角色、AI 资源接口。
  *
  * 要别的领域在组装时提供：[[ModelCheck]]（模型领域）、[[ToolCatalog]]（全部内置工具，由各领域静态的工具定义给出）、
  * [[ResourcePaths]] 里的平台私有文件列表；各领域的 [[AgentHook]]（忙碌判断与删除清理）在启动时交给 `Agents.start`。
  *
  * 本领域自己的表结构迁移放在 `resources/db/resources/`。
  */
object Module extends DomainModule:
  val name = "resources"

  override val sections: List[ConfigSection[?]] = List(ResourcesSettings.section, VaultSettings.section)
