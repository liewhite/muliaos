package muliao.workspace

import muliao.contract.JobId
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.{AgentHook, ResourcePaths, Resources}
import muliao.resources.agents.Agents
import muliao.resources.guard.AssetGuard
import muliao.resources.tree.AssetStore
import muliao.resources.vault.Vault
import muliao.sessions.{Contribution, Files, JobControl, Sessions}
import muliao.toolkit.ToolSpec
import muliao.workspace.jobs.Jobs
import muliao.workspace.pages.{PageSigner, Pages}
import muliao.workspace.scripts.{ScriptGrants, SkillRun}
import muliao.workspace.tools.{CommandRunner, FileTools, ShellTools}
import zio.*

/** 工作区领域的组装：起各服务；给会话领域的工具、skill 脚本的批准执行者、对 agent 生命周期的参与（[[contribution]]），
  * 以及后台任务的状态与取消（[[jobControl]]）。
  */
object Workspace:
  type Env = Database & ChangeBus & Sessions & Resources & AssetGuard & AssetStore & Vault & Agents & ResourcePaths & PageSigner

  type Services = AgentExec & FileAccess & Jobs & ScriptGrants & Pages & Browse

  /** 本领域工具的静态定义（组装时汇进全部内置工具目录）。 */
  val specs: List[ToolSpec] = FileTools.specs ++ ShellTools.specs ++ SkillRun.specs

  val contribution: URIO[Files & FileAccess & AgentExec & Jobs & ScriptGrants, Contribution] =
    for
      files  <- ZIO.service[Files]
      access <- ZIO.service[FileAccess]
      exec   <- ZIO.service[AgentExec]
      jobs   <- ZIO.service[Jobs]
      grants <- ZIO.service[ScriptGrants]
      runner  = CommandRunner(exec, jobs)
    yield Contribution(
      tools = FileTools.all(access, files) ++ ShellTools.all(runner, jobs) :+ SkillRun.tool(runner, grants),
      agentHook = Some(
        AgentHook(
          "工作区",
          cleanup = agent => jobs.cleanup(agent) *> grants.cleanup(agent),
          busy = agent => jobs.running(agent).map(n => Option.when(n > 0)(s"有 $n 个后台任务在跑"))
        )
      )
    )

  /** 会话在等后台任务时：心跳检查里的一句现状、会话被停止时结束任务。 */
  val jobControl: URIO[Jobs, JobControl] =
    ZIO.serviceWith[Jobs](jobs =>
      new JobControl:
        def status(job: JobId): UIO[String] = jobs.brief(job).catchAll(e => ZIO.succeed(s"（查不到：${e.message}）"))
        def cancel(job: JobId): UIO[Unit]   = jobs.kill(job).unit
    )

  val layer: ZLayer[Env, Nothing, Services] =
    ZLayer.makeSome[Env, Services](
      AgentExec.layer,
      FileAccess.layer,
      Jobs.layer,
      ScriptGrants.layer,
      Pages.layer,
      Browse.layer
    )
