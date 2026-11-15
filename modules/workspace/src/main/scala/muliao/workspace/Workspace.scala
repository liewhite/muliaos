package muliao.workspace

import muliao.contract.AgentId
import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.resources.{AgentHook, AgentHooks, ResourcePaths, Resources, ToolInfo}
import muliao.resources.agents.Agents
import muliao.resources.guard.AssetGuard
import muliao.resources.tree.AssetStore
import muliao.resources.vault.Vault
import muliao.sessions.{Files, JobControl, SessionHooks, Sessions}
import muliao.toolkit.Tool
import muliao.sessions.RunContext
import muliao.workspace.jobs.Jobs
import muliao.workspace.pages.{PageSigner, Pages}
import muliao.workspace.scripts.{ScriptGrants, SkillRun}
import muliao.workspace.tools.{CommandRunner, FileTools, ShellTools}
import muliao.contract.JobId
import zio.*

/** 本领域给 agent 的全部工具（已接进会话领域）。 */
final case class WorkspaceTools(tools: List[Tool[RunContext]])

/** 工作区领域的组装：起各服务，把工具、skill 脚本的批准执行者、后台任务这种异步工作、agent 生命周期的参与登记到上游。 */
object Workspace:
  type Env = Database & Sessions & SessionHooks & Files & Resources & AssetGuard & AssetStore & Vault & Agents & AgentHooks & ResourcePaths &
    PageSigner

  type Services = AgentExec & FileAccess & Jobs & ScriptGrants & Pages & Browse & WorkspaceHttp & WorkspaceTools

  /** 本领域的工具名与类别（组装时汇进资源领域的全部内置工具目录）。 */
  val toolInfos: List[ToolInfo] =
    (FileTools.catalog ++ ShellTools.catalog ++ SkillRun.catalog).map((n, a) => ToolInfo(n, a))

  private val register: ZLayer[Files & FileAccess & AgentExec & Jobs & ScriptGrants & SessionHooks & AgentHooks, Nothing, WorkspaceTools] =
    ZLayer {
      for
        files  <- ZIO.service[Files]
        access <- ZIO.service[FileAccess]
        exec   <- ZIO.service[AgentExec]
        jobs   <- ZIO.service[Jobs]
        grants <- ZIO.service[ScriptGrants]
        hooks  <- ZIO.service[SessionHooks]
        agentHooks <- ZIO.service[AgentHooks]
        runner = CommandRunner(exec, jobs)
        tools  = FileTools.all(access, files) ++ ShellTools.all(runner, jobs) :+ SkillRun.tool(runner, grants)
        _ <- hooks.addTools(tools)
        _ <- hooks.addApproval(SkillRun.approval(runner, grants))
        _ <- hooks.setJobs(new JobControl:
               def status(job: JobId): UIO[String] = jobs.brief(job).catchAll(e => ZIO.succeed(s"（查不到：${e.message}）"))
               def cancel(job: JobId): UIO[Unit]   = jobs.kill(job).unit
             )
        _ <- agentHooks.register(new AgentHook:
               val name = "工作区"
               def busy(agent: AgentId): IO[AppError, Option[String]] =
                 jobs.running(agent).map(n => Option.when(n > 0)(s"有 $n 个后台任务在跑"))
               def cleanup(agent: AgentId): IO[AppError, Unit] = jobs.cleanup(agent) *> grants.cleanup(agent)
             )
      yield WorkspaceTools(tools)
    }

  val layer: ZLayer[Env, Nothing, Services] =
    ZLayer.makeSome[Env, Services](
      AgentExec.layer,
      FileAccess.layer,
      Jobs.layer,
      ScriptGrants.layer,
      Pages.layer,
      Browse.layer,
      WorkspaceHttp.layer,
      register
    )
