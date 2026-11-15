package muliao.workspace

import muliao.platform.DomainModule

/** 工作区：agent 的文件、命令与后台任务、skill 脚本、页面、工作目录与 skill 目录浏览。
  *
  * 给别的领域用的接口：
  *   - [[FileAccess]]：按可写范围读写文件，资源目录里的文件经资源领域的写入口；
  *   - [[jobs.Jobs]]：后台任务（登记为会话的异步工作，结局与心跳回会话）；`recover` 在启动时、[[muliao.sessions.Sessions.start]] 之后调用；
  *   - [[WorkspaceHttp]]：界面的目录浏览、后台任务、页面、始终允许接口，以及页面文件（`/site/...`，签名链接、隔离响应头）。
  *
  * 组装：[[Workspace.layer]] 起全部服务；[[Workspace.contribution]] 是交给会话与资源领域的工具（read_file、write_file、edit_file、
  * share_file、run_shell、job_status、job_kill、skill_run，静态定义在 [[Workspace.specs]]）、skill 脚本的批准执行者、agent 忙碌判断与
  * 删除清理；[[Workspace.jobControl]] 是会话等后台任务时的状态与取消。
  * 要组装方提供 [[pages.PageSigner]]（由服务的签名密钥派生）与资源领域的 [[muliao.resources.AgentExec]]（起命令）。
  *
  * 本领域自己的表结构迁移放在 `resources/db/workspace/`。
  */
object Module extends DomainModule:
  val name = "workspace"
