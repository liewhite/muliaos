package muliao.workspace

import muliao.contract.{AgentId, WorkspaceApi}
import muliao.platform.AppError
import muliao.platform.server.{Endpoints, Route}
import muliao.workspace.jobs.Jobs
import muliao.workspace.pages.Pages
import muliao.workspace.scripts.ScriptGrants
import sttp.capabilities.zio.ZioStreams
import sttp.model.{Header, StatusCode}
import sttp.tapir.ztapir.*
import zio.*

/** 本领域的 HTTP 接口：工作目录与 skill 目录浏览、后台任务、页面列表与页面文件、skill 脚本的始终允许。 */
final case class WorkspaceHttp(browse: Browse, jobs: Jobs, pages: Pages, grants: ScriptGrants):
  import Endpoints.*

  private def asTuple(d: Download): (String, String, Array[Byte]) = (d.contentType, d.disposition, d.bytes)

  /** 页面文件：`/site/<agent>/<page>/<签名>/<文件路径>`，不要登录（页面拿不到 cookie），靠签名；带隔离响应头。 */
  val pageEndpoint: ZServerEndpoint[Any, ZioStreams] =
    endpoint.get
      .in(Pages.Prefix / path[String]("agent") / path[String]("page") / path[String]("sig"))
      .in(paths)
      .out(statusCode)
      .out(headers)
      .out(byteArrayBody)
      .name("pageFile")
      .zServerLogic[Any] { (agent, page, sig, file) =>
        pages
          .file(AgentId.wrap(agent), page, sig, file)
          .map(f => (StatusCode.Ok, Header("Content-Type", f.contentType) :: Pages.IsolationHeaders.map(Header(_, _)), f.bytes))
          .catchAll {
            case e: AppError.Domain => ZIO.succeed((StatusCode.NotFound, List(Header("Content-Type", "text/plain; charset=utf-8")), e.message.getBytes("UTF-8")))
            case e =>
              ZIO.logError(s"取页面文件失败：${e.message}").as(
                (StatusCode.InternalServerError, List(Header("Content-Type", "text/plain; charset=utf-8")), "内部错误".getBytes("UTF-8"))
              )
          }
      }

  def routes: List[Route] =
    List(
      WorkspaceApi.listDir.secured((agent, path) => browse.listDir(agent, path)),
      WorkspaceApi.workspaceFile.secured((agent, path) => browse.workspaceFile(agent, path).map(asTuple)),
      WorkspaceApi.skillFile.secured((skill, path) => browse.skillFile(skill, path).map(asTuple)),
      WorkspaceApi.listJobs.secured(jobs.list),
      WorkspaceApi.jobLog.secured((id, lines) => jobs.tail(id, lines.getOrElse(200).max(1).min(5000))),
      WorkspaceApi.killJob.secured(id => jobs.kill(id).unit),
      WorkspaceApi.listPages.secured(_ => pages.list),
      WorkspaceApi.listGrants.secured(_ => grants.list),
      WorkspaceApi.revokeGrant.secured(grants.revoke),
      Route.unsecured(pageEndpoint)
    )
