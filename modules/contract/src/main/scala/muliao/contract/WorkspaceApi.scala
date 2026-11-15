package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import sttp.tapir.generic.auto.*
import zio.json.*

// ---- 后台任务 ----

/** 后台任务的状态。 */
enum JobStatus(val label: String) extends Labeled:
  case Running     extends JobStatus("running")
  case Exited      extends JobStatus("exited")
  /** 被结束（job_kill、界面、会话被停止） */
  case Killed      extends JobStatus("killed")
  /** 服务关停或崩溃时它还在跑：平台没有接管它 */
  case Interrupted extends JobStatus("interrupted")

object JobStatus extends Labels[JobStatus]("后台任务状态", JobStatus.values)

/** 一个后台任务。`exitCode`：结束了（exited / killed）时的退出码；`log` 是日志相对 agent 工作目录的路径。 */
final case class JobView(
    id: JobId,
    agent: AgentId,
    session: SessionId,
    label: String,
    state: JobStatus,
    exitCode: Option[Int],
    startedAt: Long,
    endedAt: Option[Long],
    log: String
) derives JsonCodec, Schema

// ---- 文件浏览 ----

final case class DirEntry(name: String, dir: Boolean, size: Long, modifiedAt: Long) derives JsonCodec, Schema

/** 一个目录的内容。`path` 相对浏览的根（空串是根本身）；`truncated`：条目太多只列了前面一部分。 */
final case class DirListing(path: String, entries: List[DirEntry], truncated: Boolean) derives JsonCodec, Schema

// ---- 页面 ----

/** agent 写的一个网页（工作目录的 `site/<page>/index.html`）。`url` 是带签名的地址，内嵌打开。 */
final case class PageView(agent: AgentId, agentName: String, page: String, url: String, modifiedAt: Long) derives JsonCodec, Schema

// ---- skill 脚本的始终允许 ----

/** `current`：skill 的文件和记下时一样（改过之后这条不再生效，可以撤销）。 */
final case class ScriptGrantView(
    id: Long,
    agent: AgentId,
    agentName: String,
    skill: SkillName,
    script: String,
    args: List[String],
    secrets: List[String],
    current: Boolean,
    createdAt: Long
) derives JsonCodec, Schema

object WorkspaceApi:
  private val agentOne = Api.secure.in("agents" / path[AgentId]("id"))

  /** 下载：一律按不可信内容处理，只有常见图片内联显示，其余作为下载；禁止嗅探、在沙箱里打开。 */
  private def download[I](e: Endpoint[Option[String], I, ApiError, Unit, Any]) =
    e.out(header[String]("Content-Type").and(header[String]("Content-Disposition")).and(byteArrayBody))
      .out(header("X-Content-Type-Options", "nosniff").and(header("Content-Security-Policy", "sandbox")))

  val listDir: Endpoint[Option[String], (AgentId, Option[String]), ApiError, DirListing, Any] =
    agentOne.get
      .in("workspace")
      .in(query[Option[String]]("path"))
      .out(jsonBody[DirListing])
      .name("listWorkspace")
      .description("列 agent 工作目录里的一个目录（`path` 相对工作目录，不给是根）；路径不能走出工作目录。")

  val workspaceFile: Endpoint[Option[String], (AgentId, String), ApiError, (String, String, Array[Byte]), Any] =
    download(agentOne.get.in("workspace" / "file").in(query[String]("path")))
      .name("workspaceFile")
      .description("取 agent 工作目录里的一个文件（不可信内容：只有常见图片内联显示）。")

  val skillFile: Endpoint[Option[String], (SkillName, String), ApiError, (String, String, Array[Byte]), Any] =
    download(Api.secure.get.in("assets" / "skills" / path[SkillName]("name") / "raw").in(query[String]("path")))
      .name("skillFile")
      .description("取 skill 目录里的一个文件原样内容（附件里的图片、二进制文件；文本用 readAssetFile）。")

  val listJobs: Endpoint[Option[String], AgentId, ApiError, List[JobView], Any] =
    agentOne.get.in("jobs").out(jsonBody[List[JobView]]).name("listJobs").description("agent 的后台任务（最近 200 个）。")

  val jobLog: Endpoint[Option[String], (JobId, Option[Int]), ApiError, String, Any] =
    Api.secure.get
      .in("jobs" / path[JobId]("job") / "log")
      .in(query[Option[Int]]("lines"))
      .out(stringBody)
      .name("jobLog")
      .description("后台任务日志的最后几行（默认 200）。")

  val killJob: Endpoint[Option[String], JobId, ApiError, Unit, Any] =
    Api.secure.post
      .in("jobs" / path[JobId]("job") / "kill")
      .out(statusCode(StatusCode.NoContent))
      .name("killJob")
      .description("结束一个在跑的后台任务；结局作为事件回到发起它的会话。")

  val listPages: Endpoint[Option[String], Unit, ApiError, List[PageView], Any] =
    Api.secure.get.in("pages").out(jsonBody[List[PageView]]).name("listPages").description("全部 agent 写的网页。")

  val listGrants: Endpoint[Option[String], Unit, ApiError, List[ScriptGrantView], Any] =
    Api.secure.get.in("script-grants").out(jsonBody[List[ScriptGrantView]]).name("listScriptGrants").description("skill 脚本的「始终允许」列表。")

  val revokeGrant: Endpoint[Option[String], Long, ApiError, Unit, Any] =
    Api.secure.delete
      .in("script-grants" / path[Long]("grant"))
      .out(statusCode(StatusCode.NoContent))
      .name("revokeScriptGrant")
      .description("撤销一条「始终允许」：之后同样的调用又要批准。")

  val all = List(listDir, workspaceFile, skillFile, listJobs, jobLog, killJob, listPages, listGrants, revokeGrant)
