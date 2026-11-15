package muliao.sessions

import muliao.contract.{AgentId, AgentName, CallId, FileId, JobId, RunId, SessionId}
import muliao.platform.config.ConfigSection
import muliao.resources.RunResources
import muliao.toolkit.{Tool, ToolContext, ToolFailure, ToolOutcome}
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.Path

/** 配置文件 `[sessions]` 段。
  * @param compactionThresholdTokens 上一次请求的输入超过这么多 token 就压缩上下文
  * @param keepRecentSteps           压缩时保留最近几步（模型回复）原文
  * @param toolOutputLimit           一次工具结果的文字最多多少字符，超出截断并说明
  * @param uploadMaxBytes            上传一个文件最多多少字节
  */
@jsonMemberNames(SnakeCase)
final case class SessionSettings(
    compactionThresholdTokens: Long = 700000,
    keepRecentSteps: Int = 6,
    toolOutputLimit: Int = 100000,
    uploadMaxBytes: Long = 50L * 1024 * 1024
) derives JsonCodec

object SessionSettings:
  val section: ConfigSection[SessionSettings] = ConfigSection("sessions")

/** 本领域用到的目录。`files`：附件内容（按 sha256 存）。 */
final case class SessionPaths(files: Path)

/** 一次工具调用的上下文。 */
final case class RunContext(
    agent: AgentId,
    agentName: AgentName,
    session: SessionId,
    run: RunId,
    call: CallId,
    origin: Origin,
    /** agent 的工作目录 */
    workspace: Path,
    /** 本次运行开始时取的资源快照 */
    resources: RunResources,
    /** 用户在等这个会话的回答（`ask_user` 只在这时可用） */
    userWaiting: Boolean
) extends ToolContext

/** 会话结束一段工作时的结局。 */
enum Outcome:
  /** 回到空闲：`answer` 是最后一条回复的正文 */
  case Answered(answer: String)
  case Failed(error: String)
  /** 被停止：`partial` 是停止前最后一条回复的正文 */
  case Stopped(partial: String)

/** 本领域发给别的领域的事件（通知、看板、触发器订阅）。都在提交后发出。 */
enum SessionEvent:
  /** 会话回到空闲（回答完）、最终失败或被停止。`toUser`：用户在等这个回答 */
  case Settled(session: SessionId, agent: AgentId, origin: Origin, outcome: Outcome, toUser: Boolean)
  /** 有调用在等用户（提问、等批准）；`toUser`：用户在等这个会话 */
  case NeedsUser(session: SessionId, agent: AgentId, need: UserNeed, toUser: Boolean)
  case Deleted(session: SessionId, agent: AgentId, origin: Origin)

/** 挂起的调用要用户做什么。 */
enum UserNeed:
  case Answers(questions: Int)
  case Approval(summary: String)

  /** 给用户看的一句话。 */
  def text: String = this match
    case Answers(n)  => s"有 $n 个问题等你回答"
    case Approval(s) => s"等你批准：$s"

trait SessionListener:
  def on(event: SessionEvent): UIO[Unit]

/** 后台任务（工作区领域实现）：会话在等它时，心跳检查报告状态，会话被停止时结束它。 */
trait JobControl:
  /** 心跳检查里写的一句现状。 */
  def status(job: JobId): UIO[String]
  /** 会话被停止，放弃这个任务。 */
  def cancel(job: JobId): UIO[Unit]

object JobControl:
  val none: JobControl = new JobControl:
    def status(job: JobId): UIO[String] = ZIO.succeed("（状态未知）")
    def cancel(job: JobId): UIO[Unit]   = ZIO.unit

/** 用户批准后接着执行挂起的调用（如 skill 脚本）。 */
trait ApprovalHandler:
  def name: ApproverName
  /** `always`：用户选了「以后同样的调用不再问」。 */
  def approved(payload: Json, always: Boolean, ctx: RunContext): IO[ToolFailure, ToolOutcome]

/** 会话的未读（通知领域实现：有指向它的未读通知）。 */
trait UnreadSource:
  def unread(ids: List[SessionId]): UIO[Set[SessionId]]
  def markRead(id: SessionId): UIO[Unit]

object UnreadSource:
  val none: UnreadSource = new UnreadSource:
    def unread(ids: List[SessionId]): UIO[Set[SessionId]] = ZIO.succeed(Set.empty)
    def markRead(id: SessionId): UIO[Unit]                = ZIO.unit

/** 别的领域在组装时接进来的东西：工具、事件订阅、后台任务、批准后的执行者、未读来源。 */
final class SessionHooks private (
    toolsRef: Ref[List[Tool[RunContext]]],
    listenersRef: Ref[List[SessionListener]],
    jobsRef: Ref[JobControl],
    approvalsRef: Ref[Map[ApproverName, ApprovalHandler]],
    unreadRef: Ref[UnreadSource]
):
  def addTools(ts: List[Tool[RunContext]]): UIO[Unit] = toolsRef.update(_ ++ ts)
  def addListener(l: SessionListener): UIO[Unit]       = listenersRef.update(_ :+ l)
  def setJobs(j: JobControl): UIO[Unit]                = jobsRef.set(j)
  def addApproval(h: ApprovalHandler): UIO[Unit]       = approvalsRef.update(_.updated(h.name, h))
  def setUnread(u: UnreadSource): UIO[Unit]            = unreadRef.set(u)

  def tools: UIO[List[Tool[RunContext]]]                      = toolsRef.get
  def listeners: UIO[List[SessionListener]]                   = listenersRef.get
  def jobs: UIO[JobControl]                                   = jobsRef.get
  def approval(n: ApproverName): UIO[Option[ApprovalHandler]] = approvalsRef.get.map(_.get(n))
  def unread: UIO[UnreadSource]                               = unreadRef.get

  def emit(e: SessionEvent): UIO[Unit] =
    listeners.flatMap(ZIO.foreachDiscard(_)(l => l.on(e).catchAllCause(c => ZIO.logErrorCause("会话事件订阅方出错", c))))

object SessionHooks:
  val layer: ULayer[SessionHooks] = ZLayer(
    for
      t <- Ref.make(List.empty[Tool[RunContext]])
      l <- Ref.make(List.empty[SessionListener])
      k <- Ref.make(JobControl.none)
      a <- Ref.make(Map.empty[ApproverName, ApprovalHandler])
      u <- Ref.make(UnreadSource.none)
    yield SessionHooks(t, l, k, a, u)
  )

  /** 一个上下文不限的工具也能当本领域的工具用。 */
  def widen(t: Tool[ToolContext]): Tool[RunContext] = t

/** 新建会话时的标题：取正文第一行的前 40 个字。 */
object Titles:
  def of(text: String): String =
    val line = text.linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("（无标题）")
    if line.codePointCount(0, line.length) <= 40 then line
    else line.substring(0, line.offsetByCodePoints(0, 40)) + "…"

/** 本次请求用的编号。 */
private[sessions] object Ids:
  private val rnd = java.security.SecureRandom()
  def session: SessionId = SessionId.wrap(hex(8))
  def file: FileId       = FileId.wrap(hex(12))
  private def hex(bytes: Int): String =
    val b = new Array[Byte](bytes)
    rnd.nextBytes(b)
    b.map(x => f"${x & 0xff}%02x").mkString
