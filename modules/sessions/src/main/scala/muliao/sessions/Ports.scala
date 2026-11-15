package muliao.sessions

import muliao.contract.{AgentId, AgentName, CallId, FileId, JobId, RunId, SessionId}
import muliao.platform.config.ConfigSection
import muliao.resources.{AgentHook, RunResources}
import muliao.toolkit.{Display, Tool, ToolContext, ToolFailure, ToolOutcome}
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

/** 会挂起等用户批准的工具（如 skill 脚本）：批准后由它自己接着执行。 */
trait Approvable extends Tool[RunContext, Pause]:
  /** `payload`：挂起时 [[Pause.Approval]] 里交出的数据；`always`：用户选了「以后同样的调用不再问」。 */
  def approved(payload: Json, always: Boolean, ctx: RunContext): IO[ToolFailure, ToolOutcome[Pause]]

object Approvable:
  def apply(tool: Tool[RunContext, Pause])(onApproved: (Json, Boolean, RunContext) => IO[ToolFailure, ToolOutcome[Pause]]): Approvable =
    new Approvable:
      val spec                                                      = tool.spec
      def invoke(args: Json, ctx: RunContext)                       = tool.invoke(args, ctx)
      def approved(payload: Json, always: Boolean, ctx: RunContext) = onApproved(payload, always, ctx)

/** 会话的未读（通知领域实现：有指向它的未读通知）。 */
trait UnreadSource:
  def unread(ids: List[SessionId]): UIO[Set[SessionId]]
  def markRead(id: SessionId): UIO[Unit]

/** 一个领域交给本领域的东西：给 agent 的工具、会话事件订阅，以及它对 agent 生命周期的参与
  * （组装入口转交给 `Agents.start`）。下游领域用纯函数给出它，组装入口在 [[Sessions.start]] 时一并交进来。
  */
final case class Contribution(
    tools: List[Tool[RunContext, Pause]] = Nil,
    listener: Option[SessionListener] = None,
    agentHook: Option[AgentHook] = None
)

/** 启动时交给本领域的全部端口：各领域的贡献，加上只有一个实现、必须接上的后台任务与未读。 */
final case class SessionPorts(contributions: List[Contribution], jobs: JobControl, unread: UnreadSource)

/** 启动时定下的端口（写一次）。定下之前读它的会等到定下：组装入口在接受任何请求前就启动，所以实际不会等。 */
private[sessions] final class Ports private (wired: Promise[Nothing, Ports.Wired]):
  def settle(p: SessionPorts): UIO[Unit] = wired.succeed(Ports.Wired(p)).unit

  def tools: UIO[List[Tool[RunContext, Pause]]] = wired.await.map(_.tools)
  def jobs: UIO[JobControl]                     = wired.await.map(_.ports.jobs)
  def unread: UIO[UnreadSource]                 = wired.await.map(_.ports.unread)
  /** 界面怎么展示某个工具（MCP 工具不在其中，按纯文字）。 */
  def display: UIO[String => Display] = wired.await.map(w => n => w.display.getOrElse(n, Display.Plain))

  /** 同步地逐个通知订阅方（按贡献的顺序），一个出错不影响别的。 */
  def emit(e: SessionEvent): UIO[Unit] =
    wired.await.flatMap(w => ZIO.foreachDiscard(w.listeners)(l => l.on(e).catchAllCause(c => ZIO.logErrorCause("会话事件订阅方出错", c))))

private[sessions] object Ports:
  final case class Wired(ports: SessionPorts):
    val tools: List[Tool[RunContext, Pause]] = ports.contributions.flatMap(_.tools)
    val listeners: List[SessionListener]     = ports.contributions.flatMap(_.listener)
    val display: Map[String, Display]        = tools.map(t => t.spec.name -> t.spec.display).toMap

  val layer: ULayer[Ports] = ZLayer(Promise.make[Nothing, Wired].map(Ports(_)))

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
