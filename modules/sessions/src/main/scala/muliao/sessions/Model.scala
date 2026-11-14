package muliao.sessions

import muliao.contract.{AgentId, IssueId, SessionId, ThreadName, TriggerName}
import muliao.toolkit.Suspension
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** 上传文件（附件）的编号。 */
object FileId extends muliao.contract.StringId
type FileId = FileId.Type

/** 会话的来源：谁开的。 */
enum Origin:
  /** 用户在界面上新建 */
  case User
  case Trigger(name: TriggerName)
  /** 某 agent 处理某 issue 的专属会话（每个 issue + agent 一个） */
  case Issue(id: IssueId)
  /** 委派开出的 thread：挂在发起会话下（发起会话删了就是 None），同名续聊 */
  case Thread(parent: Option[SessionId], name: ThreadName)

  def label: String = this match
    case User       => "user"
    case Trigger(_) => "trigger"
    case Issue(_)   => "issue"
    case Thread(_, _) => "thread"

/** 来信的来源。用户能给任何会话写信，其他来源只能续写自己开出的会话（见 [[Source.mayWrite]]）。 */
enum Source derives JsonCodec:
  case User
  case Trigger(name: String)
  case Issue(id: Long)
  /** 委派：发起会话与发起方 agent 名 */
  case Delegation(from: String, agent: String)
  /** 平台事件：后台任务结束、委派回报、唤醒、心跳检查等。`kind` 只用于展示 */
  case Event(kind: String)

  /** 时间线与 session_read 里的标签。 */
  def label: String = this match
    case User             => "USER"
    case Trigger(_)       => "TRIGGER"
    case Issue(_)         => "ISSUE"
    case Delegation(_, _) => "DELEGATION"
    case Event(_)         => "EVENT"

object Source:
  /** 这个来源能不能往这种会话里写；不能时说明原因。 */
  def mayWrite(source: Source, origin: Origin): Either[String, Unit] = (source, origin) match
    case (User, _) | (Event(_), _)                                        => Right(())
    case (Trigger(n), Origin.Trigger(t)) if n == t.value                  => Right(())
    case (Issue(i), Origin.Issue(j)) if i == j.value                      => Right(())
    case (Delegation(from, _), Origin.Thread(Some(p), _)) if from == p.value => Right(())
    case _ => Left("只能续写自己开出的会话（用户可以给任何会话发消息）")

/** 回答交给谁：会话回到空闲时，按已交给模型的来信里记下的去向交回。 */
enum ReplyTo derives JsonCodec:
  /** 用户在等：推送通知 */
  case User
  /** 委派来的：交回发起方 */
  case Delegation(id: Long)

/** 一封来信。
  * @param checkIn 心跳检查：没事可做时 agent 回一个约定的简短回复，界面收起
  */
final case class Letter(
    source: Source,
    text: String,
    files: Chunk[String] = Chunk.empty,
    replyTo: Option[ReplyTo] = None,
    checkIn: Boolean = false
) derives JsonCodec

/** 来信寄往哪里。 */
enum Address:
  case To(session: SessionId)
  /** 新开一个会话（用户新建、触发器） */
  case Fresh(agent: AgentId, origin: Origin, title: String)
  /** 某 agent 处理某 issue 的会话：有就续写，没有就新开 */
  case IssueOf(agent: AgentId, issue: IssueId, title: String)
  /** 发起会话下某 agent 的某个 thread：有就续聊，没有就新开 */
  case ThreadOf(requester: SessionId, agent: AgentId, name: ThreadName)

/** 投递成功：投进了哪个会话、是不是新开的。 */
final case class Delivered(session: SessionId, created: Boolean)

/** 工作状态。全系统（会话列表、看板、触发器防重叠、thread 列表）都用这一个判断，见 [[SessionStates]]。 */
enum WorkState(val label: String):
  case Running extends WorkState("running")
  case Waiting extends WorkState("waiting")
  case Idle    extends WorkState("idle")

/** 运行的状态。 */
enum RunState(val label: String):
  case Running   extends RunState("running")
  /** 有工具调用挂起（等用户回答、批准，或等别的 agent 回答） */
  case Suspended extends RunState("suspended")
  case Finished  extends RunState("finished")
  /** 最终失败，可重试 */
  case Failed    extends RunState("failed")
  case Stopped   extends RunState("stopped")

object RunState:
  def parse(s: String): RunState = values.find(_.label == s).getOrElse(throw IllegalStateException(s"未知的运行状态 $s"))

// ---- 挂起 ----

/** `ask_user` 的一道选择题。 */
final case class Question(
    question: String,
    header: String,
    options: List[QuestionOption],
    multiSelect: Boolean
) derives JsonCodec

final case class QuestionOption(label: String, description: String) derives JsonCodec

/** 工具调用挂起时在等什么。只有这几种：工具返回 [[muliao.toolkit.ToolOutcome.Suspended]] 时交出其中之一。 */
sealed trait Pause extends Suspension derives JsonCodec

object Pause:
  /** 等用户回答选择题（只在用户在等时可用）。 */
  final case class AskUser(questions: List[Question]) extends Pause

  /** 等用户批准；批准后由名为 `handler` 的 [[ApprovalHandler]] 接着执行，`payload` 是它要的数据。 */
  final case class Approval(handler: String, summary: String, payload: Json) extends Pause

  /** 等委派的 thread 回答（`ask_agent`）。 */
  final case class AwaitThread(delegation: Long, thread: String, agent: String) extends Pause

/** 用户对挂起调用的答复。 */
enum Answer:
  /** 选择题的回答：每题一个（多选时用顿号连起来），「其他」时是用户自己写的话 */
  case Answers(answers: List[String])
  /** 批准与否；`always`：以后同样的调用不再问 */
  case Decision(approved: Boolean, always: Boolean)
