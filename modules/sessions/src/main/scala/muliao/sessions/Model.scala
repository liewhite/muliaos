package muliao.sessions

import muliao.contract.*
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** 交给某个 thread 的一件要交回结局的事（`report_back` 的委派与 `ask_agent`）。 */
object DelegationId extends LongId
type DelegationId = DelegationId.Type

/** 一项进行中的异步工作的登记。 */
object WaitId extends LongId
type WaitId = WaitId.Type

/** 来信的来源。用户能给任何会话写信，其他来源只能续写自己开出的会话（见 [[Source.mayWrite]]）。 */
enum Source derives JsonCodec:
  case User
  case Trigger(name: TriggerName)
  case Issue(id: IssueId)
  /** 委派：发起会话与发起方 agent 名 */
  case Delegation(from: SessionId, agent: AgentName)
  /** 平台事件：后台任务结束、委派回报、唤醒、心跳检查 */
  case Event(kind: EventKind)

  /** 接口里的样子（存库的 JSON 写法与接口不同，所以分两个类型）。 */
  def view: SourceView = this match
    case User                => SourceView.User
    case Trigger(n)          => SourceView.Trigger(n)
    case Issue(i)            => SourceView.Issue(i)
    case Delegation(from, a) => SourceView.Delegation(from, a)
    case Event(k)            => SourceView.Event(k)

object Source:
  /** 这个来源能不能往这种会话里写；不能时说明原因。 */
  def mayWrite(source: Source, origin: Origin): Either[String, Unit] = (source, origin) match
    case (User, _) | (Event(_), _)                                        => Right(())
    case (Trigger(n), Origin.Trigger(t)) if n == t                  => Right(())
    case (Issue(i), Origin.Issue(j)) if i == j                      => Right(())
    case (Delegation(from, _), Origin.Thread(Some(p), _)) if from == p => Right(())
    case _ => Left("只能续写自己开出的会话（用户可以给任何会话发消息）")

/** 回答交给谁：会话回到空闲时，按已交给模型的来信里记下的去向交回。 */
enum ReplyTo derives JsonCodec:
  /** 用户在等：推送通知 */
  case User
  /** 委派来的：交回发起方 */
  case Delegation(id: DelegationId)

/** 一封来信。
  * @param checkIn 心跳检查：没事可做时 agent 回一个约定的简短回复，界面收起
  */
final case class Letter(
    source: Source,
    text: String,
    files: Chunk[FileId] = Chunk.empty,
    replyTo: Option[ReplyTo] = None,
    checkIn: Boolean = false
) derives JsonCodec

/** 来信寄往哪里。 */
enum Address:
  case To(session: SessionId)
  /** 新开一个会话（用户新建、触发器）；issue 会话与 thread 由下面两种按键找或开 */
  case Fresh(agent: AgentId, origin: Origin.User.type | Origin.Trigger, title: String)
  /** 某 agent 处理某 issue 的会话：有就续写，没有就新开 */
  case IssueOf(agent: AgentId, issue: IssueId, title: String)
  /** 发起会话下某 agent 的某个 thread：有就续聊，没有就新开 */
  case ThreadOf(requester: SessionId, agent: AgentId, name: ThreadName)

/** 某 agent 处理某 issue 的会话。 */
final case class IssueSession(issue: IssueId, agent: AgentId, session: SessionId)

/** 投递成功：投进了哪个会话、是不是新开的。 */
final case class Delivered(session: SessionId, created: Boolean)

// ---- 挂起 ----

/** `ask_user` 的一道选择题。 */
final case class Question(
    question: String,
    header: String,
    options: List[QuestionOption],
    multiSelect: Boolean
) derives JsonCodec

final case class QuestionOption(label: String, description: String) derives JsonCodec

/** 工具调用挂起时在等什么。只有这几种：本领域的工具是 `Tool[RunContext, Pause]`，挂起时交出其中之一。 */
sealed trait Pause derives JsonCodec:
  /** 用户发来新信时当作没答复（等用户的提问与批准）；等别的 agent 回答的不算。 */
  def declinable: Boolean = this match
    case _: Pause.AskUser | _: Pause.Approval => true
    case _: Pause.AwaitThread                 => false

object Pause:
  /** 等用户回答选择题（只在用户在等时可用）。 */
  final case class AskUser(questions: List[Question]) extends Pause

  /** 等用户批准；批准后由发起调用的工具（[[Approvable]]）接着执行，`payload` 是它要的数据。 */
  final case class Approval(summary: String, payload: Json) extends Pause

  /** 等委派的 thread 回答（`ask_agent`）。 */
  final case class AwaitThread(delegation: DelegationId, thread: ThreadName, agent: AgentName) extends Pause

/** 一次工具调用走到哪一步，各步带着它的数据。 */
enum CallStatus:
  /** 还没开始 */
  case Pending
  case Running(startedAt: Long)
  /** 挂起：等用户回答、批准，或等别的 agent 回答 */
  case Suspended(pause: Pause, startedAt: Long)
  /** 用户批准了，等接着执行；`always`：以后同样的调用不再问 */
  case Approved(approval: Pause.Approval, always: Boolean, startedAt: Long)
  /** 有了结果；没开始就收尾的（被打断、跳过）没有开始时间 */
  case Done(result: CallResult, startedAt: Option[Long], finishedAt: Long)

  def state: CallState = this match
    case Pending               => CallState.Pending
    case Running(_)            => CallState.Running
    case Suspended(_, _)       => CallState.Suspended
    case Approved(_, false, _) => CallState.Approved
    case Approved(_, true, _)  => CallState.ApprovedAlways
    case Done(_, _, _)         => CallState.Done

  def started: Option[Long] = this match
    case Pending           => None
    case Running(t)        => Some(t)
    case Suspended(_, t)   => Some(t)
    case Approved(_, _, t) => Some(t)
    case Done(_, t, _)     => t

  def finished: Option[Long] = this match
    case Done(_, _, t) => Some(t)
    case _             => None

  def outcome: Option[CallResult] = this match
    case Done(r, _, _) => Some(r)
    case _             => None

  /** 挂起着的话在等什么。 */
  def waitingOn: Option[Pause] = this match
    case Suspended(p, _) => Some(p)
    case _               => None

  /** 驱动接下来要执行它。 */
  def runnable: Boolean = this match
    case Pending | Approved(_, _, _) => true
    case _                           => false

/** 用户对挂起调用的答复。 */
enum Answer:
  /** 选择题的回答：每题一个（多选时用顿号连起来），「其他」时是用户自己写的话 */
  case Answers(answers: List[String])
  /** 批准与否；`always`：以后同样的调用不再问 */
  case Decision(approved: Boolean, always: Boolean)

// ---- 一段时间里的活动（工作历史的活动回顾用） ----

/** 一个会话在这段时间里的活动。`messages`：来信与回复的条数，`first` / `last`：其中最早、最晚一条的开始时间；`runs`：这段时间里开始或结束的运行。 */
final case class ActiveSession(
    id: SessionId,
    agent: AgentId,
    origin: Origin,
    title: String,
    state: WorkState,
    messages: Int,
    first: Long,
    last: Long,
    runs: Int,
    failedRuns: Int
)

/** 用户亲手发的一条消息（来源是用户，不含转述）。 */
final case class UserSaid(session: SessionId, agent: AgentId, at: Long, text: String)

/** 一次最终失败的运行。 */
final case class RunFailure(session: SessionId, agent: AgentId, run: RunId, at: Long, error: String)

/** 一段时间里的会话活动，各项按时间排。 */
final case class SessionActivity(sessions: List[ActiveSession], said: List[UserSaid], failures: List[RunFailure])
