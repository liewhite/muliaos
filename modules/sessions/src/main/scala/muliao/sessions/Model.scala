package muliao.sessions

import muliao.contract.*
import muliao.toolkit.Suspension
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** 交给某个 thread 的一件要交回结局的事（`report_back` 的委派与 `ask_agent`）。 */
object DelegationId extends LongId
type DelegationId = DelegationId.Type

/** 一项进行中的异步工作的登记。 */
object WaitId extends LongId
type WaitId = WaitId.Type

/** 批准后接着执行挂起调用的执行者名（见 [[ApprovalHandler]]）。 */
object ApproverName extends StringId
type ApproverName = ApproverName.Type

/** 会话的来源：谁开的。 */
enum Origin:
  /** 用户在界面上新建 */
  case User
  case Trigger(name: TriggerName)
  /** 某 agent 处理某 issue 的专属会话（每个 issue + agent 一个） */
  case Issue(id: IssueId)
  /** 委派开出的 thread：挂在发起会话下（发起会话删了就是 None），同名续聊 */
  case Thread(parent: Option[SessionId], name: ThreadName)

  def kind: OriginKind = this match
    case User         => OriginKind.User
    case Trigger(_)   => OriginKind.Trigger
    case Issue(_)     => OriginKind.Issue
    case Thread(_, _) => OriginKind.Thread

  /** 存库的引用：触发器名 / issue 号 / thread 名。 */
  def ref: Option[String] = this match
    case User         => None
    case Trigger(n)   => Some(n.value)
    case Issue(i)     => Some(i.value.toString)
    case Thread(_, n) => Some(n.value)

  /** thread 的发起会话。 */
  def parentId: Option[SessionId] = this match
    case Thread(p, _) => p
    case _            => None

object Origin:
  /** 从库里的种类与引用还原。 */
  def of(kind: OriginKind, ref: Option[String], parent: Option[SessionId]): Either[String, Origin] = (kind, ref) match
    case (OriginKind.User, _)          => Right(User)
    case (OriginKind.Trigger, Some(n)) => TriggerName.make(n).map(Trigger(_))
    case (OriginKind.Issue, Some(n))   => n.toLongOption.flatMap(i => IssueId.make(i).toOption).map(Issue(_)).toRight(s"issue 号 $n")
    case (OriginKind.Thread, Some(n))  => ThreadName.make(n).map(Thread(parent, _))
    case (k, None)                     => Left(s"${k.label} 没有引用")

/** 平台事件的种类（只用于展示）。 */
enum EventKind(val label: String) extends Labeled:
  /** 约的唤醒到点 */
  case Wake       extends EventKind("wake")
  /** 异步工作的心跳检查 */
  case CheckIn    extends EventKind("check-in")
  /** 后台任务结束 */
  case Job        extends EventKind("job")
  /** 委派回报 */
  case Delegation extends EventKind("delegation")

object EventKind extends Labels[EventKind]("事件种类", EventKind.values)

/** 来信的来源。用户能给任何会话写信，其他来源只能续写自己开出的会话（见 [[Source.mayWrite]]）。 */
enum Source derives JsonCodec:
  case User
  case Trigger(name: TriggerName)
  case Issue(id: IssueId)
  /** 委派：发起会话与发起方 agent 名 */
  case Delegation(from: SessionId, agent: AgentName)
  /** 平台事件：后台任务结束、委派回报、唤醒、心跳检查 */
  case Event(kind: EventKind)

  /** 来源的种类（时间线与 session_read 里的标签）。 */
  def sourceKind: SourceKind = this match
    case User             => SourceKind.User
    case Trigger(_)       => SourceKind.Trigger
    case Issue(_)         => SourceKind.Issue
    case Delegation(_, _) => SourceKind.Delegation
    case Event(_)         => SourceKind.Event

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

/** 工具调用挂起时在等什么。只有这几种：工具返回 [[muliao.toolkit.ToolOutcome.Suspended]] 时交出其中之一。 */
sealed trait Pause extends Suspension derives JsonCodec:
  /** 用户发来新信时当作没答复（等用户的提问与批准）；等别的 agent 回答的不算。 */
  def declinable: Boolean = this match
    case _: Pause.AskUser | _: Pause.Approval => true
    case _: Pause.AwaitThread                 => false

object Pause:
  /** 等用户回答选择题（只在用户在等时可用）。 */
  final case class AskUser(questions: List[Question]) extends Pause

  /** 等用户批准；批准后由名为 `handler` 的 [[ApprovalHandler]] 接着执行，`payload` 是它要的数据。 */
  final case class Approval(handler: ApproverName, summary: String, payload: Json) extends Pause

  /** 等委派的 thread 回答（`ask_agent`）。 */
  final case class AwaitThread(delegation: DelegationId, thread: ThreadName, agent: AgentName) extends Pause

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
