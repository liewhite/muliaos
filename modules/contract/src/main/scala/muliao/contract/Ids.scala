package muliao.contract

// 跨领域、前后端都要用到的编号。只在一个领域内部用的编号放在那个领域自己的模块里。

/** agent 不变的内部编号（改名不变）。 */
object AgentId extends StringId
type AgentId = AgentId.Type

/** agent 的显示名，唯一、可改。 */
object AgentName extends StringId
type AgentName = AgentName.Type

object SessionId extends StringId
type SessionId = SessionId.Type

/** 会话里的一次运行。 */
object RunId extends LongId
type RunId = RunId.Type

object MessageId extends LongId
type MessageId = MessageId.Type

/** 模型给出的一次工具调用的编号。 */
object CallId extends StringId
type CallId = CallId.Type

/** 委派开出的 thread 名（与目标 agent 一起标识一个 thread）。 */
object ThreadName extends StringId
type ThreadName = ThreadName.Type

object IssueId extends LongId
type IssueId = IssueId.Type

object TriggerName extends StringId
type TriggerName = TriggerName.Type

/** 后台任务。 */
object JobId extends LongId
type JobId = JobId.Type

object NotificationId extends LongId
type NotificationId = NotificationId.Type

object RoleName extends StringId
type RoleName = RoleName.Type
