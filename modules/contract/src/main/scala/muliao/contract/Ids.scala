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

/** passkey secret 的一个授权请求（UUID，不随重启重复）。 */
object AskId extends StringId
type AskId = AskId.Type

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

/** 触发器名：小写字母、数字和 `-`，至多 64 个字符；在没结束的触发器之间唯一。 */
object TriggerName extends StringId:
  private val Pattern = "^[a-z0-9-]{1,64}$".r
  protected override def validate(a: String): Option[String] =
    Option.unless(Pattern.matches(a))(s"触发器名只能用小写字母、数字和 -（1–64 个字符）：$a")
type TriggerName = TriggerName.Type

/** 触发器不变的编号（已结束的一次性触发器名字可以被重用，靠它区分）。 */
object TriggerId extends LongId
type TriggerId = TriggerId.Type

/** 后台任务。 */
object JobId extends LongId
type JobId = JobId.Type

object NotificationId extends LongId
type NotificationId = NotificationId.Type

object RoleName extends StringId
type RoleName = RoleName.Type

/** 上传文件（附件）。 */
object FileId extends StringId
type FileId = FileId.Type

/** skill 名（它所在的目录名），同一 agent 能用的 skill 之间唯一。 */
object SkillName extends StringId
type SkillName = SkillName.Type
