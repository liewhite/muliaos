package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- 触发器 ----

/** 触发器的配置：哪些字段有意义由种类定（`kind`）。
  *   - schedule：`cron`（5 段 cron，按服务器时区）
  *   - once：`at`（ISO 8601 时刻，不带时区按服务器时区，须在将来）
  *   - stream：`command`（常驻 bash 命令）、`secrets`（注入的 secret 名）、`debounceSeconds`（防抖秒数，默认 5）
  */
final case class TriggerConfig(
    cron: Option[String] = None,
    at: Option[String] = None,
    command: Option[String] = None,
    secrets: Option[List[String]] = None,
    debounceSeconds: Option[Double] = None
) derives JsonCodec, Schema

/** 前置检查。`fireWhen`：output（有输出才触发，输出作为事件）/ changed（输出变了才触发，附 diff）。 */
final case class PrecheckSpec(command: String, fireWhen: String = "output", secrets: List[String] = Nil) derives JsonCodec, Schema

/** 脚本故障。`source`：precheck（前置检查）/ stream（常驻命令连续失败）。 */
final case class FaultView(source: String, error: String, at: Long) derives JsonCodec, Schema

/** 一个触发器。
  * @param nextAt       下次触发时间（定时、一次性）
  * @param skipped      上一次开的会话没做完而连续跳过的次数
  * @param runningSince 常驻命令这次起来的时间（没在跑为空）
  * @param finishedAt   一次性触发器到点后结束的时间
  */
final case class TriggerView(
    id: TriggerId,
    name: TriggerName,
    agent: AgentId,
    agentName: String,
    kind: String,
    config: TriggerConfig,
    prompt: String,
    enabled: Boolean,
    precheck: Option[PrecheckSpec],
    nextAt: Option[Long],
    skipped: Int,
    runningSince: Option[Long],
    lastSession: Option[SessionId],
    lastFiredAt: Option[Long],
    finishedAt: Option[Long],
    faults: List[FaultView],
    createdAt: Long,
    updatedAt: Long
) derives JsonCodec, Schema

final case class NewTrigger(
    name: String,
    agent: AgentId,
    kind: String,
    config: TriggerConfig,
    prompt: String,
    enabled: Boolean = true,
    precheck: Option[PrecheckSpec] = None
) derives JsonCodec, Schema

/** 改触发器：只改给出的字段。`config` 整份替换；`removePrecheck` 去掉前置检查。 */
final case class TriggerPatch(
    prompt: Option[String] = None,
    config: Option[TriggerConfig] = None,
    enabled: Option[Boolean] = None,
    precheck: Option[PrecheckSpec] = None,
    removePrecheck: Boolean = false
) derives JsonCodec, Schema

/** 立即运行的结果：开出的会话，或没开的原因。 */
final case class FireResult(session: Option[SessionId], note: String) derives JsonCodec, Schema

final case class TriggerKindView(kind: String, description: String) derives JsonCodec, Schema

object TriggersApi:
  private val triggers = Api.secure.in("triggers")
  private val one      = triggers.in(path[TriggerId]("id"))

  val list: Endpoint[Option[String], Option[Boolean], ApiError, List[TriggerView], Any] =
    triggers.get
      .in(query[Option[Boolean]]("finished"))
      .out(jsonBody[List[TriggerView]])
      .name("listTriggers")
      .description("触发器列表；`finished=true` 列已结束的一次性触发器。")

  val kinds: Endpoint[Option[String], Unit, ApiError, List[TriggerKindView], Any] =
    triggers.get.in("kinds").out(jsonBody[List[TriggerKindView]]).name("triggerKinds").description("触发器的种类与配置说明。")

  val create: Endpoint[Option[String], NewTrigger, ApiError, TriggerView, Any] =
    triggers.post.in(jsonBody[NewTrigger]).out(jsonBody[TriggerView]).name("createTrigger").description("新建触发器。")

  val update: Endpoint[Option[String], (TriggerId, TriggerPatch), ApiError, TriggerView, Any] =
    one.patch.in(jsonBody[TriggerPatch]).out(jsonBody[TriggerView]).name("updateTrigger").description("改触发器（只改给出的字段）。")

  val run: Endpoint[Option[String], TriggerId, ApiError, FireResult, Any] =
    one.post.in("run").out(jsonBody[FireResult]).name("runTrigger").description("立即运行（先跑前置检查；不消耗一次性的那次）。")

  val delete: Endpoint[Option[String], TriggerId, ApiError, Unit, Any] =
    one.delete.out(statusCode(StatusCode.NoContent)).name("deleteTrigger").description("删除触发器（含已结束的）。")

  val all = List(list, kinds, create, update, run, delete)
