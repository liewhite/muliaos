package muliao.triggers

import com.cronutils.model.CronType
import com.cronutils.model.definition.CronDefinitionBuilder
import com.cronutils.model.time.ExecutionTime
import com.cronutils.parser.CronParser
import muliao.contract.*
import muliao.platform.config.ConfigSection
import zio.*
import zio.json.*

import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeParseException
import scala.util.Try

/** 5 段 cron 表达式（分 时 日 月 周），只经 [[CronSpec.parse]] 构造。 */
final class CronSpec private (val text: String, execution: ExecutionTime):
  /** `after` 之后（不含）的下一次，按 `zone` 解释表达式。 */
  def next(after: Instant, zone: ZoneId): Option[Instant] =
    val t = execution.nextExecution(ZonedDateTime.ofInstant(after, zone))
    if t.isPresent then Some(t.get.toInstant) else None

object CronSpec:
  private val parser = CronParser(CronDefinitionBuilder.instanceDefinitionFor(CronType.UNIX))

  def parse(text: String): Either[String, CronSpec] =
    val t = text.trim
    Try(parser.parse(t).validate())
      .toEither
      .left
      .map(e => s"cron 表达式不对（要 5 段：分 时 日 月 周）：$t（${e.getMessage}）")
      .map(c => CronSpec(t, ExecutionTime.forCron(c)))

/** 触发器的种类。加一种 = 这里加一项、[[Mode]] 加一个变体，编译器会列出要补的地方。 */
/** 各种类给 agent 的说明（`trigger_kinds`）。加一种种类 = contract 的 [[TriggerKind]] 加一项、[[Mode]] 加一个变体，编译器会列出要补的地方。 */
object Kinds:
  def description(k: TriggerKind): String = k match
    case TriggerKind.Schedule =>
      "按 cron 定时触发：config {\"cron\": \"<5 段 cron>\"}，按服务器时区解释，如 {\"cron\": \"*/5 * * * *\"}。上一次触发开的会话还没做完（运行中，或在等后台任务、委派、唤醒、用户回答）就跳过这一次，status.skipped 记连续跳过的次数；那个会话在等待时通知用户一次。停机期间错过的只补一次，停用期间的不补。只做一次的事用 once：带日期的 cron 每年都会再触发。"
    case TriggerKind.Once =>
      "到点触发一次然后结束：config {\"at\": \"<ISO 8601 时刻>\"}，如 {\"at\": \"2026-10-22T10:30\"}，不带时区按服务器时区，必须在将来。它开一个新会话；要以后接着当前会话（如正在做的 issue）用 wake_me。结束后不在默认列表里，用 trigger_list(finished=true) 能查到它开的会话。agent 到点时已归档或前置检查失败，会推送告诉用户没做。"
    case TriggerKind.Stream =>
      "平台托管一条常驻命令，标准输出每行一个事件：config {\"command\": \"<bash 命令>\", \"secrets\": [\"VAULT_KEY\", ...], \"debounce_seconds\": 5}。命令在 agent 的工作目录里跑，secret 作为环境变量注入；退出后按退避（1 秒起翻倍、最长 5 分钟）重启，连续 3 次在 60 秒内退出记为脚本故障；停用、删除或改 config 时结束它，改了它跑的脚本要重启就先停用再启用（status.running_since 为空表示还没起来）。第一行到达后 debounce_seconds 内的行合在一起触发一次；上一次开的会话没做完时事件排队，做完后合并成一次触发。标准错误写进工作目录 trigger-logs/<名字>.stderr.log。排队的事件在服务重启时丢失，所以重连、去重、游标由命令自己负责。"


/** 怎么触发：每个种类只带自己的配置，都是校验过的值。 */
enum Mode:
  case Schedule(cron: CronSpec)
  case Once(at: Instant)
  case Stream(command: String, secrets: List[String], debounce: Duration)

  def kind: TriggerKind = this match
    case _: Schedule => TriggerKind.Schedule
    case _: Once     => TriggerKind.Once
    case _: Stream   => TriggerKind.Stream

  /** 给界面与工具看的配置（一次性的时刻按 `zone` 写成带偏移的 ISO 8601）。 */
  def config(zone: ZoneId): TriggerConfig = this match
    case Schedule(c)       => TriggerConfig(cron = Some(c.text))
    case Once(at)          => TriggerConfig(at = Some(OffsetDateTime.ofInstant(at, zone).toString))
    case Stream(c, s, d)   => TriggerConfig(command = Some(c), secrets = Some(s), debounceSeconds = Some(d.toMillis / 1000.0))

  /** 这次到期之后的下一次（定时）；一次性与常驻流没有「下一次」。 */
  def nextAfter(t: Instant, zone: ZoneId): Option[Instant] = this match
    case Schedule(c) => c.next(t, zone)
    case Once(_)     => None
    case Stream(_, _, _) => None

  /** 启用（或新建、改配置）时的下次到期。 */
  def firstDue(now: Instant, zone: ZoneId): Option[Instant] = this match
    case Schedule(c)     => c.next(now, zone)
    case Once(at)        => Some(at)
    case Stream(_, _, _) => None

object Mode:
  val DefaultDebounce: Duration = 5.seconds

  /** 按种类解读配置：只认这个种类用得上的字段，多给了别的字段也报错（多半是种类写错了）。 */
  def parse(kind: TriggerKind, c: TriggerConfig, zone: ZoneId): Either[String, Mode] =
    def only(allowed: Set[String]): Either[String, Unit] =
      val present = List("cron" -> c.cron, "at" -> c.at, "command" -> c.command, "secrets" -> c.secrets, "debounce_seconds" -> c.debounceSeconds)
        .collect { case (k, Some(_)) => k }
      val extra = present.filterNot(allowed)
      Either.cond(extra.isEmpty, (), s"${kind.label} 的 config 不用 ${extra.mkString("、")}（只有 ${allowed.mkString("、")}）")
    def need[A](name: String, v: Option[A]): Either[String, A] = v.toRight(s"${kind.label} 的 config 缺少 $name")
    kind match
      case TriggerKind.Schedule =>
        only(Set("cron")).flatMap(_ => need("cron", c.cron)).flatMap(CronSpec.parse).map(Schedule(_))
      case TriggerKind.Once =>
        only(Set("at")).flatMap(_ => need("at", c.at)).flatMap(parseTime(_, zone)).map(Once(_))
      case TriggerKind.Stream =>
        for
          _       <- only(Set("command", "secrets", "debounce_seconds"))
          command <- need("command", c.command).filterOrElse(_.trim.nonEmpty, "command 不能为空")
          secs     = c.debounceSeconds.getOrElse(DefaultDebounce.toMillis / 1000.0)
          _       <- Either.cond(secs >= 0 && secs <= 3600, (), s"debounce_seconds 要在 0 到 3600 之间：$secs")
        yield Stream(command, c.secrets.getOrElse(Nil).distinct, Duration.fromMillis((secs * 1000).round))

  /** ISO 8601 时刻：带偏移的照用，不带的按 `zone`。 */
  def parseTime(s: String, zone: ZoneId): Either[String, Instant] =
    val t = s.trim
    Try(OffsetDateTime.parse(t).toInstant)
      .orElse(Try(ZonedDateTime.parse(t).toInstant))
      .orElse(Try(LocalDateTime.parse(t).atZone(zone).toInstant))
      .toEither
      .left
      .map {
        case _: DateTimeParseException => s"时刻不是 ISO 8601 格式：$t（如 2026-10-22T10:30 或 2026-10-22T10:30+08:00）"
        case e                         => s"时刻不对：$t（${e.getMessage}）"
      }

/** 前置检查：触发前先跑的一条命令（有时限），决定触发不触发。 */
final case class Precheck(command: String, fireWhen: FireWhen, secrets: List[String]):
  def spec: PrecheckSpec = PrecheckSpec(command, fireWhen, secrets)

object Precheck:
  def parse(p: PrecheckSpec): Either[String, Precheck] =
    for
      command <- Either.cond(p.command.trim.nonEmpty, p.command, "前置检查的 command 不能为空")
    yield Precheck(command, p.fireWhen, p.secrets.distinct)

/** 一条脚本故障。同一触发器、同一来源只留最新一条。 */
final case class Fault(source: FaultSource, error: String, at: Long):
  def view: FaultView = FaultView(source, error, at)

/** 一个触发器。
  * @param nextAt       下次到期（定时、一次性；停用时不看它，重新启用时重算）
  * @param skipped      上一次开的会话没做完而连续跳过的次数
  * @param lastSession  上一次触发开的会话（防重叠看它）
  * @param blockNotified 已经为挡路的这个会话通知过用户
  * @param baseline     前置检查 changed 模式的上一次输出
  * @param finishedAt   一次性的到期被消耗的时间：之后不再触发、不能改或运行
  */
final case class Trigger(
    id: TriggerId,
    name: TriggerName,
    agent: AgentId,
    prompt: String,
    mode: Mode,
    enabled: Boolean,
    precheck: Option[Precheck],
    nextAt: Option[Long],
    skipped: Int,
    lastSession: Option[SessionId],
    lastFiredAt: Option[Long],
    blockNotified: Option[SessionId],
    baseline: Option[String],
    finishedAt: Option[Long],
    createdAt: Long,
    updatedAt: Long
):
  def finished: Boolean = finishedAt.isDefined

/** 配置文件 `[triggers]` 段。
  * @param precheckTimeoutSeconds 前置检查从起命令算起最多跑多久，超时算失败（记为脚本故障）
  */
@jsonMemberNames(SnakeCase)
final case class TriggerSettings(precheckTimeoutSeconds: Int = 60) derives JsonCodec:
  def precheckTimeout: Duration = precheckTimeoutSeconds.seconds

object TriggerSettings:
  val section: ConfigSection[TriggerSettings] = ConfigSection("triggers")

/** 运行节奏（不是用户配置；测试里调小）。
  * @param busyPoll      常驻流等上一次会话做完时多久看一次
  * @param restartFirst  常驻命令退出后第一次重启前等多久，之后翻倍
  * @param restartMax    重启等待的上限
  * @param stableAfter   常驻命令跑了这么久算稳定（清掉连续失败与故障）
  * @param failuresToFault 连续这么多次没跑到稳定就退出，记为脚本故障
  * @param queueLines    常驻流排队事件的上限（行）；满了不再读命令的输出，让它自己阻塞
  * @param stderrMaxBytes stderr 日志超过这么大就轮换
  */
final case class Tuning(
    busyPoll: Duration = 2.seconds,
    restartFirst: Duration = 1.second,
    restartMax: Duration = 5.minutes,
    stableAfter: Duration = 60.seconds,
    failuresToFault: Int = 3,
    queueLines: Int = 1000,
    stderrMaxBytes: Long = 1024L * 1024
)
