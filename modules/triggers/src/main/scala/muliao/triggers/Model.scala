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

  /** 同一个表达式就是同一个 cron。 */
  override def equals(o: Any): Boolean = o match
    case c: CronSpec => c.text == text
    case _           => false
  override def hashCode: Int = text.hashCode
  override def toString: String = text

object CronSpec:
  private val parser = CronParser(CronDefinitionBuilder.instanceDefinitionFor(CronType.UNIX))

  def parse(text: String): Either[String, CronSpec] =
    val t = text.trim
    Try(parser.parse(t).validate())
      .toEither
      .left
      .map(e => s"cron 表达式不对（要 5 段：分 时 日 月 周）：$t（${e.getMessage}）")
      .map(c => CronSpec(t, ExecutionTime.forCron(c)))

/** 各种类给 agent 的说明（`trigger_kinds`）。加一种种类 = contract 的 [[TriggerKind]] 与 [[TriggerConfig]] 各加一项、[[Mode]] 加一个变体，编译器会列出要补的地方。 */
object Kinds:
  def description(k: TriggerKind): String = k match
    case TriggerKind.Schedule =>
      "按 cron 定时触发：config {\"cron\": \"<5 段 cron>\"}，按服务器时区解释，如 {\"cron\": \"*/5 * * * *\"}。上一次触发开的会话还没做完（运行中，或在等后台任务、委派、唤醒、用户回答）就跳过这一次，status.skipped 记连续跳过的次数；那个会话在等待时通知用户一次。停机期间错过的只补一次，停用期间的不补。只做一次的事用 once：带日期的 cron 每年都会再触发。"
    case TriggerKind.Once =>
      "到点触发一次然后结束：config {\"at\": \"<ISO 8601 时刻>\"}，如 {\"at\": \"2026-10-22T10:30\"}，不带时区按服务器时区，必须在将来。它开一个新会话；要以后接着当前会话（如正在做的 issue）用 wake_me。结束后不在默认列表里，用 trigger_list(finished=true) 能查到它开的会话。agent 到点时已归档或前置检查失败，会推送告诉用户没做。"
    case TriggerKind.Stream =>
      "平台托管一条常驻命令，标准输出每行一个事件：config {\"command\": \"<bash 命令>\", \"secrets\": [\"VAULT_KEY\", ...], \"debounce_seconds\": 5}。命令在 agent 的工作目录里跑，secret 作为环境变量注入；退出后按退避（1 秒起翻倍、最长 5 分钟）重启，连续 3 次在 60 秒内退出记为脚本故障；停用、删除或改 config 时结束它，改了它跑的脚本要重启就先停用再启用（status.running_since 为空表示还没起来）。第一行到达后 debounce_seconds 内的行合在一起触发一次；上一次开的会话没做完时事件排队，做完后合并成一次触发。标准错误写进工作目录 trigger-logs/<名字>.stderr.log。排队的事件在服务重启时丢失，所以重连、去重、游标由命令自己负责。"


/** 定时触发器的开关：启用时带下次到期（cron 再也到不了时为空）。 */
enum Armed:
  case Off
  case On(next: Option[Long])

/** 一次性触发器的进度：停用、等着到点、已结束（`at` 是到期被消耗的时间，之后不再触发、不能改或运行）。 */
enum OnceState:
  case Off
  case Waiting
  case Finished(at: Long)

/** 怎么触发，带着现在的开关与进度：每个种类只带自己的配置（都是校验过的值）和自己能处的状态。 */
enum Mode:
  case Schedule(cron: CronSpec, armed: Armed)
  case Once(at: Instant, state: OnceState)
  case Stream(command: String, secrets: List[String], debounce: Duration, on: Boolean)

  def kind: TriggerKind = this match
    case Schedule(_, _)     => TriggerKind.Schedule
    case Once(_, _)         => TriggerKind.Once
    case Stream(_, _, _, _) => TriggerKind.Stream

  /** 给界面、工具与存库的配置（一次性的时刻按 `zone` 写成带偏移的 ISO 8601）。 */
  def config(zone: ZoneId): TriggerConfig = this match
    case Schedule(c, _)     => TriggerConfig.Schedule(c.text)
    case Once(at, _)        => TriggerConfig.Once(OffsetDateTime.ofInstant(at, zone).toString)
    case Stream(c, s, d, _) => TriggerConfig.Stream(c, s, d.toMillis / 1000.0)

  def enabled: Boolean = this match
    case Schedule(_, a)      => a != Armed.Off
    case Once(_, s)          => s == OnceState.Waiting
    case Stream(_, _, _, on) => on

  /** 下次到期（启用的定时与一次性）。 */
  def nextAt: Option[Long] = this match
    case Schedule(_, Armed.On(n))    => n
    case Once(at, OnceState.Waiting) => Some(at.toEpochMilli)
    case _                           => None

  def finishedAt: Option[Long] = this match
    case Once(_, OnceState.Finished(t)) => Some(t)
    case _                              => None

  def finished: Boolean = finishedAt.isDefined

  /** 怎么触发一样（不看开关与进度）。 */
  def sameFiring(o: Mode): Boolean = (this, o) match
    case (Schedule(a, _), Schedule(b, _))               => a == b
    case (Once(a, _), Once(b, _))                       => a == b
    case (Stream(c1, s1, d1, _), Stream(c2, s2, d2, _)) => c1 == c2 && s1 == s2 && d1 == d2
    case _                                              => false

  /** 打开或关掉；打开时从 `now` 重算下次到期（停用期间错过的不补）。已结束的不变。 */
  def switched(on: Boolean, now: Instant, zone: ZoneId): Mode = this match
    case Once(_, OnceState.Finished(_)) => this
    case Schedule(c, _)                 => Schedule(c, if on then Armed.On(c.next(now, zone).map(_.toEpochMilli)) else Armed.Off)
    case Once(at, _)                    => Once(at, if on then OnceState.Waiting else OnceState.Off)
    case Stream(c, s, d, _)             => Stream(c, s, d, on)

  /** 消耗这次到期：定时推进到 `at` 之后的下一次，一次性结束；没在等到期的是 None。 */
  def consumed(at: Long, zone: ZoneId): Option[Mode] = this match
    case Schedule(c, Armed.On(_))   => Some(Schedule(c, Armed.On(c.next(Instant.ofEpochMilli(at), zone).map(_.toEpochMilli))))
    case Once(t, OnceState.Waiting) => Some(Once(t, OnceState.Finished(at)))
    case _                          => None

  /** 到期却没做成时要告诉用户（一次性的错过就没有下一次了）。 */
  def reportsMiss: Boolean = this match
    case Once(_, _) => true
    case _          => false

object Mode:
  /** 解读配置，得到停用的触发器（要启用用 [[Mode.switched]]）。 */
  def parse(c: TriggerConfig, zone: ZoneId): Either[String, Mode] = c match
    case TriggerConfig.Schedule(cron) => CronSpec.parse(cron).map(Schedule(_, Armed.Off))
    case TriggerConfig.Once(at)       => parseTime(at, zone).map(Once(_, OnceState.Off))
    case TriggerConfig.Stream(command, secrets, secs) =>
      for
        _ <- Either.cond(command.trim.nonEmpty, (), "command 不能为空")
        _ <- Either.cond(secs >= 0 && secs <= 3600, (), s"debounce_seconds 要在 0 到 3600 之间：$secs")
      yield Stream(command, secrets.distinct, Duration.fromMillis((secs * 1000).round), on = false)

  /** 从库里的列还原：已结束的一次性不看开关；定时的下次到期只在启用时有意义。 */
  def stored(c: TriggerConfig, enabled: Boolean, nextAt: Option[Long], finishedAt: Option[Long], zone: ZoneId): Either[String, Mode] =
    parse(c, zone).map {
      case Schedule(cron, _)    => Schedule(cron, if enabled then Armed.On(nextAt) else Armed.Off)
      case Once(at, _)          => Once(at, finishedAt.fold(if enabled then OnceState.Waiting else OnceState.Off)(OnceState.Finished(_)))
      case Stream(cmd, s, d, _) => Stream(cmd, s, d, enabled)
    }

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

/** 一个触发器。开关、下次到期、是否结束都在 `mode` 里。
  * @param skipped      上一次开的会话没做完而连续跳过的次数
  * @param lastSession  上一次触发开的会话（防重叠看它）
  * @param blockNotified 已经为挡路的这个会话通知过用户
  * @param baseline     前置检查 changed 模式的上一次输出
  */
final case class Trigger(
    id: TriggerId,
    name: TriggerName,
    agent: AgentId,
    prompt: String,
    mode: Mode,
    precheck: Option[Precheck],
    skipped: Int,
    lastSession: Option[SessionId],
    lastFiredAt: Option[Long],
    blockNotified: Option[SessionId],
    baseline: Option[String],
    createdAt: Long,
    updatedAt: Long
)

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
