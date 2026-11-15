package muliao.web.pages.triggers

import muliao.contract.*

import scala.scalajs.js

/** 触发方式在界面上的名字与选择时的一句话说明（后端 kinds 的说明是写给 agent 的，太长）。 */
extension (k: TriggerKind)
  def title: String = k match
    case TriggerKind.Schedule => "定时"
    case TriggerKind.Once     => "一次性"
    case TriggerKind.Stream   => "流式"
  def summary: String = k match
    case TriggerKind.Schedule => "按 cron 表达式周期运行"
    case TriggerKind.Once     => "到指定时间运行一次，之后移入「已结束」"
    case TriggerKind.Stream   => "托管一条常驻命令，它输出的每一行是一个事件"

/** 前置检查的触发条件在界面上的说法。 */
extension (w: FireWhen)
  def title: String = w match
    case FireWhen.Output  => "有输出就触发"
    case FireWhen.Changed => "输出变化才触发"
  def description: String = w match
    case FireWhen.Output  => "没有输出就跳过；适合脚本自己记游标、只打印新内容"
    case FireWhen.Changed => "与上次输出比较，交给 agent 差异和完整输出；第一次只记录基线"

/** 脚本故障的来源：卡片上的一句话与详情里的标题。 */
extension (f: FaultSource)
  def short: String = f match
    case FaultSource.Precheck => "前置检查出错"
    case FaultSource.Stream   => "流式命令反复退出"
  def title: String = f match
    case FaultSource.Precheck => "前置检查出错，未触发"
    case FaultSource.Stream   => "流式命令反复退出"

/** 逗号分隔的 secret 名（输入框里的写法）。 */
object SecretList:
  def parse(s: String): List[String] = s.split(',').map(_.trim).filter(_.nonEmpty).toList
  def show(l: List[String]): String  = l.mkString(", ")

/** 「挂钟时间」：不带时区的 YYYY-MM-DDTHH:mm，正是 `<input type="datetime-local">` 的值；一次性触发器按它传给后端，由后端按服务端时区解释。 */
object WallClock:
  /** 后端回来的时间带着服务端时区的偏移，前 16 个字符就是那边的挂钟时间。 */
  def of(at: String): String = at.take(16)

  /** 显示用：把 T 换成空格。 */
  def show(at: String): String = of(at).replace('T', ' ')

  /** 新建一次性触发器的默认时间：浏览器本地时间一小时后。 */
  def inAnHour(nowMs: Double): String =
    val d                = new js.Date(nowMs + 3600000)
    def pad(n: Double)   = f"${n.toInt}%02d"
    s"${d.getFullYear().toInt}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}T${pad(d.getHours())}:${pad(d.getMinutes())}"

/** 种类与它的配置的编辑草稿：字段是输入框里的原文，提交前由 [[toConfig]] 校验。 */
enum ConfigDraft:
  case Schedule(cron: String)
  case Once(at: String)
  case Stream(command: String, debounce: String, secrets: String)

  def kind: TriggerKind = this match
    case _: Schedule => TriggerKind.Schedule
    case _: Once     => TriggerKind.Once
    case _: Stream   => TriggerKind.Stream

  /** 不合法时是原因：保存、创建按钮据此禁用，不会提交上一次的合法值。范围等由服务端校验（不合法时报 422）。 */
  def toConfig: Either[String, TriggerConfig] = this match
    case Schedule(cron) => nonBlank(cron, "请填 cron 表达式").map(c => TriggerConfig.Schedule(c.trim))
    case Once(at)       => nonBlank(at, "请选择触发时间").map(TriggerConfig.Once(_))
    case Stream(command, debounce, secrets) =>
      val seconds = debounce.trim match
        case ""  => Right(ConfigDraft.DefaultDebounce)
        case txt => txt.toDoubleOption.filter(!_.isNaN).toRight("合并窗口要填数字")
      for
        cmd <- nonBlank(command, "请填常驻命令")
        sec <- seconds
      yield TriggerConfig.Stream(cmd, SecretList.parse(secrets), sec)

  private def nonBlank(s: String, why: String): Either[String, String] = Either.cond(s.trim.nonEmpty, s, why)

object ConfigDraft:
  /** 合并窗口不填时的秒数（与接口的默认值一致）。 */
  val DefaultDebounce: Double = 5.0

  /** 新建时的初始配置：在打开表单、切换方式时才算（一次性的默认时间是那时起一小时后）。 */
  def empty(kind: TriggerKind, nowMs: Double): ConfigDraft = kind match
    case TriggerKind.Schedule => Schedule("*/5 * * * *")
    case TriggerKind.Once     => Once(WallClock.inAnHour(nowMs))
    case TriggerKind.Stream   => Stream("", "", "")

  /** 已有触发器的配置。 */
  def of(t: TriggerView): ConfigDraft = t.config match
    case TriggerConfig.Schedule(cron)       => Schedule(cron)
    case TriggerConfig.Once(at)             => Once(WallClock.of(at))
    case TriggerConfig.Stream(cmd, secs, d) => Stream(cmd, seconds(d), SecretList.show(secs))

  /** 5.0 显示成 5。 */
  def seconds(d: Double): String = if d == math.floor(d) && !d.isInfinite then d.toLong.toString else d.toString

/** 前置检查的编辑草稿：关着时每次都交给 agent。 */
final case class PrecheckDraft(on: Boolean, command: String, fireWhen: FireWhen, secrets: String):
  def toSpec: Either[String, Option[PrecheckSpec]] =
    if !on then Right(None)
    else Either.cond(command.trim.nonEmpty, Some(PrecheckSpec(command, fireWhen, SecretList.parse(secrets))), "请填前置检查的命令")

object PrecheckDraft:
  val off: PrecheckDraft = PrecheckDraft(on = false, "", FireWhen.Output, "")

  def of(p: Option[PrecheckSpec]): PrecheckDraft =
    p.fold(off)(s => PrecheckDraft(on = true, s.command, s.fireWhen, SecretList.show(s.secrets)))

/** 新建表单里填的东西。 */
final case class NewDraft(name: String, agent: Option[AgentId], config: ConfigDraft, precheck: PrecheckDraft, prompt: String):
  def toRequest: Either[String, NewTrigger] =
    for
      n   <- Either.cond(name.trim.nonEmpty, name.trim, "请填名称")
      a   <- agent.toRight("请选择 agent")
      c   <- config.toConfig
      pc  <- precheck.toSpec
      p   <- Either.cond(prompt.trim.nonEmpty, prompt, "请填处理说明")
    yield NewTrigger(n, a, c, p, precheck = pc)

/** 编辑：与开始编辑时的触发器比，只把改过的字段放进 Patch（没动的字段别人可能改过；前置检查改了还会重置 changed 模式的基线）。 */
object Edit:
  def patch(base: TriggerView, prompt: String, config: ConfigDraft, precheck: PrecheckDraft): Either[String, TriggerPatch] =
    for
      cfg <- config.toConfig
      pc  <- precheck.toSpec
    yield
      val configChanged = config != ConfigDraft.of(base)
      val (newPrecheck, remove) = (pc, base.precheck) match
        case (Some(p), old) if !old.contains(p) => (Some(p), false)
        case (None, Some(_))                    => (None, true)
        case _                                  => (None, false)
      TriggerPatch(
        prompt = Option.when(prompt != base.prompt)(prompt),
        config = Option.when(configChanged)(cfg),
        precheck = newPrecheck,
        removePrecheck = remove
      )

  def isEmpty(p: TriggerPatch): Boolean = p == TriggerPatch()

/** 展示用的派生信息。 */
object Show:
  /** 按 agent 分组：组的顺序同 agent 列表（便于找），列表里没有的 agent 排在最后；组内保持接口给的顺序。 */
  def groupByAgent(triggers: List[TriggerView], agentOrder: List[AgentId]): List[(AgentId, List[TriggerView])] =
    val rank = agentOrder.zipWithIndex.toMap
    // groupBy 保持每组内的原有顺序
    triggers.groupBy(_.agent).toList.sortBy((id, _) => rank.getOrElse(id, rank.size))

  private def when(t: TriggerView, fmt: Long => String): String =
    t.nextAt.map(fmt).orElse(t.config match
      case TriggerConfig.Once(at) => Some(WallClock.show(at))
      case _                      => None
    ).getOrElse("—")

  /** 卡片上的一行配置摘要，完整内容在详情里。 */
  def summary(t: TriggerView, fmt: Long => String): String = t.config match
    case TriggerConfig.Schedule(cron)     => t.nextAt.fold(cron)(n => s"$cron · 下次 ${fmt(n)}")
    case TriggerConfig.Once(_)            => s"计划 ${when(t, fmt)}"
    case TriggerConfig.Stream(cmd, _, _)  => cmd

  def planned(t: TriggerView, fmt: Long => String): String = when(t, fmt)

  /** 需要人注意的状态：卡片上只给一句话，原因在详情里。 */
  def warning(t: TriggerView): Option[String] =
    t.faults.headOption.map(_.source.short).orElse(Option.when(t.skipped > 0)(s"已连续跳过 ${t.skipped} 次"))

  /** 常驻命令的标准错误日志（工作目录里的相对路径，与服务端 Streams.logName 一致）。 */
  def stderrLog(t: TriggerView): String = s"trigger-logs/${t.name.value}.stderr.log"
