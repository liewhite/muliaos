package muliao.sessions

import muliao.contract.{AgentName, SessionId, ThreadName, WorkState}
import muliao.resources.agents.Agents
import muliao.platform.AppError
import muliao.toolkit.*
import zio.*
import zio.json.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{description, fieldName}

import java.time.{Instant, LocalDateTime, OffsetDateTime}
import java.time.format.DateTimeParseException

/** 本领域给 agent 的工具：ask_user、delegate_task、ask_agent、list_threads、stop_thread、wake_me。 */
private[sessions] object SessionTools:

  final case class OptionIn(
      @description("按钮上的短文字") label: String,
      @description("选它意味着什么（不明显时写）") description: String = ""
  )
  given Schema[OptionIn] = DeriveSchema.gen[OptionIn]

  final case class QuestionIn(
      @description("完整的问题") question: String,
      @description("很短的标签，如「时间范围」") header: String,
      @description("2–6 个选项；用户总能选「其他」自己写，不要自己加「其他」") options: List[OptionIn],
      @fieldName("multi_select") @description("可多选") multiSelect: Boolean = false
  )
  given Schema[QuestionIn] = DeriveSchema.gen[QuestionIn]

  final case class AskUserIn(@description("1–4 个问题，一次问完") questions: List[QuestionIn])
  given Schema[AskUserIn] = DeriveSchema.gen[AskUserIn]

  final case class DelegateIn(
      @description("目标 agent 名（list_agents 查）；可以是你自己：开一个新的你做旁支任务") agent: AgentName,
      @description("自成一体的任务说明：对方只看得到这个 thread 里的内容") task: String,
      @description("thread 名，如 weekly-report；同一个 agent 同名即续聊（带着之前的上下文），不相关的事用新名字") thread: ThreadName,
      @fieldName("report_back") @description("true：回答作为事件交回你这里（期间你处于等待中）；false：回答直接推送给用户，不回给你") reportBack: Boolean = false,
      @fieldName("heartbeat_seconds") @description("仅 report_back：每隔多少秒给你投一条它在做什么的检查（不填不检查）") heartbeatSeconds: Option[Double] = None
  )
  given Schema[DelegateIn] = DeriveSchema.gen[DelegateIn]

  final case class AskAgentIn(
      @description("目标 agent 名；可以是你自己") agent: AgentName,
      @description("自成一体的请求：对方只看得到这个 thread 里的内容") message: String,
      @description("thread 名；同名续聊") thread: ThreadName
  )
  given Schema[AskAgentIn] = DeriveSchema.gen[AskAgentIn]

  final case class NoArgs()
  given Schema[NoArgs] = DeriveSchema.gen[NoArgs]

  final case class StopThreadIn(
      @description("thread 名") thread: ThreadName,
      @description("thread 的 agent；同名 thread 只有一个时可省略") agent: Option[AgentName] = None
  )
  given Schema[StopThreadIn] = DeriveSchema.gen[StopThreadIn]

  final case class WakeIn(
      @fieldName("after_seconds") @description("多少秒后醒（与 at 二选一）") afterSeconds: Option[Double] = None,
      @description("什么时候醒，ISO 8601，如 2026-10-22T10:30；不带时区按本地时间（与 after_seconds 二选一）") at: Option[String] = None,
      @description("醒来时看到的提醒：要做什么（醒时只看得到这句）") note: Option[String] = None,
      @description("true：取消已约的唤醒（其他参数都不给）") cancel: Boolean = false
  )
  given Schema[WakeIn] = DeriveSchema.gen[WakeIn]

  private def fail(e: AppError): ToolFailure = ToolFailure.fromAppError(e)
  private def reject(m: String): ToolFailure = ToolFailure.Rejected(m)

  val AskUserDef = ToolDef[RunContext, AskUserIn](
    "ask_user",
    "问用户 1–4 个选择题（他总能选「其他」自己写），需要他拍板或给偏好时用；答案作为这个调用的结果回来。只在用户在等这个会话时可用（触发器、issue、别人委派来的会话里不可用：改用 notify_user，或在 issue 上设 blocked 交给用户）。能自己查到或定下的事别问。",
    Access.Read,
    Scheduling.Ordered,
    _ => Display.Question
  )

  val DelegateTaskDef = ToolDef[RunContext, DelegateIn](
    "delegate_task",
    "把一件事交给某个 agent 的 thread（以你这个会话为发起方），不等结果，立即返回。默认它的回答推送给用户、不回给你；report_back=true 时回答作为事件交回这里（期间你处于等待中，可配心跳检查）。要等它的回答才能继续时用 ask_agent。",
    Access.Write,
    Scheduling.Ordered,
    in => Display.Delegate(in.agent, in.thread)
  )

  val AskAgentDef = ToolDef[RunContext, AskAgentIn](
    "ask_agent",
    "问某个 agent 并等它回答：交给它的 thread（同名续聊），它的最后一条回答就是这个调用的结果。期间这个会话处于等待中。不需要等结果时用 delegate_task。",
    Access.Write,
    Scheduling.Ordered,
    in => Display.Delegate(in.agent, in.thread)
  )

  val ListThreadsDef = ToolDef[RunContext, NoArgs](
    "list_threads",
    "列出这个会话开出的 thread：thread 名、agent、会话编号、state（running/waiting/idle）、failure（最近一次运行最终失败的原因）、answer（最新回复，没做完时是中间回复）。",
    Access.Read,
    display = _ => Display.JsonData
  )

  val StopThreadDef = ToolDef[RunContext, StopThreadIn](
    "stop_thread",
    "停下这个会话开出的某个 thread：它正在跑的停下（已做的保留），排队的工作丢弃，它开出的 thread 也一并停下。",
    Access.Write,
    Scheduling.Ordered
  )

  val WakeMeDef = ToolDef[RunContext, WakeIn](
    "wake_me",
    "约一个时间把这个会话叫醒：到时 note 作为事件投到这里，你接着做。一个会话至多一个，再约就替换；cancel=true 取消。期间会话处于等待中。约好后结束这一轮回答。",
    Access.Write,
    Scheduling.Ordered
  )

  /** 本领域工具的静态定义（进全部内置工具目录）。 */
  val specs: List[ToolSpec] = List(AskUserDef, DelegateTaskDef, AskAgentDef, ListThreadsDef, StopThreadDef, WakeMeDef).map(_.spec)

  def all(delegations: Delegations, control: Control, waits: Waits, states: States, agents: Agents, zone: java.time.ZoneId): List[Tool[RunContext, Pause]] =

    val askUser = AskUserDef { (in, ctx) =>
      val bad =
        if !ctx.userWaiting then Some("用户现在没在等这个会话，ask_user 用不了：需要他决定的事用 notify_user 告诉他，或在 issue 上设 blocked、指派给用户。")
        else if in.questions.isEmpty || in.questions.size > 4 then Some("questions 要 1–4 个。")
        else in.questions.zipWithIndex.collectFirst {
          case (q, i) if q.options.size < 2 || q.options.size > 6 => s"第 ${i + 1} 个问题的 options 要 2–6 个。"
          case (q, i) if q.question.isBlank || q.header.isBlank   => s"第 ${i + 1} 个问题的 question、header 不能为空。"
        }
      bad match
        case Some(m) => ZIO.fail(reject(m))
        case None =>
          ZIO.succeed(ToolOutcome.Suspended(Pause.AskUser(in.questions.map(q =>
            Question(q.question, q.header, q.options.map(o => QuestionOption(o.label, o.description)), q.multiSelect)
          ))))
    }

    val delegateTask = DelegateTaskDef { (in, ctx) =>
      val name = in.thread
      for
        target <- agents.resolve(in.agent.value).mapError(fail)
        hb      = in.heartbeatSeconds.map(s => Duration.fromMillis((s * 1000).toLong.max(1000)))
        done    = (thread: SessionId, back: String) => s"已交给 ${target.name.value} 的 thread「${name.value}」（会话 ${thread.value}）。$back"
        text <- (if in.reportBack then delegations.track(ctx, target, name, in.task, TrackedMode.Report, hb).map(t => done(t.thread, "回答会作为事件交回这里。"))
                 else delegations.notifyUser(ctx, target, name, in.task).map(done(_, "回答会推送给用户，不回给你。"))).mapError(fail)
      yield ToolOutcome.text(text)
    }

    val askAgent = AskAgentDef { (in, ctx) =>
      val name = in.thread
      for
        target <- agents.resolve(in.agent.value).mapError(fail)
        out    <- delegations.track(ctx, target, name, in.message, TrackedMode.Ask, None).mapError(fail)
      yield ToolOutcome.Suspended(Pause.AwaitThread(out.delegation, name, target.name))
    }

    val listThreads = ListThreadsDef { (_, ctx) =>
      delegations.threads(ctx.session).map(ts => ToolOutcome.text(ts.toJsonPretty)).mapError(fail)
    }

    val stopThread = StopThreadDef { (in, ctx) =>
      val name = in.thread
      (for
        agent <- ZIO.foreach(in.agent)(a => agents.resolve(a.value))
        row   <- delegations.findThread(ctx.session, name, agent)
        st    <- states.state(row.id)
        _     <- control.stop(row.id)
      yield ToolOutcome.text(if st == WorkState.Idle then s"thread「${name.value}」本来就空闲。" else s"已停下 thread「${name.value}」。")).mapError(fail)
    }

    val wakeMe = WakeMeDef { (in, ctx) =>
      val at: IO[ToolFailure, Option[Long]] = (in.cancel, in.afterSeconds, in.at) match
        case (true, None, None)          => ZIO.none
        case (true, _, _)                => ZIO.fail(reject("cancel=true 时不要再给 after_seconds 或 at。"))
        case (false, Some(s), None)      =>
          if s <= 0 then ZIO.fail(reject("after_seconds 要大于 0。"))
          else Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS).map(t => Some(t + (s * 1000).toLong))
        case (false, None, Some(text))   => ZIO.fromEither(parseTime(text, zone)).mapError(reject).map(Some(_))
        case (false, Some(_), Some(_))   => ZIO.fail(reject("after_seconds 和 at 只给一个。"))
        case (false, None, None)         => ZIO.fail(reject("给 after_seconds 或 at（取消用 cancel=true）。"))
      for
        due  <- at
        note <- (due, in.note.map(_.trim).filter(_.nonEmpty)) match
                  case (Some(_), None)    => ZIO.fail(reject("约唤醒要写 note：醒来时只看得到它。"))
                  case (_, n)             => ZIO.succeed(n.getOrElse(""))
        text <- waits.wake(ctx.session, due, note).mapError(fail)
      yield ToolOutcome.text(text)
    }

    List(askUser, delegateTask, askAgent, listThreads, stopThread, wakeMe)

  /** ISO 8601 时间；不带时区按 `zone`。 */
  def parseTime(text: String, zone: java.time.ZoneId): Either[String, Long] =
    val t = text.trim
    try Right(OffsetDateTime.parse(t).toInstant.toEpochMilli)
    catch
      case _: DateTimeParseException =>
        try Right(Instant.parse(t).toEpochMilli)
        catch
          case _: DateTimeParseException =>
            try Right(LocalDateTime.parse(t).atZone(zone).toInstant.toEpochMilli)
            catch case _: DateTimeParseException => Left(s"at 不是 ISO 8601 时间（如 2026-10-22T10:30 或 2026-10-22T10:30+08:00）：$t")
