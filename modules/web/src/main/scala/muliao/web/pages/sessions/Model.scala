package muliao.web.pages.sessions

import muliao.contract.*

import scala.scalajs.js
import scala.util.Try

/** 会话时间线到展示块的整理：纯函数，与渲染分开。 */
object Model:

  /** 会话来源在列表上的标签。 */
  def originTag(o: OriginKind): String = o match
    case OriginKind.User    => "对话"
    case OriginKind.Trigger => "触发"
    case OriginKind.Thread  => "委派"
    case OriginKind.Issue   => "Issue"

  /** 会话列表的分类：每个来源恰好属于一类；「全部」不筛。 */
  enum Filter(val label: String, val origins: List[OriginKind]):
    case All  extends Filter("全部", Nil)
    case Chat extends Filter("对话", List(OriginKind.User, OriginKind.Thread))
    case Iss  extends Filter("issue", List(OriginKind.Issue))
    case Auto extends Filter("自动任务", List(OriginKind.Trigger))

  object Filter:
    def parse(s: String): Filter = values.find(_.toString == s).getOrElse(Chat)

  /** 按 agent 分组；组内保持接口顺序，组按各自第一次出现的位置排（最近活跃的 agent 在前）。 */
  final case class AgentGroup(agent: AgentId, name: String, sessions: List[SessionSummary]):
    def unread: Int = sessions.count(_.unread)

  def groupByAgent(list: List[SessionSummary]): List[AgentGroup] =
    list
      .foldLeft(Vector.empty[AgentGroup]) { (acc, s) =>
        acc.indexWhere(_.agent == s.agent) match
          case -1 => acc :+ AgentGroup(s.agent, s.agentName, List(s))
          case i  => acc.updated(i, acc(i).copy(sessions = acc(i).sessions :+ s))
      }
      .toList

  /** 进行中（运行或等待）的会话置顶，其余按 agent 分组。 */
  def splitActive(list: List[SessionSummary]): (List[SessionSummary], List[SessionSummary]) =
    list.partition(_.state != WorkState.Idle)

  // ---- 工具调用 ----

  /** 工具调用的状态（展示用的唯一判定）。 */
  enum ToolStatus(val label: String):
    case Running    extends ToolStatus("运行中")
    case Unfinished extends ToolStatus("未完成")
    /** 参数不合法或工具报了可纠正的错误，模型会据此修正（不是故障） */
    case Rejected   extends ToolStatus("被退回")
    case Done       extends ToolStatus("完成")

  /** live：调用来自正在生成的回复。没有结果的调用只有在当前运行里才在执行；历史里的是没做完。 */
  def toolStatus(c: CallView, live: Boolean, runActive: Boolean): ToolStatus = c.result match
    case Some(r) if r.isError => ToolStatus.Rejected
    case Some(_)              => ToolStatus.Done
    case None =>
      c.state match
        case CallState.Running | CallState.Approved | CallState.ApprovedAlways | CallState.Pending if live || runActive => ToolStatus.Running
        case _                                                                                                       => ToolStatus.Unfinished

  /** 调用参数里的一个字符串字段（命令、路径……）；不是 JSON 或没有就是 None。 */
  def argString(args: String, key: String): Option[String] =
    Try(js.JSON.parse(args)).toOption.flatMap { v =>
      val f = v.asInstanceOf[js.Dynamic].selectDynamic(key)
      if js.isUndefined(f) || f == null then None else Some(f.toString)
    }

  /** 参数原文里值得显示的：没有参数（空串、空对象）时为 None；是 JSON 的按缩进排版。 */
  def argsShown(args: String): Option[String] =
    if args.trim.isEmpty then None
    else
      Try(js.JSON.parse(args)).toOption match
        case Some(v) if js.typeOf(v) == "object" && v != null && !js.Array.isArray(v) && js.Object.keys(v.asInstanceOf[js.Object]).isEmpty => None
        case Some(v) => Some(js.JSON.stringify(v, null: js.Array[js.Any], 2))
        case None    => Some(args)

  def prettyJson(text: String): String = Try(js.JSON.stringify(js.JSON.parse(text), null: js.Array[js.Any], 2)).getOrElse(text)

  /** share_file 一类工具的结果：交给用户的文件。 */
  def sharedFile(c: CallView): Option[FileView] =
    import zio.json.*
    Option.when(c.display == Display.SharedFile)(c.result).flatten.filterNot(_.isError).flatMap(_.text.fromJson[FileView].toOption)

  // ---- 展示块 ----

  /** 过程步骤：思考与工具调用。连续的步骤收进一个折叠组。 */
  enum Step:
    case Thinking(text: String)
    case Tool(call: CallView, entry: Long)

  /** 来信的来源。 */
  enum From:
    case User
    case Trigger(name: Option[String])
    case Issue(ref: Option[String])
    case Delegation(from: Option[String])
    case Event(kind: Option[String])

  object From:
    def of(source: Option[String], detail: Option[String]): From = source match
      case Some("TRIGGER")    => Trigger(detail)
      case Some("ISSUE")      => Issue(detail)
      case Some("DELEGATION") => Delegation(detail)
      case Some("EVENT")      => Event(detail)
      case _                  => User

  enum Block:
    case Context(text: String, at: Long)
    case Letter(from: From, text: String, files: List[FileView], at: Long)
    /** 运行中检查（同一来源连续的合成一条） */
    case CheckIns(kind: Option[String], checks: List[(String, Long)])
    case Event(kind: Option[String], text: String, at: Long)
    case Assistant(text: String)
    case Process(steps: List[Step])
    /** 在等用户回答的提问，或已回答的 */
    case Ask(call: CallView, entry: Long)
    /** 要用户批准的调用 */
    case Approval(call: CallView, entry: Long)
    case Compaction(summary: String, at: Long)
    case Notice(kind: String, text: String, at: Long)
    case Interrupted

  private def isAsk(c: CallView): Boolean = c.name == "ask_user" && c.pending.forall(_.isInstanceOf[PendingView.Question])
  private def isApproval(c: CallView): Boolean =
    c.pending.exists(_.isInstanceOf[PendingView.Approval]) || c.state == CallState.Approved || c.state == CallState.ApprovedAlways

  /** 一条时间线到展示块（工具调用与结果已由后端合在一起）。 */
  def blocks(item: TimelineItem): List[Block] = item.kind match
    case "context" => List(Block.Context(item.text.getOrElse(""), item.startedAt))
    case "letter" =>
      From.of(item.source, item.sourceDetail) match
        case From.Event(k) if item.checkIn => List(Block.CheckIns(k, List(item.text.getOrElse("") -> item.startedAt)))
        case From.Event(k)                 => List(Block.Event(k, item.text.getOrElse(""), item.startedAt))
        case f                             => List(Block.Letter(f, item.text.getOrElse(""), item.files, item.startedAt))
    case "reply"      => replyBlocks(item.parts, item.id) ++ Option.when(item.interrupted)(Block.Interrupted)
    case "compaction" => List(Block.Compaction(item.text.getOrElse(""), item.startedAt))
    case "notice"     => List(Block.Notice(item.noticeKind.getOrElse("Info"), item.text.getOrElse(""), item.startedAt))
    case _            => Nil

  def replyBlocks(parts: List[PartView], entry: Long): List[Block] =
    parts.flatMap { p =>
      (p.kind, p.call) match
        case ("call", Some(c)) if isAsk(c)      => Some(Block.Ask(c, entry))
        case ("call", Some(c)) if isApproval(c) => Some(Block.Approval(c, entry))
        case ("call", Some(c))                  => Some(Block.Process(List(Step.Tool(c, entry))))
        case ("thinking", _)                    => p.text.filter(_.trim.nonEmpty).map(t => Block.Process(List(Step.Thinking(t))))
        case _                                  => p.text.filter(_.trim.nonEmpty).map(Block.Assistant(_))
    }

  /** 一步有没有不随过程折叠的成果（交给用户的文件、开出的 thread）。 */
  private def hasOutputs(s: Step, threads: CallView => Option[SessionSummary]): Boolean = s match
    case Step.Tool(c, _) => sharedFile(c).isDefined || threads(c).isDefined
    case _               => false

  /** 连续的步骤合成一个过程组；有成果的组到此为止（成果显示在组下方，之后的步骤另起一组，否则看起来像发生在成果之前）。 */
  def groupSteps(bs: List[Block], threads: CallView => Option[SessionSummary]): List[Block] =
    bs.foldLeft(Vector.empty[(Block, Boolean)]) { (acc, b) =>
      b match
        case Block.Process(List(step)) =>
          val out = hasOutputs(step, threads)
          acc.lastOption match
            case Some((Block.Process(prev), closed)) if !(closed && !out) => acc.init :+ (Block.Process(prev :+ step) -> (closed || out))
            case _                                                     => acc :+ (Block.Process(List(step)) -> out)
        case other => acc :+ (other -> false)
    }.map(_._1).toList

  /** agent 对检查只回的占位答复（「…」），合并检查时一起收起。 */
  private def isFiller(b: Block): Boolean = b match
    case Block.Assistant(t) => t.trim == "…" || t.trim == "..."
    case _                  => false

  private def onlyThinking(b: Block): Boolean = b match
    case Block.Process(steps) => steps.forall { case Step.Thinking(_) => true; case _ => false }
    case _                    => false

  /** 连续的运行中检查（夹在中间的占位答复、紧接着占位答复的纯思考一并收起）按来源合成一条。 */
  def foldCheckIns(bs: List[Block]): List[Block] =
    val arr = bs.toVector
    arr.indices
      .foldLeft((Vector.empty[Block], false)) { case ((out, inRun), i) =>
        arr(i) match
          case b if inRun && onlyThinking(b) && arr.lift(i + 1).exists(isFiller) => (out, true)
          case Block.CheckIns(k, cs) =>
            out.lastIndexWhere { case Block.CheckIns(k2, _) => k2 == k; case _ => false } match
              case j if inRun && j >= 0 =>
                val Block.CheckIns(_, prev) = out(j).asInstanceOf[Block.CheckIns]
                (out.updated(j, Block.CheckIns(k, prev ++ cs)), true)
              case _ => (out :+ Block.CheckIns(k, cs), true)
          case b if inRun && isFiller(b) => (out, true)
          case b                         => (out :+ b, false)
      }
      ._1
      .toList

  /** 一组步骤的标题：如「思考 · 8 次工具调用」。 */
  def stepsLabel(steps: List[Step]): String =
    val tools = steps.count { case Step.Tool(_, _) => true; case _ => false }
    (Option.when(steps.exists { case Step.Thinking(_) => true; case _ => false })("思考") ++ Option.when(tools > 0)(s"$tools 次工具调用")).mkString(" · ")

  /** 一组步骤的状态一句话。 */
  def stepsStatus(steps: List[Step], live: Boolean, runActive: Boolean): (Option[String], String) =
    val calls    = steps.collect { case Step.Tool(c, _) => c }
    val statuses = calls.map(c => c -> toolStatus(c, live, runActive))
    val running  = statuses.collectFirst { case (c, ToolStatus.Running) => c.name }
    val unfinished = statuses.count(_._2 == ToolStatus.Unfinished)
    val rejected   = statuses.count(_._2 == ToolStatus.Rejected)
    val text = running match
      case Some(n) => s"运行中：$n"
      case None    => ((if unfinished > 0 then s"$unfinished 次未完成" else "完成") :: Option.when(rejected > 0)(s"$rejected 次被退回").toList).mkString(" · ")
    (running, text)

  // ---- 时间 ----

  /** 4.2s、42s、1m23s、1h02m；whole：每秒刷新的计时只到整秒。 */
  def formatDuration(ms: Double, whole: Boolean = false): String =
    val s = ms / 1000
    if s < 10 && !whole then f"$s%.1fs"
    else if s < 60 then s"${s.toInt}s"
    else
      val m = (s / 60).toInt
      if m < 60 then f"${m}m${(s % 60).toInt}%02ds" else f"${m / 60}h${m % 60}%02dm"

  /** 本地时间 HH:MM:SS；不是今天的前面加上 M-D。 */
  def formatClock(at: Double, now: Double): String =
    val d = new js.Date(at)
    def p(n: Double) = f"${n.toInt}%02d"
    val time = s"${p(d.getHours())}:${p(d.getMinutes())}:${p(d.getSeconds())}"
    if new js.Date(now).toDateString() == d.toDateString() then time else s"${d.getMonth().toInt + 1}-${d.getDate().toInt} $time"

  /** 一次调用或一组调用的时间：运行中「已运行 1m23s」，结束后「13:05:12 · 4.2s」（组的耗时是各调用耗时之和）。 */
  def spanLabel(calls: List[CallView], running: CallView => Boolean, now: Double): Option[String] =
    val started = calls.flatMap(c => c.startedAt.map(c -> _))
    started.map(_._2).minOption.map { start =>
      val spent = started.map { (c, s) => c.finishedAt.map(f => (f - s).max(0L).toDouble).getOrElse(if running(c) then (now - s).max(0) else 0.0) }.sum
      if started.exists((c, _) => running(c)) then s"已运行 ${formatDuration(spent, whole = true)}"
      else if started.exists((c, _) => c.finishedAt.isEmpty) then formatClock(start.toDouble, now)
      else s"${formatClock(start.toDouble, now)} · ${formatDuration(spent)}"
    }

  // ---- 用量 ----

  /** token 数按千/百万缩写。 */
  def formatTokens(n: Long): String =
    if n >= 1000000 then f"${n / 1e6}%.1fM"
    else if n >= 10000 then s"${math.round(n / 1000.0)}K"
    else if n >= 1000 then f"${n / 1000.0}%.1fK"
    else n.toString

  def cacheHitPercent(input: Long, cacheRead: Long): Long = if input == 0 then 0 else math.round(cacheRead * 100.0 / input)

  /** 模型名（去掉服务商前缀）。 */
  def modelName(m: String): String = m.indexOf(':') match
    case -1 => m
    case i  => m.substring(i + 1)

  /** 把转写出的文字接到已经输入的内容后面。 */
  def appendTranscript(draft: String, text: String): String =
    val head = draft.reverse.dropWhile(_.isWhitespace).reverse
    if text.isEmpty then draft else if head.isEmpty then text else s"$head $text"
