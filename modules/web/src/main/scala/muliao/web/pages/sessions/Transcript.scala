package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import scala.collection.mutable
import scala.concurrent.Future

import Model.*

/** 画会话条目要用到的会话信息。
  * @param runActive 最近一次运行还在进行（没有结果的调用是在执行，而不是没做完）
  * @param threads   这个会话开出的 thread（委派），按调用参数里的 agent 与 thread 名对上
  */
final case class TranscriptCtx(
    session: SessionId,
    runActive: Boolean,
    threads: List[SessionSummary],
    answer: (Long, CallId, AnswerIn) => Future[Unit]
):
  /** 委派调用开出（或续写）的 thread。 */
  def threadOf(c: CallView): Option[SessionSummary] = c.display match
    case Display.Delegate(agent, thread) => threads.find(t => t.origin == Origin.Thread(Some(session), thread) && t.agentName == agent.value)
    case _                               => None

/** 会话时间线怎么画。展开状态按条目的键记在页面里：数据刷新、条目重画都不会把展开的收起来。 */
final class TranscriptView:
  private val expanded = mutable.Map.empty[String, Var[Boolean]]
  private def open(key: String): Var[Boolean] = expanded.getOrElseUpdate(key, Var(false))

  /** 每秒一跳的当前时间（运行中步骤的计时）。 */
  private val now: Signal[Double] = Clock.every(1000)

  /** 虚线折叠块，展开状态按条目的键记在页面里。 */
  private def collapsible(key: String, title: Modifier[HtmlElement], lead: Modifier[HtmlElement] = emptyMod, extra: String = "max-w-[88%] self-start")(body: => Modifier[HtmlElement]): HtmlElement =
    Fold.dashed(open(key), title, lead, extra)(body)

  // ---- 工具调用 ----

  private def toolBlock(c: CallView, live: Boolean, ctx: TranscriptCtx): HtmlElement =
    val running = (x: CallView) => toolStatus(x, live, ctx.runActive) == ToolStatus.Running
    collapsible(
      s"tool:${c.id}",
      Seq[Modifier[HtmlElement]](
        span(cls := "font-mono text-foreground", c.name),
        span(
          cls := "ml-auto flex gap-2 whitespace-nowrap text-xs",
          span(cls := "tabular-nums", child.text <-- now.map(n => spanLabel(List(c), running, n).getOrElse(""))),
          span(toolStatus(c, live, ctx.runActive).label)
        )
      ),
      extra = "self-stretch"
    )(ToolDetail.view(c))

  private def threadLink(t: SessionSummary): HtmlElement =
    Ui.appLink(Route.session(t.id).url)(
      cls := "flex items-center gap-2 self-start rounded-lg px-2 py-1 text-sm hover:bg-surface-hover",
      Icon("GitBranch", "size-3.5 shrink-0 text-muted"),
      span(cls := "truncate font-medium", t.origin match
        case Origin.Thread(_, name) => name.value
        case _                      => "thread"),
      span(cls := "truncate text-muted", t.agentName),
      Option.when(t.unread)(span(cls := "size-2 shrink-0 rounded-full bg-accent", aria.label := "未读")),
      span(cls := "ml-auto shrink-0 text-xs text-muted", Format.time(t.updatedAt))
    )

  /** 连续的工具调用收成一组，默认折叠；标题汇总步骤数与状态。交给用户的文件、开出的 thread 是成果，显示在组下方。 */
  private def process(key: String, steps: List[Step], live: Boolean, ctx: TranscriptCtx): HtmlElement =
    val calls         = steps.map(_.call)
    val (running, st) = stepsStatus(steps, live, ctx.runActive)
    val isRunning     = (x: CallView) => toolStatus(x, live, ctx.runActive) == ToolStatus.Running
    div(
      cls := "flex flex-col gap-1.5 self-stretch",
      collapsible(
        key,
        Seq[Modifier[HtmlElement]](
          running.map(_ => Ui.spinner("spinner--sm spinner--accent")),
          span(cls := "text-foreground", stepsLabel(steps)),
          span(
            cls := "ml-auto flex gap-2 whitespace-nowrap text-xs",
            span(cls := "tabular-nums", child.text <-- now.map(n => spanLabel(calls, isRunning, n).getOrElse(""))),
            span(st)
          )
        )
      )(
        div(
          cls := "flex flex-col gap-2",
          calls.map(toolBlock(_, live, ctx))
        )
      ),
      Attachments.list(calls.flatMap(sharedFile)),
      calls.flatMap(ctx.threadOf).distinctBy(_.id).map(threadLink)
    )

  // ---- 提问与批准 ----

  /** 在等用户的调用是待办卡片（[[CallAsks]]，与通知页同一个）；答复过的留下结果。 */
  private def waiting(entry: Long, c: CallView, ctx: TranscriptCtx): Option[HtmlElement] =
    Option.when(c.state == CallState.Suspended)(CallAsks.card(c, Todo.Place.InSession, ctx.answer(entry, c.id, _))).flatten

  private def note(icon: String, text: String): HtmlElement =
    div(cls := "flex items-center gap-1.5 self-start text-xs text-muted", Icon(icon, "size-3.5"), text)

  private def askBlock(entry: Long, c: CallView, ctx: TranscriptCtx): HtmlElement =
    waiting(entry, c, ctx).getOrElse(
      c.result match
        case Some(r) if !r.isError => div(cls := "flex max-w-[88%] flex-col gap-1.5 self-start rounded-2xl border border-dashed border-separator px-3.5 py-2.5 text-sm whitespace-pre-wrap", r.text)
        case Some(r)               => note("CircleHelp", r.text.linesIterator.nextOption().getOrElse("没有回答这些问题"))
        case None                  => note("CircleHelp", "提问中…")
    )

  /** 要批准的调用：在等时是待办卡片；批准或拒绝后显示决定、工具与参数。 */
  private def approvalBlock(entry: Long, c: CallView, ctx: TranscriptCtx): HtmlElement =
    waiting(entry, c, ctx).getOrElse {
      val stateText = c.state match
        case CallState.Approved              => "已批准"
        case CallState.ApprovedAlways        => "已始终允许"
        case _ if c.result.exists(_.isError) => "已拒绝或没执行"
        case _                               => "已提交决定"
      div(
        cls := "flex flex-col gap-2 self-stretch rounded-xl border border-separator bg-surface p-3",
        div(cls := "flex items-center gap-1.5 text-sm", Icon("ShieldCheck", "size-4 shrink-0 text-muted"), span(stateText), span(cls := "font-mono text-muted", c.name)),
        ToolDetail.view(c)
      )
    }

  // ---- 来信与事件 ----

  private def userBubble(text: String, files: List[FileView], label: Option[Modifier[HtmlElement]], pending: Boolean = false): HtmlElement =
    div(
      cls := "flex flex-col items-end gap-1.5 self-stretch" + (if pending then " opacity-70" else ""),
      label.map(l => div(cls := "flex items-center gap-1 text-xs text-muted", l)),
      Attachments.list(files, alignEnd = true),
      Option.when(text.nonEmpty)(div(cls := "max-w-[88%] whitespace-pre-wrap break-words rounded-2xl rounded-br-md bg-accent px-3.5 py-2.5 text-accent-foreground", text))
    )

  private def fromLabel(src: SourceView): Option[Modifier[HtmlElement]] = src match
    case SourceView.User          => None
    case SourceView.Trigger(name) => Some(Seq[Modifier[HtmlElement]](Icon("Zap", "size-3.5"), s"触发器 ${name.value}"))
    case SourceView.Issue(n) =>
      Some(Seq[Modifier[HtmlElement]](Icon("KanbanSquare", "size-3.5"), Ui.appLink(Route.issue(n).url)(cls := "text-accent hover:underline", s"issue #${n.value}")))
    case SourceView.Delegation(from, agent) =>
      Some(Seq[Modifier[HtmlElement]](Icon("GitBranch", "size-3.5"), "来自 ", Ui.appLink(Route.session(from).url)(cls := "text-accent hover:underline", agent.value), " 的委派"))
    case SourceView.Event(k) => Some(Seq[Modifier[HtmlElement]](Icon("BellDot", "size-3.5"), eventKind(k)))

  private def eventKind(k: EventKind): String = k match
    case EventKind.Job        => "后台任务"
    case EventKind.Delegation => "委派"
    case EventKind.Wake       => "到点唤醒"
    case EventKind.CheckIn    => "进度检查"

  // ---- 一块 ----

  def block(key: String, b: Block, live: Boolean, ctx: TranscriptCtx): HtmlElement = b match
    case Block.Context(text, _) =>
      collapsible(key, span(cls := "truncate", "本次运行的上下文"), lead = Icon("Info", "size-3.5 shrink-0"))(pre(cls := Ui.CodeBlock, text))
    case Block.Letter(f, text, files, _) => userBubble(text, files, fromLabel(f))
    case Block.Event(k, text, _) =>
      collapsible(key, span(cls := "truncate", text.linesIterator.nextOption().getOrElse("")), lead = Seq(Icon("BellDot", "size-3.5 shrink-0"), span(cls := "shrink-0 text-foreground", eventKind(k))))(pre(cls := Ui.CodeBlock, text))
    case Block.CheckIns(k, checks) =>
      collapsible(
        key,
        span(cls := "truncate", s"仍在进行 · 已检查 ${checks.size} 次 · 最近 ${Format.time(checks.last._2)}"),
        lead = Seq(Icon("BellDot", "size-3.5 shrink-0"), span(cls := "shrink-0 text-foreground", eventKind(k)))
      )(div(cls := "flex flex-col gap-2", checks.map((t, at) => div(cls := "flex flex-col gap-1", span(cls := "text-xs", Format.time(at)), pre(cls := Ui.CodeBlock, t)))))
    case Block.Assistant(text) =>
      div(cls := "max-w-[88%] self-start rounded-2xl rounded-bl-md bg-surface px-3.5 py-2.5 shadow-sm", Markdown.view(text))
    case Block.Process(steps)        => process(key, steps, live, ctx)
    case Block.Ask(c, entry)         => askBlock(entry, c, ctx)
    case Block.Approval(c, entry)    => approvalBlock(entry, c, ctx)
    case Block.Compaction(summary, at) =>
      val o = open(key)
      div(
        cls := "self-stretch border-y border-dashed border-separator py-1.5 text-sm text-muted",
        button(
          tpe := "button",
          cls := "flex w-full items-center justify-center gap-2",
          onClick --> (_ => o.update(!_)),
          s"以上内容已压缩 · ${Format.time(at)}",
          Fold.chevron(o.signal)
        ),
        child.maybe <-- o.signal.map(x => Option.when(x)(div(cls := "pt-2 text-foreground", Markdown.view(summary))))
      )
    case Block.Notice(kind, text, at) =>
      kind match
        case NoticeKind.Stopped => note("CircleStop", if text.isEmpty then "已停止" else text)
        case NoticeKind.Failed  => div(cls := "flex flex-col gap-1 self-stretch rounded-xl bg-danger-soft p-3 text-sm text-danger-soft-foreground", div(s"运行失败了（${Format.time(at)}）"), pre(cls := Ui.CodeBlock, text))
        case NoticeKind.Info    => note("Info", text)
    case Block.Interrupted => note("CircleStop", "已停止（生成到一半）")

  /** 时间线条目 → 带键的展示块（键在数据刷新间稳定，用来复用 DOM 与记住展开状态）。 */
  def keyed(items: List[TimelineItem], ctx: TranscriptCtx, keyPrefix: String = ""): List[(String, Block)] =
    val raw = foldCheckIns(groupSteps(items.flatMap(blocks), ctx.threadOf))
    val seen = mutable.Map.empty[String, Int]
    raw.map { b =>
      val base = b match
        case Block.Process(Step(c, _) :: _)      => s"p:${c.id}"
        case Block.Ask(c, _)                     => s"ask:${c.id}"
        case Block.Approval(c, _)                => s"appr:${c.id}"
        case Block.Letter(_, _, _, at)           => s"l:$at"
        case Block.Event(_, _, at)               => s"e:$at"
        case Block.CheckIns(k, cs)               => s"c:${k.label}:${cs.head._2}"
        case Block.Compaction(_, at)             => s"cp:$at"
        case Block.Notice(_, _, at)              => s"n:$at"
        case Block.Context(_, at)                => s"ctx:$at"
        case Block.Assistant(t)                  => s"a:${t.take(60).hashCode}"
        case other                               => other.productPrefix
      val n = seen.updateWith(base)(c => Some(c.fold(0)(_ + 1))).get
      (s"$keyPrefix$base#$n", b)
    }

  /** 一串块：每块按键复用，块变了只重画这一块（展开状态记在页面里，不丢）。 */
  def view(blocks: Signal[List[(String, Block)]], live: Boolean, ctx: Signal[TranscriptCtx]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      children <-- blocks.combineWith(ctx).map((bs, c) => bs.map((k, b) => (k, b, c))).splitSeq(_._1) { s =>
        div(cls := "flex flex-col", child <-- s.distinct.map((k, b, c) => block(k, b, live, c)))
      }
    )

  /** 没有被任何委派调用对上的 thread（如调用在压缩前、或参数对不上），列在最后。 */
  def unlinkedThreads(items: List[TimelineItem], ctx: TranscriptCtx): Option[HtmlElement] =
    val calls  = items.flatMap(_.entry match
      case EntryView.Reply(parts, _, _, _) => parts.collect { case PartView.Call(c) => c }
      case _                               => Nil
    )
    val linked = calls.flatMap(ctx.threadOf).map(_.id).toSet
    val rest   = ctx.threads.filterNot(t => linked(t.id))
    Option.when(rest.nonEmpty)(
      div(
        cls := "flex flex-col gap-1 rounded-xl border border-dashed border-separator p-3 text-sm",
        div(cls := "flex items-center gap-1.5 text-muted", Icon("GitBranch", "size-3.5"), "开出的 thread"),
        rest.map(threadLink)
      )
    )

  /** 排队中的消息（运行中发来的，下一次请求模型前一起交给它）。 */
  def queued(q: QueuedView): HtmlElement =
    q.source match
      case SourceView.Event(k) => collapsible(s"q:${q.id}", span(cls := "truncate", q.text.linesIterator.nextOption().getOrElse("")), lead = Seq(Icon("BellDot", "size-3.5 shrink-0"), span(cls := "shrink-0", eventKind(k))))(pre(cls := Ui.CodeBlock, q.text))
      case src                 => userBubble(q.text, q.files, fromLabel(src), pending = true)
