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
  private val now: Signal[Double] = EventStream.periodic(1000).map(_ => scala.scalajs.js.Date.now()).toSignal(scala.scalajs.js.Date.now())

  /** 虚线框、默认收起的一块；`lead` 放在展开按钮之前、自己可以点（如指向 thread 的链接）。 */
  private def collapsible(key: String, title: Modifier[HtmlElement], lead: Modifier[HtmlElement] = emptyMod, extra: String = "max-w-[88%] self-start")(body: => Modifier[HtmlElement]): HtmlElement =
    val o = open(key)
    div(
      cls := s"rounded-xl border border-dashed border-separator px-3 py-1.5 text-sm text-muted $extra",
      div(
        cls := "flex items-center gap-2",
        lead,
        button(
          tpe := "button",
          cls := "flex min-w-0 flex-1 items-center gap-2 text-left",
          aria.expanded <-- o.signal,
          onClick --> (_ => o.update(!_)),
          title,
          child <-- o.signal.map(x => Icon("ChevronDown", "size-4 shrink-0 transition-transform " + (if x then "rotate-180" else "")))
        )
      ),
      child.maybe <-- o.signal.map(x => Option.when(x)(div(cls := "pt-1", body)))
    )

  // ---- 工具调用 ----

  private val diffLine: String => String = l =>
    if l.startsWith("+") && !l.startsWith("+++") then "bg-emerald-500/15 text-emerald-700 dark:text-emerald-300"
    else if l.startsWith("-") && !l.startsWith("---") then "bg-red-500/15 text-red-700 dark:text-red-300"
    else if l.startsWith("@@") then "text-muted"
    else ""

  private def terminal(command: Option[String], output: Option[String], failed: Boolean): HtmlElement =
    div(
      cls := "my-1 max-h-96 overflow-auto rounded-lg bg-zinc-900 p-2.5 font-mono text-xs leading-relaxed text-zinc-100 dark:bg-black",
      command.map(c => div(cls := "whitespace-pre-wrap break-all", span(cls := "select-none text-emerald-400", "$ "), c)),
      output.map(o => div(cls := "whitespace-pre-wrap break-all " + (if failed then "text-red-400" else "text-zinc-300"), o))
    )

  private def titled(t: Option[String]): Option[HtmlElement] = t.map(x => div(cls := "mb-0.5 break-all font-mono text-xs text-muted", x))

  /** 参数按字段画：短的字段排成两列，多行的（文件内容、提示词）单独成块。不是 JSON 对象的照原文。 */
  private def argFields(args: String): Option[HtmlElement] =
    argsShown(args).map { shown =>
      scala.util.Try(scala.scalajs.js.JSON.parse(args).asInstanceOf[scala.scalajs.js.Dictionary[scala.scalajs.js.Any]]).toOption
        .filter(d => scala.scalajs.js.typeOf(d) == "object" && !scala.scalajs.js.Array.isArray(d)) match
        case None => pre(cls := Ui.CodeBlock, shown)
        case Some(d) =>
          val fields = d.toList.map { (k, v) =>
            k -> (if scala.scalajs.js.typeOf(v) == "string" then v.asInstanceOf[String] else scala.scalajs.js.JSON.stringify(v, null: scala.scalajs.js.Array[scala.scalajs.js.Any], 2))
          }
          val (long, short) = fields.partition((_, v) => v.contains('\n') || v.length > 120)
          div(
            Option.when(short.nonEmpty)(
              dl(cls := "my-1 grid grid-cols-[auto_1fr] gap-x-3 gap-y-0.5 text-xs", short.map((k, v) => div(cls := "contents", dt(cls := "text-muted", k), dd(cls := "break-all text-foreground", v))))
            ),
            long.map((k, v) => div(titled(Some(k)), pre(cls := Ui.CodeBlock, v)))
          )
    }

  /** 参数与结果：按工具的展示方式画；可切到原文。 */
  private def toolDetail(c: CallView): HtmlElement =
    val raw     = Var(false)
    val display = c.display
    val result  = c.result
    def rendered: Modifier[HtmlElement] = display match
      case Display.Terminal(Some(command)) =>
        terminal(Some(command), result.map(_.text), result.exists(_.isError))
      case Display.Terminal(None) =>
        // 没有命令参数的（skill 脚本）：参数按字段，输出进终端框
        Seq(argFields(c.args), result.map(r => terminal(None, Some(r.text), r.isError)))
      case Display.Code(path) =>
        Seq(titled(path), result.map(r => pre(cls := Ui.CodeBlock, r.text)))
      case Display.Diff(path) =>
        Seq(
          titled(path),
          result.map(r => pre(cls := Ui.CodeBlock, r.text.split("\n", -1).toList.map(l => div(cls := diffLine(l), if l.isEmpty then " " else l))))
        )
      case Display.JsonData =>
        Seq(argsShown(c.args).map(a => pre(cls := Ui.CodeBlock, a)), result.map(r => pre(cls := Ui.CodeBlock, prettyJson(r.text))))
      case Display.Markdown =>
        Seq(
          argsShown(c.args).map(a => pre(cls := Ui.CodeBlock, a)),
          result.map(r => div(cls := "my-1 rounded-lg border border-separator px-3 py-2 text-foreground", Markdown.view(r.text)))
        )
      case Display.SharedFile | Display.Plain | Display.Question | Display.Delegate(_, _) =>
        Seq(argFields(c.args), result.map(r => pre(cls := Ui.CodeBlock, r.text)))
    def original: Modifier[HtmlElement] =
      Seq(argsShown(c.args).map(a => pre(cls := Ui.CodeBlock, a)), result.map(r => pre(cls := Ui.CodeBlock, r.text)))
    div(
      child <-- raw.signal.map(r => div(if r then original else rendered)),
      Option.when(display != Display.Plain || argsShown(c.args).isDefined)(
        div(
          cls := "flex justify-end",
          button(tpe := "button", cls := "text-xs text-muted hover:text-foreground", onClick --> (_ => raw.update(!_)), child.text <-- raw.signal.map(r => if r then "渲染" else "原文"))
        )
      ),
      result.flatMap(r => Attachments.images(r.images))
    )

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
    )(toolDetail(c))

  private def threadLink(t: SessionSummary): HtmlElement =
    Ui.appLink(Route.session(t.id).url)(
      cls := "flex items-center gap-2 self-start rounded-lg px-2 py-1 text-sm hover:bg-surface-hover",
      Icon("GitBranch", "size-3.5 shrink-0 text-muted"),
      span(cls := "truncate font-medium", t.origin match
        case Origin.Thread(_, name) => name.value
        case _                      => "thread"),
      span(cls := "truncate text-muted", t.agentName),
      Option.when(t.unread)(span(cls := "size-2 shrink-0 rounded-full bg-accent", aria.label := "未读")),
      span(cls := "ml-auto shrink-0 text-xs text-muted", Ui.formatTime(t.updatedAt))
    )

  /** 连续的思考与工具调用收成一组，默认折叠；标题汇总步骤数与状态。交给用户的文件、开出的 thread 是成果，显示在组下方。 */
  private def process(key: String, steps: List[Step], live: Boolean, ctx: TranscriptCtx): HtmlElement =
    val calls         = steps.collect { case Step.Tool(c, _) => c }
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
          steps.zipWithIndex.map {
            case (Step.Tool(c, _), _)    => toolBlock(c, live, ctx)
            case (Step.Thinking(t), i) => collapsible(s"$key:think:$i", "思考", extra = "self-stretch")(div(cls := "whitespace-pre-wrap", t))
          }
        )
      ),
      Attachments.list(calls.flatMap(sharedFile)),
      calls.flatMap(ctx.threadOf).distinctBy(_.id).map(threadLink)
    )

  // ---- 提问与批准 ----

  private val askCard = "flex w-full max-w-[88%] flex-col gap-3 self-start rounded-2xl border border-accent bg-surface p-3.5 text-sm"

  /** agent 在等用户回答的问题：单选或多选，每题都可以在「其他」里自己写；全部答完才能提交。 */
  private def questionForm(entry: Long, c: CallView, qs: List[QuestionView], ctx: TranscriptCtx): HtmlElement =
    final case class Draft(selected: List[String], other: String):
      def answered: Boolean = selected.nonEmpty || other.trim.nonEmpty
      def text: String       = (selected ++ Option(other.trim).filter(_.nonEmpty)).mkString("、")
    val drafts = Var(qs.map(_ => Draft(Nil, "")))
    val action = new Action
    def toggle(qi: Int, label: String): Unit = drafts.update(_.zipWithIndex.map { (d, i) =>
      if i != qi then d
      else
        val on = d.selected.contains(label)
        if qs(qi).multiSelect then d.copy(selected = if on then d.selected.filterNot(_ == label) else d.selected :+ label)
        else d.copy(selected = if on then Nil else List(label))
    })
    div(
      cls := s"$askCard scroll-mt-4",
      idAttr := Waits.askAnchor(c.id),
      div(cls := "flex items-center gap-1.5 text-accent", Icon("CircleHelp", "size-4"), "需要你回答"),
      qs.zipWithIndex.map { (q, qi) =>
        fieldSet(
          cls := "flex flex-col gap-2",
          legend(
            cls := "mb-2 flex items-center gap-2",
            Ui.chip(q.header)(),
            span(cls := "font-medium", q.question),
            Option.when(q.multiSelect)(span(cls := "text-xs text-muted", "可多选"))
          ),
          q.options.map { o =>
            val on = drafts.signal.map(_(qi).selected.contains(o.label))
            button(
              tpe := "button",
              role := (if q.multiSelect then "checkbox" else "radio"),
              aria.checked <-- on.map(_.toString),
              onClick --> (_ => toggle(qi, o.label)),
              cls <-- on.map(x => "flex flex-col items-start rounded-xl border px-3 py-2 text-left " + (if x then "border-accent bg-accent-soft" else "border-separator hover:bg-surface-hover")),
              span(o.label),
              Option.when(o.description.nonEmpty)(span(cls := "text-xs text-muted", o.description))
            )
          },
          input(
            placeholder := "其他（自己写）",
            aria.label := s"${q.header}：其他",
            cls := "rounded-xl border border-separator bg-transparent px-3 py-2 outline-none focus:border-accent",
            controlled(value <-- drafts.signal.map(_(qi).other), onInput.mapToValue --> (v => drafts.update(ds => ds.updated(qi, ds(qi).copy(other = v)))))
          )
        )
      },
      action.errorView(),
      Ui.btn(pending = action.busy.signal, disabledIf = drafts.signal.map(!_.forall(_.answered)))(
        cls := "self-start",
        "提交回答",
        onClick --> (_ => action.fire(ctx.answer(entry, c.id, AnswerIn(answers = Some(drafts.now().map(_.text))))))
      ),
      div(cls := "text-xs text-muted", "也可以不回答，直接在下面发消息。")
    )

  private def note(icon: String, text: String): HtmlElement =
    div(cls := "flex items-center gap-1.5 self-start text-xs text-muted", Icon(icon, "size-3.5"), text)

  private def askBlock(entry: Long, c: CallView, ctx: TranscriptCtx): HtmlElement =
    c.pending match
      case Some(PendingView.Question(qs)) if c.state == CallState.Suspended => questionForm(entry, c, qs, ctx)
      case _ =>
        c.result match
          case Some(r) if !r.isError => div(cls := "flex max-w-[88%] flex-col gap-1.5 self-start rounded-2xl border border-dashed border-separator px-3.5 py-2.5 text-sm whitespace-pre-wrap", r.text)
          case Some(r)               => note("CircleHelp", r.text.linesIterator.nextOption().getOrElse("没有回答这些问题"))
          case None                  => note("CircleHelp", "提问中…")

  /** 停下来等用户批准的调用：显示工具与参数，待批准时给出拒绝、始终允许、批准（始终允许：以后同样的调用不再问，设置页可撤销）。 */
  private def approvalBlock(entry: Long, c: CallView, ctx: TranscriptCtx): HtmlElement =
    val pendingNow = c.state == CallState.Suspended
    val action     = new Action
    def decide(ok: Boolean, always: Boolean): Unit = action.run(ctx.answer(entry, c.id, AnswerIn(approved = Some(ok), always = always))): Unit
    val stateText = c.state match
      case CallState.Suspended      => "这次调用要你批准才执行"
      case CallState.Approved       => "已批准"
      case CallState.ApprovedAlways => "已始终允许"
      case _ if c.result.exists(_.isError) => "已拒绝或没执行"
      case _                 => "已提交决定"
    div(
      idAttr := Waits.approvalAnchor(c.id),
      cls := "flex scroll-mt-4 flex-col gap-2 self-stretch rounded-xl border border-warning/40 bg-surface p-3 shadow-sm",
      div(cls := "flex items-center gap-1.5 text-sm", Icon("ShieldQuestion", "size-4 shrink-0 text-warning"), span(stateText), span(cls := "font-mono text-muted", c.name)),
      c.pending.collect { case PendingView.Approval(s) => div(cls := "text-sm whitespace-pre-wrap", s) },
      toolDetail(c),
      action.errorView(),
      Option.when(pendingNow)(
        div(
          cls := "flex justify-end gap-2",
          Ui.btn(Variant.Secondary, Size.Sm, disabledIf = action.busy.signal)("拒绝", onClick --> (_ => decide(false, false))),
          Ui.btn(Variant.Secondary, Size.Sm, disabledIf = action.busy.signal)("始终允许", onClick --> (_ => decide(true, true))),
          Ui.btn(Variant.Primary, Size.Sm, disabledIf = action.busy.signal)("批准", onClick --> (_ => decide(true, false)))
        )
      )
    )

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
        span(cls := "truncate", s"仍在进行 · 已检查 ${checks.size} 次 · 最近 ${Ui.formatTime(checks.last._2)}"),
        lead = Seq(Icon("BellDot", "size-3.5 shrink-0"), span(cls := "shrink-0 text-foreground", eventKind(k)))
      )(div(cls := "flex flex-col gap-2", checks.map((t, at) => div(cls := "flex flex-col gap-1", span(cls := "text-xs", Ui.formatTime(at)), pre(cls := Ui.CodeBlock, t)))))
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
          s"以上内容已压缩 · ${Ui.formatTime(at)}",
          child <-- o.signal.map(x => Icon("ChevronDown", "size-4 transition-transform " + (if x then "rotate-180" else "")))
        ),
        child.maybe <-- o.signal.map(x => Option.when(x)(div(cls := "pt-2 text-foreground", Markdown.view(summary))))
      )
    case Block.Notice(kind, text, at) =>
      kind match
        case NoticeKind.Stopped => note("CircleStop", if text.isEmpty then "已停止" else text)
        case NoticeKind.Failed  => div(cls := "flex flex-col gap-1 self-stretch rounded-xl bg-danger-soft p-3 text-sm text-danger-soft-foreground", div(s"运行失败了（${Ui.formatTime(at)}）"), pre(cls := Ui.CodeBlock, text))
        case NoticeKind.Info    => note("Info", text)
    case Block.Interrupted => note("CircleStop", "已停止（生成到一半）")

  /** 时间线条目 → 带键的展示块（键在数据刷新间稳定，用来复用 DOM 与记住展开状态）。 */
  def keyed(items: List[TimelineItem], ctx: TranscriptCtx, keyPrefix: String = ""): List[(String, Block)] =
    val raw = foldCheckIns(groupSteps(items.flatMap(blocks), ctx.threadOf))
    val seen = mutable.Map.empty[String, Int]
    raw.map { b =>
      val base = b match
        case Block.Process(Step.Tool(c, _) :: _) => s"p:${c.id}"
        case Block.Process(Step.Thinking(t) :: _) => s"pt:${t.take(40).hashCode}"
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
