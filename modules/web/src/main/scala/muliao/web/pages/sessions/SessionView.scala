package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import org.scalajs.dom
import zio.json.*

import scala.concurrent.Future
import scala.scalajs.js

import Http.ec

/** 会话页的滚动：新内容到来时只在用户本来就在底部时跟随；往上翻看历史时不把人拽下来。
  *
  * 「在不在底部」只由用户的滚动改变：滚到离底部 50px 以内就钉住，向上滚离开就解除（程序滚到底产生的 scroll 事件下一帧才到，那时内容可能又长了，
  * 不能按距底距离重算）。高度变化（新数据、流式输出、图片加载完、输入框变高）时钉住就滚到底。打开会话、发送消息就是重新钉住。
  */
final class FollowBottom:
  private val Slack                              = 50
  private var scroller: Option[dom.html.Element] = None
  private var pinned                             = true
  private var lastTop                            = 0.0
  val atBottom: Var[Boolean]                     = Var(true)

  private def toEnd(): Unit = scroller.foreach { el =>
    el.scrollTop = el.scrollHeight.toDouble
    lastTop = el.scrollTop
  }

  private val observer = js.Dynamic.newInstance(js.Dynamic.global.ResizeObserver)((_: js.Any) => if pinned then toEnd())

  private val onScroll: js.Function1[dom.Event, Unit] = _ =>
    scroller.foreach { el =>
      if el.scrollHeight - el.scrollTop - el.clientHeight <= Slack then pinned = true
      else if el.scrollTop < lastTop then pinned = false
      lastTop = el.scrollTop
      atBottom.set(pinned)
    }

  /** 挂在滚动容器上。 */
  val scrollerMod: Modifier[HtmlElement] = onMountUnmountCallback(
    ctx =>
      val el = ctx.thisNode.ref
      scroller = Some(el)
      el.addEventListener("scroll", onScroll)
      observer.observe(el)
      (),
    el =>
      el.ref.removeEventListener("scroll", onScroll)
      observer.disconnect()
      ()
  )

  /** 挂在内容容器上（它变高就跟随）。 */
  val contentMod: Modifier[HtmlElement] = onMountCallback { ctx => observer.observe(ctx.thisNode.ref); () }

  /** 回到底部并跟随之后的新内容。 */
  def pin(): Unit =
    pinned = true
    atBottom.set(true)
    toEnd()

/** 正在生成的回复（实时事件拼出来的）。`covered`：重新拉取的时间线已经包含它（存进库了），暂不显示，等下一次增长再显示。 */
final case class LiveReply(parts: Vector[PartView], covered: Boolean):
  def text: String = parts.map(p => p.text.getOrElse("") + p.call.fold("")(_.args)).mkString

object LiveReply:
  val empty: LiveReply = LiveReply(Vector.empty, covered = false)

  /** 应用一条实时事件（纯函数）。 */
  def step(r: LiveReply, v: LiveView): LiveReply = v.kind match
    case "snapshot" => LiveReply(v.parts.toVector, covered = false)
    case "begin"    => empty
    case "part" =>
      (v.index, v.part) match
        case (Some(i), Some(p)) if i < r.parts.size  => LiveReply(r.parts.updated(i, p), covered = false)
        case (Some(i), Some(p)) if i == r.parts.size => LiveReply(r.parts :+ p, covered = false)
        case _                                       => r
    case "grew" =>
      (v.index, v.text) match
        case (Some(i), Some(t)) if i < r.parts.size =>
          val p = r.parts(i)
          val grown = v.growth match
            case Some("ToolArgs") => p.copy(call = p.call.map(c => c.copy(args = c.args + t)))
            case _                => p.copy(text = Some(p.text.getOrElse("") + t))
          LiveReply(r.parts.updated(i, grown), covered = false)
        case _ => r
    case _ => r

  /** 时间线最后一条回复已经包含正在生成的内容：存进库了。 */
  def coveredBy(r: LiveReply, timeline: List[TimelineItem]): Boolean =
    r.parts.nonEmpty && timeline.reverse.find(_.kind == "reply").exists { last =>
      val saved = last.parts.map(p => p.text.getOrElse("") + p.call.fold("")(_.args)).mkString
      saved.startsWith(r.text)
    }

/** 会话页。 */
object SessionView:
  private def statusLabel(s: IssueStatus): String = s match
    case IssueStatus.Backlog    => "待办"
    case IssueStatus.InProgress => "进行中"
    case IssueStatus.InReview   => "待审核"
    case IssueStatus.Blocked    => "阻塞"
    case IssueStatus.Done       => "完成"
    case IssueStatus.Cancelled  => "取消"

  /** issue 会话的页头标题：指回它的 issue，显示编号、标题、当前状态。 */
  private def issueTitle(n: Long, fallback: String): HtmlElement =
    val issue = Remote("issues")(Http.call(BoardApi.get)(IssueId.wrap(n)))
    Ui.appLink(Route.Issue(n).url)(
      cls := "flex min-w-0 items-center gap-1.5 hover:text-accent",
      child <-- issue.map {
        case Load.Ready(d)    => span(cls := "min-w-0 truncate", s"#$n ${d.summary.title}")
        case Load.Stale(d, _) => span(cls := "min-w-0 truncate", s"#$n ${d.summary.title}")
        case _                => span(cls := "min-w-0 truncate", fallback)
      },
      child.maybe <-- issue.map(_.data.map(d => span(cls := "shrink-0 text-xs font-normal text-muted", statusLabel(d.summary.status)))),
      child.maybe <-- issue.map {
        case Load.Failed(m) => Some(span(cls := "shrink-0 text-xs font-normal text-danger", m))
        case _              => None
      }
    )

  /** 输入框上方的一行小字：上下文现在多大、到多大会压缩，以及累计花掉的 token。 */
  private def usageLine(d: SessionDetail): HtmlElement =
    div(
      cls := "flex flex-wrap items-center gap-x-2 px-1 text-xs text-muted",
      span(title := "发给模型的上下文：现在的大小 / 压缩阈值（超过就压缩）",
        s"上下文 ${if d.contextTokens == 0 then "—" else Model.formatTokens(d.contextTokens)} / ${Model.formatTokens(d.compactionThreshold)}"),
      span(title := "这个会话累计花掉的 token（每次模型请求完成就记上）",
        s"累计 输入 ${Model.formatTokens(d.usage.input)} · 输出 ${Model.formatTokens(d.usage.output)} · 缓存命中 ${Model.cacheHitPercent(d.usage.input, d.usage.cacheRead)}%")
    )

  /** 最近一次运行没正常结束：在跑（暂时失败在退避重试时说明原因），或最终失败（可重试）。 */
  private def runStatus(run: Option[RunView], onRetry: () => Unit): Option[HtmlElement] = run.flatMap { r =>
    r.state match
      case "failed" =>
        Some(
          div(
            cls := "flex flex-col gap-2 rounded-xl bg-danger-soft p-3 text-sm text-danger-soft-foreground",
            div(s"运行失败了（${Ui.formatTime(r.finishedAt.getOrElse(r.startedAt))}）"),
            pre(cls := Ui.CodeBlock, r.error.getOrElse("")),
            div(cls := "text-xs", "如果是上下文过长，先点右上角「压缩」，完成后再重试。重试从失败的那一步继续。"),
            Ui.btn(Variant.Secondary, Size.Sm)(cls := "self-start", Icon("RotateCcw"), "重试", onClick --> (_ => onRetry()))
          )
        )
      case "running" =>
        Some(
          div(
            cls := "flex flex-col items-end gap-1 self-end text-xs text-muted",
            div(cls := "flex items-center gap-2", Ui.spinner("spinner--sm"), if r.retryNote.isDefined then "出错了，自动重试中…" else "运行中…"),
            r.retryNote.map(n => div(cls := "max-w-full break-all text-danger", s"上次错误：$n"))
          )
        )
      case _ => None
  }

  def view(idText: String): HtmlElement =
    val id         = SessionId.wrap(idText)
    val bottom     = new FollowBottom
    val transcript = new TranscriptView
    val draft      = Var("")
    val uploads    = new Uploads
    val action     = new Action
    // 压缩单独一个：它要做完才返回，期间停止等操作照常可用
    val compaction = new Action

    // 这个会话的实时事件：连上（含重连）与「库里变了」时重新拉取详情与时间线
    val frames  = Live.frames(s"/api/sessions/${Http.enc(idText)}/live", "live")
    val views   = frames.collect { case Frame.Data(j) => j.fromJson[LiveView].toOption }.collect { case Some(v) => v }
    val reloads = EventStream.merge(frames.collect { case Frame.Ready => () }, views.filter(_.kind == "changed").mapToUnit, Events.refreshNow.events)
    val detail   = Remote.triggered(reloads)(Http.call(SessionsApi.get)(id))
    val timeline = Remote.triggered(reloads)(Http.call(SessionsApi.timeline)(id))
    val live     = Var(LiveReply.empty)

    val markRead: () => Unit = () => Http.call(SessionsApi.read)(id).failed.foreach(e => dom.console.error("marking session read failed", e.getMessage))

    def canSend(text: String, ids: List[String], blocking: Boolean) = (text.trim.nonEmpty || ids.nonEmpty) && !blocking
    val sendable = draft.signal.combineWith(uploads.ids, uploads.blocking).map(canSend)
    val sendableNow = Var(false)

    /** 发出 `content`（默认输入框里的）；成功后清掉输入框（发送中又改了就保留新内容）与这一批附件。 */
    def send(content: Option[String] = None): Future[Boolean] =
      val text = content.getOrElse(draft.now())
      if action.busy.now() || !canSend(text, uploads.done.map(_._2.id), uploads.items.now().exists(!_.state.isInstanceOf[uploads.State.Done])) then Future.successful(false)
      else
        val batch = uploads.done
        val shown = draft.now()
        action.run(Http.call(SessionsApi.send)((id, MessageIn(text.trim, batch.map(_._2.id))))).map { ok =>
          if ok then
            bottom.pin()
            if draft.now() == shown then draft.set("")
            uploads.remove(batch.map(_._1).toSet)
          ok
        }

    // 说完就发：转写出的文字接在已输入的内容后面一起发出；发不了或发送失败时留在输入框里
    val voice = new Voice(text => {
      val content = Model.appendTranscript(draft.now(), text)
      send(Some(content)).foreach(ok => if !ok then draft.set(content))
    })

    val answer: (Long, String, AnswerIn) => Future[Unit] = (entry, call, a) => Http.call(SessionsApi.answer)((id, entry, call, a))
    val ctx = detail
      .map(_.data)
      .map(d => (d.flatMap(_.run).exists(_.state == "running"), d.map(_.threads).getOrElse(Nil)))
      .distinct
      .map((active, threads) => TranscriptCtx(id, active, threads, answer))

    val summary = detail.map(_.data.map(_.summary))
    val parent  = summary.map(_.flatMap(_.parent))
    // 返回：thread 回发起会话，其余回列表（来源页优先，见 Router.back）
    val backTo  = parent.map(_.fold(Route.Sessions)(p => Route.Session(p.value)))
    val backNow = Var[Route](Route.Sessions)

    def remove(): Unit =
      Confirm.ask("删除这个会话？", Some("会话的全部消息都会被删除；它开出的 thread 保留。"), "删除", danger = true).foreach { ok =>
        if ok then action.run(Http.call(SessionsApi.delete)(id), leave = Some(() => Router.go(backNow.now()))): Unit
      }

    val active = detail.map(_.data.exists(d => Model.Work.of(d.summary.state) != Model.Work.Idle)).combineWith(compaction.busy.signal).map(_ || _)

    val composer = div(
      cls := "flex flex-col gap-2",
      uploads.pendingView,
      div(
        cls := "flex items-end gap-2",
        uploads.attachButton(action.busy.signal),
        voice.button(action.busy.signal),
        textArea(
          cls := "textarea textarea--secondary flex-1 resize-none",
          aria.label := "回复",
          placeholder := s"回复…（${SendKey.hint}）",
          rows <-- draft.signal.map(d => d.count(_ == '\n').+(1).max(1).min(6)),
          uploads.onPasteFiles,
          onKeyDown.filter(SendKey.isSend) --> { e => e.preventDefault(); send(): Unit },
          controlled(value <-- draft.signal, onInput.mapToValue --> draft)
        ),
        child.maybe <-- active.map(a =>
          Option.when(a)(Ui.iconBtn("Square", "停止", Variant.DangerSoft, Size.Md, "size-4", disabledIf = action.busy.signal)(onClick --> (_ => action.fire(Http.call(SessionsApi.stop)(id)))))
        ),
        Ui.iconBtn("SendHorizontal", "发送", Variant.Primary, Size.Md, "size-4", disabledIf = action.busy.signal.combineWith(sendable).map((b, s) => b || !s))(
          onClick --> (_ => send(): Unit)
        )
      )
    )

    Ui.page(
      title = child <-- summary.map {
        case Some(s) if s.origin == "issue" => s.originRef.flatMap(_.toLongOption).fold[HtmlElement](span(s.title))(n => issueTitle(n, s.title))
        case Some(s)                        => span(s.title)
        case None                           => span("会话")
      },
      backTo = Some(() => backNow.now()),
      actions = Seq(
        Ui.btn(Variant.Secondary, Size.Sm, pending = compaction.busy.signal, disabledIf = action.busy.signal)(
          title := "把早期对话压缩成摘要，减少发给模型的上下文",
          child.maybe <-- compaction.busy.signal.map(b => Option.when(!b)(Icon("Archive"))),
          "压缩",
          onClick --> (_ => compaction.fire(Http.call(SessionsApi.compact)(id)))
        ),
        Ui.iconBtn("Trash2", "删除会话", iconCls = "size-4")(onClick --> (_ => remove()))
      ),
      footer = div(
        // 底部安全区由下面的导航栏留出，这里不重复
        cls := "border-t border-separator bg-surface px-3 pt-2 pb-2",
        div(
          cls := "relative mx-auto flex w-full max-w-3xl flex-col gap-2",
          child.maybe <-- bottom.atBottom.signal.map(at =>
            Option.when(!at)(
              Ui.iconBtn("ArrowDown", "回到底部", Variant.Secondary, iconCls = "size-4")(cls := "absolute -top-14 right-0 rounded-full shadow", onClick --> (_ => bottom.pin()))
            )
          ),
          action.errorView(),
          compaction.errorView(),
          Ui.errorAlert(voice.error.signal),
          child.maybe <-- detail.map(_.data.map(usageLine)),
          composer
        )
      ),
      main = bottom.scrollerMod
    )(
      // 打开即已读；重连（隐藏期间断开）与看着它时又来了通知，都再标一次
      onMountCallback(_ => markRead()),
      frames.collect { case Frame.Ready => () } --> (_ => markRead()),
      Events.changes.filter(_ == "notifications") --> (_ => markRead()),
      sendable --> sendableNow,
      backTo --> backNow,
      views --> (v => live.update(LiveReply.step(_, v))),
      timeline.map(_.data).updates.collect { case Some(t) => t } --> (t => live.update(r => if LiveReply.coveredBy(r, t) then r.copy(covered = true) else r)),
      Ui.loading(detail) { d =>
        div(
          cls := "flex flex-col gap-3",
          bottom.contentMod,
          onMountCallback(_ => bottom.pin()),
          div(
            cls := "flex items-center justify-center gap-2",
            child <-- d.map(x => Ui.appLink(Route.Agent(x.summary.agent.value).url)(Ui.chip(x.summary.agentName, Tone.Accent)())),
            span(cls := "text-xs text-muted", child.text <-- d.map(x => Ui.formatTime(x.summary.createdAt)))
          ),
          child.maybe <-- d.map(x =>
            x.summary.parent.map(p =>
              Ui.appLink(Route.Session(p.value).url, withFrom = false)(
                cls := "flex items-center justify-center gap-1 text-xs text-accent",
                Icon("CornerLeftUp", "size-3.5"),
                s"来自上级会话的 thread「${x.summary.originRef.getOrElse("")}」"
              )
            )
          ),
          Ui.loading(timeline) { t =>
            div(
              cls := "flex flex-col gap-3",
              transcript.view(t.combineWith(ctx).map((items, c) => transcript.keyed(items, c)), live = false, ctx),
              transcript.view(
                live.signal.combineWith(ctx).map((r, c) =>
                  if r.covered then Nil
                  else transcript.keyed(List(TimelineItem(-1, "reply", None, 0, 0, parts = r.parts.toList)), c, "live:")
                ),
                live = true,
                ctx
              ),
              child.maybe <-- t.combineWith(ctx).map((items, c) => transcript.unlinkedThreads(items, c))
            )
          },
          div(cls := "flex flex-col gap-3", children <-- d.map(_.queued).splitSeq(_.id)(q => transcript.queued(q.now()))),
          child.maybe <-- d.map(_.run).distinct.map(r => runStatus(r, () => action.run(Http.call(SessionsApi.retry)(id)): Unit)),
          Waits.panel(d, action)
        )
      }
    )
