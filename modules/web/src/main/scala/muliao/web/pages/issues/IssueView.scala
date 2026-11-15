package muliao.web.pages.issues

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import org.scalajs.dom

import Http.ec
import IssueModel.*
import IssueParts.*

/** /issues/<号>：属性（随改随存）、开始与叫停、依赖、关联会话、审核、描述、时间线、评论；标题、描述与依赖在编辑器里改。 */
object IssueView:
  def view(id: Long): HtmlElement =
    val issueId = IssueId.wrap(id)
    val load    = Remote(BoardApi.get)(issueId)
    val agents  = AgentsData.load().map(_.data.getOrElse(Nil))
    val editing = Var(false)
    val remove  = new Action
    def delete(): Unit =
      Confirm.ask(s"删除 #$id？", Some("评论与时间线一起删除，agent 的会话保留。"), "删除", danger = true).foreach { ok =>
        if ok then remove.run(Http.call(BoardApi.delete)(issueId), toast = Some("删除"), leave = Some(() => Router.go(Route.Issues))): Unit
      }
    Ui.page(
      s"#$id",
      back = Some(Route.Issues),
      actions = Seq(
        Ui.iconBtn("Pencil", "编辑", iconCls = "size-4")(onClick --> (_ => editing.set(true))),
        Ui.iconBtn("Trash2", "删除", iconCls = "size-4")(disabled <-- remove.busy.signal, onClick --> (_ => delete()))
      )
    )(
      // 看到了 issue 就看到了请你审核它的通知
      onMountCallback(_ =>
        Http.call(NotificationsApi.readPage)(ReadPage(Route.Issue(id).url)).failed.foreach(e => dom.console.error("标记 issue 通知已读失败", e.getMessage))
      ),
      Ui.loading(load)(d => body(issueId, d, agents, editing))
    )

  private def body(id: IssueId, d: Signal[IssueDetail], agents: Signal[List[AgentSummary]], editing: Var[Boolean]): Modifier[HtmlElement] =
    val change  = new Action
    val summary = d.map(_.summary)
    // 页面上正显示的那份：下拉框选了当前值不提交
    val shown   = Var(Option.empty[IssueSummary])
    def patchIf(same: IssueSummary => Boolean)(p: IssuePatch): Unit =
      if !shown.now().exists(same) then change.run(Http.call(BoardApi.update)((id, p))): Unit
    Seq(
      summary --> (s => shown.set(Some(s))),
      // 标题下面先放属性、审核与关联会话（打开页面最常看、最常改的），再放正文；编辑时标题和正文一起在编辑器里
      child <-- editing.signal.distinct.withCurrentValueOf(d).map {
        case (true, detail) => editor(id, detail, () => editing.set(false))
        case (false, _) =>
          h2(cls := "flex items-center gap-2 text-xl font-semibold", child <-- summary.map(s => statusDot(s.status)), child.text <-- summary.map(_.title))
      },
      div(
        cls := "grid grid-cols-2 gap-3 sm:grid-cols-4",
        Fields.select[AgentId](
          "负责项目（owner）",
          summary.map(_.owner),
          o => patchIf(_.owner.contains(o))(IssuePatch(owner = Some(o))),
          agents.combineWith(summary).map((as, s) => ownerOptions(as, s.owner, assignable = true)),
          placeholderText = "无负责项目",
          disabledIf = change.busy.signal
        ),
        Fields.select[IssueStatus](
          "状态",
          summary.map(s => Some(s.status)),
          st => patchIf(_.status == st)(IssuePatch(status = Some(st))),
          Val(statusOptions),
          disabledIf = change.busy.signal
        ),
        Fields.select[Priority](
          "优先级",
          summary.map(s => Some(s.priority)),
          pr => patchIf(_.priority == pr)(IssuePatch(priority = Some(pr))),
          Val(priorityOptions),
          disabledIf = change.busy.signal
        ),
        Fields.select[AssigneeChoice](
          "指派给",
          summary.map(s => Some(AssigneeChoice.of(s.assignee))),
          a => patchIf(s => AssigneeChoice.of(s.assignee) == a)(assigneePatch(a)),
          agents.combineWith(summary).map((as, s) => assigneeOptions(as, AssigneeChoice.of(s.assignee))),
          disabledIf = change.busy.signal
        )
      ),
      controls(id, summary, change),
      change.errorView(),
      links(d),
      sessions(d),
      child.maybe <-- summary.map(reviewable).distinct.map(r => Option.when(r)(review(id))),
      child.maybe <-- editing.signal.map(e =>
        Option.when(!e)(
          div(
            cls := "border-t border-separator pt-3",
            child <-- d.map(_.description.trim.isEmpty).distinct.map {
              case true  => p(cls := "text-sm text-muted", "没有描述")
              case false => Markdown.view(d.map(_.description))
            }
          )
        )
      ),
      h3(cls := "mt-2 text-sm font-semibold text-muted", "动态"),
      timeline(d),
      commentBox(id)
    )

  private def assigneePatch(a: AssigneeChoice): IssuePatch = a.toAssignee match
    case Some(x) => IssuePatch(assignee = Some(x))
    case None    => IssuePatch(unassign = true)

  // ---- 开始与叫停 ----

  /** 开始与叫停的按钮行（都不可用时不占位）+ 展开的叫停表单。 */
  private def controls(id: IssueId, summary: Signal[IssueSummary], change: Action): Modifier[HtmlElement] =
    val stopping = Var(false)
    Seq(
      div(
        cls := "flex flex-wrap items-center gap-2 empty:hidden",
        child.maybe <-- summary.map(startable).distinct.map(_.hint.map { hint =>
          span(
            cls := "flex items-center gap-2",
            Ui.btn(size = Size.Sm, pending = change.busy.signal)(Icon("Play"), "开始", onClick --> (_ => change.run(Http.call(BoardApi.start)(id)): Unit)),
            span(cls := "text-xs text-muted", hint)
          )
        }),
        child.maybe <-- summary.map(stoppable).distinct.map(on =>
          Option.when(on)(Ui.btn(Variant.DangerSoft, Size.Sm)(Icon("Square"), "叫停", onClick --> (_ => stopping.update(!_))))
        )
      ),
      child.maybe <-- stopping.signal.combineWith(summary.map(stoppable)).map((open, can) => Option.when(open && can)(stopForm(id, () => stopping.set(false))))
    )

  private def stopForm(id: IssueId, close: () => Unit): HtmlElement =
    val halt    = Var(Halt.Backlog: Halt)
    val message = Var("")
    val act     = new Action
    val ready   = halt.signal.combineWith(message.signal).map((h, m) => !h.needsMessage || m.trim.nonEmpty)
    def submit(): Unit =
      val h = halt.now()
      val m = Option(message.now().trim).filter(_.nonEmpty)
      if !h.needsMessage || m.isDefined then
        act.run(Http.call(BoardApi.stop)((id, StopIssueIn(h.status, m)))).foreach(ok => if ok then close())
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-3",
        p(cls := "text-sm font-medium", "叫停：停掉指派的 agent 处理它的会话，再改状态"),
        Fields.segmented("叫停后改成", halt.signal, halt.set, Halt.values.toList.map(h => h -> h.label), showLabel = true),
        Fields.area(
          "留言（记成评论并交给那个会话，让它收尾或按新方向做）",
          message,
          hint = Some(child.text <-- halt.signal.map(h => if h.needsMessage then "改方向必须留言，说清接下来做什么" else "可不填")),
          rowsN = 2,
          onSubmit = Some(() => submit())
        ),
        act.errorView(),
        div(
          cls := "flex gap-2",
          Ui.btn(Variant.Danger, pending = act.busy.signal)("确认叫停", disabled <-- ready.combineWith(act.busy.signal).map((r, b) => !r || b), onClick --> (_ => submit())),
          Ui.btn(Variant.Ghost)("取消", onClick --> (_ => close()))
        )
      )
    )

  // ---- 依赖与会话 ----

  private def issueLink(l: IssueLink): HtmlElement =
    Ui.appLink(Route.Issue(l.id.value).url)(cls := "flex items-center gap-1 text-accent", title := s"${l.title}（${statusLabel(l.status)}）", statusDot(l.status), s"#${l.id.value}")

  private def links(d: Signal[IssueDetail]): HtmlElement =
    div(
      cls := "flex flex-col gap-1 text-sm empty:hidden",
      children <-- d.map { x =>
        List(
          Option.when(x.dependsOn.nonEmpty)(
            div(
              cls := "flex flex-wrap items-center gap-2",
              span(cls := "text-muted", "依赖"),
              x.dependsOn.map(issueLink),
              span(cls := "text-muted", if x.summary.waitingOn.nonEmpty then s"等待 ${refs(x.summary.waitingOn)} 完成" else "已全部完成")
            )
          ),
          Option.when(x.blocks.nonEmpty)(
            div(cls := "flex flex-wrap items-center gap-2", span(cls := "text-muted", "被依赖"), x.blocks.map(issueLink))
          )
        ).flatten
      }
    )

  private def sessions(d: Signal[IssueDetail]): HtmlElement =
    div(
      cls := "flex flex-wrap items-center gap-x-3 gap-y-1 text-sm empty:hidden",
      children <-- d.map(_.sessions).map {
        case Nil => Nil
        case ss =>
          span(cls := "text-muted", "关联会话") :: ss.map { s =>
            Ui.appLink(Route.Session(s.session.value).url)(
              cls := "flex items-center gap-1 text-accent",
              Icon("MessageSquare", "size-4"),
              s"${s.agentName} 的会话",
              span(cls := "text-xs text-muted", s"（${workLabel(Work.of(s.state))}）")
            )
          }
      }
    )

  // ---- 审核 ----

  /** 指派给用户的 issue 在这里审核（推送通知点开就是这一页；通知上的「打回」带 ?review=reject 打开，聚焦理由）。 */
  private def review(id: IssueId): HtmlElement =
    val reason = Var("")
    val act    = new Action
    val reject = Router.now.params.get("review").contains("reject")
    def sendBack(): Unit =
      if !act.busy.now() && reason.now().trim.nonEmpty then
        act.run(Http.call(BoardApi.sendBack)((id, SendBackIn(reason.now().trim)))).foreach(ok => if ok then reason.set(""))
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-2",
        p(cls := "text-sm font-medium", "待你审核"),
        Fields.area("打回理由（记成评论，回到进行中并指派回交给你的 agent）", reason, rowsN = 2, autoFocusOn = reject, onSubmit = Some(() => sendBack())),
        act.errorView(),
        div(
          cls := "flex gap-2",
          Ui.btn(pending = act.busy.signal)("通过", onClick --> (_ => act.run(Http.call(BoardApi.approve)(id)): Unit)),
          Ui.btn(Variant.DangerSoft, pending = act.busy.signal)(
            "打回",
            disabled <-- reason.signal.combineWith(act.busy.signal).map((r, b) => r.trim.isEmpty || b),
            onClick --> (_ => sendBack())
          )
        )
      )
    )

  // ---- 时间线与评论 ----

  /** 这条动态发生在哪个会话：点开看过程（返回回到这个 issue）。 */
  private def sourceSession(s: Option[SessionId]): Option[HtmlElement] =
    s.map(sid =>
      Ui.appLink(Route.Session(sid.value).url)(
        cls := "flex shrink-0 items-center text-muted hover:text-accent",
        aria.label := "来源会话",
        title := "打开做这件事的会话",
        Icon("MessageSquare", "size-3.5")
      )
    )

  private def timeline(d: Signal[IssueDetail]): HtmlElement =
    ol(
      cls := "flex flex-col gap-3",
      children <-- d.map(_.events).splitSeq(_.id) { sig =>
        val e = sig.now()
        e.comment match
          case Some(body) =>
            li(
              Ui.card(
                div(
                  cls := "card__content flex flex-col gap-1 p-3",
                  div(
                    cls := "flex items-center gap-2 text-xs text-muted",
                    span(cls := "font-medium text-foreground", e.actor),
                    span(e.text),
                    span(Ui.formatTime(e.at)),
                    sourceSession(e.session)
                  ),
                  Markdown.view(body)
                )
              )
            )
          case None =>
            li(
              cls := "flex items-center gap-2 px-1 text-xs text-muted",
              span(cls := "size-1.5 shrink-0 rounded-full bg-muted", aria.hidden := true),
              span(cls := "font-medium text-foreground", e.actor),
              span(cls := "min-w-0 break-words", e.text),
              span(cls := "ml-auto shrink-0", Ui.formatTime(e.at)),
              sourceSession(e.session)
            )
      }
    )

  private def commentBox(id: IssueId): HtmlElement =
    val text = Var("")
    val act  = new Action
    def post(): Unit =
      // 快捷键不经过按钮的禁用状态：请求进行中再按一次不能重复发表
      if !act.busy.now() && text.now().trim.nonEmpty then
        act.run(Http.call(BoardApi.comment)((id, CommentIn(text.now())))).foreach(ok => if ok then text.set(""))
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-2",
        Fields.area("评论", text, rowsN = 3, onSubmit = Some(() => post())),
        act.errorView(),
        div(
          Ui.btn(pending = act.busy.signal)(
            "发表评论",
            disabled <-- text.signal.combineWith(act.busy.signal).map((t, b) => t.trim.isEmpty || b),
            onClick --> (_ => post())
          )
        )
      )
    )

  // ---- 编辑 ----

  /** 编辑标题、正文与依赖：草稿只在打开时取一次，之后的刷新不冲掉；只提交改过的字段（编辑期间 agent 可能改了别的）。 */
  private def editor(id: IssueId, base: IssueDetail, done: () => Unit): HtmlElement =
    val title  = Var(base.summary.title)
    val desc   = Var(base.description)
    val deps   = Var(base.summary.dependsOn.map(i => s"#${i.value}").mkString(", "))
    val parsed = deps.signal.map(parseRefs)
    val act    = new Action
    val ready  = title.signal.combineWith(parsed).map((t, p) => t.trim.nonEmpty && p.isDefined)
    def save(): Unit =
      parseRefs(deps.now()) match
        case Some(ds) if title.now().trim.nonEmpty =>
          val p = IssuePatch(
            title = Option(title.now().trim).filter(_ != base.summary.title),
            description = Option(desc.now()).filter(_ != base.description),
            // 依赖按解析后的编号比较：只改了空格不算改过
            dependsOn = Option(ds).filter(_ != base.summary.dependsOn)
          )
          if p == IssuePatch() then done()
          else act.run(Http.call(BoardApi.update)((id, p))).foreach(ok => if ok then done())
        case _ => ()
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-3",
        Fields.text("标题", title),
        Fields.area("描述（Markdown）", desc, rowsN = 8),
        Fields.text("依赖的 issue（编号，逗号分隔；全部完成或取消后才交给被指派的 agent）", deps),
        child.maybe <-- parsed.map(r => Option.when(r.isEmpty)(p(cls := "text-xs text-danger", "只能填 issue 编号，如 #12, #15"))),
        act.errorView(),
        div(
          cls := "flex gap-2",
          Ui.btn(pending = act.busy.signal)("保存", disabled <-- ready.combineWith(act.busy.signal).map((r, b) => !r || b), onClick --> (_ => save())),
          Ui.btn(Variant.Ghost)("取消", onClick --> (_ => done()))
        )
      )
    )
