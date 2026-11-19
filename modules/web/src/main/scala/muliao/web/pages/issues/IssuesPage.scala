package muliao.web.pages.issues

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import com.raquo.airstream.state.StrictSignal

import scala.scalajs.js

import Http.ec
import IssueModel.*
import IssueParts.*

/** /issues：看板、列表、依赖图三种视图，按状态、指派、负责项目筛选（条件在地址的查询参数里），新建 issue。 */
object IssuesPage:
  private enum View(val label: String) extends Shown.Self:
    case Board extends View("看板")
    case List  extends View("列表")
    case Graph extends View("依赖图")

  // 选过的视图与依赖图范围跨刷新记住
  private val viewVar = Stored.choice("muliao.issues.view", View.values.toList, View.Board)
  private val scope   = Stored.choice("muliao.issues.graphScope", IssueGraph.Scope.values.toList, IssueGraph.Scope.Open)

  /** @param filters 地址里的筛选条件（改它就是改地址） */
  def view(filters: Signal[IssueQuery]): HtmlElement =
    val agents  = AgentsData.list
    val loaded  = filters.distinct.flatMapSwitch(f => Remote(BoardApi.list)(f))
    val edits   = new EventBus[IssueQuery => IssueQuery]
    // 拖拽后先在本地挪动，接口成功、重新拉到的数据落下后以服务端为准；连着拖几张时每张各占一项
    val moves   = Var(Map.empty[IssueId, Move])
    val shown   = loaded.combineWith(moves.signal).map((l, m) => l.map(applyMoves(_, m)))
    val mover   = new Mover(moves)
    Ui.page("Issues", wide = true)(
      loaded.updates --> (_ => moves.update(_.filterNot(_._2.settled))),
      edits.events.withCurrentValueOf(filters) --> ((edit, f) => Router.update(Route.issues(edit(f)))),
      div(cls := "mx-auto w-full max-w-3xl px-4 pt-3", toolbar(viewVar, scope, filters, edits.writer, agents)),
      Ui.loading(shown) { issues =>
        child <-- viewVar.signal.map {
          case View.Board => IssueBoard.view(issues, filters.map(_.columns), agents, mover.move)
          case View.List  => div(cls := "mx-auto flex w-full max-w-3xl flex-col gap-3 px-4 py-3", list(issues, agents))
          case View.Graph => IssueGraphView.view(issues, scope.signal, agents)
        }
      }
    )

  /** 拖拽改状态：失败用 toast 提示（连着拖几张时各自报，不互相冲掉）。 */
  private final class Mover(moves: Var[Map[IssueId, Move]]):
    private val act   = new Action
    private var token = 0

    def move(id: IssueId, status: IssueStatus): Unit =
      token += 1
      val m = Move(status, js.Date.now().toLong, token, settled = false)
      moves.update(_ + (id -> m))
      act.run(Http.call(BoardApi.update)((id, IssuePatch(status = Some(status)))), toast = Some(s"把 #${id.value} 移到「${status.shown}」")).foreach { ok =>
        // 同一张又被拖了一次：那一次还在提交，留着它的
        moves.update { all =>
          all.get(id) match
            case Some(cur) if cur.token == m.token => if ok then all.updated(id, cur.copy(settled = true)) else all - id
            case _                                 => all
        }
      }

  // ---- 工具栏 ----

  private def toolbar(
      viewVar: Var[View],
      scope: Var[IssueGraph.Scope],
      filters: Signal[IssueQuery],
      edit: Observer[IssueQuery => IssueQuery],
      agents: Signal[List[AgentSummary]]
  ): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      Fields.segmented("视图", viewVar.signal, viewVar.set, View.values.toList),
      child.maybe <-- viewVar.signal.map(v =>
        Option.when(v == View.Graph)(
          Fields.segmented("依赖图范围", scope.signal, scope.set, IssueGraph.Scope.values.toList)
        )
      ),
      div(
        cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
        AgentPicker[Option[AgentId]](
          "负责项目",
          filters.map(f => Some(f.owner)),
          o => edit.onNext(_.copy(owner = o)),
          agents,
          a => Some(a.id),
          fixed = List(Opt(None, "所有项目")),
          archived = AgentPicker.Archived.Listed,
          prefix = ownerPrefix
        ),
        AgentPicker[Option[Assignee]](
          "指派给",
          filters.map(f => Some(f.assignee)),
          a => edit.onNext(_.copy(assignee = a)),
          agents,
          a => Some(Assignee.Agent(a.id)),
          fixed = List(Opt(None, "所有人"), Opt(Some(Assignee.User), "我")),
          archived = AgentPicker.Archived.Listed
        )
      ),
      statusFilter(filters, edit),
      newIssueForm(agents)
    )

  /** 按状态筛选：可多选，都不选是全部。 */
  private def statusFilter(filters: Signal[IssueQuery], edit: Observer[IssueQuery => IssueQuery]): HtmlElement =
    div(
      cls := "flex flex-col gap-1",
      span(cls := "label", "状态"),
      div(
        cls := "flex flex-wrap gap-1.5",
        role := "group",
        aria.label := "按状态筛选",
        Statuses.map { s =>
          val on = filters.map(_.status.contains(s))
          button(
            tpe := "button",
            cls := "toggle-button toggle-button--default toggle-button--sm flex items-center gap-1.5",
            aria.pressed <-- on.map(_.toString),
            dataAttr("selected") <-- on.map(_.toString),
            onClick --> (_ => edit.onNext(_.toggle(s))),
            statusDot(s),
            s.shown
          )
        }
      )
    )

  // ---- 新建 ----

  private def newIssueForm(agents: Signal[List[AgentSummary]]): HtmlElement =
    val title    = Var("")
    val desc     = Var("")
    val status   = Var(IssueStatus.InProgress)
    val priority = Var(Priority.Unset)
    val assignee = Var(AssigneeChoice.Nobody: AssigneeChoice)
    // 负责的项目必填：不猜
    val owner    = Var(Option.empty[AgentId])
    val deps     = Var("")
    val parsed   = deps.signal.map(parseRefs)
    val act      = new Action
    val ready    = title.signal.combineWith(owner.signal, parsed).map((t, o, p) => t.trim.nonEmpty && o.isDefined && p.isDefined)
    def submit(): Unit =
      (owner.now(), parseRefs(deps.now())) match
        case (Some(o), Some(ds)) if title.now().trim.nonEmpty =>
          val req = NewIssue(title.now().trim, o, desc.now(), status.now(), priority.now(), assignee.now().toAssignee, ds)
          act.run(Http.call(BoardApi.create)(req)).foreach { ok =>
            if ok then
              title.set("")
              desc.set("")
              deps.set("")
          }
        case _ => ()
    Ui.collapsibleCard("新建 issue")(Seq[Modifier[HtmlElement]](
      Fields.text("标题", title),
      Fields.area("描述（Markdown）", desc, rowsN = 4),
      div(
        cls := "grid grid-cols-1 gap-3 sm:grid-cols-2",
        AgentPicker[AgentId]("负责项目（owner）", owner.signal, o => owner.set(Some(o)), agents, _.id, prefix = ownerPrefix, hint = Some("转派给别人做也不变")),
        Fields.select[IssueStatus]("状态", status.signal.map(Some(_)), status.set, Val(statusOptions)),
        Fields.select[Priority]("优先级", priority.signal.map(Some(_)), priority.set, Val(priorityOptions)),
        AgentPicker[AssigneeChoice]("指派给", assignee.signal.map(Some(_)), assignee.set, agents, a => AssigneeChoice.Agent(a.id), fixed = assigneeFixed)
      ),
      depsField("依赖的 issue（编号，逗号分隔，可不填）", deps),
      p(cls := "text-xs text-muted", "指派给 agent 且状态为进行中时，会唤醒它来处理；有依赖的要等依赖全部完成或取消后才唤醒。"),
      act.errorView(),
      div(Ui.btn(pending = act.busy.signal, disabledIf = ready.map(!_))("创建", onClick --> (_ => submit())))
    ))

  // ---- 列表 ----

  private def list(issues: Signal[List[IssueSummary]], agents: Signal[List[AgentSummary]]): Modifier[HtmlElement] =
    val groups   = issues.map(groupByStatus)
    val nonEmpty = groups.map(g => Statuses.filter(g(_).nonEmpty))
    Seq(
      child.maybe <-- nonEmpty.map(n => Option.when(n.isEmpty)(Ui.emptyHint("还没有 issue"))),
      div(
        cls := "flex flex-col gap-4",
        children <-- nonEmpty.splitSeq(identity) { sig =>
          val status   = sig.now()
          val items    = groups.map(_(status))
          val expanded = Var(false)
          sectionTag(
            cls := "flex flex-col",
            h2(cls := "flex items-center gap-2 py-1 text-sm font-semibold text-muted", statusDot(status), status.shown, span(cls := "font-normal", child.text <-- items.map(_.size.toString))),
            children <-- items.combineWith(expanded.signal).map(fold).splitSeq(_.id)(row(agents)),
            foldToggle(items.map(_.size), expanded)
          )
        }
      )
    )

  private def row(agents: Signal[List[AgentSummary]])(sig: StrictSignal[IssueSummary]): HtmlElement =
    val i0 = sig.now()
    Ui.appLink(Route.issue(i0.id).url)(
      cls := "flex items-center gap-3 border-b border-separator px-1 py-2 text-sm hover:bg-surface-hover",
      children <-- sig.combineWith(agents).map { (i, as) =>
        List(
          Some(span(cls := "w-10 shrink-0 text-xs text-muted", s"#${i.id.value}")),
          Some(statusDot(i.status)),
          Some(ownerDot(ownerHue(as, i.owner))),
          Some(span(cls := "min-w-0 flex-1 truncate", i.title)),
          Option.when(i.stalled)(span(cls := "shrink-0 text-xs text-warning", "没人在处理")),
          Option.when(i.priority != Priority.Unset)(span(cls := "shrink-0 text-xs text-muted", i.priority.shown)),
          Some(span(cls := "w-20 shrink-0 truncate text-right text-xs text-muted", assigneeLabel(i)))
        ).flatten
      }
    )
