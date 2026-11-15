package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import scala.concurrent.Future

import Http.ec
import Model.*

/** 会话列表（首页左列）。 */
object SessionList:
  /** 一页的会话数（服务端上限 200）。 */
  private val PageSize = 200

  // 选中的分类与折叠的 agent 跨刷新记住（不放地址：宽屏分屏时右侧切会话会换掉地址）
  private val filter    = Stored.choice("muliao.sessions.filter", Filter.values.toList, Filter.Chat)
  private val collapsed = Stored.text("muliao.sessions.collapsed", "")

  /** 取够 `count` 条：第一页之后按游标接着取。 */
  private def fetch(filter: Filter, keyword: String, count: Int): Future[List[SessionSummary]] =
    def loop(before: Option[Long], acc: List[SessionSummary]): Future[List[SessionSummary]] =
      Http
        .call(SessionsApi.list)(SessionQuery(None, filter.origins, Option(keyword).filter(_.nonEmpty), before, Some(PageSize)))
        .flatMap { page =>
          val all = acc ++ page.items.filterNot(s => acc.exists(_.id == s.id))
          page.next match
            case Some(n) if all.size < count => loop(Some(n), all)
            case _                           => Future.successful(all)
        }
    loop(None, Nil)

  private def stateCard(w: WorkState): String = w match
    case WorkState.Running => "border-l-4 border-accent bg-accent/5"
    case WorkState.Waiting => "border-l-4 border-accent/40"
    case WorkState.Idle    => ""

  /** 一张会话卡片；`withAgent` 时（置顶区）带上 agent 名与它现在配置的模型。 */
  private def card(id: SessionId, s: Signal[SessionSummary], withAgent: Boolean, models: Signal[Map[AgentId, String]]): HtmlElement =
    Ui.appLink(Route.session(id).url, withFrom = false)(
      cls := "block",
      div(
        cls <-- s.combineWith(Router.location).map((x, l) =>
          s"card card--default transition-colors hover:bg-surface-hover ${stateCard(x.state)} " +
            (if l.route.contains(Route.session(id)) then "ring-2 ring-accent" else "")
        ),
        div(
          cls := "card__content flex flex-row items-center gap-3",
          div(
            cls := "min-w-0 flex-1",
            div(
              cls := "flex items-center gap-2 font-medium",
              child.maybe <-- s.map(x =>
                x.state match
                  case WorkState.Running => Some(Ui.spinner("spinner--sm spinner--accent"))
                  case WorkState.Waiting => Some(span(aria.label := "等待中", Icon("Hourglass", "size-4 shrink-0 text-muted")))
                  case WorkState.Idle    => None
              ),
              child.maybe <-- s.map(x => Option.when(x.unread)(span(cls := "size-2 shrink-0 rounded-full bg-accent", aria.label := "未读"))),
              span(cls := "truncate", child.text <-- s.map(_.title))
            ),
            div(
              cls := "mt-1 flex flex-wrap gap-1.5",
              children <-- s.combineWith(models).map { (x, ms) =>
                (Option.when(withAgent)(x.agentName).toList ++ Option.when(withAgent)(ms.get(x.agent)).flatten.toList ++ List(originTag(x.origin)))
                  .map(t => Ui.chip(t, soft = false)(cls := "chip--secondary")) ++
                  (x.state match
                    case WorkState.Running => List(Ui.chip("运行中", Tone.Accent)())
                    case WorkState.Waiting => List(Ui.chip("等待中")())
                    case WorkState.Idle    => Nil
                  )
              }
            )
          ),
          span(cls := "shrink-0 text-xs text-muted", child.text <-- s.map(x => Ui.relativeTime(x.updatedAt)))
        )
      )
    )

  def view(): HtmlElement =
    val text      = Var("")
    // 关键字输入停下 300ms 后才查（搜消息全文），空白等于不搜
    val keyword = text.signal.updates.debounce(300).map(_.trim).toSignal("")
    // 一次多取一页：满了就在底下给「显示更多」；换分类、换关键字从第一页取
    val count = Var(PageSize)
    val query = filter.signal.combineWith(keyword).distinct
    val models = AgentsData.list.map(_.flatMap(a => a.model.map(m => a.id -> modelName(m))).toMap)
    val sessions: Signal[Load[List[SessionSummary]]] =
      query.flatMapSwitch { (f, k) =>
        count.set(PageSize)
        Remote.following(SessionsApi.list, count.signal.updates.mapToUnit)(fetch(f, k, count.now()))
      }
    def isCollapsed(id: AgentId): Signal[Boolean] = collapsed.signal.map(_.split(',').contains(id.value))
    def toggle(id: AgentId): Unit = collapsed.update { c =>
      val set = c.split(',').filter(_.nonEmpty).toSet
      (if set(id.value) then set - id.value else set + id.value).mkString(",")
    }
    Ui.page("会话")(
      Fields.segmented("会话分类", filter.signal, filter.set, Filter.values.toList),
      div(
        cls := "relative",
        Icon("Search", "pointer-events-none absolute top-1/2 left-3 size-4 -translate-y-1/2 text-muted"),
        input(
          tpe := "search",
          cls := "input input--secondary input--full-width pl-9",
          aria.label := "搜索会话",
          placeholder := "搜索标题或消息内容",
          controlled(value <-- text.signal, onInput.mapToValue --> text)
        )
      ),
      Ui.loading(sessions) { data =>
        val split = data.map(splitActive)
        div(
          cls := "flex flex-col gap-4",
          child.maybe <-- data.combineWith(keyword).map((l, k) => Option.when(l.isEmpty)(Ui.emptyHint(if k.isEmpty then "没有会话" else "没有匹配的会话"))),
          child.maybe <-- split.map(_._1).map(active =>
            Option.when(active.nonEmpty)(
              sectionTag(
                cls := "flex flex-col gap-2",
                div(cls := "flex items-center gap-2 text-sm font-semibold text-accent", Ui.spinner("spinner--sm spinner--current"), span("进行中"), span(cls := "font-normal", active.size.toString))
              )
            )
          ),
          div(
            cls := "flex flex-col gap-2",
            children <-- split.map(_._1).splitSeq(_.id)(s => card(s.now().id, s, withAgent = true, models))
          ),
          children <-- split.map(x => groupByAgent(x._2)).splitSeq(_.agent) { g =>
            val gid  = g.now().agent
            val open = isCollapsed(gid).combineWith(keyword).map((c, k) => k.nonEmpty || !c)
            sectionTag(
              cls := "flex flex-col gap-2",
              button(
                tpe := "button",
                cls := "flex items-center gap-2 text-left text-sm font-semibold text-muted hover:text-foreground",
                aria.expanded <-- open,
                // 搜索时一律展开：命中的会话不被折叠藏起来
                disabled <-- keyword.map(_.nonEmpty),
                onClick --> (_ => toggle(gid)),
                child <-- open.map(o => Icon(if o then "ChevronDown" else "ChevronRight", "size-4 shrink-0")),
                span(cls := "shrink-0", child.text <-- g.map(_.name)),
                span(cls := "font-normal", child.text <-- g.map(_.sessions.size.toString)),
                child.maybe <-- models.map(_.get(gid).map(m => span(cls := "min-w-0 truncate text-xs font-normal", m))),
                child.maybe <-- g.map(x => Option.when(x.unread > 0)(Ui.chip(s"${x.unread} 未读", Tone.Accent)()))
              ),
              div(
                cls := "flex flex-col gap-2",
                children <-- g.combineWith(open).map((x, o) => if o then x.sessions else Nil).splitSeq(_.id)(s => card(s.now().id, s, withAgent = false, models))
              )
            )
          },
          child.maybe <-- data.map(l =>
            Option.when(l.nonEmpty && l.size >= count.now())(Ui.btn(Variant.Secondary)("显示更多", onClick --> (_ => count.update(_ + PageSize))))
          )
        )
      }
    )
