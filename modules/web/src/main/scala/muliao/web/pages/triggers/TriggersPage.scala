package muliao.web.pages.triggers

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import scala.scalajs.js

import Http.ec

/** 自动发起会话的入口：定时、一次性、流式，方式的差异只在各自的配置。按 agent 分组的卡片网格，点卡片看详情。 */
object TriggersPage:
  def view(): HtmlElement =
    val triggers = Remote("triggers")(Http.call(TriggersApi.list)(None))
    val agents   = AgentsData.load()
    val agentList = agents.map(_.data.getOrElse(Nil))
    val openId   = Var(Option.empty[TriggerId])
    Ui.page("触发器", wide = true)(
      div(
        cls := "mx-auto flex w-full max-w-7xl flex-col gap-4 px-4 py-3",
        NewForm.view(agentList),
        Ui.loading(triggers) { list =>
          val groups = list.combineWith(agentList).map((ts, as) => Show.groupByAgent(ts, as.map(_.id)))
          Seq(
            child.maybe <-- list.map(l => Option.when(l.isEmpty)(Ui.emptyHint("没有触发器"))),
            children <-- groups.splitSeq(_._1) { g =>
              val ts = g.map(_._2)
              Ui.groupSection(g.now()._2.headOption.fold(g.now()._1.value)(_.agentName), ts.map(_.size))(
                Ui.cardGrid(children <-- ts.splitSeq(_.id)(t => Card.view(t, showAgent = false, () => openId.set(Some(t.now().id)))))
              )
            },
            Detail.modal(openId, list)
          )
        },
        Finished.section()
      )
    )

/** 网格里的一张卡片：各项只显示开头，点开看详情。整张卡片可点（铺满的透明按钮），开关浮在它上面。 */
object Card:
  def view(t: Signal[TriggerView], showAgent: Boolean, onOpen: () => Unit): HtmlElement =
    val finished = t.map(_.finishedAt.isDefined)
    div(
      cls <-- finished.map(f => s"card ${if f then "card--secondary" else "card--default"} relative gap-2 transition-shadow hover:shadow-md"),
      dataAttr("slot") := "card",
      button(
        tpe := "button",
        aria.label <-- t.map(x => s"查看触发器「${x.name.value}」的详情"),
        cls := "absolute inset-0 z-0 cursor-pointer rounded-[inherit] outline-none focus-visible:ring-2 focus-visible:ring-accent",
        onClick --> (_ => onOpen())
      ),
      div(
        cls := "pointer-events-none flex items-start gap-2",
        div(
          cls := "min-w-0 flex-1",
          div(cls := "truncate font-medium", child.text <-- t.map(_.name.value)),
          div(
            cls := "mt-1 flex flex-wrap items-center gap-1.5",
            child <-- t.map(x => Ui.chip(x.kind.title)()),
            Option.when(showAgent)(child <-- t.map(x => Ui.chip(x.agentName, Tone.Accent)())),
            child.maybe <-- t.map(x => Show.warning(x).map(w => Ui.chip(w, Tone.Warning)()))
          )
        ),
        // 已结束的触发器不能启停
        child.maybe <-- finished.distinct.map(f => Option.when(!f)(div(cls := "pointer-events-auto relative z-10", EnableSwitch.view(t))))
      ),
      div(
        cls := "pointer-events-none flex min-w-0 flex-col gap-1.5",
        code(cls := "truncate font-mono text-xs text-muted", child.text <-- t.map(Show.summary(_, Ui.formatTime))),
        child.maybe <-- t.map(_.precheck.map(p => div(cls := "truncate text-xs text-muted", "前置检查：", code(cls := "font-mono", p.command)))),
        p(cls := "line-clamp-3 whitespace-pre-wrap break-words text-sm", child.text <-- t.map(_.prompt)),
        div(
          cls := "text-xs text-muted",
          child.text <-- t.map(x =>
            x.finishedAt
              .map(f => s"完成 ${Ui.formatTime(f)}")
              .orElse(x.lastFiredAt.map(l => s"上次触发 ${Ui.formatTime(l)}"))
              .getOrElse("还没有触发过")
          )
        )
      )
    )

/** 启用开关：卡片和详情里共用；失败用 toast 提示（卡片上没有放错误的位置）。 */
object EnableSwitch:
  def view(t: Signal[TriggerView]): HtmlElement =
    val act = new Action
    div(
      child <-- t.map(x => (x.id, x.name.value)).distinct.map { (id, name) =>
        Fields.switch(
          s"启用触发器「$name」",
          t.map(_.enabled),
          on => act.run(Http.call(TriggersApi.update)((id, TriggerPatch(enabled = Some(on)))), toast = Some(s"${if on then "启用" else "停用"}「$name」")).foreach(_ => ()),
          disabledIf = act.busy.signal
        )
      }
    )

/** 详情弹窗：跟着列表走，重新加载后显示最新内容，删掉后自动关闭。 */
object Detail:
  def modal(openId: Var[Option[TriggerId]], list: Signal[List[TriggerView]]): Modifier[HtmlElement] =
    val current = openId.signal.combineWith(list).map((id, l) => id.flatMap(i => l.find(_.id == i)))
    Seq(
      // 详情里删掉了它、或列表刷新后不再有它：关掉
      list.updates --> (l => if openId.now().exists(i => !l.exists(_.id == i)) then openId.set(None)),
      child.maybe <-- current.map(_.map(_.id)).distinct.map(_.map { id =>
        val t = present(current.map(_.filter(_.id == id)))
        Dialog(() => openId.set(None), "触发器详情", dialogCls = "sm:max-w-3xl")(item(t))
      })
    )

  /** 只取有值的时刻：详情只在有值时挂着，取不到时整个弹窗随即卸下，这期间保留最近一次的值。 */
  private def present[A](s: Signal[Option[A]]): Signal[A] =
    var last: Option[A] = None
    s.map { v =>
      v.foreach(a => last = Some(a))
      last.getOrElse(throw new IllegalStateException("还没有值"))
    }

  /** 一个触发器的详情：查看或编辑（编辑器只在编辑时挂上，每次进入编辑都从当时的值开始）。 */
  private def item(t: Signal[TriggerView]): Modifier[HtmlElement] =
    val latest   = Var(Option.empty[TriggerView])
    val editing  = Var(Option.empty[TriggerView])
    val finished = t.map(_.finishedAt.isDefined).distinct
    Seq(
      t --> (x => latest.set(Some(x))),
      Dialog.header(
        cls := "flex flex-row items-start gap-3 pr-8",
        div(
          cls := "min-w-0 flex-1",
          Dialog.heading(cls := "break-all", child.text <-- t.map(_.name.value)),
          div(
            cls := "mt-1 flex flex-wrap items-center gap-1.5 text-xs text-muted",
            child <-- t.map(x => Ui.chip(x.kind.title)()),
            child <-- t.map(x => Ui.chip(x.agentName, Tone.Accent)()),
            child <-- t.map(kindStatus),
            child.maybe <-- t.map(_.finishedAt.map(f => span(s"· 完成 ${Ui.formatTime(f)}")))
          )
        ),
        child.maybe <-- finished.map(f => Option.when(!f)(EnableSwitch.view(t)))
      ),
      child <-- finished.combineWith(editing.signal).distinct.map {
        case (true, _)           => finishedBody(t)
        case (false, Some(base)) => Editor.view(base, facts(t), () => editing.set(None))
        case (false, None)       => shown(t, () => editing.set(latest.now()))
      }
    )

  /** 这种方式自己的运行状态：下次时间、进程等。 */
  private def kindStatus(t: TriggerView): HtmlElement = t.kind match
    case TriggerKind.Schedule =>
      span(
        code(cls := "rounded bg-surface-secondary px-1.5 py-0.5 font-mono text-xs", t.config.cron.getOrElse("")),
        t.nextAt.filter(_ => t.enabled).map(n => s" · 下次 ${Ui.formatTime(n)}"),
        Option.when(t.skipped > 0)(span(cls := "text-warning", s" · 已连续跳过 ${t.skipped} 次：上次的会话还没做完"))
      )
    case TriggerKind.Once => span(s"计划 ${Show.planned(t, Ui.formatTime)}")
    case TriggerKind.Stream =>
      span(
        cls := "flex flex-col gap-1",
        code(cls := "break-all rounded bg-surface-secondary px-1.5 py-0.5 font-mono text-xs", t.config.command.getOrElse("")),
        span(
          t.runningSince.fold("未在运行")(s => s"运行中，自 ${Ui.formatTime(s)}"),
          " · stderr：",
          code(cls := "font-mono text-xs", Show.stderrLog(t))
        )
      )

  private def sessionLink(id: SessionId, text: String): HtmlElement =
    Ui.appLink(Route.Session(id.value).url)(cls := "text-accent", text)

  /** 详情正文开头的几行，查看与编辑时都显示。 */
  def facts(t: Signal[TriggerView]): Modifier[HtmlElement] = Seq(
    div(
      cls := "text-xs text-muted",
      child.text <-- t.map(_.lastFiredAt.fold("还没有触发过")(l => s"上次触发 ${Ui.formatTime(l)}")),
      child.maybe <-- t.map(_.lastSession.map(s => span(" · ", sessionLink(s, "上次会话"))))
    ),
    children <-- t.map(_.faults.map(f => Ui.errorAlert(f.source.title, f.error)))
  )

  private def precheckSummary(p: PrecheckSpec): HtmlElement =
    div(
      cls := "flex flex-col gap-1 text-xs text-muted",
      div(s"前置检查 · ${p.fireWhen.title}"),
      pre(cls := Ui.CodeBlock, p.command)
    )

  /** 立即运行的结果：开出的会话，或没开的原因。 */
  private def fireResult(r: FireResult): HtmlElement =
    div(
      cls := "text-xs text-muted",
      "立即运行：",
      r.session.fold[Modifier[HtmlElement]](r.note)(s => sessionLink(s, "已触发，打开这次的会话"))
    )

  private def shown(t: Signal[TriggerView], onEdit: () => Unit): HtmlElement =
    val act   = new Action
    val fired = Var(Option.empty[FireResult])
    def remove(x: TriggerView): Unit =
      Confirm.ask(s"删除触发器「${x.name.value}」？", confirmLabel = "删除", danger = true).foreach { ok =>
        if ok then act.run(Http.call(TriggersApi.delete)(x.id)).foreach(_ => ())
      }
    def run(x: TriggerView): Unit =
      act.run(Http.call(TriggersApi.run)(x.id).map { r =>
        fired.set(Some(r))
        r.session.fold(Toast.info(r.note))(_ => Toast.success("已触发"))
      }).foreach(_ => ())
    div(
      cls := "contents",
      Dialog.body(
        cls := "flex flex-col gap-3",
        facts(t),
        child.maybe <-- fired.signal.map(_.map(fireResult)),
        child.maybe <-- t.map(_.precheck.map(precheckSummary)),
        p(cls := "whitespace-pre-wrap text-sm", child.text <-- t.map(_.prompt)),
        act.errorView()
      ),
      Dialog.footer(
        cls := "flex flex-wrap justify-start gap-2",
        Ui.btn(Variant.Secondary, Size.Sm)(Icon("Pencil"), "编辑", onClick --> (_ => onEdit())),
        Ui.btn(Variant.Secondary, Size.Sm, pending = act.busy.signal)(
          Icon("Play"),
          "立即运行",
          disabled <-- act.busy.signal.combineWith(t.map(_.enabled)).map((b, e) => b || !e),
          onClick(_.sample(t)) --> run
        ),
        Ui.btn(Variant.DangerSoft, Size.Sm)(Icon("Trash2"), "删除", disabled <-- act.busy.signal, onClick(_.sample(t)) --> remove)
      )
    )

  /** 已结束的触发器（跑完的一次性触发器）：只能看结果与删除，不能再改、启用或运行。 */
  private def finishedBody(t: Signal[TriggerView]): HtmlElement =
    val act = new Action
    def remove(x: TriggerView): Unit =
      Confirm.ask(s"删除「${x.name.value}」的记录？", body = Some("它发起的会话不会被删除。"), confirmLabel = "删除", danger = true).foreach { ok =>
        if ok then act.run(Http.call(TriggersApi.delete)(x.id)).foreach(_ => ())
      }
    div(
      cls := "contents",
      Dialog.body(
        cls := "flex flex-col gap-3",
        div(
          cls := "text-xs text-muted",
          child <-- t.map(_.lastSession.fold[HtmlElement](span("没有触发会话（前置检查判定没事，或触发失败）"))(s => sessionLink(s, "这次触发的会话")))
        ),
        children <-- t.map(_.faults.map(f => Ui.errorAlert(f.source.title, f.error))),
        p(cls := "whitespace-pre-wrap text-sm", child.text <-- t.map(_.prompt)),
        act.errorView()
      ),
      Dialog.footer(
        cls := "justify-start",
        Ui.btn(Variant.DangerSoft, Size.Sm)(Icon("Trash2"), "删除记录", disabled <-- act.busy.signal, onClick(_.sample(t)) --> remove)
      )
    )

/** 编辑：草稿在打开编辑时从当时的值初始化一次，之后列表刷新不冲掉；保存时只发改过的字段。 */
object Editor:
  /** @param facts 详情正文开头的几行（上次触发、故障），编辑时也显示 */
  def view(base: TriggerView, facts: Modifier[HtmlElement], onDone: () => Unit): HtmlElement =
    val prompt   = Var(base.prompt)
    val config   = Var(ConfigDraft.of(base))
    val precheck = Var(PrecheckDraft.of(base.precheck))
    val act      = new Action
    val patch    = prompt.signal
      .combineWith(config.signal, precheck.signal)
      .map((p, c, pc) => Edit.patch(base, p, c, pc))
    def save(p: Either[String, TriggerPatch]): Unit = p match
      case Right(x) if Edit.isEmpty(x) => onDone()
      case Right(x)                    => act.run(Http.call(TriggersApi.update)((base.id, x))).foreach(ok => if ok then onDone())
      case Left(why)                   => act.error.set(Some(why))
    div(
      cls := "contents",
      Dialog.body(
        cls := "flex flex-col gap-3",
        facts,
        Editors.config(config),
        Editors.precheck(precheck),
        Fields.area("处理说明", prompt, rowsN = 5),
        act.errorView()
      ),
      Dialog.footer(
        cls := "flex flex-wrap justify-start gap-2",
        Ui.btn(size = Size.Sm, pending = act.busy.signal)(
          "保存",
          disabled <-- patch.map(_.isLeft).combineWith(act.busy.signal).map(_ || _),
          onClick(_.sample(patch)) --> save
        ),
        Ui.btn(Variant.Tertiary, Size.Sm)("取消", onClick --> (_ => onDone()))
      )
    )

/** 新建：建好一条就换一份新的表单，各输入回到初始值。 */
object NewForm:
  def view(agents: Signal[List[AgentSummary]]): HtmlElement =
    val generation = Var(0)
    Ui.collapsibleForm("新建触发器")(
      child <-- generation.signal.map(_ => fields(agents, () => generation.update(_ + 1)))
    )

  private def fields(agents: Signal[List[AgentSummary]], onCreated: () => Unit): HtmlElement =
    val config   = Var(ConfigDraft.empty(TriggerKind.Schedule, js.Date.now()))
    val name     = Var("")
    val agent    = Var(Option.empty[AgentId])
    val precheck = Var(PrecheckDraft.off)
    val prompt   = Var("")
    val act      = new Action
    val request = name.signal
      .combineWith(agent.signal, config.signal, precheck.signal, prompt.signal)
      .map((n, a, c, pc, p) => NewDraft(n, a, c, pc, p).toRequest)
    def create(r: Either[String, NewTrigger]): Unit = r match
      case Right(req) =>
        act.run(Http.call(TriggersApi.create)(req)).foreach(ok =>
          if ok then
            Toast.success("触发器已创建")
            onCreated()
        )
      case Left(why) => act.error.set(Some(why))
    div(
      cls := "flex flex-col gap-4",
      Fields.select[TriggerKind](
        "触发方式",
        config.signal.map(c => Some(c.kind)),
        k => config.set(ConfigDraft.empty(k, js.Date.now())),
        Val(TriggerKind.values.toList.map(k => Opt(k, k.title, Some(k.summary))))
      ),
      Fields.text("名称", name, hint = Some("小写字母、数字和 -，如 daily-report"), mono = true),
      Fields.select[AgentId](
        "Agent",
        agent.signal,
        a => agent.set(Some(a)),
        agents.combineWith(agent.signal).map((as, keep) => AgentsData.selectable(as, keep).map(a => Opt(a.id, a.name.value))),
        placeholderText = "选择 agent"
      ),
      child <-- config.signal.map(_.kind).distinct.map(_ => Editors.config(config)),
      Editors.precheck(precheck),
      Fields.area(
        "处理说明",
        prompt,
        hint = Some("每次触发新开这个 agent 的一个会话；事件内容（如流式命令输出的行）附在其后。触发的会话不会自动推送，需要时在说明里要求 agent 调用 notify_user"),
        rowsN = 5
      ),
      act.errorView(),
      div(
        Ui.btn(pending = act.busy.signal)(
          "创建",
          disabled <-- request.map(_.isLeft).combineWith(act.busy.signal).map(_ || _),
          onClick(_.sample(request)) --> create
        )
      )
    )

/** 已结束的触发器：展开时才取，每次展开都是最新的。 */
object Finished:
  def section(): HtmlElement =
    Ui.card(
      Ui.disclosure(Var(false), "flex w-full items-center gap-2 px-1 font-medium", "flex flex-col gap-3 pt-4")(Icon("Archive"), "已结束") {
        val finished = Remote("triggers")(Http.call(TriggersApi.list)(Some(true)))
        val openId   = Var(Option.empty[TriggerId])
        Ui.loading(finished) { list =>
          Seq(
            child.maybe <-- list.map(l => Option.when(l.isEmpty)(Ui.emptyHint("还没有跑完的一次性触发器"))),
            Ui.cardGrid(children <-- list.splitSeq(_.id)(t => Card.view(t, showAgent = true, () => openId.set(Some(t.now().id))))),
            Detail.modal(openId, list)
          )
        }
      }
    )
