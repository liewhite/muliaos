package muliao.board

import muliao.contract.*
import muliao.platform.AppError
import muliao.resources.agents.Agents
import muliao.sessions.RunContext
import muliao.toolkit.*
import zio.*
import zio.json.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{caseName, description, fieldName}

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

/** 本领域给 agent 的工具：issue_list、issue_show、issue_create、issue_update、issue_comment、issue_stop。 */
object BoardTools:

  // ---- 参数里的枚举 ----

  /** 叫停后能进的状态。 */
  enum HaltArg:
    @caseName("backlog") case Backlog
    @caseName("blocked") case Blocked
    @caseName("cancelled") case Cancelled
    @caseName("in_progress") case InProgress

    def value: IssueStatus = IssueStatus.valueOf(toString)
  given Schema[HaltArg] = DeriveSchema.gen[HaltArg]

  // ---- 参数 ----

  final case class ListIn(
      @description("只看指派给这个 agent（名字）或 `user` 的") assignee: Option[String] = None,
      @description("只看这个 agent（名字）负责的：它项目的全部历史") owner: Option[String] = None,
      @description("只看这些状态；不给就是全部") statuses: List[IssueStatus] = Nil
  )
  given Schema[ListIn] = DeriveSchema.gen[ListIn]

  final case class IssueRef(@fieldName("issue_id") @description("issue 号") issueId: Long)
  given Schema[IssueRef] = DeriveSchema.gen[IssueRef]

  final case class ItemIn(
      @description("这一项在本批里的名字，别的项在 depends_on_keys 里引用它，如 `api`") key: Option[String] = None,
      @description("一句话标题") title: String,
      @description("负责项目的 agent（名字），通常是你自己；改指派也不变") owner: String,
      @description("Markdown 详情：背景、做成什么样算完") description: String = "",
      @description("初始状态：马上开始的用 in_progress，以后做的用 backlog") status: IssueStatus = IssueStatus.Backlog,
      @description("优先级") priority: Priority = Priority.Unset,
      @description("agent 名、`user`，或不给（不指派）") assignee: Option[String] = None,
      @fieldName("depends_on") @description("等这些已有 issue 结束（done 或 cancelled）才投递") dependsOn: List[Long] = Nil,
      @fieldName("depends_on_keys") @description("等本批里这些键的项结束才投递") dependsOnKeys: List[String] = Nil
  )
  given Schema[ItemIn] = DeriveSchema.gen[ItemIn]

  final case class CreateIn(
      @description("一句话标题（用 issues 时不给）") title: Option[String] = None,
      @description("负责项目的 agent（名字），通常是你自己（用 issues 时不给）") owner: Option[String] = None,
      @description("Markdown 详情：背景、做成什么样算完") description: String = "",
      @description("初始状态：马上开始的用 in_progress，以后做的用 backlog") status: IssueStatus = IssueStatus.Backlog,
      @description("优先级") priority: Priority = Priority.Unset,
      @description("agent 名、`user`，或不给（不指派）") assignee: Option[String] = None,
      @fieldName("depends_on") @description("等这些 issue 结束（done 或 cancelled）才投递；不能成环") dependsOn: List[Long] = Nil,
      @description("一次建一批互相依赖的（给了它就不给上面的单项字段）：每项可带 key，别的项用 depends_on_keys 引用；任一项不合法整批不建，建完才投递")
      issues: List[ItemIn] = Nil
  )
  given Schema[CreateIn] = DeriveSchema.gen[CreateIn]

  final case class UpdateIn(
      @fieldName("issue_id") @description("issue 号") issueId: Long,
      @description("新标题") title: Option[String] = None,
      @description("新描述（整段替换）") description: Option[String] = None,
      @description("新状态：做完设 in_review（有别的 issue 依赖它时设 done，下游才开始）；只有用户能处理时设 blocked 并指派 user") status: Option[IssueStatus] = None,
      @description("新优先级") priority: Option[Priority] = None,
      @description("新指派：agent 名或 `user`（交给用户会通知他）") assignee: Option[String] = None,
      @description("true：去掉指派") unassign: Boolean = false,
      @description("新 owner（agent 名）") owner: Option[String] = None,
      @fieldName("depends_on") @description("完整的新依赖列表（替换旧的，[] 去掉全部）；不能成环") dependsOn: Option[List[Long]] = None
  )
  given Schema[UpdateIn] = DeriveSchema.gen[UpdateIn]

  final case class CommentIn(
      @fieldName("issue_id") @description("issue 号") issueId: Long,
      @description("评论正文（Markdown）：进展、发现、结果、给用户的问题") body: String
  )
  given Schema[CommentIn] = DeriveSchema.gen[CommentIn]

  final case class StopIn(
      @fieldName("issue_id") @description("issue 号") issueId: Long,
      @description("叫停后的状态：backlog、blocked、cancelled，或 in_progress（改方向，要给 message）") status: HaltArg,
      @description("记成你的评论并投给被叫停的会话，它会再跑一次照此收尾或改做别的") message: Option[String] = None
  )
  given Schema[StopIn] = DeriveSchema.gen[StopIn]

  // ---- 输出 ----

  final case class Line(
      id: Long,
      title: String,
      owner: Option[String],
      status: String,
      priority: String,
      assignee: Option[String],
      @jsonField("assignee_state") assigneeState: Option[WorkState],
      stalled: Boolean,
      @jsonField("depends_on") dependsOn: List[Long],
      @jsonField("waiting_on") waitingOn: List[Long],
      @jsonField("updated_at") updatedAt: String
  ) derives JsonEncoder

  val IssueListDef = ToolDef[RunContext, ListIn](
    "issue_list",
    "列出看板上的 issue，最近更新的在前。每项带 owner、status、priority、assignee，assignee_state（指派的 agent 处理它的会话：running / waiting / idle，没有为 null）、stalled（in_progress 指派给 agent、不等上游，但它的会话空闲或还没有：没人在处理）、depends_on 与 waiting_on（还没结束的上游）。",
    Access.Read,
    display = _ => Display.JsonData
  )

  val IssueShowDef = ToolDef[RunContext, IssueRef](
    "issue_show",
    "看一个 issue 的全部：描述、依赖（在等哪些）、被谁依赖（blocks）、处理它的会话、完整时间线（评论与每个改动）。",
    Access.Read,
    display = _ => Display.Markdown
  )

  val IssueCreateDef = ToolDef[RunContext, CreateIn](
    "issue_create",
    """建 issue。指派给 agent 且 in_progress 的，建好就投进那个 agent 专门处理它的会话（每个 issue + agent 一个），之后每条新动态都投到那里；指派给你自己也一样：在这里建，在那个会话里做。
      |要按顺序做就给 depends_on：全部上游结束（done 或 cancelled）才投递，所以一条链可以一次建好、都设 in_progress，上一步做完下一步自动开始。
      |一次建几个互相依赖的用 issues（每项给 key，别的项在 depends_on_keys 里引用）；拆大活时把步骤和一个依赖全部步骤的父 issue（写总目标）一起建。""".stripMargin,
    Access.Write,
    Scheduling.Ordered
  )

  val IssueUpdateDef = ToolDef[RunContext, UpdateIn](
    "issue_update",
    """改 issue，只改给出的字段。改动投给正在处理它的 agent（你在它自己的 issue 会话里改的不投回给你）；指派给用户会通知他。
      |做完：没有别的 issue 依赖它就设 in_review、指派 user，由用户审核后设 done；有下游依赖它（issue_show 的「被依赖」）就自己设 done，下游才开始。
      |只有用户能处理的设 blocked、指派 user，并评论说明要他做什么。改状态不会停下正在跑的会话，要叫停用 issue_stop。""".stripMargin,
    Access.Write,
    Scheduling.Ordered
  )

  val IssueCommentDef = ToolDef[RunContext, CommentIn](
    "issue_comment",
    "在 issue 上评论（Markdown）：进展、发现、结果、给用户的问题。用户看的是 issue，不是你的会话，结论要记在这里。评论投给正在处理它的 agent；没人处理时，用户的评论投给 owner。",
    Access.Write,
    Scheduling.Ordered
  )

  val IssueStopDef = ToolDef[RunContext, StopIn](
    "issue_stop",
    "叫停指派的 agent 正在处理某 issue 的会话（像用户点停止：当前运行结束、排队的消息丢弃、它开的 thread 一并停下），再把 issue 改成 status。message 记成你的评论并投给那个会话，它会再跑一次照此收尾或改方向。用来叫停已不需要的工作；只改状态不会停下正在跑的会话。不能在这个 issue 自己的会话里用（那里用 issue_update）。",
    Access.Write,
    Scheduling.Ordered
  )

  /** 本领域工具的静态定义（进全部内置工具目录）。 */
  val specs: List[ToolSpec] = List(IssueListDef, IssueShowDef, IssueCreateDef, IssueUpdateDef, IssueCommentDef, IssueStopDef).map(_.spec)

  def all(board: Board, agents: Agents, zone: ZoneId): List[Tool[RunContext, Nothing]] =
    val iso  = DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(zone)
    val time = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(zone)

    def by(ctx: RunContext): By =
      By(
        Actor.Agent(ctx.agent, Some(ctx.session)),
        ctx.origin match
          case Origin.Issue(i) => Some(i)
          case _               => None
      )

    def agentId(name: String): IO[AppError, AgentId] = agents.resolve(name).map(_.id)

    def assignee(name: String): IO[AppError, Assignee] =
      if name == Assignee.UserKey then ZIO.succeed(Assignee.User) else agentId(name).map(Assignee.Agent(_))

    def issueId(n: Long): IO[AppError, IssueId] = ZIO.fromEither(IssueId.make(n)).mapError(e => AppError.invalid(s"issue 号 $n：$e"))

    def run[A](effect: IO[AppError, A])(out: A => String): IO[ToolFailure, ToolOutcome[Nothing]] =
      effect.mapBoth(ToolFailure.fromAppError, a => ToolOutcome.text(out(a)))

    val issueList = IssueListDef { (in, _) =>
      run(
        for
          a  <- ZIO.foreach(in.assignee)(assignee)
          o  <- ZIO.foreach(in.owner)(agentId)
          xs <- board.list(Filter(in.statuses.toSet, a, o))
        yield xs
      )(xs =>
        xs.map(s =>
          Line(s.id.value, s.title, s.ownerName, s.status.label, s.priority.label, s.assigneeName, s.assigneeState, s.stalled,
            s.dependsOn.map(_.value), s.waitingOn.map(_.value), iso.format(Instant.ofEpochMilli(s.updatedAt)))
        ).toJsonPretty
      )
    }

    def links(ls: List[IssueLink]): String = if ls.isEmpty then "无" else ls.map(l => s"#${l.id.value} (${l.status.label})").mkString(", ")

    def show(d: IssueDetail): String =
      val s       = d.summary
      val waiting = if s.waitingOn.isEmpty then "无" else s.waitingOn.map(w => s"#${w.value}").mkString(" ")
      val sessions =
        if d.sessions.isEmpty then "" else "\n处理它的会话：" + d.sessions.map(v => s"${v.agentName}（${v.state}，${v.session.value}）").mkString("，")
      val events = d.events.map { e =>
        val head = s"- [${time.format(Instant.ofEpochMilli(e.at))}] ${e.actor} ${e.text}"
        e.comment.fold(head)(c => head + "：\n" + c.linesIterator.map("  " + _).mkString("\n"))
      }
      s"""# Issue #${s.id.value}：${s.title}
         |owner：${s.ownerName.getOrElse("无")} | 状态：${s.status.label} | 优先级：${s.priority.label} | 指派：${s.assigneeName.getOrElse("无")}${s.assigneeState.fold("")(st => s"（${st.label}）")}${if s.stalled then " | 没人在处理" else ""} | 建立者：${d.createdBy}
         |依赖：${links(d.dependsOn)} | 在等：$waiting | 被依赖：${links(d.blocks)}$sessions
         |
         |${if d.description.trim.isEmpty then "（没有描述）" else d.description}
         |
         |## 动态
         |${events.mkString("\n")}""".stripMargin

    val issueShow = IssueShowDef { (in, _) => run(issueId(in.issueId).flatMap(board.show))(show) }

    def draft(i: ItemIn): IO[AppError, Draft] =
      for
        o  <- agentId(i.owner)
        a  <- ZIO.foreach(i.assignee)(assignee)
        up <- ZIO.foreach(i.dependsOn)(issueId)
      yield Draft(i.key, i.title, o, i.description, i.status, i.priority, a, up.map(Upstream.Existing(_)) ++ i.dependsOnKeys.map(Upstream.Key(_)))

    val issueCreate = IssueCreateDef { (in, ctx) =>
      val single = in.title.isDefined || in.owner.isDefined
      val drafts: IO[AppError, List[Draft]] = (single, in.issues) match
        case (true, _ :: _) => ZIO.fail(AppError.invalid("给了 issues 就不要再给单项的 title、owner"))
        case (false, Nil)   => ZIO.fail(AppError.invalid("要么给 title 与 owner 建一个，要么给 issues 建一批"))
        case (false, items) => ZIO.foreach(items)(draft)
        case (true, Nil) =>
          for
            t <- ZIO.fromOption(in.title).orElseFail(AppError.invalid("缺 title"))
            o <- ZIO.fromOption(in.owner).orElseFail(AppError.invalid("缺 owner：负责项目的 agent，通常是你自己"))
            d <- draft(ItemIn(None, t, o, in.description, in.status, in.priority, in.assignee, in.dependsOn))
          yield List(d)
      run(drafts.flatMap(ds => board.create(by(ctx), ds).map(ds.zip(_))))(created =>
        "已建：" + created.map((d, id) => s"#${id.value}（${d.status.label}）${d.title}").mkString("；")
      )
    }

    val issueUpdate = IssueUpdateDef { (in, ctx) =>
      val patch =
        for
          id <- issueId(in.issueId)
          _  <- ZIO.fail(AppError.invalid("assignee 与 unassign 只能给一个")).when(in.assignee.isDefined && in.unassign)
          a  <- ZIO.foreach(in.assignee)(assignee)
          o  <- ZIO.foreach(in.owner)(agentId)
          up <- ZIO.foreach(in.dependsOn)(ZIO.foreach(_)(issueId))
        yield id -> Patch(in.title, in.description, in.status, in.priority, o,
          if in.unassign then Some(None) else a.map(Some(_)), up)
      run(patch.flatMap((id, p) => board.update(by(ctx), id, p).as(id)))(id => s"已更新 #${id.value}")
    }

    val issueComment = IssueCommentDef { (in, ctx) => run(issueId(in.issueId).flatMap(id => board.comment(by(ctx), id, in.body).as(id)))(id => s"已评论 #${id.value}") }

    val issueStop = IssueStopDef { (in, ctx) =>
      run(
        for
          id <- issueId(in.issueId)
          h  <- ZIO.fromEither(Halt.make(in.status.value, in.message)).mapError(AppError.invalid)
          _  <- board.stop(by(ctx), id, h)
        yield id
      )(id => s"已叫停 #${id.value}")
    }

    List(issueList, issueShow, issueCreate, issueUpdate, issueComment, issueStop)
