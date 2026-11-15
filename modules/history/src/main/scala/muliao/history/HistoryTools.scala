package muliao.history

import muliao.contract.AgentId
import muliao.platform.AppError
import muliao.resources.agents.Agents
import muliao.sessions.{RunContext, SessionInfo}
import muliao.toolkit.*
import zio.*
import zio.json.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{description, fieldName}

import java.time.{LocalDate, LocalDateTime}
import java.util.regex.PatternSyntaxException
import scala.util.matching.Regex

/** 本领域给 agent 的工具：history_append、history_search、activity_overview、session_read。 */
object HistoryTools:

  /** 工具参数里「全部 agent」的写法。 */
  val AllAgents = "all"

  // ---- 参数 ----

  final case class EntryIn(
      @description("做这件事的那天（YYYY-MM-DD，配置时区的日历日），不是记录的日子") day: String,
      @description("一行标题") title: String,
      @description("2–5 行：做了什么、结果如何、以后要知道什么") summary: String,
      @description("1 日常、2 值得注意、3 重大（决定、事故、里程碑）") importance: Int = 1,
      @description("引用：issue 号（如 `#155`）、会话编号、提交哈希") refs: List[String] = Nil
  )
  given Schema[EntryIn] = DeriveSchema.gen[EntryIn]

  final case class AppendIn(@description("要记的事，一件一条") entries: List[EntryIn])
  given Schema[AppendIn] = DeriveSchema.gen[AppendIn]

  final case class SearchIn(
      @description("关键字，空白分开：每个都要出现在标题、摘要或引用里，按相关度排（中文按字面子串匹配）。不给就按日期倒序列出") query: Option[String] = None,
      @description("agent 名，`all` 是全部；不给是你自己") agent: Option[String] = None,
      @description("起始日期（含），YYYY-MM-DD") since: Option[String] = None,
      @description("截止日期（含），YYYY-MM-DD") until: Option[String] = None,
      @fieldName("min_importance") @description("只要至少这么重要的（1–3）") minImportance: Int = 1,
      @description("最多几条（1–200）") limit: Int = 20
  )
  given Schema[SearchIn] = DeriveSchema.gen[SearchIn]

  final case class OverviewIn(
      @description("agent 名，`all` 是全部；不给是你自己") agent: Option[String] = None,
      @description("哪天（YYYY-MM-DD，配置时区）；不给是今天") day: Option[String] = None,
      @description("从第几行起（1 起），按上次输出最后一行的说明给") offset: Int = 1
  )
  given Schema[OverviewIn] = DeriveSchema.gen[OverviewIn]

  final case class ReadIn(
      @description("会话编号或它的唯一前缀（8 位通常够）") session: String,
      @description("只要这些标签的条目（区分大小写，`call` 也匹配 `call run_shell`）：USER、TRIGGER、ISSUE、DELEGATION、EVENT、ASSISTANT、call、result、error、CONTEXT、COMPACTION、NOTICE")
      labels: List[String] = Nil,
      @description("只要标签或正文匹配这个正则的（不分大小写）；与 labels 同时给时两者都要满足") grep: Option[String] = None,
      @description("只要这个时刻之后的（配置时区，如 `2026-11-04` 或 `2026-11-04T09:30`）") since: Option[String] = None,
      @description("工具调用、结果、事件、上下文的正文截到这么多字；0 不截。用户与回答的正文总是全文") clip: Int = 2000,
      @description("从筛选后的第几条起（1 起），按上次输出最后一行的说明给") offset: Int = 1
  )
  given Schema[ReadIn] = DeriveSchema.gen[ReadIn]

  // ---- 输出 ----

  final case class Line(
      id: Long,
      day: String,
      agent: String,
      title: String,
      summary: String,
      refs: List[String],
      importance: Int
  ) derives JsonEncoder

  val HistoryAppendDef = ToolDef[RunContext, AppendIn](
    "history_append",
    """往你自己的工作历史追加记录，一件事一条（日期、标题、2–5 行摘要、引用、重要程度）。记下后不能改、不能删，所以先用 history_search（since、until 都设成那天）确认那天还没记过。
      |一批里任一条不合法（日期格式不对或在今天之后、标题或摘要为空、重要程度不在 1–3）整批不记。""".stripMargin,
    Access.Write,
    Scheduling.Ordered
  )

  val HistorySearchDef = ToolDef[RunContext, SearchIn](
    "history_search",
    """检索工作历史（做过什么、什么时候、关联哪些 issue 或提交），结果是 JSON 数组，每项有 id、day、agent、title、summary、refs、importance。
      |给 query 时每个词都要出现在标题、摘要或引用里，相关的在前；不给 query 就按日期倒序列出（适合列某段日期的全部）。
      |只记到 agent 自己写下的为止，通常到昨天；今天的事用 activity_overview。都查不到时答「未核实」，不答「没有」。""".stripMargin,
    Access.Read,
    display = _ => Display.JsonData
  )

  val ActivityOverviewDef = ToolDef[RunContext, OverviewIn](
    "activity_overview",
    """某天（配置时区）的活动汇总，文字：活跃的会话（编号前 8 位、来源、标题、状态、当天消息数与时间跨度、运行与失败次数）、用户当天亲手发的每条消息（全文）、
      |对 issue 的操作（该 agent 做的，加上用户在它负责或被指派的 issue 上做的，评论全文）、失败的运行及错误、当天的工作历史。
      |回顾今天或最近的工作用它（history_search 一般只到昨天），要细看某个会话再用 session_read。输出太长时最后一行说明从哪个 offset 继续。""".stripMargin,
    Access.Read,
    display = _ => Display.Markdown
  )

  val SessionReadDef = ToolDef[RunContext, ReadIn](
    "session_read",
    """按时间读一个会话（你的或别的 agent 的）的时间线：来信（USER 是用户在界面上亲手发的，TRIGGER / ISSUE / DELEGATION 是开会话的一方发的，EVENT 是平台事件）、
      |回答正文（ASSISTANT）、工具调用与结果（call / result / error）、运行上下文、压缩摘要、失败与停止。用户和回答的正文给全文，其余按 clip 截断。
      |可按标签、正则、时刻筛选；输出太长时最后一行说明从哪个 offset 继续。会话从 activity_overview 或 issue_show 里找。""".stripMargin,
    Access.Read
  )

  /** 本领域工具的静态定义（进全部内置工具目录）。 */
  val specs: List[ToolSpec] = List(HistoryAppendDef, HistorySearchDef, ActivityOverviewDef, SessionReadDef).map(_.spec)

  def all(history: History, agents: Agents): List[Tool[RunContext, Nothing]] =

    def run[A](effect: IO[AppError, A])(out: A => String): IO[ToolFailure, ToolOutcome[Nothing]] =
      effect.mapBoth(ToolFailure.fromAppError, a => ToolOutcome.text(out(a)))

    def day(field: String, s: String): IO[AppError, LocalDate] =
      ZIO.attempt(LocalDate.parse(s.trim)).orElseFail(AppError.invalid(s"$field「$s」不是 YYYY-MM-DD 的日期"))

    /** 不给是自己，`all` 是全部，其余按名字找。 */
    def whose(arg: Option[String], ctx: RunContext): IO[AppError, Option[AgentId]] =
      arg.map(_.trim).filter(_.nonEmpty) match
        case None            => ZIO.some(ctx.agent)
        case Some(AllAgents) => ZIO.none
        case Some(name)      => agents.resolve(name).map(a => Some(a.id))

    def importance(field: String, n: Int): IO[AppError, Importance] =
      ZIO.fromEither(Importance.of(n)).mapError(e => AppError.invalid(s"$field：$e"))

    val append = HistoryAppendDef { (in, ctx) =>
      run(
        for
          today <- history.today
          es <- ZIO.foreach(in.entries.zipWithIndex) { (e, i) =>
                  for
                    d   <- day(s"第 ${i + 1} 条的 day", e.day)
                    imp <- importance(s"第 ${i + 1} 条的 importance", e.importance)
                    ne  <- ZIO.fromEither(NewEntry.make(d, e.title, e.summary, e.refs, imp, today)).mapError(m => AppError.invalid(s"第 ${i + 1} 条：$m"))
                  yield ne
                }
          ids <- history.append(ctx.agent, Some(ctx.session), es)
        yield es.zip(ids)
      )(xs => s"已记 ${xs.size} 条：" + xs.map((e, id) => s"$id ${e.day} ${e.title}").mkString("；"))
    }

    val search = HistorySearchDef { (in, ctx) =>
      run(
        for
          a     <- whose(in.agent, ctx)
          since <- ZIO.foreach(in.since)(day("since", _))
          until <- ZIO.foreach(in.until)(day("until", _))
          imp   <- importance("min_importance", in.minImportance)
          _     <- ZIO.fail(AppError.invalid("limit 要在 1–200 之间")).when(in.limit < 1 || in.limit > 200)
          found <- history.search(HistoryQuery(a, HistoryQuery.words(in.query), since, until, imp, in.limit))
          ns    <- history.names(found.map(_.agent))
        yield found.map(e => Line(e.id, e.day.toString, ns(e.agent), e.title, e.summary, e.refs, e.importance.level))
      )(_.toJsonPretty)
    }

    val overview = ActivityOverviewDef { (in, ctx) =>
      run(
        for
          a     <- whose(in.agent, ctx)
          d     <- in.day.fold(history.today)(day("day", _))
          o     <- history.overview(a, d)
          ns    <- history.names(a.toList ++ o.sessions.sessions.map(_.agent) ++ o.sessions.said.map(_.agent) ++ o.sessions.failures.map(_.agent) ++ o.history.map(_.agent))
          lines  = OverviewText(ns, history.zone).lines(o)
          page  <- ZIO.fromEither(Pages.slice(lines, in.offset, "行")).mapError(AppError.invalid)
        yield page
      )(identity)
    }

    def since(s: String): IO[AppError, Long] =
      val t = s.trim
      ZIO
        .attempt(LocalDateTime.parse(t))
        .orElse(ZIO.attempt(LocalDate.parse(t).atStartOfDay))
        .mapBoth(_ => AppError.invalid(s"since「$s」不是日期或日期时间（如 2026-11-04 或 2026-11-04T09:30）"), _.atZone(history.zone).toInstant.toEpochMilli)

    def regex(s: String): IO[AppError, Regex] =
      ZIO.attempt(Regex("(?iu)" + s)).tap(r => ZIO.attempt(r.pattern.matcher(""))).refineOrDie { case e: PatternSyntaxException =>
        AppError.invalid(s"grep 不是合法的正则：${e.getDescription}")
      }

    def header(i: SessionInfo, agent: String): String = s"# 会话 ${i.id.value}（$agent，${OverviewText.origin(i.origin)}）：${i.title}"

    val read = SessionReadDef { (in, _) =>
      run(
        for
          _      <- ZIO.fail(AppError.invalid("clip 不能是负数")).when(in.clip < 0)
          g      <- ZIO.foreach(in.grep.filter(_.nonEmpty))(regex)
          s      <- ZIO.foreach(in.since.filter(_.trim.nonEmpty))(since)
          read   <- history.read(in.session)
          i       = read.info
          xs      = read.items
          ns     <- history.names(List(i.agent))
          picked  = xs.filter(ReadFilter(in.labels, g, s).accepts)
          blocks  = picked.zipWithIndex.map((x, n) => Reading.render(n + 1, x, in.clip, history.zone))
          page   <- ZIO.fromEither(Pages.slice(blocks, in.offset, "条")).mapError(AppError.invalid)
        yield
          val body = if picked.isEmpty then s"（没有符合条件的条目；全部 ${xs.size} 条）" else page
          header(i, ns(i.agent)) + "\n\n" + body
      )(identity)
    }

    List(append, search, overview, read)
