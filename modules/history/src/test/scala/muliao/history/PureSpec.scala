package muliao.history

import muliao.board.IssueActivity
import muliao.contract.*
import muliao.sessions.*
import zio.test.*

import java.time.{LocalDate, ZoneId}

/** 纯函数：检索相关度、分页、读会话的条目与筛选、活动回顾的写法。 */
object PureSpec extends ZIOSpecDefault:

  private val bob   = AgentId.wrap("b0b")
  private val carol = AgentId.wrap("ca7")
  private val zone  = ZoneId.of("Asia/Shanghai")

  private def entry(id: Long, title: String, summary: String, refs: List[String] = Nil, imp: Importance = Importance.Routine, day: String = "2026-11-10") =
    HistoryEntry(id, bob, LocalDate.parse(day), title, summary, refs, imp, None, 0L)

  /** 2026-11-15 的某个时刻（上海时区）。 */
  private def at(hh: Int, mm: Int): Long = LocalDate.parse("2026-11-15").atTime(hh, mm).atZone(zone).toInstant.toEpochMilli

  def spec = suite("纯函数")(
    test("检索：每个词都要出现（中文按子串），标题、引用里的比摘要里的靠前，同分按重要程度、日期") {
      val es = List(
        entry(1, "整理磁盘", "清理了部署备份和日志"),
        entry(2, "部署新版本", "部署后自检通过", imp = Importance.Notable),
        entry(3, "修复投递", "看板投递 bug", refs = List("#871")),
        entry(4, "部署回滚", "回滚到上一版", day = "2026-11-12"),
        entry(5, "Deploy 脚本", "改 deploy.sh")
      )
      assertTrue(
        Search.rank(es, List("部署"), 10).map(_.id) == List(2, 4, 1),
        Search.rank(es, List("部署", "自检"), 10).map(_.id) == List(2),
        Search.rank(es, List("#871"), 10).map(_.id) == List(3),
        Search.rank(es, List("deploy"), 10).map(_.id) == List(5),
        Search.rank(es, List("部署"), 1).map(_.id) == List(2),
        Search.rank(es, List("不存在"), 10).isEmpty,
        HistoryQuery.words(Some("  部署  Deploy 部署 ")) == List("部署", "deploy")
      )
    },
    test("记录：标题、摘要不空，日期不在今天之后，重要程度 1–3") {
      val today = LocalDate.parse("2026-11-15")
      assertTrue(
        NewEntry.make(today, " 做了 ", " 摘要 ", List(" #1 ", ""), Importance.Major, today).map(e => (e.title, e.summary, e.refs)) == Right(("做了", "摘要", List("#1"))),
        NewEntry.make(today.plusDays(1), "做了", "摘要", Nil, Importance.Routine, today).isLeft,
        NewEntry.make(today, " ", "摘要", Nil, Importance.Routine, today).isLeft,
        NewEntry.make(today, "做了", "", Nil, Importance.Routine, today).isLeft,
        Importance.of(0).isLeft,
        Importance.of(3) == Right(Importance.Major)
      )
    },
    test("分页：按预算截住并说明从哪继续，至少给一项，offset 越界报错") {
      val items = (1 to 5).map(i => s"第$i 行" + "x" * 10).toList
      val all   = Pages.slice(items, 1, "行")
      val cut   = Pages.slice(items, 2, "行", budget = 30)
      val big   = Pages.slice(List("y" * 100, "z"), 1, "行", budget = 10)
      assertTrue(
        all == Right(items.mkString("\n")),
        cut == Right(s"${items(1)}\n${items(2)}\n[到了输出上限，停在第 3 行（共 5 行）；从 offset=4 继续]"),
        big.map(_.linesIterator.toList.head) == Right("y" * 100),
        big.exists(_.contains("从 offset=2 继续")),
        Pages.slice(Nil, 1, "条") == Right(""),
        Pages.slice(items, 6, "行").isLeft,
        Pages.slice(items, 0, "行").isLeft
      )
    },
    test("读会话：来信按来源打标签，回答正文、调用与结果、通知各成一条，思考不读出；按标签、正则、时刻筛选；只截可截的") {
      val call = CallView(CallId.wrap("c1"), "run_shell", """{"command":"ls"}""", Display.Terminal(Some("ls")), CallState.Done, Some(ResultView("a\nb", Nil, isError = false)), None, Some(at(9, 1)), Some(at(9, 2)))
      val bad  = CallView(CallId.wrap("c2"), "read_file", "{}", Display.Code(None), CallState.Done, Some(ResultView("没有这个文件", List(ImageRef("image/png", "s")), isError = true)), None, Some(at(9, 3)), Some(at(9, 4)))
      val tl = List(
        TimelineItem(1, Some(1), at(9, 0), at(9, 0), EntryView.Context("当前时间 ...")),
        TimelineItem(2, Some(1), at(9, 0), at(9, 0), EntryView.Letter(SourceView.User, "帮我看看目录" * 300, Nil, checkIn = false)),
        TimelineItem(3, Some(1), at(9, 1), at(9, 1), EntryView.Reply(List(PartView.Text("好的"), PartView.Call(call), PartView.Call(bad)), "m", UsageView(0, 0, 0, 0), interrupted = false)),
        TimelineItem(4, Some(2), at(10, 0), at(10, 0), EntryView.Letter(SourceView.Trigger(TriggerName.wrap("daily")), "每日检查", Nil, checkIn = false)),
        TimelineItem(5, Some(2), at(10, 1), at(10, 1), EntryView.Notice(NoticeKind.Failed, "模型不可用"))
      )
      val xs = Reading.items(tl)
      def pick(f: ReadFilter) = xs.filter(f.accepts).map(_.label)
      val r = scala.util.matching.Regex("(?i)RUN_SHELL")
      assertTrue(
        xs.map(_.label) == List("CONTEXT", "USER", "ASSISTANT", "call run_shell", "result run_shell", "call read_file", "error read_file", "TRIGGER", "NOTICE Failed"),
        xs.find(_.label == "error read_file").exists(_.text.endsWith("[图片 1 张]")),
        pick(ReadFilter(labels = List("call"))) == List("call run_shell", "call read_file"),
        pick(ReadFilter(labels = List("USER", "NOTICE"))) == List("USER", "NOTICE Failed"),
        pick(ReadFilter(grep = Some(r))) == List("call run_shell", "result run_shell"),
        pick(ReadFilter(labels = List("call"), grep = Some(r))) == List("call run_shell"),
        pick(ReadFilter(since = Some(at(10, 0)))) == List("TRIGGER", "NOTICE Failed"),
        Reading.render(2, xs(1), 10, zone).startsWith("[2] 2026-11-15 09:00:00 USER\n帮我看看目录帮我"),
        !Reading.render(2, xs(1), 10, zone).contains("截断"),
        Reading.render(1, xs.head.copy(text = "x" * 50), 10, zone).endsWith("x" * 10 + "\n[截断：共 50 字，clip=0 看全文]"),
        Reading.render(1, xs.head.copy(text = "x" * 50), 0, zone).endsWith("x" * 50),
        Reading.render(8, xs(7), 10, zone).startsWith("[8] 2026-11-15 10:00:00 TRIGGER（daily）\n每日检查")
      )
    },
    test("活动回顾的写法：分节、各节计数、正文缩进；看全部 agent 时每项带 agent 名") {
      val sid = SessionId.wrap("0123456789abcdef")
      val o = DayOverview(
        agent = None,
        day = LocalDate.parse("2026-11-15"),
        zone = zone,
        sessions = SessionActivity(
          List(ActiveSession(sid, bob, Origin.Issue(IssueId.wrap(7)), "#7 修 bug", WorkState.Idle, 4, at(9, 0), at(9, 30), 2, 1)),
          List(UserSaid(sid, bob, at(9, 0), "第一行\n第二行")),
          List(RunFailure(sid, bob, RunId.wrap(3), at(9, 30), "模型不可用"))
        ),
        issues = List(IssueActivity(IssueId.wrap(7), "修 bug", at(9, 10), "carol", None, "评论", Some("看过了"))),
        history = List(entry(9, "修好了", "改了投递", refs = List("#7"), imp = Importance.Notable))
      )
      val names = Map(bob -> "bob", carol -> "carol")
      val text  = OverviewText(names, zone).lines(o).mkString("\n")
      val one   = OverviewText(names, zone).lines(o.copy(agent = Some(bob), sessions = SessionActivity(Nil, Nil, Nil))).mkString("\n")
      assertTrue(
        text.startsWith("# 2026-11-15 全部 agent 的活动（时区 Asia/Shanghai）"),
        text.contains("## 活跃的会话（1）\n- 01234567 bob [issue #7] #7 修 bug | idle | 4 条消息 09:00–09:30 | 运行 2 次，失败 1 次"),
        text.contains("## 用户说的话（1）\n- [09:00] 01234567 bob 说：\n  第一行\n  第二行"),
        text.contains("## 对 issue 的操作（1）\n- [09:10] #7 修 bug：carol 评论：\n  看过了"),
        text.contains("## 失败的运行（1）\n- [09:30] 01234567 bob 运行 3：\n  模型不可用"),
        text.contains("## 当天的工作历史（1）\n- bob [2] 修好了（#7）\n  改了投递"),
        one.startsWith("# 2026-11-15 bob 的活动"),
        one.contains("## 活跃的会话（0）\n（无）"),
        one.contains("- [2] 修好了（#7）")
      )
    }
  )
