package muliao.history

import muliao.board.*
import muliao.contract.*
import muliao.notifications.{Notifications, SessionLink}
import muliao.notifications.push.{Device, PushFailure, PushGateway}
import muliao.platform.{AppError, GeneralSettings, Retry}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import muliao.sessions.*
import muliao.sessions.TestEnv.*
import zio.*
import zio.json.*
import zio.test.*

import java.sql.DriverManager
import java.time.LocalDate

/** 工作历史端到端：真的数据库、会话、看板与通知领域，假的模型。覆盖工具（记、查、活动回顾、读会话）、只追加、按天汇总、HTTP。 */
object HistorySpec extends ZIOSpecDefault:

  private val noPush: PushGateway = new PushGateway:
    def send(device: Device, payload: Array[Byte]): IO[PushFailure, Unit] = ZIO.unit

  type Env = TestEnv.Env & Notifications & Board & History & Dirs

  private def fresh[A](script: muliao.models.llm.ModelRequest => Script)(body: ZIO[Env & Scope, AppError, A]): ZIO[Scope, AppError, A] =
    for
      d   <- dirs
      m   <- fakeModels(script)
      env <- layer(
               d,
               m,
               more = List(muliao.notifications.Module.migrations, muliao.board.Module.migrations, muliao.history.Module.migrations),
               moreTools = SessionLink.specs ++ BoardTools.specs ++ HistoryTools.specs
             ).build
      n   <- Notifications.make(env.get[Database], env.get[ChangeBus], noPush, "PUBLIC", Retry.Policy(10.millis, 40.millis, 400.millis))
      gen  = GeneralSettings(d.data, d.assets)
      b   <- Board.layer.build.provideSomeEnvironment[Scope](_ ++ env.add(n).add(gen)).map(_.get[Board])
      h   <- History.layer.build.provideSomeEnvironment[Scope](_ ++ env.add(n).add(b).add(gen)).map(_.get[History])
      all  = env.add(n).add(b).add(gen).add(h).add(d)
      p   <- probe
      a <- (for
             cs <- ZIO.collectAll(List(SessionLink.contribution, BoardLink.contribution, HistoryLink.contribution))
             u  <- SessionLink.unread
             _  <- start(p, cs, unread = u)
             r  <- body
           yield r).provideSomeEnvironment[Scope](_ ++ all)
    yield a

  private def history = ZIO.service[History]

  /** 会话里全部工具结果（按顺序）：名字、正文、是否出错。 */
  private def results(id: SessionId): ZIO[Sessions, AppError, List[(String, String, Boolean)]] =
    timeline(id).map(_.flatMap(_.parts.flatMap(_.call)).flatMap(c => c.result.map(r => (c.name, r.text, r.isError))))

  /** 用户开一个会话，等它做完，返回会话与其中的工具结果。 */
  private def ask(a: AgentId, text: String): ZIO[Sessions, AppError, (SessionId, List[(String, String, Boolean)])] =
    for
      s  <- userSession(a, text)
      _  <- eventually(s"「$text」回答完")(timeline(s))(_.exists(_.parts.exists(_.text.contains("好"))))
      _  <- idle(s)
      rs <- results(s)
    yield s -> rs

  private def entry(day: LocalDate, title: String, summary: String, imp: Importance = Importance.Routine, refs: List[String] = Nil) =
    NewEntry.make(day, title, summary, refs, imp, day).toOption.get

  final case class Got(id: Long, day: String, agent: String, title: String, summary: String, refs: List[String], importance: Int) derives JsonDecoder

  private def got(json: String): List[Got] = json.fromJson[List[Got]].toOption.getOrElse(Nil)

  /** 假模型：用户说「工具 <名字> <参数 JSON>」就调那个工具，拿到结果后回「好」。 */
  private val toolScript: muliao.models.llm.ModelRequest => Script = req =>
    val t = lastUserText(req)
    if t.startsWith("工具 ") then
      val (name, args) = t.drop(3).span(_ != ' ')
      Script.Say(List(call("c1", name, args.trim)))
    else Script.Say(List(text("好")))

  private def tool(name: String, args: String) = s"工具 $name $args"

  def spec = suite("工作历史")(
    test("记：经工具记一批（记下是哪个会话），任一条不合法整批不记；库里拒绝改、删") {
      ZIO.scoped(fresh(toolScript)(
        for
          bob   <- agent("bob")
          h     <- history
          today <- h.today
          (s1, r1) <- ask(bob, tool("history_append", s"""{"entries":[{"day":"$today","title":"部署新版本","summary":"部署后自检通过","importance":2,"refs":["#865"]},{"day":"2026-11-01","title":"整理磁盘","summary":"清理备份"}]}"""))
          (_, r2)  <- ask(bob, tool("history_append", s"""{"entries":[{"day":"2026-11-02","title":"好的一条","summary":"x"},{"day":"${today.plusDays(1)}","title":"未来的","summary":"y"}]}"""))
          (_, r3)  <- ask(bob, tool("history_append", """{"entries":[{"day":"11/02","title":"t","summary":"s"}]}"""))
          (_, r4)  <- ask(bob, tool("history_append", """{"entries":[{"day":"2026-11-02","title":"t","summary":"s","importance":5}]}"""))
          all   <- h.search(HistoryQuery(limit = 200))
          d     <- ZIO.service[Dirs]
          upd   <- ZIO.attemptBlocking {
                     val c = DriverManager.getConnection(s"jdbc:sqlite:${d.data.resolve("t.db")}")
                     try
                       val u = scala.util.Try(c.createStatement().executeUpdate("UPDATE history_entry SET title = 'x'"))
                       val r = scala.util.Try(c.createStatement().executeUpdate("DELETE FROM history_entry"))
                       (u.failed.map(_.getMessage).toOption, r.failed.map(_.getMessage).toOption)
                     finally c.close()
                   }.orDie
          after <- h.search(HistoryQuery(limit = 200))
        yield assertTrue(
          r1.map(_._1) == List("history_append"),
          r1.head._2.startsWith("已记 2 条："),
          all.map(_.title) == List("部署新版本", "整理磁盘"),
          all.head.session.contains(s1),
          all.head.importance == Importance.Notable,
          all.head.refs == List("#865"),
          all.forall(_.agent == bob),
          r2.head._3,
          r2.head._2.contains("第 2 条") && r2.head._2.contains("之后"),
          r3.head._3 && r3.head._2.contains("YYYY-MM-DD"),
          r4.head._3 && r4.head._2.contains("重要程度"),
          upd._1.exists(_.contains("只能追加")),
          upd._2.exists(_.contains("只能追加")),
          after.map(_.title) == all.map(_.title)
        )
      ))
    },
    test("查：默认只查自己，`all` 查全部；关键字按相关度；日期、重要程度、条数筛选；名字不对报错") {
      ZIO.scoped(fresh(toolScript)(
        for
          bob   <- agent("bob")
          carol <- agent("carol")
          h     <- history
          d      = LocalDate.parse("2026-11-10")
          _     <- h.append(bob, None, List(entry(d, "整理磁盘", "清理了部署备份"), entry(d.plusDays(1), "部署新版本", "部署后自检通过", Importance.Notable), entry(d.plusDays(2), "修投递", "看板", refs = List("#871"))))
          _     <- h.append(carol, None, List(entry(d, "部署 carol 的服务", "部署", Importance.Major)))
          (_, mine) <- ask(bob, tool("history_search", """{"query":"部署"}"""))
          (_, all)  <- ask(bob, tool("history_search", """{"query":"部署","agent":"all"}"""))
          (_, list) <- ask(bob, tool("history_search", """{"agent":"all","since":"2026-11-11"}"""))
          (_, imp)  <- ask(bob, tool("history_search", """{"agent":"carol","min_importance":3}"""))
          (_, lim)  <- ask(bob, tool("history_search", """{"limit":1}"""))
          (_, ref)  <- ask(bob, tool("history_search", """{"query":"#871 看板"}"""))
          (_, bad)  <- ask(bob, tool("history_search", """{"agent":"nobody"}"""))
          (_, big)  <- ask(bob, tool("history_search", """{"limit":500}"""))
        yield assertTrue(
          got(mine.head._2).map(_.title) == List("部署新版本", "整理磁盘"),
          got(mine.head._2).head == Got(got(mine.head._2).head.id, "2026-11-11", "bob", "部署新版本", "部署后自检通过", Nil, 2),
          got(all.head._2).map(_.title) == List("部署 carol 的服务", "部署新版本", "整理磁盘"),
          got(list.head._2).map(_.title) == List("修投递", "部署新版本"),
          got(imp.head._2).map(_.agent) == List("carol"),
          got(lim.head._2).map(_.title) == List("修投递"),
          got(ref.head._2).map(_.refs) == List(List("#871")),
          bad.head._3,
          big.head._3
        )
      ))
    },
    test("活动回顾：活跃的会话、用户亲手发的话、对 issue 的操作、失败的运行、当天的工作历史；单个 agent 与全部；别的日子是空的") {
      ZIO.scoped(fresh { req =>
        val t = lastUserText(req)
        if t == "这次会失败" then Script.Fail(AppError.External("模型说不行"))
        else toolScript(req)
      }(
        for
          bob   <- agent("bob")
          carol <- agent("carol")
          h     <- history
          b     <- ZIO.service[Board]
          today <- h.today
          (s1, _) <- ask(bob, "帮我部署")
          s2    <- userSession(bob, "这次会失败")
          _     <- eventually("失败")(ZIO.serviceWithZIO[SessionConsole](_.detail(s2)))(_.run.exists(_.state == RunState.Failed))
          (s3, _) <- ask(carol, "carol 的事")
          ids   <- b.create(By.user, List(Draft(None, "bob 的活", bob, status = IssueStatus.Backlog, assignee = Some(Assignee.Agent(bob))),
                     Draft(None, "carol 的活", carol, status = IssueStatus.Backlog)))
          _     <- b.comment(By(Actor.Agent(bob, Some(s1))), ids.head, "bob 的进展")
          _     <- b.comment(By.user, ids.head, "用户的意见")
          _     <- b.comment(By.user, ids(1), "给 carol 的意见")
          _     <- b.comment(By(Actor.Agent(carol, None)), ids(1), "carol 的进展")
          _     <- b.update(By(Actor.Agent(bob, Some(s1))), ids.head, Patch(status = Some(IssueStatus.Blocked), assignee = Some(Some(Assignee.User))))
          _     <- b.update(By(Actor.Agent(bob, Some(s1))), ids.head, Patch(status = Some(IssueStatus.Backlog)))
          _     <- h.append(bob, None, List(entry(today, "今天做的", "部署"), entry(today.minusDays(1), "昨天做的", "整理")))
          (_, rs) <- ask(bob, tool("activity_overview", "{}"))
          bobDay  = rs.head._2
          (_, ra) <- ask(bob, tool("activity_overview", """{"agent":"all"}"""))
          allDay  = ra.head._2
          (_, ry) <- ask(bob, tool("activity_overview", s"""{"day":"${today.minusDays(1)}"}"""))
          (_, rp) <- ask(bob, tool("activity_overview", """{"offset":3}"""))
          (_, rb) <- ask(bob, tool("activity_overview", """{"day":"昨天"}"""))
          o     <- h.overview(Some(bob), today)
        yield assertTrue(
          bobDay.startsWith(s"# $today bob 的活动（时区 Asia/Shanghai）"),
          bobDay.contains(s"- ${s1.value.take(8)} [用户新建] 帮我部署 | idle | 2 条消息"),
          bobDay.contains(s"${s2.value.take(8)} [用户新建] 这次会失败 | idle | 1 条消息"),
          bobDay.contains("运行 1 次，失败 1 次"),
          bobDay.contains(s"${s1.value.take(8)} 说：\n  帮我部署"),
          bobDay.contains("说：\n  工具 activity_overview {}"),
          !bobDay.contains("carol 的事"),
          bobDay.contains(s"## 对 issue 的操作（1 个 issue，6 条动态）\n- #${ids.head.value} bob 的活 | 6 条动态"),
          bobDay.contains("  状态：backlog → blocked → backlog\n  指派：bob → user\n"),
          bobDay.contains("] 用户 建了这个 issue\n"),
          bobDay.contains("] bob 评论：\n    bob 的进展\n"),
          bobDay.contains("] 用户 评论：\n    用户的意见"),
          !bobDay.contains("把状态从"),
          !bobDay.contains("给 carol 的意见"),
          !bobDay.contains("carol 的进展"),
          bobDay.contains("## 失败的运行（1）"),
          bobDay.contains("  模型说不行"),
          bobDay.contains("## 当天的工作历史（1）\n- [1] 今天做的\n  部署"),
          allDay.contains(s"${s3.value.take(8)} carol [用户新建] carol 的事"),
          allDay.contains("给 carol 的意见"),
          allDay.contains("carol 的活 | 3 条动态"),
          allDay.contains("] carol 评论：\n    carol 的进展"),
          allDay.contains("- bob [1] 今天做的"),
          ry.head._2.contains("## 活跃的会话（0）"),
          ry.head._2.contains("## 当天的工作历史（1）\n- [1] 昨天做的"),
          rp.head._2.startsWith("## 活跃的会话（"),
          rb.head._3,
          o.sessions.failures.map(_.session) == List(s2)
        )
      ))
    },
    test("读会话：按编号前缀找，带标签与时间，可按标签、正则筛选，截断与续读；前缀不对、不唯一、不存在都说明") {
      ZIO.scoped(fresh(toolScript)(
        for
          bob    <- agent("bob")
          h      <- history
          (s1, _) <- ask(bob, tool("echo", "{\"text\":\"" + "长" * 3000 + "\"}"))
          p       = s1.value.take(8)
          (_, r1) <- ask(bob, tool("session_read", s"""{"session":"$p"}"""))
          (_, r2) <- ask(bob, tool("session_read", s"""{"session":"$p","labels":["call","result"],"clip":0}"""))
          (_, r3) <- ask(bob, tool("session_read", s"""{"session":"$p","grep":"ECHO","labels":["result"]}"""))
          (_, r4) <- ask(bob, tool("session_read", s"""{"session":"$p","labels":["USER"],"offset":2}"""))
          (_, r5) <- ask(bob, tool("session_read", s"""{"session":"$p","grep":"("}"""))
          (_, r6) <- ask(bob, tool("session_read", """{"session":"zz"}"""))
          (_, r7) <- ask(bob, tool("session_read", """{"session":"","labels":[]}"""))
          miss   <- h.read("ffffffffffffffff").either
          many   <- h.read("").either
          read   <- h.read(p.toUpperCase)
          info    = read.info
          items   = read.items
          all    = r1.head._2
        yield assertTrue(
          all.startsWith(s"# 会话 ${s1.value}（bob，用户新建）："),
          all.contains("USER\n工具 echo"),
          all.contains("call echo\n{\"text\":\"" + "长" * 1990),
          all.contains("[截断：共"),
          all.contains("result echo\necho:" + "长" * 1995),
          all.contains("ASSISTANT\n好"),
          r2.head._2.contains("[1] ") && r2.head._2.contains("[2] ") && !r2.head._2.contains("[3] "),
          !r2.head._2.contains("截断"),
          r3.head._2.contains("[1] ") && r3.head._2.contains("result echo"),
          r4.head._3 && r4.head._2.contains("超出"),
          r5.head._3 && r5.head._2.contains("正则"),
          r6.head._3 && r6.head._2.contains("十六进制"),
          r7.head._3,
          miss.left.exists(_.isInstanceOf[AppError.Domain]),
          many.isLeft,
          info.id == s1,
          items.map(_.label).contains("call echo")
        )
      ))
    },
    test("HTTP：列出工作历史（按 agent、关键字、日期），参数不对 400") {
      ZIO.scoped(fresh(toolScript)(
        for
          bob  <- agent("bob")
          carol <- agent("carol")
          h    <- history
          d     = LocalDate.parse("2026-11-10")
          _    <- h.append(bob, None, List(entry(d, "部署", "上线"), entry(d.plusDays(1), "整理", "磁盘")))
          _    <- h.append(carol, None, List(entry(d, "carol 部署", "上线")))
          bus  <- ZIO.service[ChangeBus]
          set   = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
          auth <- Auth.layer.build.map(_.get[Auth]).provideSomeLayer[Scope](ZLayer.succeed(set))
          port <- HttpServer.serve(set, auth, bus, HistoryHttp(h).routes).orDie
          c     = muliao.sessions.SessionsHttpSpec.Client(s"http://127.0.0.1:$port")
          call  = (m: String, p: String) => c.call(m, p, None).orDie
          _    <- c.call("POST", "/api/auth/login", Some("""{"password":"pw"}""")).orDie
          all  <- call("GET", "/api/history")
          mine <- call("GET", s"/api/history?agent=${bob.value}")
          q    <- call("GET", "/api/history?q=%E9%83%A8%E7%BD%B2&until=2026-11-10")
          bad  <- call("GET", "/api/history?since=yesterday")
        yield
          def views(b: String) = b.fromJson[List[HistoryEntryView]].toOption.getOrElse(Nil)
          assertTrue(
            all.status == 200,
            views(all.body).size == 3,
            views(mine.body).map(_.title) == List("整理", "部署"),
            views(mine.body).forall(_.agentName == "bob"),
            views(q.body).map(_.title).toSet == Set("部署", "carol 部署"),
            bad.status == 400
          )
      ))
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(120.seconds) @@ TestAspect.sequential
