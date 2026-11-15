package muliao.board

import muliao.contract.*
import muliao.notifications.{Notifications, SessionLink}
import muliao.notifications.push.{Device, PushFailure, PushGateway}
import muliao.platform.{AppError, GeneralSettings, Retry}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import muliao.resources.agents.{AgentPatch, Agents}
import muliao.sessions.*
import muliao.sessions.TestEnv.*
import zio.*
import zio.json.*
import zio.test.*

/** 看板端到端：真的数据库、会话与通知领域，假的模型。覆盖投递规则、依赖放行、停滞与请表态、审核与冲突、会话失败、叫停、开始、删除、HTTP。 */
object BoardSpec extends ZIOSpecDefault:

  private val noPush: PushGateway = new PushGateway:
    def send(device: Device, payload: Array[Byte]): IO[PushFailure, Unit] = ZIO.unit

  type Env = TestEnv.Env & Notifications & Board

  /** 起一套会话 + 通知 + 看板环境，接好并启动会话领域。 */
  private def fresh[A](script: muliao.models.llm.ModelRequest => Script)(body: ZIO[Env & Scope, AppError, A]): ZIO[Scope, AppError, A] =
    for
      d   <- dirs
      m   <- fakeModels(script)
      env <- layer(d, m, more = List(muliao.notifications.Module.migrations, muliao.board.Module.migrations), moreTools = SessionLink.toolNames ++ BoardTools.toolNames).build
      n   <- Notifications.make(env.get[Database], env.get[ChangeBus], noPush, "PUBLIC", Retry.Policy(10.millis, 40.millis, 400.millis))
      gen  = GeneralSettings(d.data, d.assets)
      b   <- Board.layer.build.provideSomeEnvironment[Scope](_ ++ env.add(n).add(gen)).map(_.get[Board])
      all  = env.add(n).add(b).add(gen)
      p   <- probe
      a   <- (SessionLink.attach *> BoardLink.attach *> start(p) *> body).provideSomeEnvironment[Scope](_ ++ all)
    yield a

  private def board = ZIO.service[Board]
  private def iid(n: Long) = IssueId.wrap(n)

  private def sessionOf(i: Long, a: AgentId): ZIO[Sessions, AppError, Option[SessionId]] =
    ZIO.serviceWithZIO[Sessions](_.issueSessions(List(iid(i)))).map(_.find(_.agent == a).map(_.session))

  /** agent 处理 issue 的会话收到的信（正文）。 */
  private def letters(i: Long, a: AgentId): ZIO[Sessions, AppError, List[String]] =
    sessionOf(i, a).flatMap {
      case None    => ZIO.succeed(Nil)
      case Some(s) => timeline(s).map(_.filter(_.kind == "letter").flatMap(_.text))
    }

  /** 会话收到 `n` 封信并空闲下来，再等一会儿确认不再有新信。 */
  private def settled(i: Long, a: AgentId, n: Int): ZIO[Sessions, AppError, List[String]] =
    for
      _  <- eventually(s"#$i 的会话收到 $n 封信")(letters(i, a))(_.size >= n)
      s  <- sessionOf(i, a).someOrFail(AppError.Internal("没有会话"))
      _  <- idle(s)
      _  <- Clock.sleep(300.millis)
      _  <- idle(s)
      ls <- letters(i, a)
    yield ls

  private def detail(i: Long) = board.flatMap(_.show(iid(i)))
  private def summary(i: Long) = detail(i).map(_.summary)

  private def draft(title: String, owner: AgentId, status: IssueStatus = IssueStatus.InProgress, assignee: Option[AgentId] = None, key: Option[String] = None, deps: List[Upstream] = Nil) =
    Draft(key, title, owner, status = status, assignee = assignee.map(Assignee.Agent(_)), dependsOn = deps)

  private def asAgent(a: AgentId, inIssue: Option[Long] = None) = By(Actor.Agent(a, None), inIssue.map(iid))

  private def says(req: muliao.models.llm.ModelRequest, mark: String) = lastUserText(req).contains(mark)

  def spec = suite("issue 看板")(
    test("投递：建好就投给在做的 agent；它在自己的 issue 会话里的改动不投回；别人的动态照投；没人处理时用户的评论投给 owner；投不进记一条动态") {
      ZIO.scoped(fresh { req =>
        if says(req, "有新动态") && says(req, "【评论】") then Script.Say(List(call("c1", "issue_comment", """{"issue_id":1,"body":"bob 收到"}""")))
        else Script.Say(List(text("好")))
      }(
        for
          bob   <- agent("bob")
          carol <- agent("carol")
          dave  <- agent("dave")
          b     <- board
          ids   <- b.create(By.user, List(draft("A【评论】", carol, assignee = Some(bob))))
          // bob 收到建立的信，在 issue 会话里评论（不投回），运行结束后停滞，被请表态一次（又评论一次，仍不投回）
          ls1   <- settled(1, bob, 2)
          ev1   <- detail(1).map(_.events)
          bobS  <- sessionOf(1, bob)
          _     <- b.comment(asAgent(carol), iid(1), "carol 的意见")
          ls2   <- settled(1, bob, 4)
          // backlog：用户的评论投给 owner，agent 的评论不投
          _     <- b.create(By.user, List(draft("B", carol, status = IssueStatus.Backlog, assignee = Some(bob))))
          _     <- b.comment(asAgent(bob), iid(2), "bob 说两句")
          _     <- b.comment(By.user, iid(2), "用户的问题")
          ls3   <- settled(2, carol, 1)
          bob2  <- letters(2, bob)
          // 指派给已归档的 agent：建时拒绝；建好后才归档的，投不进记一条动态，改动照常保存
          _     <- b.create(By.user, List(draft("C", carol, assignee = Some(dave))))
          _     <- settled(3, dave, 2)
          ags   <- ZIO.service[Agents]
          _     <- ags.update(dave, AgentPatch(archived = Some(true)))
          _     <- b.comment(By.user, iid(3), "还在吗")
          ev3   <- detail(3).map(_.events)
          bad   <- b.create(By.user, List(draft("D", carol, assignee = Some(dave)))).either
        yield assertTrue(
          ids == List(iid(1)),
          ls1.head.startsWith("issue #1 有新动态：A【评论】 | 状态 in_progress | 指派 bob | 依赖 无"),
          ls1.head.contains("用户 建了这个 issue"),
          ls1(1).contains("请表态"),
          ev1.count(e => e.actor == "bob" && e.comment.contains("bob 收到")) == 2,
          ev1.filter(_.actor == "bob").forall(_.session == bobS),
          !ls2.exists(_.contains("bob 收到")),
          ls2(2).contains("carol 评论：\n  carol 的意见"),
          ls3.size == 1,
          ls3.head.contains("用户的问题"),
          !ls3.head.contains("bob 说两句"),
          bob2.isEmpty,
          ev3.last.text.startsWith("没能投给 dave："),
          ev3.exists(_.comment.contains("还在吗")),
          bad.isLeft
        )
      ))
    },
    test("依赖：一批互相依赖的一起建（不合法整批不建）；上游结束才放行下游，下游这时收到第一封信；成环拒绝并指出环") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("好"))))(
        for
          bob    <- agent("bob")
          carol  <- agent("carol")
          b      <- board
          badKey <- b.create(By.user, List(draft("X", bob, deps = List(Upstream.Key("nope"))))).either
          loop   <- b.create(By.user, List(draft("X", bob, key = Some("x"), deps = List(Upstream.Key("y"))), draft("Y", bob, key = Some("y"), deps = List(Upstream.Key("x"))))).either
          none   <- b.list(Filter())
          ids <- b.create(
                   By.user,
                   List(
                     draft("下游 B", bob, assignee = Some(carol), key = Some("b"), deps = List(Upstream.Key("a"))),
                     draft("上游 A", bob, assignee = Some(bob), key = Some("a")),
                     draft("父 P", bob, status = IssueStatus.Backlog, deps = List(Upstream.Key("a"), Upstream.Key("b")))
                   )
                 )
          (down, up, parent) = (ids(0), ids(1), ids(2))
          _      <- settled(up.value, bob, 2)
          before <- b.list(Filter())
          early  <- letters(down.value, carol)
          cyc    <- b.update(By.user, up, Patch(dependsOn = Some(List(parent)))).either.map(_.swap.toOption.map(_.message).getOrElse(""))
          self   <- b.update(By.user, up, Patch(dependsOn = Some(List(up)))).either.map(_.swap.toOption.map(_.message).getOrElse(""))
          // bob 在 A 自己的会话里设 done：不投回给 bob，下游 B 记一条并投给 carol
          _      <- b.update(asAgent(bob, Some(up.value)), up, Patch(status = Some(IssueStatus.Done)))
          ls     <- settled(down.value, carol, 1)
          bobLs  <- letters(up.value, bob)
          after  <- summary(down.value)
          pDet   <- detail(parent.value)
          aDet   <- detail(up.value)
        yield
          val b0 = before.find(_.id == down).get
          assertTrue(
            badKey.isLeft,
            loop.left.exists(_.message.contains("成环")),
            none.isEmpty,
            ids.map(_.value) == List(1L, 2L, 3L),
            b0.waitingOn == List(up),
            !b0.stalled,
            early.isEmpty,
            cyc.contains("依赖成环") && cyc.contains(s"#${up.value}") && cyc.contains(s"#${parent.value}"),
            self.contains("不能依赖自己"),
            ls.head.contains(s"bob 把依赖 #${up.value} 的状态从 in_progress 改成 done"),
            ls.head.contains(s"依赖 #${up.value} (done)"),
            bobLs.size == 2,
            after.waitingOn.isEmpty,
            pDet.dependsOn.map(_.id) == List(down, up),
            pDet.summary.waitingOn == List(down),
            aDet.blocks.map(_.id).toSet == Set(down, parent)
          )
      ))
    },
    test("停滞：运行结束后没人处理就请表态，只请一次；有新动态投来后才会再请；列表标出停滞与会话状态") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("好"))))(
        for
          bob   <- agent("bob")
          b     <- board
          _     <- b.create(By.user, List(draft("闲着", bob, assignee = Some(bob))))
          ls1   <- settled(1, bob, 2)
          sum1  <- summary(1)
          _     <- b.comment(By.user, iid(1), "动一动")
          ls2   <- settled(1, bob, 4)
          _     <- b.update(asAgent(bob, Some(1)), iid(1), Patch(status = Some(IssueStatus.InReview), assignee = Some(Some(Assignee.User))))
          sum2  <- summary(1)
          ls3   <- letters(1, bob)
        yield assertTrue(
          ls1(1).contains("请表态"),
          sum1.stalled,
          sum1.assigneeState.contains(WorkState.Idle),
          ls2(2).contains("动一动"),
          ls2(3).contains("请表态"),
          !sum2.stalled,
          ls3.size == 4
        )
      ))
    },
    test("审核：交给用户发通知（正文是它刚写的评论）；打回给交出它的 agent 并带理由；通过；过时的通过、打回被拒") {
      ZIO.scoped(fresh { req =>
        if says(req, "【交审】") && says(req, "建了这个 issue") then
          Script.Say(List(
            call("c1", "issue_comment", """{"issue_id":1,"body":"做完了，见报告"}"""),
            call("c2", "issue_update", """{"issue_id":1,"status":"in_review","assignee":"user"}""")
          ))
        else Script.Say(List(text("好")))
      }(
        for
          bob   <- agent("bob")
          b     <- board
          n     <- ZIO.service[Notifications]
          _     <- b.create(By.user, List(draft("报告【交审】", bob, assignee = Some(bob))))
          box   <- eventually("交审通知")(n.inbox(10))(_.items.exists(_.url.contains("/issues/1")))
          s1    <- summary(1)
          early <- b.approve(iid(2)).either
          _     <- b.sendBack(iid(1), "再补测试")
          ls    <- settled(1, bob, 3)
          s2    <- summary(1)
          stale <- b.approve(iid(1)).either
          _     <- b.update(asAgent(bob, Some(1)), iid(1), Patch(status = Some(IssueStatus.InReview), assignee = Some(Some(Assignee.User))))
          _     <- b.approve(iid(1))
          s3    <- summary(1)
          again <- b.approve(iid(1)).either
          back  <- b.sendBack(iid(1), "x").either
        yield
          val note = box.items.find(_.url.contains("/issues/1")).get
          assertTrue(
            note.title == "#1 交给你：报告【交审】",
            note.body == "做完了，见报告",
            s1.status == IssueStatus.InReview,
            s1.assignee.contains(Assignee.User),
            early.isLeft,
            ls(1).contains("用户 打回：\n  再补测试"),
            s2.status == IssueStatus.InProgress,
            s2.assignee.contains(Assignee.Agent(bob)),
            stale.left.exists(_.message.contains("没有指派给你")),
            s3.status == IssueStatus.Done,
            again.left.exists(_.message.contains("已经是 done")),
            back.isLeft
          )
      ))
    },
    test("会话最终失败只记一条动态不重投；叫停停掉在做的会话、改状态并把留言投给它；不能在自己的会话里叫停；开始；删除去掉下游的依赖；删 agent 置空") {
      ZIO.scoped(fresh { req =>
        if says(req, "【坏】") then Script.Fail(AppError.External("坏请求"))
        else if says(req, "叫停") then Script.Say(List(text("收尾了")))
        else if says(req, "【卡住】") then Script.Say(List(call("k1", "block", "{}")))
        else Script.Say(List(text("好")))
      }(
        for
          bob    <- agent("bob")
          carol  <- agent("carol")
          b      <- board
          _      <- b.create(By.user, List(draft("会坏【坏】", carol, assignee = Some(bob))))
          ev1    <- eventually("失败记下")(detail(1).map(_.events))(_.exists(_.text.contains("最终失败")))
          _      <- Clock.sleep(300.millis)
          ls1    <- letters(1, bob)
          _      <- b.create(By.user, List(draft("会卡【卡住】", carol, assignee = Some(bob))))
          s2     <- eventually("bob 卡住")(sessionOf(2, bob))(_.isDefined).map(_.get)
          _      <- eventually("调了 block")(timeline(s2))(_.exists(_.kind == "reply"))
          own    <- b.stop(asAgent(bob, Some(2)), iid(2), Halt.make(IssueStatus.Blocked, None).toOption.get).either
          _      <- b.stop(asAgent(carol), iid(2), Halt.make(IssueStatus.Blocked, Some("先别做了，收尾")).toOption.get)
          ls2    <- settled(2, bob, 2)
          sum2   <- summary(2)
          tl2    <- timeline(s2)
          _      <- b.create(By.user, List(draft("后做", carol, status = IssueStatus.Backlog, assignee = Some(bob))))
          _      <- b.start(iid(3))
          ls3    <- settled(3, bob, 1)
          _      <- b.create(By.user, List(draft("下游", carol, status = IssueStatus.Backlog, deps = List(Upstream.Existing(iid(3))))))
          early  <- b.start(iid(4)).either
          _      <- b.delete(iid(3))
          d4     <- detail(4)
          gone   <- b.show(iid(3)).either
          _      <- b.forgetAgent(bob)
          _      <- b.forgetAgent(bob)
          s1     <- summary(1)
          s4     <- summary(4)
        yield assertTrue(
          ev1.count(_.text.contains("最终失败")) == 1,
          ev1.find(_.text.contains("最终失败")).flatMap(_.comment).exists(_.contains("坏请求")),
          ev1.find(_.text.contains("最终失败")).flatMap(_.session).isDefined,
          ls1.size == 1,
          own.left.exists(_.message.contains("自己的会话")),
          tl2.exists(i => i.kind == "notice" && i.noticeKind.contains("Stopped")),
          ls2(1).contains("carol 叫停了处理它的会话"),
          ls2(1).contains("先别做了，收尾"),
          sum2.status == IssueStatus.Blocked,
          ls3.head.contains("用户 点了开始"),
          early.left.exists(_.message.contains("没有指派")),
          d4.summary.dependsOn.isEmpty,
          d4.events.last.text.contains("删除了依赖 #3"),
          gone.isLeft,
          s1.assignee.isEmpty,
          s1.owner.contains(carol),
          s4.owner.contains(carol)
        )
      ))
    },
    test("工具：一批建（键互相引用）、列表 JSON、详情文字；参数不合法时告诉模型哪里错") {
      val seen = java.util.concurrent.ConcurrentLinkedQueue[String]()
      ZIO.scoped(fresh { req =>
        val last = lastUserText(req)
        if !isAgent(req, "alice") then Script.Say(List(text("好")))
        else
          seen.add(last)
          if last.contains("建一批") then
            Script.Say(List(call("t1", "issue_create",
              """{"issues":[{"key":"b","title":"B","owner":"alice","status":"in_progress","assignee":"bob","depends_on_keys":["a"]},{"key":"a","title":"A","owner":"alice","status":"in_progress","assignee":"bob","priority":"high"}]}""")))
          else if last.startsWith("已建") then Script.Say(List(call("t2", "issue_list", """{"statuses":["in_progress"],"assignee":"bob"}""")))
          else if last.startsWith("[") then Script.Say(List(call("t3", "issue_show", """{"issue_id":1}""")))
          else if last.startsWith("# Issue #1") then
            Script.Say(List(call("t4", "issue_create", """{"title":"x","owner":"alice","issues":[{"title":"y","owner":"alice"}]}""")))
          else if last.contains("给了 issues") then Script.Say(List(call("t5", "issue_update", """{"issue_id":1,"status":"doing"}""")))
          else Script.Say(List(text("完")))
      }(
        for
          alice <- agent("alice")
          bob   <- agent("bob")
          s     <- userSession(alice, "建一批")
          _     <- eventually("alice 做完")(timeline(s))(_.exists(_.parts.exists(_.text.contains("完"))))
          all   <- board.flatMap(_.list(Filter()))
        yield
          val out  = seen.toArray.toList.map(_.toString)
          val list = out.find(_.startsWith("[")).flatMap(_.fromJson[zio.json.ast.Json].toOption)
          val show = out.find(_.startsWith("# Issue #1")).getOrElse("")
          assertTrue(
            out.exists(_ == "已建：#1（in_progress）B；#2（in_progress）A"),
            all.find(_.id == iid(1)).exists(_.dependsOn == List(iid(2))),
            list.exists(_.toString.contains("\"waiting_on\":[2]")),
            list.exists(_.toString.contains("\"assignee\":\"bob\"")),
            show.contains("依赖：#2 (in_progress) | 在等：#2"),
            show.contains("## 动态\n- ["),
            show.contains("alice 建了这个 issue"),
            out.exists(_.contains("给了 issues 就不要再给单项的 title、owner")),
            out.exists(_.contains("参数 `status`")),
            all.size == 2
          )
      ))
    },
    test("HTTP：建、看、改、评论、通过（过时的 409）、列表筛选、删除") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("好"))))(
        for
          bob  <- agent("bob")
          b    <- board
          bus  <- ZIO.service[ChangeBus]
          set   = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
          auth <- Auth.layer.build.map(_.get[Auth]).provideSomeLayer[Scope](ZLayer.succeed(set))
          port <- HttpServer.serve(set, auth, bus, BoardHttp(b).endpoints(auth), "test").orDie
          c     = muliao.sessions.SessionsHttpSpec.Client(s"http://127.0.0.1:$port")
          call  = (m: String, p: String, body: Option[String]) => c.call(m, p, body).orDie
          _    <- call("POST", "/api/auth/login", Some("""{"password":"pw"}"""))
          made <- call("POST", "/api/issues", Some(NewIssue("界面建的", bob, "描述", assignee = Some(Assignee.User)).toJson))
          id    = made.body.fromJson[IssueDetail].toOption.get.summary.id
          patc <- call("PATCH", s"/api/issues/${id.value}", Some("""{"status":"in_review","priority":"high"}"""))
          com  <- call("POST", s"/api/issues/${id.value}/comments", Some(CommentIn("看过了").toJson))
          get  <- call("GET", s"/api/issues/${id.value}", None)
          ok   <- call("POST", s"/api/issues/${id.value}/approve", None)
          dup  <- call("POST", s"/api/issues/${id.value}/approve", None)
          done <- call("GET", "/api/issues?status=done&status=cancelled", None)
          open <- call("GET", "/api/issues?status=in_progress", None)
          bad  <- call("PATCH", s"/api/issues/${id.value}", Some("""{"assignee":"user","unassign":true}"""))
          del  <- call("DELETE", s"/api/issues/${id.value}", None)
          miss <- call("GET", s"/api/issues/${id.value}", None)
        yield
          val d = get.body.fromJson[IssueDetail].toOption.get
          assertTrue(
            made.status == 200,
            patc.body.fromJson[IssueDetail].toOption.exists(x => x.summary.status == IssueStatus.InReview && x.summary.priority == Priority.High),
            com.status == 204,
            d.events.map(_.text) == List("建了这个 issue", "把状态从 backlog 改成 in_review", "把优先级从 none 改成 high", "评论"),
            d.events.forall(_.actor == "用户"),
            ok.status == 204,
            dup.status == 409,
            done.body.fromJson[List[IssueSummary]].toOption.exists(_.map(_.id) == List(id)),
            open.body.fromJson[List[IssueSummary]].toOption.exists(_.isEmpty),
            bad.status == 400,
            del.status == 204,
            miss.status == 404
          )
      ))
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(120.seconds) @@ TestAspect.sequential
