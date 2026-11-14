package muliao.sessions

import muliao.contract.*
import muliao.models.llm.*
import muliao.platform.AppError
import muliao.resources.agents.Agents
import muliao.sessions.TestEnv.*
import zio.*
import zio.test.*

/** 会话与运行的端到端测试：真的库与资源目录，假的模型。 */
object SessionsSpec extends ZIOSpecDefault:

  /** 在一套新环境里跑；`f` 拿到假模型、测试工具的探针与数据目录。 */
  private def fresh[A](handler: ModelRequest => Script, settings: SessionSettings = SessionSettings())(
      f: (FakeModels, Probe, Dirs) => ZIO[TestEnv.Env, AppError, A]
  ): IO[AppError, A] =
    for
      d <- dirs
      m <- fakeModels(handler)
      p <- probe
      a <- ZIO.scoped(layer(d, m, settings).build.flatMap(env => (start(p) *> f(m, p, d)).provideEnvironment(env)))
    yield a

  private def sessions = ZIO.service[Sessions]

  def spec = suite("会话与运行")(
    test("普通对话：回答、交回用户、记用量，固定说明跨运行不变") {
      fresh(_ => Script.Say(List(text("你好呀")))) { (m, _, _) =>
        for
          events <- recordEvents
          alice  <- agent("alice")
          id     <- userSession(alice, "你好")
          _      <- idle(id)
          _      <- say(id, "再说一次")
          _      <- eventually("第二次回答")(timeline(id))(_.count(_.kind == "reply") == 2)
          _      <- idle(id)
          tl     <- timeline(id)
          reqs   <- m.requests
          evs    <- eventually("两次交回")(events.get)(_.count(_.isInstanceOf[SessionEvent.Settled]) == 2)
          detail <- sessions.flatMap(_.detail(id))
        yield assertTrue(
          tl.map(_.kind) == List("context", "letter", "reply", "context", "letter", "reply"),
          tl(2).parts.head.text.contains("你好呀"),
          reqs.size == 2,
          reqs(0).system == reqs(1).system,
          reqs(0).system.contains("agent「alice」"),
          userTexts(reqs(0)).head.startsWith("[本次运行上下文]"),
          userTexts(reqs(0)).head.contains("回答交给：用户"),
          userTexts(reqs(1)).take(2) == userTexts(reqs(0)).take(2), // 前缀不变，命中缓存
          evs.collect { case SessionEvent.Settled(`id`, _, _, Outcome.Answered(a), true) => a } == List("你好呀", "你好呀"),
          detail.usage.input == 200L,
          detail.contextTokens == 100L,
          detail.summary.state == "idle"
        )
      }
    },
    test("工具调用：并发执行、结果交回模型；敏感参数不进记录") {
      fresh { req =>
        if userTexts(req).exists(_.startsWith("echo:")) then Script.Say(List(text("做完了")))
        else Script.Say(List(call("c1", "echo", """{"text":"a"}"""), call("c2", "echo", """{"text":"b"}"""), call("c3", "keep_secret", """{"value":"s3cr3t"}""")))
      } { (m, p, _) =>
        for
          alice <- agent("alice")
          id    <- userSession(alice, "调工具")
          _     <- eventually("做完")(timeline(id))(_.exists(_.parts.exists(_.text.contains("做完了"))))
          _     <- idle(id)
          tl    <- timeline(id)
          reqs  <- m.requests
          got   <- p.secrets.get
          calls  = tl.flatMap(_.parts.flatMap(_.call))
        yield assertTrue(
          calls.map(_.state) == List("done", "done", "done"),
          calls.flatMap(_.result.map(_.text)).take(2) == List("echo:a", "echo:b"),
          userTexts(reqs(1)).containsSlice(List("echo:a", "echo:b", "收到")),
          got == List("s3cr3t"),
          calls(2).args.contains("[redacted]"),
          !tl.toString.contains("s3cr3t"),
          !reqs(1).toString.contains("s3cr3t")
        )
      }
    },
    test("重启续跑、被打断的调用补结果不重放") {
      for
        d  <- dirs
        p  <- probe
        // 第一次：请求模型卡住时服务停了
        m1 <- fakeModels(_ => Script.Hang(List(text("想到一半"))))
        id <- ZIO.scoped(layer(d, m1).build.flatMap(env =>
                (for
                  _     <- start(p)
                  alice <- agent("alice")
                  id    <- userSession(alice, "开始")
                  _     <- eventually("请求了模型")(m1.requests)(_.nonEmpty)
                yield id).provideEnvironment(env)
              ))
        // 第二次：重启后重新请求，模型要执行 block，block 卡住时服务又停了
        m2 <- fakeModels(_ => Script.Say(List(call("b1", "block", "{}"))))
        _ <- ZIO.scoped(layer(d, m2).build.flatMap(env =>
               (start(p) *> eventually("block 在执行")(timeline(id))(_.exists(_.parts.exists(_.call.exists(_.state == "running"))))).provideEnvironment(env)
             ))
        // 第三次：被打断的 block 补上结果，不重新执行
        m3 <- fakeModels(req => Script.Say(List(text(s"看到了：${lastUserText(req)}"))))
        tl <- ZIO.scoped(layer(d, m3).build.flatMap(env =>
                (start(p) *> idle(id) *> timeline(id)).provideEnvironment(env)
              ))
        reqs3 <- m3.requests
        reqs2 <- m2.requests
        gateOpen <- p.gate.isDone
      yield assertTrue(
        reqs2.size == 1,
        reqs3.size == 1,
        lastUserText(reqs3.head).contains("被打断"),
        tl.flatMap(_.parts.flatMap(_.call)).map(_.state) == List("done"),
        tl.last.parts.head.text.exists(_.contains("没有重新执行")),
        !gateOpen
      )
    },
    test("提问与回答；等批准、批准后执行") {
      fresh { req =>
        val last = lastUserText(req)
        if last.startsWith("用户的回答") then Script.Say(List(call("a1", "approve_me", "{}")))
        else if last.startsWith("批准后执行了") then Script.Say(List(text("都好了")))
        else Script.Say(List(call("q1", "ask_user", """{"questions":[{"question":"选哪个？","header":"方案","options":[{"label":"A"},{"label":"B"}]}]}""")))
      } { (_, p, _) =>
        for
          events <- recordEvents
          alice  <- agent("alice")
          id     <- userSession(alice, "帮我选")
          _      <- eventually("等提问")(stateOf(id))(_ == WorkState.Waiting)
          d1     <- sessions.flatMap(_.detail(id))
          q       = d1.pending.head
          _      <- sessions.flatMap(_.answer(id, q.entry, q.call, Answer.Answers(List("A"))))
          d2     <- eventually("等批准")(sessions.flatMap(_.detail(id)))(_.pending.exists(_.pending.kind == "approval"))
          a       = d2.pending.head
          _      <- sessions.flatMap(_.answer(id, a.entry, a.call, Answer.Decision(approved = true, always = true)))
          _      <- eventually("回答完")(timeline(id))(_.exists(_.parts.exists(_.text.contains("都好了"))))
          _      <- idle(id)
          tl     <- timeline(id)
          ok     <- p.approvals.get
          evs    <- events.get
        yield assertTrue(
          q.pending.kind == "question",
          q.pending.questions.head.options.map(_.label) == List("A", "B"),
          tl.flatMap(_.parts.flatMap(_.call)).flatMap(_.result.map(_.text)).head.contains("方案（选哪个？）：A"),
          ok == List(true),
          evs.collect { case SessionEvent.NeedsUser(`id`, _, _, true) => 1 }.size == 2
        )
      }
    },
    test("ask_user 只在用户在等时可用") {
      fresh { req =>
        if userTexts(req).exists(_.contains("ask_user 用不了")) then Script.Say(List(text("那我自己定")))
        else Script.Say(List(call("q1", "ask_user", """{"questions":[{"question":"?","header":"h","options":[{"label":"A"},{"label":"B"}]}]}""")))
      } { (_, _, _) =>
        for
          alice <- agent("alice")
          d <- sessions.flatMap(_.deliver(Address.Fresh(alice, Origin.Trigger(TriggerName.wrap("daily")), "daily"), Letter(Source.Trigger("daily"), "跑一下")))
          id = d.get.session
          _  <- idle(id)
          tl <- timeline(id)
        yield assertTrue(tl.flatMap(_.parts.flatMap(_.call)).head.result.exists(_.isError), tl.last.parts.head.text.contains("那我自己定"))
      }
    },
    test("委派：推给用户、交回发起方、等回答") {
      fresh { req =>
        val last = lastUserText(req)
        if isAgent(req, "bob") then Script.Say(List(text(s"bob 答：${last.linesIterator.toList.last}")))
        else if last.startsWith("[委派回报]") then Script.Say(List(text("收到回报")))
        else if last.startsWith("bob 答：") then Script.Say(List(text(s"alice 转述：$last")))
        else if last == "通知" then Script.Say(List(call("d1", "delegate_task", """{"agent":"bob","task":"任务一","thread":"t1"}""")))
        else if last == "回报" then Script.Say(List(call("d2", "delegate_task", """{"agent":"bob","task":"任务二","thread":"t2","report_back":true}""")))
        else if last == "等" then Script.Say(List(call("d3", "ask_agent", """{"agent":"bob","message":"任务三","thread":"t3"}""")))
        else Script.Say(List(text("好")))
      } { (_, _, _) =>
        for
          events <- recordEvents
          alice  <- agent("alice")
          _      <- agent("bob")
          // 1. 不回报：回答推给用户
          id1 <- userSession(alice, "通知")
          t1  <- eventually("thread t1")(sessions.flatMap(_.list(None, None, None, 50)))(_.items.exists(_.originRef.contains("t1"))).map(_.items.find(_.originRef.contains("t1")).get.id)
          _   <- idle(t1) *> idle(id1)
          e1  <- eventually("t1 交回用户")(events.get)(_.exists {
                   case SessionEvent.Settled(`t1`, _, _, Outcome.Answered(a), true) => a == "bob 答：任务一"
                   case _                                                         => false
                 })
          // 2. 回报：交回发起方，期间发起方在等
          id2 <- userSession(alice, "回报")
          _   <- eventually("alice 回报后回答")(timeline(id2))(_.exists(_.parts.exists(_.text.contains("收到回报"))))
          _   <- idle(id2)
          tl2 <- timeline(id2)
          // 3. 等回答：bob 的回答就是调用的结果
          id3 <- userSession(alice, "等")
          _   <- eventually("alice 转述")(timeline(id3))(_.exists(_.parts.exists(_.text.exists(_.startsWith("alice 转述")))))
          _   <- idle(id3)
          tl3 <- timeline(id3)
          threads <- sessions.flatMap(_.list(None, None, None, 50)).map(_.items.filter(_.origin == "thread"))
          byOrigin <- sessions.flatMap(_.list(None, None, None, 50, List("thread"))).map(_.items)
          d1      <- sessions.flatMap(_.detail(id1))
        yield assertTrue(
          byOrigin.map(_.id).toSet == threads.map(_.id).toSet,
          d1.threads.map(_.originRef.get) == List("t1"),
          tl2.exists(i => i.source.contains("EVENT") && i.text.exists(_.contains("bob 答：任务二"))),
          tl3.flatMap(_.parts.flatMap(_.call)).head.result.map(_.text).contains("bob 答：任务三"),
          tl3.last.parts.head.text.contains("alice 转述：bob 答：任务三"),
          threads.map(_.originRef.get).sorted == List("t1", "t2", "t3"),
          threads.forall(_.state == "idle")
        )
      }
    },
    test("停止：保存已生成的部分，逐层停下 thread，丢弃排队的信") {
      fresh { req =>
        if isAgent(req, "bob") then Script.Hang(List(text("bob 想到一半")))
        else if lastUserText(req) == "等 bob" then Script.Say(List(call("d1", "ask_agent", """{"agent":"bob","message":"慢慢做","thread":"slow"}""")))
        else Script.Hang(List(text("写到一半")))
      } { (_, _, _) =>
        for
          alice <- agent("alice")
          _     <- agent("bob")
          // 生成到一半停
          id1 <- userSession(alice, "写")
          _   <- eventually("开始生成")(sessions.flatMap(s => ZIO.scoped(s.subscribe(id1).map(_._1))))(_.exists(_.nonEmpty))
          _   <- say(id1, "排队的")
          _   <- sessions.flatMap(_.stop(id1))
          s1  <- stateOf(id1)
          tl1 <- timeline(id1)
          d1  <- sessions.flatMap(_.detail(id1))
          // 等别人回答时停：thread 一并停下
          id2 <- userSession(alice, "等 bob")
          th  <- eventually("bob 的 thread 在跑")(sessions.flatMap(_.list(None, None, None, 50)))(_.items.exists(i => i.originRef.contains("slow") && i.state == "running"))
                   .map(_.items.find(_.originRef.contains("slow")).get.id)
          _   <- eventually("alice 在等")(stateOf(id2))(_ == WorkState.Waiting)
          _   <- sessions.flatMap(_.stop(id2))
          s2  <- stateOf(id2)
          st  <- idle(th)
          tl2 <- timeline(id2)
          tlt <- timeline(th)
        yield assertTrue(
          s1 == WorkState.Idle,
          tl1.exists(i => i.kind == "reply" && i.interrupted && i.parts.head.text.contains("写到一半")),
          tl1.last.noticeKind.contains("Stopped"),
          d1.queued.isEmpty,
          d1.run.map(_.state).contains("stopped"),
          s2 == WorkState.Idle,
          st == WorkState.Idle,
          tl2.flatMap(_.parts.flatMap(_.call)).head.result.exists(r => r.isError && r.text.contains("被停止")),
          tlt.exists(i => i.interrupted && i.parts.head.text.contains("bob 想到一半"))
        )
      }
    },
    test("压缩：超过阈值总结较早的部分，之后带摘要和最近几步；原文保留") {
      fresh(
        req =>
          if lastUserText(req).startsWith("[平台请求：压缩上下文]") then Script.Say(List(text("摘要：前面聊了一和二")), Usage(50, 5, 0, 0))
          else Script.Say(List(text(s"回：${lastUserText(req)}")), Usage(1000, 10, 0, 0)),
        SessionSettings(compactionThresholdTokens = 500, keepRecentSteps = 1)
      ) { (m, _, _) =>
        for
          alice <- agent("alice")
          id    <- userSession(alice, "一")
          _     <- idle(id)
          _     <- say(id, "二") *> eventually("二")(timeline(id))(_.count(_.kind == "reply") == 2) *> idle(id)
          _     <- say(id, "三") *> eventually("三")(timeline(id))(_.count(_.kind == "reply") == 3) *> idle(id)
          reqs  <- m.requests
          tl    <- timeline(id)
          last   = reqs.last
          // 再手动压缩一次
          done  <- sessions.flatMap(_.compact(id))
          tl2   <- timeline(id)
        yield assertTrue(
          reqs.size == 4, // 一、二、压缩、三
          userTexts(reqs(2)).last.startsWith("[平台请求：压缩上下文]"),
          userTexts(last).head.contains("摘要：前面聊了一和二"),
          !userTexts(last).exists(_.contains("一\n")) && !userTexts(last).contains("一"),
          userTexts(last).contains("三"),
          tl.count(_.kind == "compaction") == 1,
          tl.exists(i => i.kind == "letter" && i.text.contains("一")),
          done,
          tl2.count(_.kind == "compaction") == 2
        )
      }
    },
    test("删除：忙时拒绝；thread 保留；删 agent 前会话要空闲") {
      fresh { req =>
        if isAgent(req, "bob") then Script.Say(List(text("bob 好了")))
        else if lastUserText(req) == "卡住" then Script.Hang()
        else if lastUserText(req) == "派活" then Script.Say(List(call("d1", "delegate_task", """{"agent":"bob","task":"活","thread":"keep"}""")))
        else Script.Say(List(text("好")))
      } { (_, _, _) =>
        for
          alice <- agent("alice")
          _     <- agent("bob")
          id    <- userSession(alice, "卡住")
          _     <- eventually("在跑")(stateOf(id))(_ == WorkState.Running)
          busy  <- sessions.flatMap(_.delete(id)).either
          agentBusy <- ZIO.serviceWithZIO[Agents](_.delete(alice)).either
          _     <- sessions.flatMap(_.stop(id))
          _     <- sessions.flatMap(_.delete(id))
          gone  <- sessions.flatMap(_.info(id)).either
          // 发起会话删了，thread 留下
          id2   <- userSession(alice, "派活")
          th    <- eventually("thread")(sessions.flatMap(_.list(None, None, None, 50)))(_.items.exists(_.originRef.contains("keep"))).map(_.items.find(_.originRef.contains("keep")).get.id)
          _     <- idle(th) *> idle(id2)
          _     <- sessions.flatMap(_.delete(id2))
          kept  <- sessions.flatMap(_.info(th))
          // 删 agent：会话一并删掉
          _     <- ZIO.serviceWithZIO[Agents](_.delete(alice))
          left  <- sessions.flatMap(_.list(Some(alice), None, None, 50))
        yield assertTrue(
          busy.left.exists(_.isInstanceOf[AppError.Domain]),
          agentBusy.isLeft,
          gone.isLeft,
          kept.origin == Origin.Thread(None, ThreadName.wrap("keep")),
          left.items.isEmpty
        )
      }
    },
    test("带回报的委派：心跳检查投回发起方，停下发起方时 thread 一并停下") {
      fresh { req =>
        val last = lastUserText(req)
        if isAgent(req, "bob") then Script.Hang(List(text("bob 在做")))
        else if last.startsWith("[检查]") then Script.Say(List(text("…")))
        else if last == "派" then Script.Say(List(call("d1", "delegate_task", """{"agent":"bob","task":"长活","thread":"long","report_back":true,"heartbeat_seconds":1}""")))
        else Script.Say(List(text("派出去了")))
      } { (_, _, _) =>
        for
          alice <- agent("alice")
          _     <- agent("bob")
          id    <- userSession(alice, "派")
          tl    <- eventually("收到检查")(timeline(id))(_.exists(i => i.checkIn && i.text.exists(_.contains("thread 现在运行中"))))
          _     <- eventually("回到等待")(stateOf(id))(_ == WorkState.Waiting)
          th    <- sessions.flatMap(_.list(None, None, None, 50)).map(_.items.find(_.originRef.contains("long")).get.id)
          d     <- sessions.flatMap(_.detail(id))
          _     <- sessions.flatMap(_.stop(id))
          s1    <- stateOf(id)
          s2    <- idle(th)
          d2    <- sessions.flatMap(_.detail(id))
        yield assertTrue(
          d.waits.map(_.kind) == List("delegation"),
          d.waits.head.heartbeatSeconds.contains(1),
          s1 == WorkState.Idle,
          s2 == WorkState.Idle,
          d2.waits.isEmpty
        )
      }
    },
    test("wake_me：到点作为事件投回，期间等待中") {
      fresh { req =>
        val last = lastUserText(req)
        if last.startsWith("[唤醒]") then Script.Say(List(text("醒了")))
        else if last == "一秒后叫我" then Script.Say(List(call("w1", "wake_me", """{"after_seconds":1,"note":"看看结果"}""")))
        else Script.Say(List(text("约好了")))
      } { (_, _, _) =>
        for
          alice <- agent("alice")
          id    <- userSession(alice, "一秒后叫我")
          _     <- eventually("等待中")(stateOf(id))(_ == WorkState.Waiting)
          _     <- eventually("醒了")(timeline(id))(_.exists(_.parts.exists(_.text.contains("醒了"))))
          _     <- idle(id)
          tl    <- timeline(id)
        yield assertTrue(tl.exists(i => i.source.contains("EVENT") && i.text.contains("[唤醒] 看看结果")))
      }
    },
    test("模型暂时失败退避重试，最终失败可重试") {
      for
        n <- Ref.make(0)
        r <- fresh(_ => Script.Fail(AppError.External("坏请求"))) { (m, _, _) =>
               for
                 events <- recordEvents
                 alice  <- agent("alice")
                 id     <- userSession(alice, "试试")
                 _      <- eventually("失败")(sessions.flatMap(_.detail(id)))(_.run.exists(_.state == "failed"))
                 failed <- eventually("失败通知")(events.get)(_.exists {
                             case SessionEvent.Settled(`id`, _, _, Outcome.Failed(_), true) => true
                             case _                                                     => false
                           })
                 _      <- m.setHandler(_ => Script.Say(List(text("这次好了"))))
                 _      <- sessions.flatMap(_.retry(id))
                 _      <- eventually("重试成功")(timeline(id))(_.exists(_.parts.exists(_.text.contains("这次好了"))))
                 _      <- idle(id)
                 d      <- sessions.flatMap(_.detail(id))
                 _      <- n.set(1)
               yield d
             }
      yield assertTrue(r.run.exists(_.state == "finished"))
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds) @@ TestAspect.sequential
