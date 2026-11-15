package muliao.triggers

import muliao.contract.*
import muliao.platform.AppError
import muliao.sessions.SessionHooks
import muliao.triggers.TestEnv.*
import zio.*
import zio.test.*

import java.util.concurrent.atomic.AtomicInteger

/** 前置检查与常驻流：真的子进程、真实时钟。 */
object CommandSpec extends ZIOSpecDefault:

  type R = TestEnv.Env

  private def fresh[A](
      script: muliao.models.llm.ModelRequest => Script,
      settings: TriggerSettings = TriggerSettings()
  )(body: (S.FakeModels, S.Probe) => ZIO[R & Scope, AppError, A]): ZIO[Scope, AppError, A] =
    for
      d <- dirs
      m <- fakeModels(script)
      p <- probe
      e <- layer(d, m, settings).build
      a <- (start(p) *> body(m, p)).provideSomeEnvironment[Scope](_ ++ e)
    yield a

  private object S:
    type FakeModels = muliao.sessions.TestEnv.FakeModels
    type Probe      = muliao.sessions.TestEnv.Probe

  private val say: muliao.models.llm.ModelRequest => Script = _ => Script.Say(List(text("收到")))

  /** 交给模型的全部用户方文字。 */
  private def seen(m: S.FakeModels): UIO[List[String]] = m.requests.map(_.toList.flatMap(userTexts))

  /** 一年才一次的定时：只用立即运行驱动。 */
  private val yearly = "0 0 1 1 *"

  def spec = suite("前置检查与常驻流（真实进程）")(
    test("前置检查 output：没输出不触发；有输出就触发，输出作为事件交给 agent") {
      ZIO.scoped(fresh(say)((m, _) =>
        for
          a  <- agent("bot")
          ws <- workspaceOf(a)
          tg <- triggers
          t  <- tg.create(schedule("check", a, yearly, "看看", Some(PrecheckSpec("cat flag 2>/dev/null; true"))))
          r1 <- tg.run(t.id)
          _  <- write(ws.resolve("flag"), "hello\n")
          r2 <- tg.run(t.id)
          _  <- eventually("agent 收到事件")(seen(m))(_.exists(_.contains("<event>\nhello\n</event>")))
          v  <- tg.view(t.id)
        yield assertTrue(r1.session.isEmpty, r1.note.contains("没事"), r2.session.nonEmpty, v.lastSession == r2.session, v.faults.isEmpty)
      ))
    },
    test("前置检查 changed：第一次只记基线，没变不触发，变了附 diff 与当前输出") {
      ZIO.scoped(fresh(say)((m, _) =>
        for
          a  <- agent("bot")
          ws <- workspaceOf(a)
          tg <- triggers
          _  <- write(ws.resolve("state.txt"), "a\n")
          t  <- tg.create(schedule("watch", a, yearly, "变了", Some(PrecheckSpec("cat state.txt", FireWhen.Changed))))
          r1 <- tg.run(t.id)
          r2 <- tg.run(t.id)
          _  <- write(ws.resolve("state.txt"), "a\nb\n")
          r3 <- tg.run(t.id)
          _  <- eventually("agent 收到 diff")(seen(m))(_.exists(_.contains("+ b\n\n当前输出：\na\nb")))
          r4 <- eventually("会话做完后再查")(tg.run(t.id))(r => !r.note.contains("没做完"))
        yield assertTrue(r1.note.contains("基线"), r2.note.contains("没事"), r3.session.nonEmpty, r4.note.contains("没事"))
      ))
    },
    test("前置检查失败（pipefail 生效）或超时：立即运行只报给调用方，不记故障、不开告警会话") {
      ZIO.scoped(fresh(say, TriggerSettings(precheckTimeoutSeconds = 1))((_, _) =>
        for
          a  <- agent("bot")
          tg <- triggers
          t1 <- tg.create(schedule("pipe", a, yearly, "x", Some(PrecheckSpec("false | cat"))))
          t2 <- tg.create(schedule("slow", a, yearly, "x", Some(PrecheckSpec("sleep 5; echo late"))))
          r1 <- tg.run(t1.id)
          c0 <- Clock.nanoTime
          r2 <- tg.run(t2.id)
          c1 <- Clock.nanoTime
          v  <- tg.list(finished = false)
          ss <- triggerSessions(a)
        yield assertTrue(
          r1.session.isEmpty,
          r1.note.contains("前置检查失败：退出码 1"),
          r2.note.contains("超过 1 秒"),
          (c1 - c0) < 4_000_000_000L,
          v.forall(_.faults.isEmpty),
          ss.isEmpty
        )
      ))
    },
    test("常驻流的前置检查失败记为故障，只告警一次（给所属 agent 开会话）；检查恢复后故障清除") {
      ZIO.scoped(fresh(say)((_, _) =>
        for
          a  <- agent("bot")
          ws <- workspaceOf(a)
          tg <- triggers
          t  <- tg.create(streamOf("ticks", a, "while true; do echo tick; sleep 0.2; done", 0.1, Some(PrecheckSpec("cat > /dev/null; test -f ok"))))
          v1 <- eventually("记下故障")(tg.view(t.id))(_.faults.nonEmpty)
          s1 <- eventually("告警会话")(triggerSessions(a))(_.nonEmpty)
          _  <- pause(1.second)
          s2 <- triggerSessions(a)
          _  <- write(ws.resolve("ok"), "")
          v2 <- eventually("恢复后清除")(tg.view(t.id))(_.faults.isEmpty)
          _  <- tg.delete(t.id)
        yield assertTrue(
          v1.faults.map(_.source) == List(FaultSource.Precheck),
          v1.faults.head.error.contains("退出码 1"),
          s1.head.title.contains("出错"),
          s2.size == 1,
          v2.runningSince.nonEmpty
        )
      ))
    },
    test("常驻流：防抖把相邻的行合成一次；会话没做完时事件排队，做完后合并成一次触发") {
      val calls = AtomicInteger(0)
      val script: muliao.models.llm.ModelRequest => Script = _ =>
        if calls.incrementAndGet() == 1 then Script.Say(List(call("b1", "block", "{}"))) else Script.Say(List(text("好")))
      ZIO.scoped(fresh(script)((m, p) =>
        for
          a  <- agent("bot")
          tg <- triggers
          _  <- tg.create(streamOf("feed", a, "echo a; echo b; sleep 1.5; echo c; echo d; sleep 60", 0.5))
          s1 <- eventually("第一次触发")(triggerSessions(a))(_.size == 1)
          _  <- eventually("在跑")(stateOf(s1.head.id))(_ == WorkState.Running)
          _  <- pause(2500.millis)
          q  <- triggerSessions(a)
          _  <- p.gate.succeed(())
          s2 <- eventually("做完后合并触发")(triggerSessions(a))(_.size == 2)
          _  <- eventually("第二次收到排队的事件")(seen(m))(_.exists(_.contains("<event>\nc\nd\n</event>")))
          xs <- seen(m)
          v  <- byName("feed")
        yield assertTrue(
          xs.exists(_.contains("<event>\na\nb\n</event>")),
          q.size == 1,
          v.lastSession.contains(s2.last.id),
          v.runningSince.nonEmpty
        )
      ))
    },
    test("常驻流：退出后退避重启，连续很快退出记故障并告警；跑稳后故障清除；停用就结束命令；标准错误进日志") {
      ZIO.scoped(fresh(say)((_, _) =>
        for
          a   <- agent("bot")
          ws  <- workspaceOf(a)
          tg  <- triggers
          t   <- tg.create(streamOf("flaky", a, "echo started >&2; if [ -f stable ]; then sleep 30.0879; else exit 3; fi"))
          v1  <- eventually("记下故障")(tg.view(t.id))(_.faults.exists(_.source == FaultSource.Stream))
          al  <- eventually("告警会话")(triggerSessions(a))(_.nonEmpty)
          log <- eventually("标准错误进日志")(read(ws.resolve(Streams.logName(TriggerName.wrap("flaky")))))(_.linesIterator.count(_ == "started") >= 3)
          _   <- write(ws.resolve("stable"), "")
          v2  <- eventually("跑稳后清除")(tg.view(t.id))(v => v.faults.isEmpty && v.runningSince.nonEmpty)
          _   <- tg.update(t.id, TriggerPatch(enabled = Some(false)))
          v3  <- eventually("停用后结束")(tg.view(t.id))(_.runningSince.isEmpty)
          ps  <- eventually("命令进程结束")(ZIO.attemptBlocking(scala.sys.process.Process(Seq("pgrep", "-f", s"[s]leep 30.0879")).lazyLines_!.toList).orDie)(_.isEmpty)
        yield assertTrue(
          v1.faults.head.error.contains("退出码 3"),
          al.head.title.contains("出错"),
          log.linesIterator.count(_ == "started") >= 3,
          log.contains("[muliao] 命令结束（退出码 3）"),
          v2.faults.isEmpty,
          !v3.enabled,
          ps.isEmpty
        )
      ))
    },
    test("工具：kinds、建（各种写错都说清楚）、列、改、立即运行、删；删 agent 时一并删掉它的触发器") {
      ZIO.scoped(fresh(say)((_, _) =>
        for
          a     <- agent("bot")
          b     <- agent("helper")
          ctx   <- muliao.workspace.TestEnv.context(a)
          tools <- ZIO.serviceWithZIO[SessionHooks](_.tools)
          call   = (name: String, args: String) =>
                     ZIO.succeed(tools.find(_.spec.name == name).get).flatMap(t =>
                       t.invoke(zio.json.ast.Json.decoder.decodeJson(args).toOption.get, ctx).either.map(muliao.workspace.TestEnv.text)
                     )
          kinds <- call("trigger_kinds", "{}")
          bad1  <- call("trigger_create", """{"name":"x","kind":"schedule","prompt":"p","config":{"cron":"* * *"}}""")
          bad2  <- call("trigger_create", """{"name":"x","kind":"schedule","prompt":"p","config":{"cron":"* * * * *","command":"ls"}}""")
          bad3  <- call("trigger_create", """{"name":"x","kind":"once","prompt":"p","config":{"at":"2000-01-01T00:00"}}""")
          bad4  <- call("trigger_create", """{"name":"X_1","kind":"schedule","prompt":"p","config":{"cron":"* * * * *"}}""")
          ok    <- call("trigger_create", """{"name":"daily","kind":"schedule","prompt":"每天","config":{"cron":"0 9 * * *"},"agent":"helper","precheck":{"command":"echo hi"}}""")
          dup   <- call("trigger_create", """{"name":"daily","kind":"schedule","prompt":"p","config":{"cron":"0 9 * * *"}}""")
          list  <- call("trigger_list", "{}")
          upd   <- call("trigger_update", """{"name":"daily","enabled":false,"remove_precheck":true}""")
          run   <- call("trigger_run", """{"name":"daily"}""")
          v     <- byName("daily")
          _     <- call("trigger_create", """{"name":"mine","kind":"stream","prompt":"p","config":{"command":"sleep 30"}}""")
          del   <- call("trigger_delete", """{"name":"daily"}""")
          gone  <- byName("daily").either
          _     <- ZIO.serviceWithZIO[muliao.resources.agents.Agents](_.delete(a))
          left  <- triggers.flatMap(_.list(finished = false))
        yield assertTrue(
          kinds.contains("schedule") && kinds.contains("once") && kinds.contains("stream"),
          bad1.startsWith("[rejected]") && bad1.contains("cron"),
          bad2.contains("不用 command"),
          bad3.contains("已经过去"),
          bad4.contains("小写字母"),
          ok.contains("已创建触发器 daily") && ok.contains("helper") && ok.contains("下次触发"),
          dup.contains("已有触发器"),
          list.contains("\"next_run_at\"") && list.contains("\"fire_when\" : \"output\""),
          upd.contains("停用"),
          v.agent == b,
          v.precheck.isEmpty,
          run.contains("已触发"),
          del.contains("已删除"),
          gone.isLeft,
          left.isEmpty
        )
      ))
    },
    test("HTTP：建、列、种类、改、立即运行、删") {
      ZIO.scoped(fresh(say)((_, _) =>
        for
          a      <- agent("bot")
          http   <- ZIO.service[TriggersHttp]
          bus    <- ZIO.service[muliao.platform.events.ChangeBus]
          settings = muliao.platform.server.ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
          auth   <- muliao.platform.server.Auth.layer.build.map(_.get[muliao.platform.server.Auth]).provideSomeLayer[Scope](ZLayer.succeed(settings))
          port   <- muliao.platform.server.HttpServer.serve(settings, auth, bus, http.endpoints(auth), "test").orDie
          c       = muliao.sessions.SessionsHttpSpec.Client(s"http://127.0.0.1:$port")
          _      <- c.call("POST", "/api/auth/login", Some("""{"password":"pw"}""")).orDie
          made   <- c.call("POST", "/api/triggers", Some(s"""{"name":"web","agent":"${a.value}","kind":"schedule","config":{"cron":"0 9 * * *"},"prompt":"p"}""")).orDie
          id      = zio.json.ast.Json.decoder.decodeJson(made.body).toOption.flatMap(_.asObject).flatMap(_.get("id")).map(_.toString).getOrElse("?")
          list   <- c.call("GET", "/api/triggers").orDie
          kinds  <- c.call("GET", "/api/triggers/kinds").orDie
          patch  <- c.call("PATCH", s"/api/triggers/$id", Some("""{"enabled":false}""")).orDie
          bad    <- c.call("PATCH", s"/api/triggers/$id", Some("""{"config":{"at":"2030-01-01T00:00"}}""")).orDie
          run    <- c.call("POST", s"/api/triggers/$id/run").orDie
          del    <- c.call("DELETE", s"/api/triggers/$id").orDie
          after  <- c.call("GET", "/api/triggers").orDie
        yield assertTrue(
          made.status == 200,
          list.body.contains("\"name\":\"web\""),
          kinds.body.contains("stream"),
          patch.body.contains("\"enabled\":false"),
          bad.status == 400,
          run.status == 200 && run.body.contains("已触发"),
          del.status == 204,
          after.body == "[]"
        )
      ))
    }
  ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.timeout(3.minutes)
