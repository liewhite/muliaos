package muliao.triggers

import muliao.contract.*
import muliao.platform.AppError
import muliao.resources.agents.{AgentPatch, Agents}
import muliao.sessions.Sessions
import muliao.triggers.TestEnv.*
import zio.*
import zio.test.*

import java.time.Instant

/** 定时与一次性：用测试时钟拨时间。 */
object ScheduleSpec extends ZIOSpecDefault:

  /** 起点：2026-01-01 00:00:30 UTC（上海 08:00:30），每分钟的触发器下一次在 00:01:00。 */
  private val t0 = Instant.parse("2026-01-01T00:00:30Z")

  private def minute(n: Int): Long = Instant.parse("2026-01-01T00:00:00Z").toEpochMilli + n * 60000L

  type R = TestEnv.Env

  private def fresh[A](script: muliao.models.llm.ModelRequest => Script)(body: ZIO[R & Scope, AppError, A]): ZIO[Scope, AppError, A] =
    for
      _ <- TestClock.setTime(t0)
      d <- dirs
      m <- fakeModels(script)
      p <- probe
      e <- layer(d, m, assetPoll = 1.hour).build
      a <- (start(p) *> body).provideSomeEnvironment[Scope](_ ++ e)
    yield a

  private val say: muliao.models.llm.ModelRequest => Script = _ => Script.Say(List(text("收到")))

  def spec = suite("定时与一次性（测试时钟）")(
    test("定时：到点触发、每次开新会话交出处理说明，下次到期往后推") {
      ZIO.scoped(fresh(say)(
        for
          a  <- agent("bot")
          t  <- triggers.flatMap(_.create(schedule("tick", a, "* * * * *", "整点报时")))
          _  <- TestClock.adjust(30.seconds)
          s1 <- eventually("第一次触发")(triggerSessions(a))(_.size == 1)
          _  <- eventually("第一个会话做完")(stateOf(s1.head.id))(_ == WorkState.Idle)
          v1 <- byName("tick")
          _  <- TestClock.adjust(60.seconds)
          s2 <- eventually("第二次触发")(triggerSessions(a))(_.size == 2)
          tl <- ZIO.serviceWithZIO[Sessions](_.timeline(s2.last.id))
        yield assertTrue(
          t.nextAt.contains(minute(1)),
          v1.nextAt.contains(minute(2)),
          v1.lastSession.contains(s1.head.id),
          s2.forall(s => s.origin == Origin.Trigger(TriggerName.wrap("tick")) && s.title == "tick"),
          tl.toString.contains("整点报时")
        )
      ))
    },
    test("防重叠：上次的会话在等待就跳过并计数，只通知用户一次；它做完后照常触发、计数清零") {
      ZIO.scoped(fresh(_ => Script.Say(List(call("c1", "approve_me", "{}"))))(
        for
          a  <- agent("bot")
          _  <- triggers.flatMap(_.create(schedule("tick", a, "* * * * *")))
          _  <- TestClock.adjust(30.seconds)
          s1 <- eventually("第一次触发")(triggerSessions(a))(_.size == 1).map(_.head.id)
          _  <- eventually("会话在等批准")(stateOf(s1))(_ == WorkState.Waiting)
          _  <- TestClock.adjust(60.seconds)
          v1 <- eventually("跳过一次")(byName("tick"))(_.skipped == 1)
          n1 <- eventually("通知挡路")(inbox)(_.nonEmpty)
          _  <- TestClock.adjust(60.seconds)
          v2 <- eventually("跳过两次")(byName("tick"))(_.skipped == 2)
          _  <- pause(200.millis)
          n2 <- inbox
          _  <- ZIO.serviceWithZIO[Sessions](_.stop(s1))
          _  <- eventually("停下")(stateOf(s1))(_ == WorkState.Idle)
          _  <- TestClock.adjust(60.seconds)
          ss <- eventually("挡路的停了，照常触发")(triggerSessions(a))(_.size == 2)
          v3 <- byName("tick")
        yield assertTrue(
          v1.lastSession.contains(s1),
          n1.size == 1,
          n1.head.title.contains("被挡住"),
          n1.head.session.contains(s1),
          v2.skipped == 2,
          n2.size == 1,
          v3.skipped == 0,
          v3.lastSession.contains(ss.last.id)
        )
      ))
    },
    test("防重叠：上次的会话还在跑也跳过，但不通知") {
      ZIO.scoped(fresh(_ => Script.Hang())(
        for
          a <- agent("bot")
          _ <- triggers.flatMap(_.create(schedule("tick", a, "* * * * *")))
          _ <- TestClock.adjust(30.seconds)
          s <- eventually("第一次触发")(triggerSessions(a))(_.size == 1).map(_.head.id)
          _ <- eventually("在跑")(stateOf(s))(_ == WorkState.Running)
          _ <- TestClock.adjust(60.seconds)
          v <- eventually("跳过")(byName("tick"))(_.skipped == 1)
          n <- inbox
          _ <- ZIO.serviceWithZIO[Sessions](_.stop(s))
        yield assertTrue(n.isEmpty, v.lastSession.contains(s))
      ))
    },
    test("停机期间错过的只补一次；停用期间错过的不补") {
      for
        _  <- TestClock.setTime(t0)
        d  <- dirs
        m  <- fakeModels(say)
        p  <- probe
        a <- ZIO.scoped(
               layer(d, m, assetPoll = 1.hour).build.flatMap(e =>
                 (start(p) *> agent("bot").tap(a => triggers.flatMap(_.create(schedule("tick", a, "* * * * *"))))).provideSomeEnvironment[Scope](_ ++ e)
               )
             )
        _ <- TestClock.adjust(10.minutes) // 停机 10 分钟：错过 10 次
        r <- ZIO.scoped(
               layer(d, m, assetPoll = 1.hour).build.flatMap(e =>
                 (for
                   _  <- start(p)
                   s1 <- eventually("重启后补一次")(triggerSessions(a))(_.size == 1)
                   _  <- pause(300.millis)
                   s2 <- triggerSessions(a)
                   v1 <- byName("tick")
                   tg <- triggers
                   _  <- eventually("会话做完")(stateOf(s1.head.id))(_ == WorkState.Idle)
                   _  <- tg.update(v1.id, TriggerPatch(enabled = Some(false)))
                   _  <- TestClock.adjust(5.minutes) // 停用 5 分钟
                   v2 <- tg.update(v1.id, TriggerPatch(enabled = Some(true)))
                   _  <- pause(300.millis)
                   s3 <- triggerSessions(a)
                   // 改配置时可以换种类：按新种类重算下次到期
                   v3 <- tg.update(v1.id, TriggerPatch(config = Some(TriggerConfig.Once("2026-01-01T00:30:00Z"))))
                 yield assertTrue(
                   v3.config == TriggerConfig.Once("2026-01-01T08:30+08:00"),
                   v3.nextAt.contains(minute(30)),
                   v3.enabled,
                   s2.size == 1,
                   v1.nextAt.contains(minute(11)),
                   v2.nextAt.contains(minute(16)),
                   s3.size == 1
                 )).provideSomeEnvironment[Scope](_ ++ e)
               )
             )
      yield r
    },
    test("一次性：到点触发后结束，进已结束列表，不能再改或运行；时刻必须在将来") {
      ZIO.scoped(fresh(say)(
        for
          a    <- agent("bot")
          tg   <- triggers
          past <- tg.create(NewTrigger("late", a, TriggerConfig.Once("2025-12-31T00:00:00Z"), "迟了")).either
          t    <- tg.create(NewTrigger("remind", a, TriggerConfig.Once("2026-01-01T08:02:00"), "提醒一下"))
          _    <- TestClock.adjust(2.minutes)
          s    <- eventually("到点触发")(triggerSessions(a))(_.size == 1)
          v    <- eventually("结束")(tg.view(t.id))(_.finishedAt.nonEmpty)
          live <- tg.list(finished = false)
          done <- tg.list(finished = true)
          run  <- tg.run(t.id).either
          upd  <- tg.update(t.id, TriggerPatch(prompt = Some("x"))).either
          same <- tg.create(NewTrigger("remind", a, TriggerConfig.Once("2026-01-02T08:00:00"), "名字可以重用"))
        yield assertTrue(
          past.left.exists(_.message.contains("已经过去")),
          t.nextAt.contains(minute(2)),
          v.lastSession.contains(s.head.id),
          !live.exists(_.id == t.id),
          done.exists(_.id == t.id),
          run.isLeft,
          upd.isLeft,
          same.id != t.id
        )
      ))
    },
    test("agent 已归档：一次性到点推送用户说没做，定时的跳过") {
      ZIO.scoped(fresh(say)(
        for
          a  <- agent("old")
          tg <- triggers
          _  <- tg.create(NewTrigger("remind", a, TriggerConfig.Once("2026-01-01T08:02:00"), "提醒"))
          _  <- tg.create(schedule("tick", a, "* * * * *"))
          _  <- ZIO.serviceWithZIO[Agents](_.update(a, AgentPatch(archived = Some(true))))
          _  <- TestClock.adjust(2.minutes)
          n  <- eventually("推送没做")(inbox)(_.nonEmpty)
          _  <- pause(200.millis)
          ss <- triggerSessions(a)
          v  <- byName("tick")
        yield assertTrue(n.size == 1, n.head.title.contains("remind"), n.head.body.contains("已归档"), ss.isEmpty, v.nextAt.contains(minute(3)))
      ))
    }
  ) @@ TestAspect.timeout(2.minutes)
