package muliao.triggers

import muliao.contract.*
import zio.*
import zio.test.*

import java.time.{Instant, ZoneId}

/** 纯函数：cron、配置解读、前置检查的结论、diff。 */
object ModelSpec extends ZIOSpecDefault:
  private val shanghai = ZoneId.of("Asia/Shanghai")

  def spec = suite("触发器的类型与纯函数")(
    test("常驻命令重起的退避：按连续没跑稳的次数翻倍到上限（与已经重起了多久无关）") {
      val s   = Streams.restart(Tuning(restartFirst = 1.second, restartMax = 5.seconds))
      val now = java.time.OffsetDateTime.parse("2026-01-01T00:00:00Z")
      // 每次重起前等几秒；输入是连续没跑稳的次数（跑稳后重新从 1 算）。时刻拉开，确认不按累计时长重置
      val runs = List(1, 2, 3, 4, 5, 1, 2, 3)
      ZIO
        .foldLeft(runs.zipWithIndex)((s.initial, List.empty[Long])) { case ((state, acc), (n, i)) =>
          val at = now.plusMinutes(10L * i)
          s.step(at, n, state).map {
            case (next, _, Schedule.Decision.Continue(iv)) => (next, acc :+ java.time.Duration.between(at, iv.start).getSeconds)
            case (next, _, Schedule.Decision.Done)         => (next, acc :+ -1L)
          }
        }
        .map((_, waits) => assertTrue(waits == List(1L, 2L, 4L, 5L, 5L, 1L, 2L, 4L)))
    },
    test("cron 按配置的时区解释：每天 9 点在上海是 UTC 1 点；写错报出来") {
      val c    = CronSpec.parse("0 9 * * *").toOption.get
      val next = c.next(Instant.parse("2026-01-01T02:00:00Z"), shanghai)
      assertTrue(
        next.contains(Instant.parse("2026-01-02T01:00:00Z")),
        CronSpec.parse("0 9 * *").isLeft,
        CronSpec.parse("61 * * * *").isLeft
      )
    },
    test("给模型的平铺配置按种类读：只认这个种类的字段，缺的、多的都报") {
      import TriggerTools.ConfigIn
      assertTrue(
        ConfigIn(cron = Some("* * * * *"), command = Some("x")).toConfig(TriggerKind.Schedule).left.exists(_.contains("不用 command")),
        ConfigIn().toConfig(TriggerKind.Stream).left.exists(_.contains("缺少 command")),
        ConfigIn(command = Some("c")).toConfig(TriggerKind.Stream) == Right(TriggerConfig.Stream("c", Nil, 5.0)),
        ConfigIn.of(TriggerConfig.Stream("c", List("K"), 2)).toConfig(TriggerKind.Stream) == Right(TriggerConfig.Stream("c", List("K"), 2))
      )
    },
    test("配置解读：不带时区的时刻按服务器时区；解读出的是停用的") {
      val once   = Mode.parse(TriggerConfig.Once("2026-10-22T10:30"), shanghai)
      val stream = Mode.parse(TriggerConfig.Stream("tail -f x", Nil, 1.5), shanghai)
      assertTrue(
        once == Right(Mode.Once(Instant.parse("2026-10-22T02:30:00Z"), OnceState.Off)),
        stream == Right(Mode.Stream("tail -f x", Nil, 1500.millis, on = false)),
        Mode.parse(TriggerConfig.Stream(" ", Nil, 1), shanghai).isLeft,
        TriggerKind.parse("cron").isLeft,
        TriggerName.make("Daily_Report").isLeft,
        TriggerName.make("daily-report-2").isRight
      )
    },
    test("配置写回再读一样，JSON 里带 kind（库里存的就是它）") {
      val configs = List(TriggerConfig.Schedule("*/5 * * * *"), TriggerConfig.Once("2026-10-22T10:30+08:00"), TriggerConfig.Stream("c", List("K"), 2))
      val again   = configs.map(c => Mode.parse(c, shanghai).toOption.get.config(shanghai))
      import zio.json.*
      assertTrue(
        again == configs,
        configs.map(_.toJson.fromJson[TriggerConfig]) == configs.map(Right(_)),
        (TriggerConfig.Schedule("* * * * *"): TriggerConfig).toJson.contains("\"kind\":\"Schedule\"")
      )
    },
    test("开关与进度：启用从此刻算下次，到期定时推进、一次性结束，结束了的不再变；库里的列还原成同样的状态") {
      val now   = Instant.parse("2026-01-01T00:00:30Z")
      val sched = Mode.parse(TriggerConfig.Schedule("* * * * *"), shanghai).toOption.get.switched(true, now, shanghai)
      val due   = Instant.parse("2026-01-01T00:01:00Z").toEpochMilli
      val once  = Mode.parse(TriggerConfig.Once("2026-01-01T08:02:00"), shanghai).toOption.get.switched(true, now, shanghai)
      val done  = once.consumed(due, shanghai).get
      assertTrue(
        sched.nextAt.contains(due),
        sched.consumed(due, shanghai).flatMap(_.nextAt).contains(due + 60000),
        sched.switched(false, now, shanghai).nextAt.isEmpty,
        sched.switched(false, now, shanghai).consumed(due, shanghai).isEmpty,
        done.finishedAt.contains(due),
        !done.enabled,
        done.switched(true, now, shanghai) == done,
        done.consumed(due, shanghai).isEmpty,
        CronSpec.parse("*/5 * * * *") == CronSpec.parse(" */5 * * * * "),
        Mode.stored(TriggerConfig.Once("2026-01-01T08:02:00"), enabled = true, None, Some(due), shanghai) == Right(done),
        Mode.stored(TriggerConfig.Schedule("* * * * *"), enabled = true, Some(due), None, shanghai) == Right(sched)
      )
    },
    test("前置检查：output 有输出才触发；changed 第一次记基线、没变不触发、变了附 diff 和当前输出") {
      assertTrue(
        Checks.decide(FireWhen.Output, None, "  \n") == Verdict.Quiet,
        Checks.decide(FireWhen.Output, None, "x\n") == Verdict.Fire("x", None),
        Checks.decide(FireWhen.Changed, None, "a\n") == Verdict.Baseline("a"),
        Checks.decide(FireWhen.Changed, Some("a"), "a\n") == Verdict.Quiet,
        Checks.decide(FireWhen.Changed, Some("a\nb"), "a\nc") == Verdict.Fire("- b\n+ c\n\n当前输出：\na\nc", Some("a\nc"))
      )
    },
    test("diff 只列变化的行") {
      assertTrue(
        LineDiff("a\nb\nc", "a\nc\nd") == "- b\n+ d",
        LineDiff("", "x") == "+ x",
        LineDiff("same", "same") == ""
      )
    }
  )
