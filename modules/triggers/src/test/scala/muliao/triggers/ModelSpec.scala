package muliao.triggers

import muliao.contract.*
import zio.*
import zio.test.*

import java.time.{Instant, ZoneId}

/** 纯函数：cron、配置解读、前置检查的结论、diff。 */
object ModelSpec extends ZIOSpecDefault:
  private val shanghai = ZoneId.of("Asia/Shanghai")

  def spec = suite("触发器的类型与纯函数")(
    test("cron 按配置的时区解释：每天 9 点在上海是 UTC 1 点；写错报出来") {
      val c    = CronSpec.parse("0 9 * * *").toOption.get
      val next = c.next(Instant.parse("2026-01-01T02:00:00Z"), shanghai)
      assertTrue(
        next.contains(Instant.parse("2026-01-02T01:00:00Z")),
        CronSpec.parse("0 9 * *").isLeft,
        CronSpec.parse("61 * * * *").isLeft
      )
    },
    test("配置按种类解读：只认这个种类的字段，缺的、多的都报；不带时区的时刻按服务器时区") {
      val once = Mode.parse(Kind.Once, TriggerConfig(at = Some("2026-10-22T10:30")), shanghai)
      val extra = Mode.parse(Kind.Schedule, TriggerConfig(cron = Some("* * * * *"), command = Some("x")), shanghai)
      val missing = Mode.parse(Kind.Stream, TriggerConfig(), shanghai)
      val stream = Mode.parse(Kind.Stream, TriggerConfig(command = Some("tail -f x"), debounceSeconds = Some(1.5)), shanghai)
      assertTrue(
        once == Right(Mode.Once(Instant.parse("2026-10-22T02:30:00Z"))),
        extra.left.exists(_.contains("不用 command")),
        missing.left.exists(_.contains("缺少 command")),
        stream == Right(Mode.Stream("tail -f x", Nil, 1500.millis)),
        Kind.parse("cron").isLeft,
        TriggerName.make("Daily_Report").isLeft,
        TriggerName.make("daily-report-2").isRight
      )
    },
    test("配置写回再读一样（库里存的就是它）") {
      val modes = List(
        Mode.parse(Kind.Schedule, TriggerConfig(cron = Some("*/5 * * * *")), shanghai),
        Mode.parse(Kind.Once, TriggerConfig(at = Some("2026-10-22T10:30+08:00")), shanghai),
        Mode.parse(Kind.Stream, TriggerConfig(command = Some("c"), secrets = Some(List("K")), debounceSeconds = Some(2)), shanghai)
      ).map(_.toOption.get)
      val again = modes.map(m => Mode.parse(m.kind, m.config(shanghai), shanghai).toOption.get)
      assertTrue(again.map(_.config(shanghai)) == modes.map(_.config(shanghai)))
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
