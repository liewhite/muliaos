package muliao.web.pages.sessions

import muliao.contract.*
import muliao.web.core.{Format, MathText}
import zio.test.*

import Model.*

object SessionModelSpec extends ZIOSpecDefault:
  private def call(id: String, name: String = "run_shell", state: CallState = CallState.Done, result: Option[ResultView] = Some(ResultView("ok", Nil, false))) =
    CallView(CallId.wrap(id), name, "{}", Display.Plain, state, result, None, Some(1000L), Some(2000L))
  private def reply(id: Long, parts: PartView*) = TimelineItem(id, Some(1L), 0, 0, EntryView.Reply(parts.toList, "m", UsageView(0, 0, 0, 0), interrupted = false))
  private def tool(c: CallView)                 = PartView.Call(c)
  private def text(t: String)                   = PartView.Text(t)
  private def checkIn(at: Long)                 = TimelineItem(at, None, at, at, EntryView.Letter(SourceView.Event(EventKind.Job), s"c$at", Nil, checkIn = true))
  private val noThreads: CallView => Option[SessionSummary] = _ => None

  def spec = suite("会话页的纯逻辑")(
    test("公式：\\( \\) 与 \\[ \\] 换成 $，价格里的 $ 转义，代码不动") {
      assertTrue(
        MathText.normalize("面积 \\(a^2\\) 元") == "面积 $a^2$ 元",
        MathText.normalize("$20,000 和 $30,000") == "\\$20,000 和 \\$30,000",
        MathText.normalize("引用 \\[1\\]") == "引用 \\[1\\]",
        MathText.normalize("```\n$x$ \\(y\\)\n```") == "```\n$x$ \\(y\\)\n```",
        MathText.normalize("`$a$` 与 $b$") == "`$a$` 与 $b$"
      )
    },
    test("连续的工具调用合成一组，回答把组断开") {
      val items = List(reply(1, tool(call("a")), tool(call("b"))), reply(2, text("好了")), reply(3, tool(call("c"))))
      val bs    = groupSteps(items.flatMap(blocks), noThreads)
      assertTrue(
        bs.map(_.productPrefix) == List("Process", "Assistant", "Process"),
        bs.head.asInstanceOf[Block.Process].steps.size == 2,
        stepsLabel(bs.head.asInstanceOf[Block.Process].steps) == "2 次工具调用"
      )
    },
    test("同一来源连续的运行中检查合成一条，占位答复一并收起") {
      val items = List(checkIn(1), reply(2, text("…")), checkIn(3), reply(4, text("做完了")))
      val bs    = foldCheckIns(groupSteps(items.flatMap(blocks), noThreads))
      assertTrue(
        bs.size == 2,
        bs.head == Block.CheckIns(EventKind.Job, List("c1" -> 1L, "c3" -> 3L)),
        bs(1) == Block.Assistant("做完了")
      )
    },
    test("工具状态：退回、完成；没结果的在当前运行里是运行中，历史里是未完成") {
      assertTrue(
        toolStatus(call("a", result = Some(ResultView("x", Nil, true))), live = false, runActive = false) == ToolStatus.Rejected,
        toolStatus(call("a"), live = false, runActive = false) == ToolStatus.Done,
        toolStatus(call("a", state = CallState.Running, result = None), live = true, runActive = false) == ToolStatus.Running,
        toolStatus(call("a", state = CallState.Running, result = None), live = false, runActive = false) == ToolStatus.Unfinished
      )
    },
    test("实时回复：开始清空、分段增长；时间线已包含时算存进库了") {
      val r = List(
        LiveView.Begin,
        LiveView.Part(0, text("")),
        LiveView.Grew(0, Growth.Text, "你好"),
        LiveView.Grew(0, Growth.Text, "，世界")
      ).foldLeft(LiveReply(Vector(text("旧的")), covered = false))(LiveReply.step)
      assertTrue(
        r.parts == Vector(text("你好，世界")),
        LiveReply.coveredBy(r, List(reply(9, text("你好，世界。")))),
        !LiveReply.coveredBy(r, List(reply(9, text("别的"))))
      )
    },
    test("token 缩写与转写拼接") {
      assertTrue(
        Format.tokens(999) == "999",
        Format.tokens(1500) == "1.5K",
        Format.tokens(23456) == "23K",
        Format.tokens(2300000) == "2.3M",
        appendTranscript("你好  ", "世界") == "你好 世界",
        appendTranscript("", "世界") == "世界"
      )
    }
  )
