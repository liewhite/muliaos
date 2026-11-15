package muliao.migrate

import muliao.contract.{AgentId, AgentName, CallId, CallState, FileId, IssueId, OriginKind, RunState, SessionId}
import muliao.sessions.*
import muliao.sessions.store.{EntryRow, SessionRow}
import zio.Chunk
import zio.json.*
import zio.test.*

object MigrateSpec extends ZIOSpecDefault:

  private val session = Old.Session("s1", "a1", "#7 修 bug", "issue", Some("7"), None, None, 1000L, 2000L)
  private val origin  = Transcript.origin(session, Map.empty, Map.empty)
  private val files   = Map("f1" -> FileInfo("ab" * 32, "image/png", "x.png"))

  private def msg(id: Long, run: Long, json: String) = Old.Message(id, "s1", run, json, 1000L + id)

  private val assistantWithCall =
    """{"role":"assistant","protocol":"anthropic","model":"claude-x","finish_reason":"tool_use",
      |"usage":{"input_tokens":10,"output_tokens":2,"cache_read_tokens":5,"cache_write_tokens":1},
      |"parts":[{"type":"thinking","text":"想","meta":{"signature":"sig"}},{"type":"tool_call","id":"c1","name":"run_shell","args":"{\"command\":\"ls\"}"},
      |{"type":"tool_call","id":"c2","name":"read_file","args":"{}"}]}""".stripMargin

  private val request =
    """{"role":"user","parts":[{"type":"text","text":"[Current local time]"},{"type":"text","text":"看下 #7"},
      |{"type":"file","media_type":"image/png","url":"/api/files/f1/x.png"},{"type":"text","text":"<event>job 结束"}],
      |"metadata":{"muliao_run_context":0,"muliao_parts":[{"part":1,"sender":"issue"},{"part":2,"sender":"issue"},{"part":3,"event":"job:12","check_in":true}]}}""".stripMargin

  private val results =
    """{"role":"user","parts":[{"type":"tool_result","call_id":"c1","tool_name":"run_shell",
      |"content":[{"type":"text","text":"ok"},{"type":"file","media_type":"image/png","url":"/api/files/f1/x.png"}],
      |"metadata":{"muliao_finished_at":"2026-11-14T10:56:08.690143Z"}}]}""".stripMargin

  private def bodies(es: List[EntryRow]): List[EntryBody] = es.map(_.body.fromJson[EntryBody].toOption.get)

  def spec = suite("迁移")(
    test("会话来源：issue 会话、来信按标记换成新版来源") {
      assertTrue(
        origin.row == SessionRow(SessionId.wrap("s1"), AgentId.wrap("a1"), OriginKind.Issue, Some("7"), None, "#7 修 bug", "[]", 1000L, 2000L),
        origin.sources.of("issue") == Source.Issue(IssueId.wrap(7)),
        origin.sources.default == Source.Issue(IssueId.wrap(7))
      )
    },
    test("已删触发器的会话按标题取名字；委派会话挂在发起会话下") {
      val t = Transcript.origin(session.copy(origin = "trigger", originRef = Some("9"), title = "selfcheck-1 · 2026-11-14 18:55"), Map.empty, Map.empty)
      val d = Transcript.origin(session.copy(origin = "delegation", originRef = None, parent = Some("p"), thread = Some("review")), Map.empty, Map("p" -> "boss"))
      assertTrue(
        t.row.originRef.contains("selfcheck-1"),
        d.row.origin == OriginKind.Thread,
        d.row.originRef.contains("review"),
        d.row.parentId.contains(SessionId.wrap("p")),
        d.sources.default == Source.Delegation(SessionId.wrap("p"), AgentName.wrap("boss"))
      )
    },
    test("上下文、来信（带附件、事件）、回复与调用结果、失败运行的提示；每个调用都有结果") {
      val runs = List(Old.Run(1, "s1", 1000, Some(1100), Some("failed"), Some("模型挂了")), Old.Run(2, "s1", 1200, None, None, None))
      val ms   = List(msg(1, 1, request), msg(2, 1, assistantWithCall), msg(3, 1, results))
      val c    = Transcript.convert(origin, runs, ms, Nil, files, 100)
      val bs   = bodies(c.entries)
      val call1 = c.calls.find(_.callId == CallId.wrap("c1")).get
      val call2 = c.calls.find(_.callId == CallId.wrap("c2")).get
      assertTrue(
        c.entries.map(_.id) == List(100L, 101, 102, 103, 104, 105),
        bs(0) == EntryBody.Context("[Current local time]"),
        bs(1) == EntryBody.Mail(Letter(Source.Issue(IssueId.wrap(7)), "看下 #7", Chunk(FileId.wrap("f1")))),
        bs(2) == EntryBody.Mail(Letter(Source.Event(EventKind.Job), "<event>job 结束", checkIn = true)),
        bs(3).isInstanceOf[EntryBody.Reply],
        bs(4) == EntryBody.Notice(NoticeKind.Failed, "模型挂了"),
        bs(5) == EntryBody.Notice(NoticeKind.Stopped, Transcript.unfinishedRun),
        c.runs.map(_.state) == List(RunState.Failed, RunState.Stopped),
        c.runs.forall(_.finishedAt.isDefined),
        call1.result.flatMap(_.fromJson[CallResult].toOption).contains(
          CallResult(Chunk(StoredPart.Text("ok"), StoredPart.Image("image/png", "ab" * 32)), isError = false)
        ),
        call1.finishedAt.contains(Old.millis("2026-11-14T10:56:08.690143Z")),
        call2.result.flatMap(_.fromJson[CallResult].toOption).contains(CallResult.error(Transcript.missingResult)),
        c.calls.forall(_.state == CallState.Done),
        c.callEntry == Map("c1" -> 103L, "c2" -> 103L),
        c.stats.missingResults == 1L
      )
    },
    test("回复带回协议数据（签名）与用量") {
      val c = Transcript.convert(origin, List(Old.Run(1, "s1", 0, Some(1), Some("done"), None)), List(msg(1, 1, assistantWithCall)), Nil, files, 1)
      bodies(c.entries).head match
        case EntryBody.Reply(parts, model, usage, stop, interrupted) =>
          assertTrue(
            parts.head == muliao.models.llm.ReplyPart.Thinking("想", Some(muliao.models.llm.Seal(muliao.models.llm.Protocol.Anthropic, """{"signature":"sig"}""".fromJson[zio.json.ast.Json].toOption.get))),
            model == "claude-x",
            usage == muliao.models.llm.Usage(10, 2, 5, 1),
            stop.contains("tool_use"),
            !interrupted
          )
        case other => assertTrue(other == null)
    },
    test("压缩点写在当时最后一条之后，from 指向被压缩部分之后的第一条") {
      val runs = List(Old.Run(1, "s1", 0, Some(1), Some("done"), None))
      val text = """{"role":"user","parts":[{"type":"text","text":"hi"}]}"""
      val ms   = List(msg(1, 1, text), msg(2, 1, text), msg(3, 1, text))
      val c    = Transcript.convert(origin, runs, ms, List(Old.Compaction(1, "s1", through = 1, at = 2, "摘要", 5)), files, 10)
      assertTrue(bodies(c.entries) == List(
        EntryBody.Mail(Letter(Source.Issue(IssueId.wrap(7)), "hi")),
        EntryBody.Mail(Letter(Source.Issue(IssueId.wrap(7)), "hi")),
        EntryBody.Compaction("摘要", 11),
        EntryBody.Mail(Letter(Source.Issue(IssueId.wrap(7)), "hi"))
      ))
    },
    test("看板动态：早期状态与指派写法") {
      val st = Convert.event(Old.Activity(1, 6, 0, "user", "status", None, Some("""{"from":"todo","to":"done"}"""), None))
      val as = Convert.event(Old.Activity(2, 6, 0, "a1", "assignee", None, Some("""{"from":{"kind":"user"},"to":{"agent_id":"a2","kind":"agent"}}"""), Some("s1")))
      val un = Convert.event(Old.Activity(3, 6, 0, "system", "unsettled", None, None, None))
      assertTrue(
        st.toOption.flatten.map(_.activity).contains((muliao.board.Activity.StatusChanged(muliao.contract.IssueStatus.InProgress, muliao.contract.IssueStatus.Done): muliao.board.Activity).toJson),
        as.toOption.flatten.map(_.activity).contains(
          (muliao.board.Activity.AssigneeChanged(Some(muliao.contract.Assignee.User), Some(muliao.contract.Assignee.Agent(AgentId.wrap("a2")))): muliao.board.Activity).toJson
        ),
        un == Right(None)
      )
    },
    test("触发器一律停用，配置换成新版写法") {
      val t = Old.Trigger(5, "watch", "a1", "看看", "stream", """{"command":"tail -f x","secrets":["K"],"debounce_seconds":1.0}""",
        Some("""{"skipped":2}"""), None, None, Some("""{"command":"true","fire_when":"changed"}"""), Some("old"), None, None, 0)
      val r = Convert.trigger(t, java.time.ZoneId.of("Asia/Shanghai"), 9)
      assertTrue(
        r.map(_._1.enabled) == Right(false),
        r.map(_._1.skipped) == Right(2),
        r.map(_._1.baseline) == Right(Some("old")),
        r.map(_._1.config.fromJson[muliao.contract.TriggerConfig]) == Right(Right(muliao.contract.TriggerConfig(command = Some("tail -f x"), secrets = Some(List("K")), debounceSeconds = Some(1.0))))
      )
    },
    test("旧版时间的几种写法") {
      assertTrue(
        Old.millis("2026-10-19 13:58:05.487150+00:00") == 1792418285487L,
        Old.millis("2026-10-19T13:58:05.487150Z") == 1792418285487L,
        Old.millis("2026-10-19 13:58:05") == 1792418285000L
      )
    }
  )
