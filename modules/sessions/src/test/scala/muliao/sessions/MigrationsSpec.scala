package muliao.sessions

import muliao.models.llm.{Protocol, ReplyPart, Seal}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.Files
import java.sql.DriverManager
import scala.io.Source

/** 迁移脚本在生产用的 sqlite-jdbc 上跑出来的数据，新代码解得开、换算对。 */
object MigrationsSpec extends ZIOSpecDefault:

  private def sql(name: String): String =
    Source.fromResource(s"db/sessions/$name", getClass.getClassLoader).mkString

  /** 在临时库里依次执行这些迁移，插入旧格式的回复，再执行 `after`，取回各条 body。 */
  private def run(before: List[String], rows: List[String], after: String): Task[List[String]] = ZIO.attemptBlocking {
    val file = Files.createTempFile("mig", ".db")
    val c    = DriverManager.getConnection(s"jdbc:sqlite:$file")
    try
      val st = c.createStatement()
      before.foreach(n => sql(n).split(";\\s*\n").map(_.trim).filter(s => s.nonEmpty && !s.linesIterator.forall(_.startsWith("--"))).foreach(st.execute))
      val ins = c.prepareStatement("INSERT INTO entry(session_id, kind, body, search, started_at, finished_at) VALUES ('s', 'reply', ?, '', 0, 0)")
      rows.foreach { b => ins.setString(1, b); ins.executeUpdate() }
      st.execute(sql(after))
      val rs  = st.executeQuery("SELECT body FROM entry ORDER BY id")
      val out = List.newBuilder[String]
      while rs.next() do out += rs.getString(1)
      out.result()
    finally
      c.close()
      Files.deleteIfExists(file): Unit
  }

  private def reply(parts: String*): String =
    s"""{"Reply":{"parts":[${parts.mkString(",")}],"model":"m","usage":{"input":0,"output":0,"cacheRead":0,"cacheWrite":0},"stopReason":null,"interrupted":false}}"""

  private def parts(body: String): Either[String, List[ReplyPart]] =
    body.fromJson[EntryBody].map {
      case EntryBody.Reply(ps, _, _, _, _) => ps.toList
      case _                               => Nil
    }

  private def o(fields: (String, Json)*): Json = Json.Obj(fields*)
  private def s(v: String): Json               = Json.Str(v)

  def spec = suite("迁移")(
    test("V3：旧思考段换成可原样回传的思考块，别的段与顺序不变，被打断的删掉") {
      val text      = """{"Text":{"text":"好的 \"Thinking\""}}"""
      val call      = """{"ToolCall":{"id":"c1","name":"read_file","args":"{}"}}"""
      val signed    = """{"Thinking":{"text":"想过了","seal":{"protocol":"anthropic","data":{"signature":"sig"}}}}"""
      val redacted  = """{"Thinking":{"text":"","seal":{"protocol":"anthropic","data":{"redacted":"rd"}}}}"""
      val summary   = """{"Thinking":{"text":"摘要","seal":{"protocol":"openai-responses","data":{"id":"rs_1","encrypted_content":"e1"}}}}"""
      val raw       = """{"Thinking":{"text":"原始推理","seal":{"protocol":"openai-responses","data":{"id":"rs_2","encrypted_content":"e2","raw":true}}}}"""
      val cut       = """{"Thinking":{"text":"半截"}}"""
      val R         = (p: Protocol, j: Json) => ReplyPart.Reasoning(Seal(p, j))
      for bodies <- run(
                      List("V1__sessions.sql", "V2__drop_notify_delegations.sql"),
                      List(reply(signed, text, call), reply(redacted, cut, summary, raw, call), reply(text), reply(cut)),
                      "V3__thinking_to_reasoning.sql"
                    )
      yield assertTrue(
        bodies.map(parts) == List(
          Right(List(
            R(Protocol.Anthropic, o("type" -> s("thinking"), "thinking" -> s("想过了"), "signature" -> s("sig"))),
            ReplyPart.Text("好的 \"Thinking\""),
            ReplyPart.ToolCall("c1", "read_file", "{}")
          )),
          Right(List(
            R(Protocol.Anthropic, o("type" -> s("redacted_thinking"), "data" -> s("rd"))),
            R(Protocol.OpenAiResponses, o("type" -> s("reasoning"), "id" -> s("rs_1"),
              "summary" -> Json.Arr(o("type" -> s("summary_text"), "text" -> s("摘要"))), "encrypted_content" -> s("e1"))),
            R(Protocol.OpenAiResponses, o("type" -> s("reasoning"), "id" -> s("rs_2"), "summary" -> Json.Arr(),
              "encrypted_content" -> s("e2"), "content" -> Json.Arr(o("type" -> s("reasoning_text"), "text" -> s("原始推理"))))),
            ReplyPart.ToolCall("c1", "read_file", "{}")
          )),
          Right(List(ReplyPart.Text("好的 \"Thinking\""))),
          Right(Nil)
        )
      )
    }
  )
