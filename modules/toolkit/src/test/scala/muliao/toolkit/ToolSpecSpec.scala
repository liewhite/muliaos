package muliao.toolkit

import muliao.contract.{AgentId, CallId, SessionId}
import muliao.platform.AppError
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.description
import zio.test.*

object ToolSpecSpec extends ZIOSpecDefault:

  enum Mode:
    case Fast, Careful

  @description("存一个口令")
  final case class SetSecret(
      @description("口令名") key: String,
      @sensitive @description("口令值") value: String,
      @description("能否在自写命令里用") allowShell: Option[Boolean] = None,
      tags: List[String] = Nil,
      mode: Mode = Mode.Fast,
      count: Int = 3
  )
  given Schema[Mode]      = DeriveSchema.gen[Mode]
  given Schema[SetSecret] = DeriveSchema.gen[SetSecret]

  final case class Ctx(agent: AgentId, session: SessionId, call: CallId) extends ToolContext
  val ctx = Ctx(AgentId.wrap("a"), SessionId.wrap("s"), CallId.wrap("c"))

  val tool: Tool[ToolContext] =
    Tool[ToolContext, SetSecret]("vault_set", "存口令", Access.Write, Scheduling.Ordered) { (in, c) =>
      if in.key == "bad" then ZIO.fail(ToolFailure.fromAppError(AppError.invalid("名字不合法")))
      else ZIO.succeed(ToolOutcome.text(s"${c.agent.value}:${in.key}:${in.count}:${in.mode}"))
    }

  def parse(s: String): Json = s.fromJson[Json].toOption.get

  def spec = suite("工具契约")(
    test("参数结构由类型生成：说明、必填、默认值、枚举、数组") {
      val schema = tool.spec.inputSchema
      val props  = schema.get(zio.json.ast.JsonCursor.field("properties").isObject).toOption.get
      def field(n: String) = props.fields.find(_._1 == n).map(_._2).get
      assertTrue(
        schema.asObject.flatMap(_.get("type")).contains(Json.Str("object")),
        schema.asObject.flatMap(_.get("description")).contains(Json.Str("存一个口令")),
        schema.asObject.flatMap(_.get("required")).contains(Json.Arr(Json.Str("key"), Json.Str("value"))),
        field("key") == parse("""{"type":"string","description":"口令名"}"""),
        field("allowShell") == parse("""{"type":"boolean","description":"能否在自写命令里用"}"""),
        field("tags") == parse("""{"type":"array","items":{"type":"string"},"default":[]}"""),
        field("mode") == parse("""{"type":"string","enum":["Fast","Careful"],"default":"Fast"}"""),
        field("count") == parse("""{"type":"integer","default":3}""")
      )
    },
    test("调用：解码参数、缺省取默认值、带上下文") {
      for out <- tool.invoke(parse("""{"key":"k","value":"v","mode":"Careful"}"""), ctx)
      yield assertTrue(out == ToolOutcome.text("a:k:3:Careful"))
    },
    test("参数写错：说哪个字段、什么问题，不回显参数值") {
      for
        e1 <- tool.invoke(parse("""{"key":"k"}"""), ctx).flip
        e2 <- tool.invoke(parse("""{"key":"k","value":"v","count":"SECRET-ish"}"""), ctx).flip
        e3 <- tool.invoke(parse("""{"key":"k","value":"v","mode":"Turbo-SECRET"}"""), ctx).flip
      yield assertTrue(
        e1 match { case ToolFailure.Rejected(m) => m.contains("value") && m.contains("缺少"); case _ => false },
        e2 match { case ToolFailure.Rejected(m) => m.contains("count") && !m.contains("SECRET"); case _ => false },
        e3 match { case ToolFailure.Rejected(m) => !m.contains("SECRET"); case _ => false }
      )
    },
    test("领域错误交给模型；暂时不可用与内部错误分开") {
      for e <- tool.invoke(parse("""{"key":"bad","value":"v"}"""), ctx).flip
      yield assertTrue(
        e == ToolFailure.Rejected("名字不合法"),
        ToolFailure.fromAppError(AppError.Unavailable("断网", None, Some(3.seconds))) == ToolFailure.Unavailable("断网", Some(3.seconds)),
        ToolFailure.fromAppError(AppError.Internal("坏了")) == ToolFailure.Aborted("坏了", None),
        ToolFailure.fromAppError(AppError.External("HTTP 404")) == ToolFailure.Rejected("HTTP 404")
      )
    },
    test("敏感参数在保存与展示时打码") {
      assertTrue(
        tool.spec.sensitiveFields == Set("value"),
        tool.spec.redact(parse("""{"key":"k","value":"hunter2"}""")) == parse("""{"key":"k","value":"[redacted]"}"""),
        tool.spec.access == Access.Write,
        tool.spec.scheduling == Scheduling.Ordered
      )
    },
    test("工具名不合法时拒绝定义") {
      for r <- ZIO.attempt(Tool[ToolContext, SetSecret]("bad name", "", Access.Read)((_, _) => ZIO.succeed(ToolOutcome.text("")))).either
      yield assertTrue(r.isLeft)
    }
  )
