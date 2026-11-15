package muliao.toolkit

import muliao.contract.{AgentId, CallId, JobId, RunState, SessionId}
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

  /** 有固定写法的枚举与编号直接当参数，不手写 given。 */
  final case class Pick(state: RunState = RunState.Running, job: JobId)
  given Schema[Pick] = DeriveSchema.gen[Pick]

  val PickDef = ToolDef[ToolContext, Pick]("pick", "", Access.Read, display = in => Display.Terminal(Some(s"job ${in.job.value}")))

  final case class Ctx(agent: AgentId, session: SessionId, call: CallId) extends ToolContext
  val ctx = Ctx(AgentId.wrap("a"), SessionId.wrap("s"), CallId.wrap("c"))

  val tool: Tool[ToolContext, Nothing] =
    ToolDef[ToolContext, SetSecret]("vault_set", "存口令", Access.Write, Scheduling.Ordered) { (in, c) =>
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
    test("Labeled 枚举与编号当参数：JSON 里是 label 与底层值，Schema 列出全部取值，编号先校验") {
      val props = PickDef.spec.inputSchema.get(zio.json.ast.JsonCursor.field("properties").isObject).toOption.get
      def field(n: String) = props.fields.find(_._1 == n).map(_._2).get
      val run = PickDef((in, _) => ZIO.succeed(ToolOutcome.text(s"${in.state.label}:${in.job.value}")))
      for
        ok      <- run.invoke(parse("""{"state":"finished","job":3}"""), ctx)
        badCase <- run.invoke(parse("""{"state":"Finished","job":3}"""), ctx).either
        badId   <- run.invoke(parse("""{"job":0}"""), ctx).either
      yield assertTrue(
        field("state") == parse("""{"type":"string","enum":["running","suspended","finished","failed","stopped"],"default":"running"}"""),
        field("job") == parse("""{"type":"integer"}"""),
        ok == ToolOutcome.text("finished:3"),
        badCase.isLeft,
        badId.isLeft
      )
    },
    test("展示由参数算出；参数解不开（还在生成、写错）时是原文") {
      assertTrue(
        PickDef.spec.display(parse("""{"job":7}""")) == Display.Terminal(Some("job 7")),
        PickDef.spec.display(parse("""{"job":"x"}""")) == Display.Plain,
        tool.spec.display(parse("{}")) == Display.Plain
      )
    },
    test("工具名不合法时拒绝定义") {
      for r <- ZIO.attempt(ToolDef[ToolContext, SetSecret]("bad name", "", Access.Read)((_, _) => ZIO.succeed(ToolOutcome.text("")))).either
      yield assertTrue(r.isLeft)
    }
  )
