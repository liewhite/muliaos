package muliao.toolkit

import muliao.contract.{AgentId, CallId, SessionId}
import muliao.platform.AppError
import zio.*
import zio.json.ast.Json
import zio.schema.Schema

import scala.annotation.StaticAnnotation

/** 工具的类别：决定默认分给谁（base 角色拿全部非管理类工具）。 */
enum Access:
  case Read, Write, Admin

/** 同一回复里多个调用怎么排：`Ordered` 的工具与同一回复里其他调用保持先后顺序（如文件读写），`Concurrent` 的可以并发。 */
enum Scheduling:
  case Concurrent, Ordered

/** 界面怎么展示工具的调用与结果（定义在 contract，前后端共用）。 */
export muliao.contract.Display

/** 标在参数字段上：值是敏感的（如 secret），保存与展示调用参数时打码。 */
final class sensitive extends StaticAnnotation

/** 工具的描述：给模型看的名字、说明、参数结构，以及平台据以调度、展示、打码的属性。 */
final case class ToolSpec(
    name: String,
    description: String,
    /** 参数的 JSON Schema（由参数类型生成） */
    inputSchema: Json,
    access: Access,
    scheduling: Scheduling,
    display: Display,
    /** 敏感参数的字段名（顶层） */
    sensitiveFields: Set[String]
):
  /** 把敏感参数换成占位，用于保存、展示与日志。 */
  def redact(args: Json): Json = args match
    case Json.Obj(fields) if sensitiveFields.nonEmpty =>
      Json.Obj(fields.map((k, v) => if sensitiveFields(k) then k -> Json.Str(ToolSpec.Redacted) else k -> v))
    case other => other

object ToolSpec:
  val Redacted = "[redacted]"
  private val NamePattern = "^[a-zA-Z0-9_-]{1,64}$".r

  def validName(name: String): Boolean = NamePattern.matches(name)

/** 一次工具调用的基本上下文。会话领域提供更具体的上下文（工作目录、权限等），所以 [[Tool]] 对上下文逆变。 */
trait ToolContext:
  def agent: AgentId
  def session: SessionId
  def call: CallId

/** 工具结果里的一块内容。 */
enum ToolContent:
  case Text(text: String)
  case Image(mediaType: String, data: Chunk[Byte])

/** 工具调用要等用户（回答提问、批准执行）时交给会话领域的说明；具体种类由会话领域定义。 */
trait Suspension

/** 工具调用的结果：完成，或挂起等用户。 */
enum ToolOutcome:
  case Completed(content: Chunk[ToolContent])
  case Suspended(suspension: Suspension)

object ToolOutcome:
  def text(s: String): ToolOutcome = Completed(Chunk(ToolContent.Text(s)))

/** 工具失败。会话领域按种类处理：
  *   - [[ToolFailure.Rejected]]（请重试）：作为出错的工具结果交给模型，模型改了参数或换个做法再来；
  *   - [[ToolFailure.Unavailable]]（暂时不可用）：不交给模型，整次运行稍后退避重试；
  *   - [[ToolFailure.Aborted]]（中止）：系统性错误，运行失败、让用户看见。
  * 挂起不是失败，见 [[ToolOutcome.Suspended]]。
  */
enum ToolFailure:
  case Rejected(message: String)
  case Unavailable(message: String, retryAfter: Option[Duration] = None)
  case Aborted(message: String, cause: Option[Throwable] = None)

object ToolFailure:
  /** 领域错误与外部永久错误交给模型自己处理；暂时不可用退避重试；内部错误中止运行。 */
  def fromAppError(e: AppError): ToolFailure = e match
    case AppError.Domain(_, m)         => Rejected(m)
    case AppError.External(m)          => Rejected(m)
    case AppError.Unavailable(m, _, a) => Unavailable(m, a)
    case AppError.Internal(m, c)       => Aborted(m, c)

/** 给 agent 的一个工具。`C` 是它需要的调用上下文。 */
trait Tool[-C]:
  def spec: ToolSpec
  def invoke(args: Json, ctx: C): IO[ToolFailure, ToolOutcome]

/** 工具的静态定义：名字、说明、参数结构与类别。它是静态值，所以全部内置工具的目录（分发、校验规则用）不用等各领域起好就能给出；
  * 实现在拿到所需服务后用 [[apply]] 接上。参数结构、JSON Schema 与敏感字段都从 `In` 的 zio-schema 生成，解码失败统一回「请重试」。
  */
final class ToolDef[C, In] private (val spec: ToolSpec, decode: Json => Either[String, In]):
  def apply(run: (In, C) => IO[ToolFailure, ToolOutcome]): Tool[C] =
    val s = spec
    new Tool[C]:
      val spec = s
      def invoke(args: Json, ctx: C): IO[ToolFailure, ToolOutcome] =
        ZIO.fromEither(decode(args)).mapError(ToolFailure.Rejected(_)).flatMap(run(_, ctx))

object ToolDef:
  def apply[C, In](
      name: String,
      description: String,
      access: Access,
      scheduling: Scheduling = Scheduling.Concurrent,
      display: Display = Display.Plain
  )(using schema: Schema[In]): ToolDef[C, In] =
    require(ToolSpec.validName(name), s"工具名不合法：$name")
    new ToolDef(
      ToolSpec(name, description, ToolArgs.jsonSchema(schema), access, scheduling, display, ToolArgs.sensitiveFields(schema)),
      ToolArgs.decoder(schema)
    )

object Tool:

  /** 定义并实现一个工具（不进工具目录的场合：测试）。 */
  def apply[C, In](
      name: String,
      description: String,
      access: Access,
      scheduling: Scheduling = Scheduling.Concurrent,
      display: Display = Display.Plain
  )(run: (In, C) => IO[ToolFailure, ToolOutcome])(using schema: Schema[In]): Tool[C] =
    ToolDef[C, In](name, description, access, scheduling, display)(run)
