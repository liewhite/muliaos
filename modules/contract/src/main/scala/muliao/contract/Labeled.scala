package muliao.contract

import sttp.tapir.{Codec, CodecFormat, Schema, Validator}
import zio.json.{JsonCodec, JsonFieldDecoder, JsonFieldEncoder}

/** 有固定文字写法的枚举值：库里、JSON 里、接口参数里都写 [[label]]。 */
trait Labeled:
  def label: String

/** 枚举的伴生对象继承它，得到按 `label` 的解析，以及 JSON、tapir 的编解码与 [[IsLabeled]] 证据（数据库层据此生成映射）：
  *
  * {{{
  * enum RunState(val label: String) extends Labeled:
  *   case Running extends RunState("running")
  *   ...
  * object RunState extends Labels[RunState]("运行状态", RunState.values)
  * }}}
  *
  * @param what 解析失败时说明里的名字
  */
abstract class Labels[E <: Labeled](what: String, all: => Array[E]):
  private lazy val byLabel: Map[String, E] = all.map(e => e.label -> e).toMap

  /** 全部取值的写法，按声明顺序。 */
  lazy val labels: List[String] = all.toList.map(_.label)

  def parse(s: String): Either[String, E] = byLabel.get(s).toRight(s"未知的$what「$s」（可选：${labels.mkString(" / ")}）")

  given IsLabeled[E] = IsLabeled(parse, _.label, labels)

  given JsonCodec[E] = JsonCodec.string.transformOrFail(parse, _.label)

  given JsonFieldEncoder[E] = JsonFieldEncoder.string.contramap(_.label)
  given JsonFieldDecoder[E] = JsonFieldDecoder.string.mapOrFail(parse)

  given Schema[E] = Schema.string[E].validate(Validator.enumeration(all.toList, e => Some(e.label)))

  given Codec[String, E, CodecFormat.TextPlain] = Codec.string.mapEither(parse)(_.label)

/** `E` 是有固定文字写法的枚举：怎么解析、怎么写、全部写法。 */
final case class IsLabeled[E](parse: String => Either[String, E], label: E => String, labels: List[String])
