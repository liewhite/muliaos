package muliao.contract

import sttp.tapir.{Codec, CodecFormat, Schema}
import zio.json.{JsonCodec, JsonFieldDecoder, JsonFieldEncoder}

/** 给底层类型套一层不透明类型：编号、名字等不能互相混用的值。
  *
  * {{{
  * object AgentId extends Newtype[String]
  * type AgentId = AgentId.Type
  * }}}
  *
  * 自带 JSON、tapir、zio-schema（工具参数）的编解码与 [[IsNewtype]] 证据（数据库等其他层据此生成映射，不必逐个类型手写）。 校验规则覆盖 [[validate]]。
  */
abstract class Newtype[A](using
    baseJson: JsonCodec[A],
    baseSchema: Schema[A],
    baseCodec: Codec[String, A, CodecFormat.TextPlain]
):
  opaque type Type = A

  /** 不合法时的说明；默认都合法。 */
  protected def validate(a: A): Option[String] = None

  /** 已知合法的值（来自数据库、内部生成）。 */
  def wrap(a: A): Type = a

  /** 来自外部输入的值：先校验。 */
  def make(a: A): Either[String, Type] = validate(a).toLeft(a)

  extension (t: Type) inline def value: A = t

  given IsNewtype[Type, A] = IsNewtype(wrap, _.value)

  given JsonCodec[Type] =
    JsonCodec(baseJson.encoder.contramap(_.value), baseJson.decoder.mapOrFail(make))

  given Schema[Type] = baseSchema.as[Type]

  given Codec[String, Type, CodecFormat.TextPlain] =
    baseCodec.mapEither(make)(_.value)

  /** 用作工具参数：外部输入，先校验。带 using 参数，前端不用它时整段被裁掉（不把 zio-schema 打进前端包）。 */
  given zioSchema(using base: zio.schema.Schema[A]): zio.schema.Schema[Type] = base.transformOrFail(make, t => Right(t.value))

/** `T` 是 `A` 的不透明包装。 */
final case class IsNewtype[T, A](wrap: A => T, unwrap: T => A)

/** 底层为字符串的编号，可作 JSON 对象的键。 */
abstract class StringId extends Newtype[String]:
  protected override def validate(a: String): Option[String] =
    Option.when(a.isEmpty)("不能为空")
  given JsonFieldEncoder[Type] = JsonFieldEncoder.string.contramap(_.value)
  given JsonFieldDecoder[Type] = JsonFieldDecoder.string.mapOrFail(make)
  given Ordering[Type]         = Ordering.by(_.value)

/** 底层为 Long 的编号（数据库自增）。 */
abstract class LongId extends Newtype[Long]:
  protected override def validate(a: Long): Option[String] =
    Option.when(a <= 0)("必须是正整数")
  given Ordering[Type] = Ordering.by(_.value)
