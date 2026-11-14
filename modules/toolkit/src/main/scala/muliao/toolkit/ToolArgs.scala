package muliao.toolkit

import zio.Chunk
import zio.json.ast.Json
import zio.schema.{Schema, StandardType}
import zio.schema.annotation.{caseName, description, fieldDefaultValue}
import zio.schema.codec.JsonCodec

/** 工具参数：从参数类型的 zio-schema 生成解码器、JSON Schema 与敏感字段表。 */
object ToolArgs:

  /** 解码参数。失败时只说哪个字段、哪类问题，不回显参数值（以免把密钥带进会话记录）。 */
  def decoder[A](schema: Schema[A]): Json => Either[String, A] =
    val d = JsonCodec.jsonDecoder(schema)
    json => d.fromJsonAST(json).left.map(describe)

  private[toolkit] def describe(err: String): String =
    val path   = err.takeWhile(_ != '(').stripPrefix(".")
    val reason = err.drop(err.indexOf('(') max 0)
    val problem =
      if reason.contains("missing") then "缺少这个必填参数"
      else if reason.contains("extra field") then "没有这个参数"
      else "类型或取值不对"
    val where = if path.isEmpty then "参数整体" else s"参数 `$path`"
    s"$where：$problem。请对照参数说明改了再调用。"

  /** 顶层带 [[sensitive]] 注解的字段名。 */
  def sensitiveFields(schema: Schema[?]): Set[String] = unwrap(schema) match
    case r: Schema.Record[?] => r.fields.filter(_.annotations.exists(_.isInstanceOf[sensitive])).map(_.name.toString).toSet
    case _                   => Set.empty

  /** 给模型看的 JSON Schema（draft 2020-12 子集）：对象、数组、基本类型、可选、默认值、只有名字的枚举、字段说明。 */
  def jsonSchema(schema: Schema[?]): Json = toJson(schema, None)

  private def unwrap(s: Schema[?]): Schema[?] = s match
    case l: Schema.Lazy[?]               => unwrap(l.schema)
    case t: Schema.Transform[?, ?, ?]    => unwrap(t.schema)
    case other                           => other

  private def descriptionOf(annotations: Chunk[Any]): Option[String] =
    annotations.collectFirst { case d: description => d.text }

  private def obj(fields: (String, Json)*): Json = Json.Obj(Chunk.fromIterable(fields))

  private def withDescription(j: Json, d: Option[String]): Json = (j, d) match
    case (Json.Obj(fs), Some(text)) => Json.Obj(fs :+ ("description" -> Json.Str(text)))
    case _                          => j

  private def toJson(schema: Schema[?], desc: Option[String]): Json =
    val body = unwrap(schema) match
      case r: Schema.Record[?] =>
        val props = r.fields.map { f =>
          val d       = descriptionOf(f.annotations)
          val default = f.annotations.collectFirst { case fieldDefaultValue(v) => v }
          val base    = toJson(f.schema, d)
          val withDefault = (base, default) match
            case (Json.Obj(fs), Some(v)) => encodeDefault(f.schema, v).fold(base)(dv => Json.Obj(fs :+ ("default" -> dv)))
            case _                       => base
          f.name.toString -> withDefault
        }
        val required = r.fields.filterNot(f => f.optional || isOptional(f.schema)).map(f => Json.Str(f.name.toString))
        val parts = List(
          "type"                 -> Json.Str("object"),
          "properties"           -> Json.Obj(props),
          "additionalProperties" -> Json.Bool(false)
        ) ++ Option.when(required.nonEmpty)("required" -> Json.Arr(required))
        withDescription(obj(parts*), descriptionOf(r.annotations))
      case o: Schema.Optional[?]          => toJson(o.schema, None)
      case s: Schema.Sequence[?, ?, ?]    => obj("type" -> Json.Str("array"), "items" -> toJson(s.elementSchema, None))
      case s: Schema.Set[?]               => obj("type" -> Json.Str("array"), "items" -> toJson(s.elementSchema, None), "uniqueItems" -> Json.Bool(true))
      case m: Schema.Map[?, ?]            => obj("type" -> Json.Str("object"), "additionalProperties" -> toJson(m.valueSchema, None))
      case e: Schema.Enum[?] if e.cases.forall(c => isSingleton(c.schema)) =>
        withDescription(obj("type" -> Json.Str("string"), "enum" -> Json.Arr(e.cases.map(c => Json.Str(caseLabel(c))))), descriptionOf(e.annotations))
      case Schema.Primitive(t, _) => primitive(t)
      case _                      => obj()
    withDescription(body, desc)

  /** 枚举值在 JSON 里的写法：有 `@caseName` 用它（解码也认它），否则用名字。 */
  private def caseLabel(c: Schema.Case[?, ?]): String =
    c.annotations.collectFirst { case caseName(n) => n }.getOrElse(c.id)

  private def isSingleton(s: Schema[?]): Boolean = unwrap(s) match
    case r: Schema.Record[?] => r.fields.isEmpty
    case _                   => false

  private def isOptional(s: Schema[?]): Boolean = unwrap(s) match
    case _: Schema.Optional[?] => true
    case _                     => false

  private def encodeDefault(s: Schema[?], v: Any): Option[Json] =
    // 可选参数缺省为 None 时不写 default
    JsonCodec.jsonEncoder(s.asInstanceOf[Schema[Any]]).toJsonAST(v).toOption.filter(_ != Json.Null)

  private def primitive(t: StandardType[?]): Json = t match
    case StandardType.StringType                                                   => obj("type" -> Json.Str("string"))
    case StandardType.BoolType                                                     => obj("type" -> Json.Str("boolean"))
    case StandardType.IntType | StandardType.LongType | StandardType.ShortType |
        StandardType.ByteType | StandardType.BigIntegerType                        => obj("type" -> Json.Str("integer"))
    case StandardType.DoubleType | StandardType.FloatType | StandardType.BigDecimalType => obj("type" -> Json.Str("number"))
    case _                                                                         => obj("type" -> Json.Str("string"))
