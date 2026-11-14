package muliao.models.llm

import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** 读写协议 JSON 的小工具：协议字段多、可缺省，逐个解码成 case class 不如按路径取值直接。 */
private[models] object JsonPath:

  extension (j: Json)
    def at(key: String): Json = j match
      case Json.Obj(fs) => fs.collectFirst { case (`key`, v) => v }.getOrElse(Json.Null)
      case _            => Json.Null

    def str: Option[String] = j match
      case Json.Str(s) => Some(s)
      case _           => None

    def strOr(default: String = ""): String = str.getOrElse(default)

    def long: Option[Long] = j match
      case Json.Num(n) => Some(n.longValue)
      case _           => None

    def bool: Option[Boolean] = j match
      case Json.Bool(b) => Some(b)
      case _            => None

    def arr: Chunk[Json] = j match
      case Json.Arr(xs) => xs
      case _            => Chunk.empty

    def isNull: Boolean = j == Json.Null

    /** 对象加（或替换）一个字段。 */
    def withField(key: String, v: Json): Json = j match
      case Json.Obj(fs) => Json.Obj(fs.filterNot(_._1 == key) :+ (key -> v))
      case other        => other

  def obj(fields: (String, Json)*): Json = Json.Obj(Chunk.fromIterable(fields))

  /** 值为 None 的字段不写。 */
  def objOpt(fields: (String, Option[Json])*): Json = Json.Obj(Chunk.fromIterable(fields.collect { case (k, Some(v)) => k -> v }))

  def s(v: String): Json  = Json.Str(v)
  def n(v: Long): Json    = Json.Num(v)
  def b(v: Boolean): Json = Json.Bool(v)
  def arr(xs: Iterable[Json]): Json = Json.Arr(Chunk.fromIterable(xs))

  def parse(text: String): Either[String, Json] = text.fromJson[Json]
