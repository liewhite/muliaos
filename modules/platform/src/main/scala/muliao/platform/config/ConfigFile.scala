package muliao.platform.config

import muliao.platform.AppError
import org.tomlj.{Toml, TomlArray, TomlTable}
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.*

/** 配置文件里的一段（一个顶层表）。哪个领域用这段配置，就由那个领域声明它：键名和对应的类型。
  *
  * 未出现在文件里的段按空表解码（字段都有默认值的段可以省略）。段内未知的键在启动时报错。
  */
final case class ConfigSection[A](key: String)(using val codec: JsonCodec[A], val tag: Tag[A])

/** 读进来、还没按段解码的配置文件。 */
final class ConfigFile private (val path: Path, tables: Map[String, Json.Obj]):

  def section[A](s: ConfigSection[A]): IO[AppError, A] =
    val raw = tables.getOrElse(s.key, Json.Obj())
    ZIO
      .fromEither(raw.as[A](using s.codec.decoder))
      .mapError(e => AppError.invalid(s"配置文件 $path 的 [${s.key}] 段不对：$e"))
      .flatMap { a =>
        val unknown = ConfigFile.extraKeys(raw, a.toJsonAST(using s.codec.encoder).getOrElse(Json.Null), s.key)
        if unknown.isEmpty then ZIO.succeed(a)
        else ZIO.fail(AppError.invalid(s"配置文件 $path 有未知的键：${unknown.mkString("、")}"))
      }

  /** 可选的段（配了才有的功能，如网页工具、语音转写）：文件里没有这一段时为 `None`，有就按 [[section]] 解码与校验。 */
  def optionalSection[A](s: ConfigSection[A]): IO[AppError, Option[A]] =
    if tables.contains(s.key) then section(s).map(Some(_)) else ZIO.none

object ConfigFile:

  /** 解析 TOML。`sections` 是全部已声明的顶层段名，出现别的顶层键即报错。 */
  def load(path: Path, sections: Set[String]): IO[AppError, ConfigFile] =
    for
      result <- ZIO
                  .attemptBlocking(Toml.parse(path))
                  .mapError(e => AppError.invalid(s"读不了配置文件 $path：${e.getMessage}"))
      _ <- ZIO.when(result.hasErrors)(
             ZIO.fail(AppError.invalid(s"配置文件 $path 不是合法的 TOML：${result.errors.asScala.map(_.toString).mkString("；")}"))
           )
      json = tableToJson(result)
      unknown = json.fields.map(_._1).filterNot(sections).toList
      _ <- ZIO.when(unknown.nonEmpty)(
             ZIO.fail(AppError.invalid(s"配置文件 $path 有未知的段：${unknown.mkString("、")}"))
           )
      tables <- ZIO.foreach(json.fields.toList) {
                  case (k, o: Json.Obj) => ZIO.succeed(k -> o)
                  case (k, _)           => ZIO.fail(AppError.invalid(s"配置文件 $path 的 $k 应是一个表（[$k]）"))
                }
    yield ConfigFile(path, tables.toMap)

  /** 文件里有、而解码结果里没有的键（逐层比较）。 */
  private[config] def extraKeys(inFile: Json, decoded: Json, at: String): List[String] =
    (inFile, decoded) match
      case (Json.Obj(gs), Json.Obj(ds)) =>
        val dm = ds.toMap
        gs.toList.flatMap { (k, v) =>
          dm.get(k) match
            case None     => List(s"$at.$k")
            case Some(dv) => extraKeys(v, dv, s"$at.$k")
        }
      case (Json.Arr(gs), Json.Arr(ds)) =>
        gs.zip(ds).zipWithIndex.toList.flatMap { case ((g, d), i) => extraKeys(g, d, s"$at[$i]") }
      case _ => Nil

  private def tableToJson(t: TomlTable): Json.Obj =
    Json.Obj(Chunk.fromIterable(t.keySet.asScala.toList.sorted.map(k => k -> valueToJson(t.get(java.util.List.of(k))))))

  private def valueToJson(v: Any): Json = v match
    case t: TomlTable          => tableToJson(t)
    case a: TomlArray          => Json.Arr(Chunk.fromIterable(a.toList.asScala.map(valueToJson)))
    case s: String             => Json.Str(s)
    case l: java.lang.Long     => Json.Num(l.longValue)
    case d: java.lang.Double   => Json.Num(d.doubleValue)
    case b: java.lang.Boolean  => Json.Bool(b.booleanValue)
    case other                 => Json.Str(other.toString) // 日期时间按 ISO 文本交给解码器

/** 配置里常用值的编解码。 */
object ConfigCodecs:
  /** 路径：开头的 `~/` 换成用户主目录（取自 JVM，不读环境变量）。 */
  given JsonCodec[Path] = JsonCodec.string.transform(expandHome, _.toString)

  def expandHome(s: String): Path =
    if s == "~" || s.startsWith("~/") then Paths.get(java.lang.System.getProperty("user.home") + s.drop(1))
    else Paths.get(s)
