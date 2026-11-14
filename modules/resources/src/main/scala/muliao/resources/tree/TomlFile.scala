package muliao.resources.tree

import org.tomlj.{Toml, TomlArray, TomlTable}

import scala.jdk.CollectionConverters.*

/** 资源文件里的 TOML：按已知键读、未知键报错；以及按固定格式写回。 */
object TomlFile:

  /** 解析一份 TOML 文本；语法错误时给出第一处的位置。 */
  def parse(text: String): Either[String, TomlTable] =
    val r = Toml.parse(text)
    if r.hasErrors then Left(s"不是合法的 TOML：${r.errors.asScala.headOption.map(_.toString).getOrElse("")}")
    else Right(r)

  /** 一张表的读取器：只允许 `known` 里的键。 */
  final class Fields(table: TomlTable, where: String):
    private def at(k: String) = if where.isEmpty then k else s"$where.$k"

    def keys: List[String] = table.keySet.asScala.toList.sorted

    def only(known: Set[String]): Either[String, Unit] =
      keys.filterNot(known) match
        case Nil     => Right(())
        case unknown => Left(s"不认识的字段：${unknown.map(at).mkString("、")}")

    private def raw(k: String): Option[Any] = Option(table.get(java.util.List.of(k)))

    def string(k: String): Either[String, Option[String]] = raw(k) match
      case None            => Right(None)
      case Some(s: String) => Right(Some(s))
      case Some(_)         => Left(s"${at(k)} 应是字符串")

    def requiredString(k: String): Either[String, String] = string(k).flatMap(_.toRight(s"缺少 ${at(k)}"))

    def bool(k: String): Either[String, Option[Boolean]] = raw(k) match
      case None                       => Right(None)
      case Some(b: java.lang.Boolean) => Right(Some(b.booleanValue))
      case Some(_)                    => Left(s"${at(k)} 应是 true 或 false")

    def strings(k: String): Either[String, List[String]] = raw(k) match
      case None => Right(Nil)
      case Some(a: TomlArray) =>
        val items = a.toList.asScala.toList
        items.collect { case s: String => s } match
          case ss if ss.size == items.size => Right(ss)
          case _                           => Left(s"${at(k)} 应是字符串数组")
      case Some(_) => Left(s"${at(k)} 应是字符串数组")

    def table(k: String): Either[String, Option[Fields]] = raw(k) match
      case None               => Right(None)
      case Some(t: TomlTable) => Right(Some(Fields(t, at(k))))
      case Some(_)            => Left(s"${at(k)} 应是一张表")

    /** 子表（`[k.<名>]`）逐个取出。 */
    def tables(k: String): Either[String, List[(String, Fields)]] =
      table(k).flatMap {
        case None => Right(Nil)
        case Some(f) =>
          f.keys.foldLeft[Either[String, List[(String, Fields)]]](Right(Nil)) { (acc, name) =>
            for
              xs <- acc
              t  <- f.table(name).flatMap(_.toRight(s"${f.at(name)} 应是一张表"))
            yield xs :+ (name -> t)
          }
      }

  def fields(t: TomlTable): Fields = Fields(t, "")

  // ---- 写 ----

  /** 基本字符串字面量。 */
  def str(s: String): String =
    val sb = StringBuilder("\"")
    s.foreach {
      case '"'           => sb ++= "\\\""
      case '\\'          => sb ++= "\\\\"
      case '\n'          => sb ++= "\\n"
      case '\t'          => sb ++= "\\t"
      case '\r'          => sb ++= "\\r"
      case c if c < 0x20 || c == 0x7f => sb ++= f"\\u${c.toInt}%04x"
      case c             => sb += c
    }
    sb += '"'
    sb.toString

  /** 字符串数组：不多于一项写一行，否则每项一行。 */
  def strArray(xs: List[String]): String = xs match
    case Nil      => "[]"
    case x :: Nil => s"[${str(x)}]"
    case _        => xs.map(x => s"    ${str(x)},").mkString("[\n", "\n", "\n]")

  /** 表名里的键：裸键能写就写裸键。 */
  def key(k: String): String =
    if k.nonEmpty && k.forall(c => c.isLetterOrDigit && c < 128 || c == '-' || c == '_') then k else str(k)
