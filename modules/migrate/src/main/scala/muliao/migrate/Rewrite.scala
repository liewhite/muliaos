package muliao.migrate

import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.regex.Pattern
import scala.jdk.CollectionConverters.*

/** 把文字里写死的旧版主目录路径（资源目录、数据目录、配置、日志）换成新版的：迁移后新版的 agent 照 skill、记忆、触发器
  * 里写的路径读写的是新版自己的那份，而不是旧版的（#878）。只换主目录下这几项，旧版程序目录（`app/` 等）不动。
  *
  * 认这几种写法：`~/.muliao/…`、`$HOME/.muliao/…`、`${HOME}/.muliao/…`、`<用户主目录>/.muliao/…`。
  */
final case class Rewrite(oldName: String, newName: String, userHome: String):
  private val pattern = Pattern.compile(
    "(~|\\$HOME|\\$\\{HOME\\}|" + Pattern.quote(userHome) + ")/" + Pattern.quote(oldName) + "/(assets|data|config\\.toml|muliao\\.log)(?![\\w-]|\\.\\w)"
  )

  /** 换过的文字与换了几处。 */
  def apply(text: String): (String, Int) =
    val m = pattern.matcher(text)
    var n = 0
    val b = StringBuilder()
    var last = 0
    while m.find() do
      n += 1
      b ++= text.substring(last, m.start()) ++= m.group(1) ++= "/" ++= newName ++= "/" ++= m.group(2)
      last = m.end()
    if n == 0 then (text, 0) else (b ++= text.substring(last)).result() -> n

  def text(s: String): String = apply(s)._1

  /** 改写目录下所有 UTF-8 文本文件（含 NUL 或不是 UTF-8 的当二进制跳过；`secrets/` 下是密文，跳过）。返回 (文件, 处数)。 */
  def tree(root: Path): List[(Path, Int)] =
    val files = Files.walk(root).iterator().asScala.filter(p => Files.isRegularFile(p) && !Files.isSymbolicLink(p)).toList
    files.filterNot(p => root.relativize(p).iterator().asScala.exists(_.toString == "secrets")).flatMap { p =>
      utf8(Files.readAllBytes(p)).flatMap { s =>
        val (out, n) = apply(s)
        Option.when(n > 0) {
          Files.writeString(p, out)
          p -> n
        }
      }
    }

  private def utf8(bytes: Array[Byte]): Option[String] =
    if bytes.contains(0: Byte) then None
    else
      try Some(StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString)
      catch case _: CharacterCodingException => None

object Rewrite:
  /** 新旧主目录都在用户主目录下时才能这样换（目录名不同）。 */
  def of(layout: Layout): Option[Rewrite] =
    val home = Path.of(java.lang.System.getProperty("user.home"))
    Option.when(layout.oldHome.getParent == home && layout.newHome.getParent == home && layout.oldHome != layout.newHome)(
      Rewrite(layout.oldHome.getFileName.toString, layout.newHome.getFileName.toString, home.toString)
    )
