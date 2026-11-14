package muliao.migrate

import org.tomlj.{Toml, TomlTable}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.security.SecureRandom
import scala.jdk.CollectionConverters.*

/** 旧版与新版的目录。
  * @param oldHome 旧版主目录（`~/.muliao`）：`config.toml`、数据目录、资源目录 `assets/`
  * @param newHome 新版主目录（`~/.muliao-scala`）：`config.toml`、`data/`、`assets/`、`muliao.log`
  */
final case class Layout(oldHome: Path, newHome: Path):
  def oldConfig: Path = oldHome.resolve("config.toml")
  def oldAssets: Path = oldHome.resolve("assets")
  def newConfig: Path = newHome.resolve("config.toml")
  def newData: Path   = newHome.resolve("data")
  def newAssets: Path = newHome.resolve("assets")
  def newDb: Path     = newData.resolve("muliao.db")
  /** 迁移用的旧库快照（留着，事后可对照核对；确认无误后可删）。 */
  def snapshot: Path  = newHome.resolve("migration").resolve("source.db")
  def reportFile: Path = newHome.resolve("migration").resolve("report.md")

/** 旧版配置里迁移要用的值。`dataDir` 已按配置文件所在目录解析成绝对路径。 */
final case class OldConfig(table: TomlTable, dataDir: Path):
  def str(key: String): Option[String] = Option(table.getString(key))
  def long(key: String): Option[Long]  = Option(table.getLong(key)).map(_.longValue)
  def keys: List[String]               = table.dottedKeySet().asScala.toList.sorted
  def providers: List[String]          = Option(table.getTable("llm.providers")).map(_.keySet().asScala.toList.sorted).getOrElse(Nil)

object OldConfig:
  def load(file: Path): Either[String, OldConfig] =
    val r = Toml.parse(file)
    if r.hasErrors then Left(s"旧配置解析失败：${r.errors().asScala.map(_.getMessage).mkString("；")}")
    else
      val dir = Option(r.getString("data_dir")).getOrElse("data")
      Right(OldConfig(r, file.getParent.resolve(dir).normalize))

/** 新版配置文件：从旧配置取登录密码、vault 主密钥、模型服务商等；端口、数据目录换成新版的；登录签名密钥新生成（要重新登录）。
  * 返回文件内容与「旧配置里有、新版没有对应项而没搬的键」（只列键名）。
  */
object NewConfig:
  val port = 1081

  /** 搬过去的旧键（其余列为没搬）。 */
  private val carried = Set(
    "data_dir", "timezone", "log_level", "log_file", "server.host", "server.port", "server.public_url", "auth.password",
    "auth.secret_key", "vault.key", "push.vapid_subject", "llm.default_model", "web.type", "web.api_key", "web.base_url",
    "transcription.type", "transcription.model", "transcription.api_key", "transcription.base_url",
    "runtime.precheck_timeout_seconds", "runtime.compaction_threshold_tokens"
  )
  private val providerKeys = Set("type", "base_url", "api_key", "max_concurrency", "max_output_tokens")

  def render(old: OldConfig, layout: Layout): Either[String, (String, List[String])] =
    def need(k: String): Either[String, String] = old.str(k).toRight(s"旧配置缺少 $k")
    for
      password <- need("auth.password")
      vaultKey <- need("vault.key")
      model    <- need("llm.default_model")
      subject  <- need("push.vapid_subject")
      providers <- old.providers.foldLeft[Either[String, List[String]]](Right(Nil)) { (acc, name) =>
                     for
                       done <- acc
                       p     = s"llm.providers.$name"
                       tpe  <- need(s"$p.type")
                       url  <- need(s"$p.base_url")
                       key  <- need(s"$p.api_key")
                     yield done :+ (
                       s"\n[models.providers.$name]\nprotocol = ${q(tpe)}\nbase_url = ${q(url)}\napi_key = ${q(key)}\n" +
                         old.long(s"$p.max_concurrency").fold("")(n => s"max_concurrency = $n\n") +
                         old.long(s"$p.max_output_tokens").fold("")(n => s"max_output_tokens = $n\n")
                     )
                   }
    yield
      val web = old.str("web.api_key").fold("")(k => s"\n[web]\napi_key = ${q(k)}\n" + old.str("web.base_url").fold("")(u => s"base_url = ${q(u)}\n"))
      val tr = (old.str("transcription.api_key"), old.str("transcription.model")) match
        case (Some(k), Some(m)) => s"\n[transcription]\napi_key = ${q(k)}\nmodel = ${q(m)}\n" + old.str("transcription.base_url").fold("")(u => s"base_url = ${q(u)}\n")
        case _                  => ""
      val text =
        s"""# muliao（Scala 版）配置：由迁移工具从 ${layout.oldConfig} 生成。
           |
           |[general]
           |data_dir = ${q(layout.newData.toString)}
           |assets_dir = ${q(layout.newAssets.toString)}
           |timezone = ${q(old.str("timezone").getOrElse("Asia/Shanghai"))}
           |
           |[log]
           |level = ${q(old.str("log_level").getOrElse("INFO"))}
           |file = ${q(layout.newHome.resolve("muliao.log").toString)}
           |
           |[server]
           |host = ${q(old.str("server.host").getOrElse("127.0.0.1"))}
           |port = $port
           |password = ${q(password)}
           |cookie_secret = ${q(randomHex(32))}
           |
           |[vault]
           |key = ${q(vaultKey)}
           |
           |[push]
           |vapid_subject = ${q(subject)}
           |
           |[models]
           |default_model = ${q(model)}
           |${providers.mkString}$web$tr
           |[resources]
           |
           |[sessions]
           |compaction_threshold_tokens = ${old.long("runtime.compaction_threshold_tokens").getOrElse(700000L)}
           |
           |[triggers]
           |precheck_timeout_seconds = ${old.long("runtime.precheck_timeout_seconds").getOrElse(60L)}
           |""".stripMargin
      val skipped = old.keys.filterNot { k =>
        carried(k) || (k.startsWith("llm.providers.") && providerKeys(k.split('.').last))
      }
      (text, skipped)

  def write(file: Path, text: String): Unit =
    Files.createDirectories(file.getParent)
    Files.writeString(file, text)
    Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-------")): Unit

  private def q(s: String): String =
    "\"" + s.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case '\n' => "\\n"
      case c    => c.toString
    } + "\""

  private def randomHex(bytes: Int): String =
    val b = new Array[Byte](bytes)
    SecureRandom().nextBytes(b)
    b.map(x => f"${x & 0xff}%02x").mkString

/** 目录复制：APFS 克隆（`cp -c`），不占额外空间；原目录只读。 */
object Copy:
  final case class Outcome(from: Path, to: Path, ok: Boolean, problems: List[String])

  /** 把 `from` 整个复制成 `to`（`to` 不能已存在）。权限位、时间保留。 */
  def tree(from: Path, to: Path): Outcome =
    Files.createDirectories(to.getParent)
    val p = ProcessBuilder("cp", "-c", "-R", "-p", from.toString, to.toString).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes()).linesIterator.toList
    val code = p.waitFor()
    Outcome(from, to, code == 0, out)

  def file(from: Path, to: Path): Outcome = tree(from, to)
