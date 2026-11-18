package muliao.resources.tree

import muliao.toolkit.McpTransport
import zio.Chunk
import org.snakeyaml.engine.v2.api.{Load, LoadSettings}
import zio.json.*
import zio.json.ast.Json

import scala.jdk.CollectionConverters.*

/** 各种资源文件的格式：解析（不合格说明原因）与写出。纯函数，不碰磁盘。 */
object Formats:

  // ---- agent.toml ----

  private val agentKeys = Set("description", "model", "group")

  def parseAgent(text: String): Either[String, AgentProfile] =
    for
      t     <- TomlFile.parse(text).map(TomlFile.fields)
      _     <- t.only(agentKeys)
      desc  <- t.requiredString("description")
      _     <- Either.cond(desc.trim.nonEmpty, (), "description 不能为空")
      model <- t.requiredString("model")
      _     <- Either.cond(model.trim.nonEmpty, (), "model 不能为空")
      group <- t.string("group")
    yield AgentProfile(desc, model, group.filter(_.nonEmpty))

  def renderAgent(p: AgentProfile): String =
    val lines = List(
      Some(s"description = ${TomlFile.str(p.description)}"),
      Some(s"model = ${TomlFile.str(p.model)}"),
      p.group.map(g => s"group = ${TomlFile.str(g)}")
    )
    lines.flatten.mkString("", "\n", "\n")

  // ---- SKILL.md ----

  /** SKILL.md 的 frontmatter 里平台认的部分。 */
  final case class SkillMeta(description: String, approval: Set[String])

  /** 解析 SKILL.md。`dirName` 是 skill 目录名，`exists` 判断目录里某个相对路径的文件在不在（审批脚本必须存在）。 */
  def parseSkill(text: String, dirName: String, exists: String => Boolean): Either[String, SkillMeta] =
    for
      yaml <- frontmatter(text)
      map <- yaml match
               case m: java.util.Map[?, ?] => Right(m.asScala.toMap.map((k, v) => String.valueOf(k) -> v))
               case _                      => Left("frontmatter 应是 YAML 键值表")
      desc <- map.get("description") match
                case Some(s: String) if s.trim.nonEmpty => Right(s.trim)
                case Some(_: String) | None             => Left("frontmatter 缺 description（必填、非空）")
                case Some(_)                            => Left("description 应是字符串")
      _ <- map.get("name") match
             case None                         => Right(())
             case Some(n: String) if n == dirName => Right(())
             case Some(n)                      => Left(s"frontmatter 的 name（$n）与目录名 $dirName 不同")
      approval <- map.get("approval") match
                    case None => Right(Set.empty[String])
                    case Some(xs: java.util.List[?]) =>
                      val items = xs.asScala.toList
                      val ss    = items.collect { case s: String => s }
                      Either.cond(ss.size == items.size, ss.toSet[String], "approval 应是脚本路径的列表")
                    case Some(_) => Left("approval 应是脚本路径的列表，如 [scripts/run.sh]")
      missing = approval.filterNot(p => safeRelative(p) && exists(p))
      _ <- Either.cond(
             missing.isEmpty,
             (),
             s"approval 里的脚本不存在：${missing.toList.sorted.mkString("、")}（整个 skill 不加载，免得脚本不经批准就能跑）"
           )
    yield SkillMeta(desc, approval)

  private def safeRelative(p: String): Boolean =
    p.nonEmpty && !p.startsWith("/") && !p.split('/').exists(s => s == ".." || s.isEmpty)

  private def frontmatter(text: String): Either[String, Any] =
    val lines = text.linesIterator.toList
    lines match
      case head :: rest if head.trim == "---" =>
        val end = rest.indexWhere(_.trim == "---")
        if end < 0 then Left("frontmatter 没有结束的 ---")
        else
          val body = rest.take(end).mkString("\n")
          try Right(Load(LoadSettings.builder().build()).loadFromString(body))
          catch case e: Exception => Left(s"frontmatter 不是合法的 YAML：${e.getMessage.linesIterator.take(2).mkString(" ")}")
      case _ => Left("SKILL.md 开头要有 --- 包起来的 YAML frontmatter（至少有 description）")

  // ---- MCP ----

  def parseMcp(text: String): Either[String, McpConfig] =
    for
      json <- text.fromJson[Json].left.map(e => s"不是合法的 JSON：$e")
      obj  <- json.asObject.toRight("应是一个 JSON 对象")
      fields = obj.fields.toMap
      known  = Set("transport", "command", "args", "env", "url", "headers", "enabled")
      unknown = obj.fields.map(_._1).filterNot(known).toList
      _ <- Either.cond(unknown.isEmpty, (), s"不认识的字段：${unknown.mkString("、")}")
      enabled <- fields.get("enabled") match
                   case None                 => Right(true)
                   case Some(Json.Bool(b))   => Right(b)
                   case Some(_)              => Left("enabled 应是 true 或 false")
      transport <- fields.get("transport") match
                     case Some(Json.Str("stdio")) =>
                       for
                         _       <- forbid(fields, "stdio", "url", "headers")
                         command <- str(fields, "command").flatMap(_.filter(_.nonEmpty).toRight("stdio 必须有 command"))
                         args    <- strList(fields, "args")
                         env     <- strMap(fields, "env")
                       yield McpTransport.Stdio(command, Chunk.fromIterable(args), env)
                     case Some(Json.Str("http")) =>
                       for
                         _       <- forbid(fields, "http", "command", "args", "env")
                         url     <- str(fields, "url").flatMap(_.toRight("http 必须有 url"))
                         _       <- Either.cond(url.startsWith("http://") || url.startsWith("https://"), (), "url 要以 http:// 或 https:// 开头")
                         headers <- strMap(fields, "headers")
                       yield McpTransport.Http(url, headers)
                     case _ => Left("transport 必须是 \"stdio\" 或 \"http\"")
    yield McpConfig(transport, enabled)

  private def forbid(fields: Map[String, Json], t: String, keys: String*): Either[String, Unit] =
    keys.filter(fields.contains).toList match
      case Nil => Right(())
      case ks  => Left(s"$t 不能有 ${ks.mkString("、")}")

  private def str(fields: Map[String, Json], k: String): Either[String, Option[String]] = fields.get(k) match
    case None              => Right(None)
    case Some(Json.Str(s)) => Right(Some(s))
    case Some(_)           => Left(s"$k 应是字符串")

  private def strList(fields: Map[String, Json], k: String): Either[String, List[String]] = fields.get(k) match
    case None => Right(Nil)
    case Some(Json.Arr(xs)) =>
      val ss = xs.collect { case Json.Str(s) => s }
      Either.cond(ss.size == xs.size, ss.toList, s"$k 应是字符串数组")
    case Some(_) => Left(s"$k 应是字符串数组")

  private def strMap(fields: Map[String, Json], k: String): Either[String, Map[String, String]] = fields.get(k) match
    case None => Right(Map.empty)
    case Some(Json.Obj(kvs)) =>
      val ss = kvs.collect { case (n, Json.Str(s)) => n -> s }
      Either.cond(ss.size == kvs.size, ss.toMap, s"$k 的值都应是字符串")
    case Some(_) => Left(s"$k 应是 JSON 对象")

  // ---- secret ----

  private val secretKeys  = Set("description", "ciphertext", "passkey", "signing", "allow_shell")
  private val passkeyKeys = Set("rp", "credential", "iv", "ciphertext")

  /** 服务端加密的写 `ciphertext`（Fernet 密文）；passkey 加密的写 `[passkey]` 表（rp、credential、iv、ciphertext）；
    * 签名钥匙写 `[signing]` 表（algorithm 加 passkey 的四项），三者恰好一个。
    */
  def parseSecret(text: String): Either[String, SecretFile] =
    for
      t     <- TomlFile.parse(text).map(TomlFile.fields)
      _     <- t.only(secretKeys)
      desc  <- t.string("description")
      ct    <- t.string("ciphertext")
      pk    <- t.table("passkey")
      sg    <- t.table("signing")
      lock  <- (ct, pk, sg) match
                 case (Some(token), None, None) => Right(SecretLock.Server(token))
                 case (None, Some(p), None)     => passkeyBox(p, passkeyKeys).map(SecretLock.Passkey(_))
                 case (None, None, Some(g)) =>
                   for
                     alg <- g.requiredString("algorithm").flatMap(muliao.contract.SigningAlgorithm.parse)
                     box <- passkeyBox(g, passkeyKeys + "algorithm")
                   yield SecretLock.Signing(alg, box)
                 case (None, None, None) => Left("缺少 ciphertext（或 [passkey]、[signing]）")
                 case _                  => Left("ciphertext、[passkey]、[signing] 只能有一个")
      shell <- t.bool("allow_shell")
    yield SecretFile(desc.getOrElse(""), lock, shell.getOrElse(true))

  private def passkeyBox(p: TomlFile.Fields, allowed: Set[String]): Either[String, muliao.contract.PasskeyBox] =
    for
      _    <- p.only(allowed)
      rp   <- p.requiredString("rp")
      cred <- p.requiredString("credential")
      iv   <- p.requiredString("iv")
      ct   <- p.requiredString("ciphertext")
    yield muliao.contract.PasskeyBox(rp, cred, iv, ct)

  def renderSecret(s: SecretFile): String =
    val desc  = s"description = ${TomlFile.str(s.description)}"
    val shell = Option.when(!s.allowShell)("allow_shell = false").toList
    def box(b: muliao.contract.PasskeyBox) =
      List(s"rp = ${TomlFile.str(b.rp)}", s"credential = ${TomlFile.str(b.credential)}", s"iv = ${TomlFile.str(b.iv)}", s"ciphertext = ${TomlFile.str(b.ciphertext)}")
    val lines = s.lock match
      case SecretLock.Server(token) => List(desc, s"ciphertext = ${TomlFile.str(token)}") ++ shell
      case SecretLock.Passkey(b)    => List(desc) ++ shell ++ List("", "[passkey]") ++ box(b)
      case SecretLock.Signing(a, b) => List(desc) ++ shell ++ List("", "[signing]", s"algorithm = ${TomlFile.str(a.label)}") ++ box(b)
    lines.mkString("", "\n", "\n")

  /** secret 名：大写字母开头，大写字母、数字、下划线。 */
  def validSecretKey(k: String): Boolean = k.matches("[A-Z][A-Z0-9_]{0,127}")

  /** skill、MCP、共享记忆的名字：小写字母、数字和 `-`。 */
  def validSlug(n: String): Boolean = n.matches("[a-z0-9][a-z0-9-]{0,63}")

  // ---- 记忆 ----

  /** 规整记忆文本：每行去掉行尾空白、连续空行合并成一个、去掉首尾空行，末尾一个换行。 */
  def normalizeMemory(text: String): String =
    val lines = text.replace("\r\n", "\n").split("\n", -1).toList.map(_.replaceAll("\\s+$", ""))
    val squeezed = lines.foldLeft(List.empty[String]) {
      case (acc @ ("" :: _), "") => acc
      case (acc, l)              => l :: acc
    }.reverse
    val trimmed = squeezed.dropWhile(_.isEmpty).reverse.dropWhile(_.isEmpty).reverse
    if trimmed.isEmpty then "" else trimmed.mkString("", "\n", "\n")

  /** 记忆长度：规整后的字符数（按 Unicode 码点）。 */
  def memoryLength(text: String): Int =
    val n = normalizeMemory(text)
    val t = n.stripSuffix("\n")
    t.codePointCount(0, t.length)
