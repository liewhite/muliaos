package muliao.models.mcp

import muliao.platform.AppError
import muliao.toolkit.{McpTransport, VaultRef}
import zio.*

/** @param name 名字，也是它的工具名前缀（`<name>_<工具名>`） */
final case class McpServerSpec(name: String, transport: McpTransport)

/** 代入 `${vault:KEY}` 时取 secret 明文。由资源领域按使用者的权限实现：不存在、无权使用、解不开都以领域错误失败。 */
trait SecretLookup:
  def reveal(key: String): IO[AppError, String]

/** 用到的 secret 明文（名字 → 值）：错误信息、工具结果、进程日志里出现就打码。值不出现在 toString 里。 */
final case class Revealed(values: Map[String, String]):
  override def toString: String = s"Revealed(${values.keys.mkString(",")})"
  def ++(o: Revealed): Revealed = Revealed(values ++ o.values)

object Revealed:
  val none: Revealed = Revealed(Map.empty)

/** 代入了 secret 的值，连同用到的明文。 */
final case class Filled[+A](value: A, revealed: Revealed):
  def map[B](f: A => B): Filled[B] = Filled(f(value), revealed)
  def zip[B](o: Filled[B]): Filled[(A, B)] = Filled((value, o.value), revealed ++ o.revealed)

object Filled:
  def all[A](xs: Iterable[Filled[A]]): Filled[List[A]] = Filled(xs.map(_.value).toList, xs.foldLeft(Revealed.none)(_ ++ _.revealed))

/** `${vault:KEY}` 模板。 */
object VaultTemplate:
  /** 代入：换掉每个 `${vault:KEY}`，并交回用到的明文。 */
  def fill(text: String, lookup: SecretLookup): IO[AppError, Filled[String]] =
    ZIO.foreach(VaultRef.keys(text).toList)(k => lookup.reveal(k).map(k -> _)).map { pairs =>
      val values = pairs.toMap
      Filled(VaultRef.Pattern.replaceAllIn(text, m => java.util.regex.Matcher.quoteReplacement(values(m.group(1)))), Revealed(values))
    }

  /** 把整份连接方式里的模板都代入。 */
  def fill(t: McpTransport, lookup: SecretLookup): IO[AppError, Filled[McpTransport]] =
    def one(s: String)              = fill(s, lookup)
    def many(xs: Chunk[String])     = ZIO.foreach(xs)(one).map(r => Filled.all(r).map(Chunk.fromIterable))
    def map(m: Map[String, String]) = ZIO.foreach(m.toList)((k, v) => one(v).map(_.map(k -> _))).map(r => Filled.all(r).map(_.toMap))
    t match
      case McpTransport.Stdio(cmd, args, env) =>
        many(args).zipWith(map(env))(_.zip(_)).map(_.map((a, e) => McpTransport.Stdio(cmd, a, e)))
      case McpTransport.Http(url, headers) =>
        one(url).zipWith(map(headers))(_.zip(_)).map(_.map((u, h) => McpTransport.Http(u, h)))
