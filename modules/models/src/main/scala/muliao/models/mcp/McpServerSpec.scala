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
final case class Filled[+A](value: A, revealed: Revealed)

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
    Ref.make(Revealed.none).flatMap { used =>
      t.substitute(s => fill(s, lookup).flatMap(f => used.update(_ ++ f.revealed).as(f.value))).zipWith(used.get)(Filled(_, _))
    }
