package muliao.resources.access

import muliao.contract.{AssetKind, Labeled, Labels}
import muliao.platform.config.ConfigCodecs

import java.nio.file.Path

/** 对资源做什么：用（进 skill 索引、注入记忆、连 MCP、解析 secret、调用工具），或改（写它的文件）。 */
enum Action(val label: String) extends Labeled:
  case Use   extends Action("use")
  case Write extends Action("write")

object Action extends Labels[Action]("动作", Action.values)

/** fnmatch 风格的名字通配：`*` 任意串、`?` 任一字符、`[...]` 字符集（`[!...]` 取反），其余按字面，区分大小写。 */
final class Glob private (val text: String):
  private val regex                   = Glob.translate(text).r
  def matches(name: String): Boolean = regex.matches(name)
  def isLiteral: Boolean              = !text.exists(c => c == '*' || c == '?' || c == '[')
  override def toString: String       = text
  override def equals(o: Any): Boolean = o match
    case g: Glob => g.text == text
    case _       => false
  override def hashCode: Int = text.hashCode

object Glob:
  def apply(text: String): Glob = new Glob(text)

  private def translate(p: String): String =
    val sb = StringBuilder()
    var i  = 0
    while i < p.length do
      p(i) match
        case '*' => sb ++= ".*"
        case '?' => sb += '.'
        case '[' =>
          val close = p.indexOf(']', i + 2)
          if close < 0 then sb ++= "\\["
          else
            val body = p.substring(i + 1, close)
            val set  = if body.startsWith("!") then "^" + body.drop(1) else body
            sb ++= "[" + set.replace("\\", "\\\\").replace("[", "\\[") + "]"
            i = close
        case c => sb ++= java.util.regex.Pattern.quote(c.toString)
      i += 1
    sb.toString

/** 规则选中的对象。 */
enum Selector:
  /** 若干种类里名字匹配通配的资源或工具。 */
  case Named(kinds: Set[AssetKind], glob: Glob)

  /** 本机资源目录以外的一个目录（含其下全部）：`write:path:<目录>`。 */
  case Path(dir: java.nio.file.Path)

/** 一条分发规则，形如 `动作:种类:名字通配` 或 `write:path:<目录>`。`text` 是原文（界面与 access.toml 里原样显示）。 */
final case class Rule private (actions: Set[Action], selector: Selector, text: String):
  def grants(action: Action): Boolean = actions(action)

  /** 是否选中某种类里的某个名字。 */
  def selects(action: Action, kind: AssetKind, name: String): Boolean =
    actions(action) && (selector match
      case Selector.Named(kinds, glob) => kinds(kind) && glob.matches(name)
      case Selector.Path(_)            => false)

  override def toString: String = text

object Rule:

  /** 解析规则原文；不合法时说明原因。 */
  def parse(text: String): Either[String, Rule] =
    text.split(":", 3).toList match
      case List(a, k, n) =>
        for
          actions <- parseAction(a)
          rule <- k match
                    case "path" =>
                      if !actions(Action.Write) then Left(s"规则 `$text`：path 只能写成 write:path:<目录>")
                      else if n.isEmpty || !(n.startsWith("/") || n.startsWith("~/")) then
                        Left(s"规则 `$text`：path 后面要写绝对路径或 ~/ 开头的路径")
                      else Right(Rule(Set(Action.Write), Selector.Path(ConfigCodecs.expandHome(n).normalize), text))
                    case _ =>
                      for
                        kinds <- parseKinds(k, text)
                        _ <- Either.cond(n.nonEmpty, (), s"规则 `$text`：缺名字（任意名字写 *）")
                        _ <- Either.cond(
                               !(kinds == Set(AssetKind.Tool) && actions == Set(Action.Write)),
                               (),
                               s"规则 `$text`：工具只能 use，没有 write"
                             )
                      yield Rule(actions, Selector.Named(kinds, Glob(n)), text)
        yield rule
      case _ => Left(s"规则 `$text` 格式不对，应写成 动作:种类:名字，如 use:skill:relay-host")

  private def parseAction(a: String): Either[String, Set[Action]] = a match
    case "*" => Right(Action.values.toSet)
    case s   => Action.parse(s).map(Set(_)).left.map(_ => s"动作 `$s` 不认识，只能是 use、write 或 *")

  private def parseKinds(k: String, text: String): Either[String, Set[AssetKind]] = k match
    case "*" => Right(AssetKind.values.toSet)
    case s =>
      AssetKind.parse(s).map(Set(_)).left.map(_ => s"规则 `$text`：种类 `$s` 不认识，只能是 tool、skill、mcp、secret、memory、path 或 *")

  /** 内部生成的规则（内置角色），原文由选择器给出。 */
  private[resources] def builtin(actions: Set[Action], kinds: Set[AssetKind], glob: String): Rule =
    val a = if actions.size == Action.values.length then "*" else actions.head.label
    val k = if kinds.size == AssetKind.values.length then "*" else kinds.head.label
    Rule(actions, Selector.Named(kinds, Glob(glob)), s"$a:$k:$glob")

  def pathOf(r: Rule): Option[Path] = r.selector match
    case Selector.Path(p) => Some(p)
    case _                => None
