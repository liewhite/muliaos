package muliao.toolkit

import muliao.contract.IsLabeled
import zio.Chunk
import zio.schema.{Schema, StandardType}

import scala.annotation.StaticAnnotation

/** 字符串参数只能取这些值（生成 JSON Schema 的 `enum`）。 */
final case class choices(values: List[String]) extends StaticAnnotation

/** 有固定写法的枚举（[[muliao.contract.Labeled]]）直接当工具参数：JSON 里写它的 `label`，JSON Schema 里列出全部取值。
  * 用法：参数类型所在处 `given Schema[IssueStatus] = LabeledArg.schema`。
  */
object LabeledArg:
  def schema[E](using l: IsLabeled[E]): Schema[E] =
    Schema.Primitive(StandardType.StringType, Chunk(choices(l.labels))).transformOrFail(l.parse, e => Right(l.label(e)))
