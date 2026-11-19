package muliao.web.core

import scala.scalajs.js
import scala.util.Try

/** 代码高亮（highlight.js）：对话里的代码块与文件预览共用。 */
object Highlight:
  /** 高亮后的 HTML（highlight.js 的输出已转义，可以直接插入）；认不出语言或出错时为 None，由调用方按纯文本显示。 */
  def html(code: String, language: String): Option[String] =
    Option
      .when(Hljs.getLanguage(language).isDefined)(language)
      .flatMap(l => Try(Hljs.highlight(code, js.Dynamic.literal(language = l, ignoreIllegals = true)).value.asInstanceOf[String]).toOption)
