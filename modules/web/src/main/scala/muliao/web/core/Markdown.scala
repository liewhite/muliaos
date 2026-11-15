package muliao.web.core

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import scala.scalajs.js

/** 把模型输出里的公式整理成 KaTeX 扩展能无歧义解析的形式。只在代码之外处理（纯函数，有测试）。
  *
  * 扩展只认 `$…$` 与 `$$…$$`，模型却常写 `\(…\)` 与 `\[…\]`；而且单个 `$` 在普通文本里多半是价格。规则：
  *   - 已有的 `$$…$$` 原样保留。
  *   - `\[…\]` 独占一行（前面可以有缩进或引用符号 >）时转成块级公式并保留前缀；不独占一行时转成行内的 `$…$`。`\(…\)` 转成行内的 `$…$`。
  *   - `\[` 与 `\(` 也是 Markdown 合法的转义（如引用编号 `\[1\]`）：括号里不像公式（没有字母、\、^、_、=、{}）就不转换。
  *   - 单个 `$` 按 Pandoc 的规则判定：开头的 `$` 右边紧跟非空白，结尾的 `$` 左边紧跟非空白、后面不是数字，所以「$20,000 和 $30,000」不是公式。
  *     不成对的 `$` 一律转义成字面的 `$`。
  *
  * 代码按 CommonMark 识别：``` 或 ~~~ 开头的围栏代码块（未闭合时到文末，流式输出中途也不改代码），以及按反引号个数配对的行内代码。
  */
object MathText:
  private val Fence         = """^ {0,3}(`{3,}|~{3,})""".r
  private val InlineCode    = """(`+)(?:(?!\1)[\s\S])*?\1(?!`)""".r
  private val LooksLikeMath = """[A-Za-z\\^_={}]""".r
  private def hole(i: Int)  = s"\uE000$i\uE001"
  private val Holes         = "\uE000(\\d+)\uE001".r

  /** 按行切成围栏代码块与其余部分；按顺序用换行连回去就是原文。 */
  private def splitFences(text: String): List[(Boolean, String)] =
    final case class Acc(done: Vector[(Boolean, Vector[String])], code: Boolean, lines: Vector[String], fence: Option[(Char, Int)])
    def flush(a: Acc, nextCode: Boolean) =
      a.copy(done = if a.lines.isEmpty then a.done else a.done :+ (a.code -> a.lines), code = nextCode, lines = Vector.empty)
    val end = text.split("\n", -1).foldLeft(Acc(Vector.empty, false, Vector.empty, None)) { (a, line) =>
      val marker = Fence.findFirstMatchIn(line).map(_.group(1))
      (a.fence, marker) match
        case (None, Some(m)) => flush(a, nextCode = true).copy(lines = Vector(line), fence = Some(m.head -> m.length))
        case (Some((c, n)), Some(m)) if m.head == c && m.length >= n && line.trim == m =>
          flush(a.copy(lines = a.lines :+ line), nextCode = false).copy(fence = None)
        case _ => a.copy(lines = a.lines :+ line)
    }
    flush(end, nextCode = false).done.toList.map((c, ls) => c -> ls.mkString("\n"))

  private def prose(text: String): String =
    val holes = scala.collection.mutable.ArrayBuffer.empty[String]
    def keep(s: String): String =
      holes += s
      hole(holes.size - 1)
    def isMath(s: String) = LooksLikeMath.findFirstIn(s).isDefined
    val q = java.util.regex.Matcher.quoteReplacement
    var out = InlineCode.replaceAllIn(text, m => q(keep(m.matched)))
    out = """\$\$[\s\S]+?\$\$""".r.replaceAllIn(out, m => q(keep(m.matched)))
    out = """(?m)^([ \t]*(?:>[ \t]?)*)\\\[([\s\S]+?)\\\][ \t]*$""".r.replaceAllIn(
      out,
      m =>
        val (prefix, math) = (m.group(1), m.group(2))
        q(if isMath(math) then keep(s"$prefix$$$$\n$prefix${math.trim}\n$prefix$$$$") else m.matched)
    )
    out = """\\\[([^\n]+?)\\\]""".r.replaceAllIn(out, m => q(if isMath(m.group(1)) then keep(s"$$${m.group(1).trim}$$") else m.matched))
    out = """\\\(([^\n]+?)\\\)""".r.replaceAllIn(out, m => q(if isMath(m.group(1)) then keep(s"$$${m.group(1).trim}$$") else m.matched))
    out = """(?<![\\$])\$(?=[^\s$])((?:[^$\\\n]|\\.)+?)(?<=\S)\$(?![\d$])""".r.replaceAllIn(out, m => q(keep(m.matched)))
    out = """(?<!\\)\$""".r.replaceAllIn(out, q("\\$"))
    while Holes.findFirstIn(out).isDefined do out = Holes.replaceAllIn(out, m => q(holes(m.group(1).toInt)))
    out

  def normalize(text: String): String =
    splitFences(text).map((code, s) => if code then s else prose(s)).mkString("\n")

/** 正文里独立的 `#数字` 是本站 issue 引用。前面紧挨字母、数字、下划线或 & / # 的不算（owner/repo#123、&#123;、URL 片段），后面紧跟字母数字的也不算。 */
object IssueRefs:
  val Pattern = """(?<![\p{L}\p{N}_&/#])#(\d+)(?![\p{L}\p{N}_])""".r

  /** 把文本节点里的引用换成链接；链接、代码、公式里的不动。 */
  def link(root: dom.Element): Unit =
    val walker = dom.document.createTreeWalker(root, dom.NodeFilter.SHOW_TEXT, null, false)
    val nodes  = Iterator.continually(walker.nextNode()).takeWhile(_ != null).toList
    nodes.foreach { n =>
      val text   = n.textContent
      val inside = Option(n.parentNode).collect { case e: dom.Element => e }.flatMap(e => Option(e.closest("a,code,pre,.katex")))
      if inside.isEmpty && Pattern.findFirstIn(text).isDefined then
        val frag = dom.document.createDocumentFragment()
        var at   = 0
        Pattern.findAllMatchIn(text).foreach { m =>
          frag.appendChild(dom.document.createTextNode(text.substring(at, m.start)))
          val a = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
          a.href = Route.issue(muliao.contract.IssueId.wrap(m.group(1).toLong)).url
          a.textContent = m.matched
          frag.appendChild(a)
          at = m.end
        }
        frag.appendChild(dom.document.createTextNode(text.substring(at)))
        n.parentNode.replaceChild(frag, n): Unit
    }

object Markdown:
  /** 本站附件的地址前缀（由下载接口生成）：只有它们的图片直接显示。 */
  lazy val FilesPath: String = Http.href(muliao.contract.SessionsApi.download)(muliao.contract.FileId.wrap("_")).stripSuffix("_")

  private lazy val marked =
    val m = new MarkedJs()
    m.use(MarkedKatex(js.Dynamic.literal(throwOnError = false, nonStandard = true)))
    m.use(
      js.Dynamic.literal(
        gfm = true,
        renderer = js.Dynamic.literal(
          code = ((token: js.Dynamic) =>
            val code = token.text.asInstanceOf[String]
            val lang = token.lang.asInstanceOf[js.UndefOr[String]].toOption.map(_.trim.split("\\s+").head).filter(_.nonEmpty)
            val html = lang.filter(l => Hljs.getLanguage(l).isDefined) match
              case Some(l) => Hljs.highlight(code, js.Dynamic.literal(language = l)).value.asInstanceOf[String]
              case None    => escape(code)
            s"""<pre><code class="hljs">$html</code></pre>"""
          ): js.Function1[js.Dynamic, String]
        )
      )
    )
    m

  private def escape(s: String) = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  // 外链图片会在打开会话时由浏览器自动请求，被注入的模型可以借它把对话内容拼进 URL 发出去：只显示本站附件里的图片，其余换成链接。
  // 外部链接在新标签页打开。
  private lazy val purifyReady =
    DomPurify.addHook(
      "afterSanitizeAttributes",
      (node: dom.Element) =>
        node.tagName match
          case "IMG" =>
            val src = Option(node.getAttribute("src")).getOrElse("")
            if src.startsWith(FilesPath) then
              node.setAttribute("loading", "lazy")
              node.setAttribute("class", "max-h-96 rounded-lg")
            else
              val a = dom.document.createElement("a")
              a.setAttribute("href", src)
              a.setAttribute("target", "_blank")
              a.setAttribute("rel", "noreferrer noopener")
              a.textContent = Option(node.getAttribute("alt")).filter(_.nonEmpty).getOrElse(src)
              Option(node.parentNode).foreach(_.replaceChild(a, node))
          case "A" =>
            val href = Option(node.getAttribute("href")).getOrElse("")
            if href.matches("(?i)^https?:.*") then
              node.setAttribute("target", "_blank")
              node.setAttribute("rel", "noreferrer noopener")
          case _ => ()
    )
    true

  def html(text: String): String =
    purifyReady
    DomPurify.sanitize(marked.parse(MathText.normalize(text)), js.Dynamic.literal(ADD_ATTR = js.Array("target")))

  /** 站内链接走前端路由并带上来源页（目标页的返回回到这里）。 */
  def interceptAppLinks(e: dom.MouseEvent): Unit =
    val a = Option(e.target).collect { case el: dom.Element => el }.flatMap(el => Option(el.closest("a")))
    a.map(_.getAttribute("href")).filter(h => h != null && Router.isAppPath(h)).foreach { href =>
      if !(e.metaKey || e.ctrlKey || e.shiftKey) then
        e.preventDefault()
        Router.go(href, withFrom = true)
    }

  /** 渲染 Markdown（随 `text` 更新）。 */
  def view(text: Signal[String]): HtmlElement =
    div(
      cls := "prose prose-sm max-w-none break-words dark:prose-invert prose-pre:overflow-x-auto",
      onClick --> interceptAppLinks,
      inContext { el =>
        text --> { t =>
          el.ref.innerHTML = html(t)
          IssueRefs.link(el.ref)
        }
      }
    )

  def view(text: String): HtmlElement = view(Val(text))
