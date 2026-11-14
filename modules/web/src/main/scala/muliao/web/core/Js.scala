package muliao.web.core

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/** lucide 的图标（npm 包 lucide，与现有界面同一套图标）。 */
@js.native
@JSImport("lucide", JSImport.Namespace)
private object LucideJs extends js.Object:
  def createElement(icon: js.Any, attrs: js.Dictionary[String]): dom.svg.SVG = js.native

/** 图标：`Icon("Menu", "size-5")`；名字是 lucide 的导出名。 */
object Icon:
  def apply(name: String, cls: String = "size-4"): SvgElement =
    val node = LucideJs.asInstanceOf[js.Dynamic].selectDynamic(name)
    if js.isUndefined(node) then throw new IllegalArgumentException(s"lucide 没有图标 $name")
    val el = LucideJs.createElement(node, js.Dictionary("class" -> cls, "aria-hidden" -> "true"))
    foreignSvgElement(el)

/** Markdown：marked（GFM）+ KaTeX 公式 + highlight.js 代码高亮，输出经 DOMPurify 清洗。 */
@js.native
@JSImport("marked", "Marked")
class MarkedJs(extensions: js.Any*) extends js.Object:
  def parse(src: String): String = js.native
  def use(ext: js.Any*): MarkedJs = js.native

@js.native
@JSImport("marked-katex-extension", JSImport.Default)
object MarkedKatex extends js.Function1[js.Any, js.Any]:
  def apply(opts: js.Any): js.Any = js.native

@js.native
@JSImport("dompurify", JSImport.Default)
object DomPurify extends js.Object:
  def sanitize(html: String, cfg: js.Any): String = js.native
  def addHook(name: String, hook: js.Function1[dom.Element, Unit]): Unit = js.native

@js.native
@JSImport("highlight.js/lib/common", JSImport.Default)
object Hljs extends js.Object:
  def highlight(code: String, opts: js.Any): js.Dynamic = js.native
  def highlightAuto(code: String): js.Dynamic = js.native
  def getLanguage(name: String): js.UndefOr[js.Any] = js.native

/** dagre 布局（issue 依赖图）。 */
@js.native
@JSImport("@dagrejs/dagre", JSImport.Default)
object Dagre extends js.Object:
  def layout(g: js.Any): Unit = js.native
  val graphlib: js.Dynamic = js.native
