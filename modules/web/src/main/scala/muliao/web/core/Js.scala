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

/** ed25519 签名（@noble/curves，#938）：passkey 加密的密钥对在浏览器里签名用。`secretKey` 是 32 字节种子。 */
@js.native
@JSImport("@noble/curves/ed25519.js", "ed25519")
object NobleEd25519 extends js.Object:
  def sign(message: scala.scalajs.js.typedarray.Uint8Array, secretKey: scala.scalajs.js.typedarray.Uint8Array): scala.scalajs.js.typedarray.Uint8Array = js.native
  def getPublicKey(secretKey: scala.scalajs.js.typedarray.Uint8Array): scala.scalajs.js.typedarray.Uint8Array = js.native

/** secp256k1（@noble/curves，#1092）：以太坊密钥对在浏览器里签名用。`sign` 签的是 32 字节摘要（`prehash: false`），
  * `format: "recovered"` 时返回 65 字节：恢复位‖r‖s。
  */
@js.native
@JSImport("@noble/curves/secp256k1.js", "secp256k1")
object NobleSecp256k1 extends js.Object:
  def sign(digest: scala.scalajs.js.typedarray.Uint8Array, secretKey: scala.scalajs.js.typedarray.Uint8Array, opts: js.Any): scala.scalajs.js.typedarray.Uint8Array = js.native
  def getPublicKey(secretKey: scala.scalajs.js.typedarray.Uint8Array, isCompressed: Boolean): scala.scalajs.js.typedarray.Uint8Array = js.native
