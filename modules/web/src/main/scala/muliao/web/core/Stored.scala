package muliao.web.core

import com.raquo.laminar.api.L.{unsafeWindowOwner, Var}
import org.scalajs.dom

/** 浏览器里跨刷新记住的小设置（localStorage），改了就存。 */
object Stored:
  private def get(key: String): Option[String] = Option(dom.window.localStorage.getItem(key))

  private def kept[T](key: String, initial: T, show: T => String): Var[T] =
    val v = Var(initial)
    v.signal.foreach(x => dom.window.localStorage.setItem(key, show(x)))(using unsafeWindowOwner)
    v

  /** 记住选了哪一项（存枚举名）；存的值不认识（旧版本留下的、被手改坏的）时取 `fallback`。全站一份，放在页面对象的字段里。 */
  def choice[T](key: String, values: Seq[T], fallback: T): Var[T] =
    kept(key, get(key).flatMap(s => values.find(_.toString.equalsIgnoreCase(s))).getOrElse(fallback), _.toString)

  def text(key: String, default: String): Var[String] = kept(key, get(key).getOrElse(default), identity)
