package muliao.web

import com.raquo.laminar.api.L.*
import org.scalajs.dom

/** 前端入口（#875 实现）。 */
object Main:
  def main(args: Array[String]): Unit =
    renderOnDomContentLoaded(dom.document.getElementById("app"), div("muliao"))
