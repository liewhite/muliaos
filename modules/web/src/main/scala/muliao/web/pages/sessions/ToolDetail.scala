package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Model.*

/** 一次工具调用的参数与结果，按工具的展示方式画、可切到原文：会话时间线和待批准的卡片（会话页、通知页）共用。 */
object ToolDetail:
  private val diffLine: String => String = l =>
    if l.startsWith("+") && !l.startsWith("+++") then "bg-emerald-500/15 text-emerald-700 dark:text-emerald-300"
    else if l.startsWith("-") && !l.startsWith("---") then "bg-red-500/15 text-red-700 dark:text-red-300"
    else if l.startsWith("@@") then "text-muted"
    else ""

  private def terminal(command: Option[String], output: Option[String], failed: Boolean): HtmlElement =
    div(
      cls := "my-1 max-h-96 overflow-auto rounded-lg bg-zinc-900 p-2.5 font-mono text-xs leading-relaxed text-zinc-100 dark:bg-black",
      command.map(c => div(cls := "whitespace-pre-wrap break-all", span(cls := "select-none text-emerald-400", "$ "), c)),
      output.map(o => div(cls := "whitespace-pre-wrap break-all " + (if failed then "text-red-400" else "text-zinc-300"), o))
    )

  private def titled(t: Option[String]): Option[HtmlElement] = t.map(x => div(cls := "mb-0.5 break-all font-mono text-xs text-muted", x))

  /** 参数按字段画：短的字段排成两列，多行的（文件内容、提示词）单独成块。不是 JSON 对象的照原文。 */
  private def argFields(args: String): Option[HtmlElement] =
    argsShown(args).map { shown =>
      scala.util.Try(scala.scalajs.js.JSON.parse(args).asInstanceOf[scala.scalajs.js.Dictionary[scala.scalajs.js.Any]]).toOption
        .filter(d => scala.scalajs.js.typeOf(d) == "object" && !scala.scalajs.js.Array.isArray(d)) match
        case None => pre(cls := Ui.CodeBlock, shown)
        case Some(d) =>
          val fields = d.toList.map { (k, v) =>
            k -> (if scala.scalajs.js.typeOf(v) == "string" then v.asInstanceOf[String] else scala.scalajs.js.JSON.stringify(v, null: scala.scalajs.js.Array[scala.scalajs.js.Any], 2))
          }
          val (long, short) = fields.partition((_, v) => v.contains('\n') || v.length > 120)
          div(
            Option.when(short.nonEmpty)(
              dl(cls := "my-1 grid grid-cols-[auto_1fr] gap-x-3 gap-y-0.5 text-xs", short.map((k, v) => div(cls := "contents", dt(cls := "text-muted", k), dd(cls := "break-all text-foreground", v))))
            ),
            long.map((k, v) => div(titled(Some(k)), pre(cls := Ui.CodeBlock, v)))
          )
    }

  /** 参数与结果：按工具的展示方式画；可切到原文。 */
  def view(c: CallView): HtmlElement =
    val raw     = Var(false)
    val display = c.display
    val result  = c.result
    def rendered: Modifier[HtmlElement] = display match
      case Display.Terminal(Some(command)) =>
        terminal(Some(command), result.map(_.text), result.exists(_.isError))
      case Display.Terminal(None) =>
        // 没有命令参数的（skill 脚本）：参数按字段，输出进终端框
        Seq(argFields(c.args), result.map(r => terminal(None, Some(r.text), r.isError)))
      case Display.Code(path) =>
        Seq(titled(path), result.map(r => pre(cls := Ui.CodeBlock, r.text)))
      case Display.Diff(path) =>
        Seq(
          titled(path),
          result.map(r => pre(cls := Ui.CodeBlock, r.text.split("\n", -1).toList.map(l => div(cls := diffLine(l), if l.isEmpty then " " else l))))
        )
      case Display.JsonData =>
        Seq(argsShown(c.args).map(a => pre(cls := Ui.CodeBlock, a)), result.map(r => pre(cls := Ui.CodeBlock, prettyJson(r.text))))
      case Display.Markdown =>
        Seq(
          argsShown(c.args).map(a => pre(cls := Ui.CodeBlock, a)),
          result.map(r => div(cls := "my-1 rounded-lg border border-separator px-3 py-2 text-foreground", Markdown.view(r.text)))
        )
      case Display.SharedFile | Display.Plain | Display.Question | Display.Delegate(_, _) =>
        Seq(argFields(c.args), result.map(r => pre(cls := Ui.CodeBlock, r.text)))
    def original: Modifier[HtmlElement] =
      Seq(argsShown(c.args).map(a => pre(cls := Ui.CodeBlock, a)), result.map(r => pre(cls := Ui.CodeBlock, r.text)))
    div(
      child <-- raw.signal.map(r => div(if r then original else rendered)),
      Option.when(display != Display.Plain || argsShown(c.args).isDefined)(
        div(
          cls := "flex justify-end",
          button(tpe := "button", cls := "text-xs text-muted hover:text-foreground", onClick --> (_ => raw.update(!_)), child.text <-- raw.signal.map(r => if r then "渲染" else "原文"))
        )
      ),
      result.flatMap(r => Attachments.images(r.images))
    )
