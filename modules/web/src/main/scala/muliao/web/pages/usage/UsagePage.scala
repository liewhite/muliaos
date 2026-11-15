package muliao.web.pages.usage

import com.raquo.laminar.api.L.*
import muliao.contract.{SessionsApi, UsageLine}
import muliao.web.core.*
import muliao.web.ui.*

import scala.scalajs.js

/** 统计区间。 */
enum Window(val label: String, val days: Int) extends Shown.Self:
  case Week  extends Window("近 7 天", 7)
  case Month extends Window("近 30 天", 30)

/** 一组用量的合计。输入是总数，缓存读写是其中的两部分，不相加。 */
final case class Totals(requests: Long, input: Long, output: Long, cacheRead: Long, cacheWrite: Long):
  def +(o: Totals): Totals = Totals(requests + o.requests, input + o.input, output + o.output, cacheRead + o.cacheRead, cacheWrite + o.cacheWrite)

  /** 缓存命中比例（整数百分比）；没有输入时为 0。 */
  def cacheHitPercent: Int = if input == 0 then 0 else math.round(cacheRead.toDouble / input * 100).toInt

object Totals:
  val zero: Totals = Totals(0, 0, 0, 0, 0)

  def of(l: UsageLine): Totals = Totals(l.requests, l.input, l.output, l.cacheRead, l.cacheWrite)

  def sum(ls: List[UsageLine]): Totals = ls.map(of).foldLeft(zero)(_ + _)

/** 一行：分组的名字与合计。 */
final case class Row(key: String, label: String, totals: Totals)

/** 接口给的是「天 × agent × 模型」的明细，页面上的各种汇总都从它算。 */
final case class Report(total: Totals, byAgent: List[Row], byModel: List[Row], byDay: List[Row])

object Report:
  /** 按某个维度汇总，用量大的在前。 */
  private def by(lines: List[UsageLine])(key: UsageLine => String, label: UsageLine => String): List[Row] =
    lines.groupBy(key).toList.map((k, ls) => Row(k, label(ls.head), Totals.sum(ls))).sortBy(r => -r.totals.input)

  def of(lines: List[UsageLine]): Report =
    Report(
      Totals.sum(lines),
      // 已删除的 agent 也有名字（接口按记账时的名字给），账目不随 agent 删除而消失
      by(lines)(_.agent.value, _.agentName),
      by(lines)(_.model, l => if l.model.isEmpty then "未知" else l.model),
      lines.groupBy(_.day).toList.map((d, ls) => Row(d, d, Totals.sum(ls))).sortBy(_.key)
    )

  /** 最近 days 个自然日：从 days-1 天前的浏览器本地零点起，到此刻（毫秒）。按天的分组由服务端按它的时区划分。 */
  def window(days: Int, nowMs: Double): (Long, Long) =
    val since = new js.Date(nowMs)
    since.setHours(0, 0, 0, 0)
    since.setDate(since.getDate() - (days - 1))
    (since.getTime().toLong, nowMs.toLong)

/** token 数量按千/百万缩写：几十万的数字逐位读没有意义。 */
object Tokens:
  def format(n: Long): String =
    if n >= 1000000 then f"${n / 1e6}%.1fM"
    else if n >= 10000 then s"${math.round(n / 1000.0)}K"
    else if n >= 1000 then f"${n / 1e3}%.1fK"
    else n.toString

object UsagePage:
  def view(): HtmlElement =
    val window = Var(Window.Week)
    Ui.page("用量")(
      div(
        Fields.segmented("统计区间", window.signal, window.set, Window.values.toList)
      ),
      // 换区间重新拉一份（用量只增不改，不随主题刷新）
      child <-- window.signal.map { w =>
        val (from, to) = Report.window(w.days, js.Date.now())
        val report     = Remote(SessionsApi.usage)((from, to)).map(_.map(Report.of))
        div(cls := "flex flex-col gap-3", Ui.loading(report)(r => children <-- r.map(sections)))
      }
    )

  private def sections(r: Report): List[HtmlElement] = List(
    div(
      cls := "card card--default p-0",
      dataAttr("slot") := "card",
      row("合计", r.total),
      div(cls := "border-t border-separator px-4 py-2 text-xs text-muted", s"输入 token 已包含缓存读写；这段时间缓存命中 ${r.total.cacheHitPercent}%")
    ),
    section("按 agent", r.byAgent),
    section("按模型", r.byModel),
    daily(r.byDay)
  )

  private def stat(label: String, value: Long): HtmlElement =
    div(cls := "flex flex-col", span(cls := "text-[11px] text-muted", label), span(cls := "font-mono text-sm tabular-nums", Tokens.format(value)))

  private def row(label: String, t: Totals): HtmlElement =
    div(
      cls := "flex flex-col gap-1.5 px-4 py-3",
      div(
        cls := "flex items-baseline justify-between gap-2",
        span(cls := "min-w-0 truncate font-medium", label),
        span(cls := "shrink-0 text-xs text-muted", s"${t.requests} 次请求")
      ),
      div(cls := "grid grid-cols-4 gap-2", stat("输入", t.input), stat("输出", t.output), stat("缓存读", t.cacheRead), stat("缓存写", t.cacheWrite))
    )

  private def titled(title: String)(content: HtmlElement): HtmlElement =
    div(
      cls := "card card--default p-0",
      dataAttr("slot") := "card",
      div(cls := "border-b border-separator px-4 py-2 text-sm font-medium text-muted", title),
      content
    )

  private def section(title: String, rows: List[Row]): HtmlElement =
    titled(title)(
      if rows.isEmpty then Ui.emptyHint("这段时间没有用量")
      else div(cls := "flex flex-col divide-y divide-separator", rows.map(r => row(r.label, r.totals)))
    )

  /** 按天的输入 token 条形图：条长按这段时间里的最大值归一，深色部分为缓存命中。 */
  private def daily(days: List[Row]): HtmlElement =
    val peak = (1L :: days.map(_.totals.input)).max.toDouble
    titled("按天（输入 token，深色部分为缓存命中）")(
      if days.isEmpty then Ui.emptyHint("这段时间没有用量")
      else
        div(
          cls := "flex flex-col gap-2 px-4 py-3",
          days.map { d =>
            div(
              cls := "flex items-center gap-2",
              span(cls := "w-12 shrink-0 font-mono text-[11px] tabular-nums text-muted", d.label.drop(5)),
              div(
                cls := "h-4 min-w-0 flex-1 overflow-hidden rounded bg-surface-secondary",
                div(
                  cls := "h-full bg-accent-soft",
                  styleAttr := s"width:${d.totals.input / peak * 100}%",
                  div(cls := "h-full bg-accent", styleAttr := s"width:${d.totals.cacheHitPercent}%")
                )
              ),
              span(cls := "w-12 shrink-0 text-right font-mono text-[11px] tabular-nums", Tokens.format(d.totals.input))
            )
          }
        )
    )
