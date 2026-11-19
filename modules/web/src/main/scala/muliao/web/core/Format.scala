package muliao.web.core

import com.raquo.airstream.core.{EventStream, Signal}

import scala.scalajs.js

/** 界面上数字、时间、模型名的写法：每种只写一份（纯函数，`time`/`clock` 按浏览器本地时区）。 */
object Format:
  private val dateFormat = js.Dynamic.newInstance(js.Dynamic.global.Intl.DateTimeFormat)(js.undefined, js.Dynamic.literal(month = "numeric", day = "numeric", hour = "2-digit", minute = "2-digit"))

  /** 「11/19 15:45」这种月日加时分。 */
  def time(ms: Long): String = dateFormat.format(new js.Date(ms.toDouble)).asInstanceOf[String]

  /** 一天内的写成「刚刚」「5 分钟前」「3 小时前」，更早的同 [[time]]。 */
  def relative(ms: Long): String =
    val diff = (js.Date.now() - ms) / 1000
    if diff < 60 then "刚刚"
    else if diff < 3600 then s"${(diff / 60).toInt} 分钟前"
    else if diff < 86400 then s"${(diff / 3600).toInt} 小时前"
    else time(ms)

  /** 本地时间 HH:MM:SS；不是今天的前面加上 M-D。 */
  def clock(at: Double, now: Double): String =
    val d = new js.Date(at)
    def p(n: Double) = f"${n.toInt}%02d"
    val t = s"${p(d.getHours())}:${p(d.getMinutes())}:${p(d.getSeconds())}"
    if new js.Date(now).toDateString() == d.toDateString() then t else s"${d.getMonth().toInt + 1}-${d.getDate().toInt} $t"

  /** 4.2s、42s、1m23s、1h02m；whole：每秒刷新的计时只到整秒。 */
  def duration(ms: Double, whole: Boolean = false): String =
    val s = ms / 1000
    if s < 10 && !whole then f"$s%.1fs"
    else if s < 60 then s"${s.toInt}s"
    else
      val m = (s / 60).toInt
      if m < 60 then f"${m}m${(s % 60).toInt}%02ds" else f"${m / 60}h${m % 60}%02dm"

  /** token 数按千/百万缩写：几十万的数字逐位读没有意义。 */
  def tokens(n: Long): String =
    if n >= 1000000 then f"${n / 1e6}%.1fM"
    else if n >= 10000 then s"${math.round(n / 1000.0)}K"
    else if n >= 1000 then f"${n / 1000.0}%.1fK"
    else n.toString

  /** 缓存命中比例（整数百分比）：输入是总数、缓存读是其中一部分；没有输入时为 0。 */
  def cacheHitPercent(input: Long, cacheRead: Long): Int = if input == 0 then 0 else math.round(cacheRead * 100.0 / input).toInt

  /** 模型 `<服务商>:<模型名>` 只显示模型名。 */
  def modelName(m: String): String = m.indexOf(':') match
    case -1 => m
    case i  => m.substring(i + 1)

  /** 模型 `<服务商>:<模型名>` 显示成「服务商 · 模型名」。 */
  def modelLabel(m: String): String = m.split(":", 2) match
    case Array(p, n) => s"$p · $n"
    case _           => m

/** 每隔一段时间一跳的当前时刻（毫秒），给运行中的计时用；没人订阅时不跳。 */
object Clock:
  def every(ms: Int): Signal[Double] = EventStream.periodic(ms).map(_ => js.Date.now()).toSignal(js.Date.now())
