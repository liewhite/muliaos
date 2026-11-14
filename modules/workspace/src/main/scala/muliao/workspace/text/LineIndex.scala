package muliao.workspace.text

/** 一段文本按行的索引：行号从 1 起。行以 `\n` 结尾，最后一行可以没有换行；空文本没有行。 */
final class LineIndex private (val text: String, starts: Array[Int]):
  /** 行数。 */
  val count: Int =
    if text.isEmpty then 0
    else if text.endsWith("\n") then starts.length - 1
    else starts.length

  /** 第 n 行开头的字符位置。 */
  def start(n: Int): Int = starts(n - 1)

  /** 第 n 行末尾（不含换行）。 */
  def end(n: Int): Int =
    val next = if n < starts.length then starts(n) else text.length
    if next > start(n) && text.charAt(next - 1) == '\n' then next - 1 else next

  /** 第 n 行末尾（含换行，如果有）。 */
  def endWithNewline(n: Int): Int = if n < starts.length then starts(n) else text.length

  /** 第 n 行的内容（不含换行与行尾的 `\r`）。 */
  def line(n: Int): String = text.substring(start(n), end(n)).stripSuffix("\r")

  /** 字符位置所在的行（超出末尾的算最后一行）。 */
  def lineAt(offset: Int): Int =
    if count == 0 then 0
    else
      val i = java.util.Arrays.binarySearch(starts, offset)
      val n = if i >= 0 then i + 1 else -i - 1
      n.max(1).min(count)

object LineIndex:
  def apply(text: String): LineIndex =
    val b = Array.newBuilder[Int]
    b += 0
    var i = text.indexOf('\n')
    while i >= 0 do
      b += i + 1
      i = text.indexOf('\n', i + 1)
    new LineIndex(text, b.result())

/** `cat -n` 的格式：行号右对齐占 6 格、一个制表符、行的原文。 */
object CatN:
  def line(n: Int, text: String): String = f"$n%6d\t$text"
