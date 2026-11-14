package muliao.workspace.text

/** 读文件时按行切出一段，按 `cat -n` 格式渲染，受行数与字符预算两重限制。纯函数。 */
object Listing:

  /** 这一段为什么停下。 */
  enum Stop:
    /** 读到了文件末尾 */
    case End
    /** 到了要求的行数，`next` 是续读的 offset */
    case Limit(next: Int)
    /** 到了输出上限，`next` 是续读的 offset */
    case Budget(next: Int)

  /** 切出的一段：`first`..`last` 行（读空文件时两者都是 0），`text` 是渲染好的行，`size` 是占用的字符数。 */
  final case class Slice(text: String, first: Int, last: Int, total: Int, stop: Stop):
    def size: Int = text.length

  /** 一行最多显示多少字符（极长的行，如压缩过的代码，截断显示）。 */
  val MaxLineChars = 4000

  /** 从第 `offset` 行起最多 `limit` 行，渲染后不超过 `budget` 个字符（但至少给一行，免得永远读不动）。 */
  def slice(index: LineIndex, offset: Int, limit: Int, budget: Int): Either[String, Slice] =
    val total = index.count
    if offset < 1 then Left("offset 从 1 起")
    else if limit < 1 then Left("limit 要大于 0")
    else if total == 0 then Right(Slice("", 0, 0, 0, Stop.End))
    else if offset > total then Left(s"offset $offset 超出了文件行数（共 $total 行）")
    else
      @annotation.tailrec
      def go(n: Int, acc: Vector[String], used: Int): Slice =
        val rendered = render(n, index.line(n))
        if n > offset && used + rendered.length > budget then Slice(acc.mkString, offset, n - 1, total, Stop.Budget(n))
        else
          val next = acc :+ rendered
          if n == total then Slice(next.mkString, offset, n, total, Stop.End)
          else if n - offset + 1 == limit then Slice(next.mkString, offset, n, total, Stop.Limit(n + 1))
          else go(n + 1, next, used + rendered.length)
      Right(go(offset, Vector.empty, 0))

  private def render(n: Int, raw: String): String =
    val text =
      if raw.length > MaxLineChars then raw.take(MaxLineChars) + s" [这一行有 ${raw.length} 个字符，只显示前 $MaxLineChars 个]"
      else raw
    CatN.line(n, text) + "\n"

  /** 单独读一个文件时，结尾说明怎么续读的那一行（读完了没有）。 */
  def continuation(s: Slice): Option[String] = s.stop match
    case Stop.End          => None
    case Stop.Limit(next)  => Some(s"[还有 ${s.total - s.last} 行；从 offset=$next 继续读]")
    case Stop.Budget(next) => Some(s"[到了输出上限，停在第 ${s.last} 行（共 ${s.total} 行）；从 offset=$next 继续读]")

  /** 多段读取时每段的标题（`head` 的样子）。 */
  def header(path: String, s: Slice): String =
    if s.total == 0 then s"==> $path (empty) <=="
    else s"==> $path lines ${s.first}-${s.last} of ${s.total} <=="

  def errorHeader(path: String, error: String): String = s"==> $path: $error <=="
