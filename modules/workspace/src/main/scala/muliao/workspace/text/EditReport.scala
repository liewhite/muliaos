package muliao.workspace.text

/** 改完后给模型看的回显：改动处及上下文，`cat -n` 格式，不相连的块用 `--` 隔开。纯函数。 */
object EditReport:
  val Context = 2

  /** 一块回显的行号范围（含两端）。 */
  private final case class Lines(first: Int, last: Int)

  def render(path: String, e: Edited): String =
    val index = LineIndex(e.text)
    val head  = s"edited $path (${e.replacements} replacements; now ${index.count} lines); cat -n of the changed lines with $Context lines of context:"
    if index.count == 0 then s"$head\n(文件现在是空的)"
    else
      val blocks = e.changed
        .map { span =>
          val first = index.lineAt(span.from)
          val last  = if span.until > span.from then index.lineAt(span.until - 1) else first
          Lines((first - Context).max(1), (last + Context).min(index.count))
        }
        .sortBy(_.first)
        .foldLeft(List.empty[Lines]) {
          case (prev :: rest, b) if b.first <= prev.last + 1 => prev.copy(last = prev.last.max(b.last)) :: rest
          case (acc, block)                                  => block :: acc
        }
        .reverse
      val body = blocks.map(b => (b.first to b.last).map(n => CatN.line(n, index.line(n))).mkString("\n")).mkString("\n--\n")
      s"$head\n$body"
