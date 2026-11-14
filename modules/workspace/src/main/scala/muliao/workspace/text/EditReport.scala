package muliao.workspace.text

/** 改完后给模型看的回显：改动处及上下文，`cat -n` 格式，不相连的块用 `--` 隔开。纯函数。 */
object EditReport:
  val Context = 2

  def render(path: String, e: Edited): String =
    val index = LineIndex(e.text)
    val head  = s"edited $path (${e.replacements} replacements; now ${index.count} lines); cat -n of the changed lines with $Context lines of context:"
    if index.count == 0 then s"$head\n(文件现在是空的)"
    else
      val blocks = e.changed
        .map { (from, until) =>
          val first = index.lineAt(from)
          val last  = if until > from then index.lineAt(until - 1) else first
          ((first - Context).max(1), (last + Context).min(index.count))
        }
        .sortBy(_._1)
        .foldLeft(List.empty[(Int, Int)]) {
          case ((s, e) :: rest, (a, b)) if a <= e + 1 => (s, e.max(b)) :: rest
          case (acc, block)                           => block :: acc
        }
        .reverse
      val body = blocks.map((a, b) => (a to b).map(n => CatN.line(n, index.line(n))).mkString("\n")).mkString("\n--\n")
      s"$head\n$body"
