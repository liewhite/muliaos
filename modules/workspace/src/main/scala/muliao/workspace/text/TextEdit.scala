package muliao.workspace.text

/** 怎么找到要改的那一块。 */
enum Locator:
  /** 按原文：在文件当前内容（同一次调用里前面的修改之后）里逐字匹配；`all` 时替换全部出现，否则必须恰好出现一次。 */
  case Text(old: String, all: Boolean)

  /** 按行号：整行 `start`..`end`（含），行号指这次调用之前的文件；`anchor` 是第 `start` 行的全文（忽略首尾空白），对不上就拒绝，防止按过时的行号改错地方。 */
  case Lines(start: Int, end: Int, anchor: String)

/** 一处修改。 */
final case class Edit(locator: Locator, replacement: String)

/** 改完的结果。`changed` 是新文本里改动过的字符区间 `[from, until)`（按位置排好，互不重叠）。 */
final case class Edited(text: String, replacements: Int, changed: List[(Int, Int)])

/** 对一个文件依次做一组修改。纯函数：任何一处不成立就整组失败，说明是第几处、为什么。 */
object TextEdit:

  /** 一次拼接：在当时文本的 `at` 处删掉 `removed` 个字符、放进 `inserted` 个字符。 */
  private final case class Splice(at: Int, removed: Int, inserted: Int):
    def delta: Int = inserted - removed

  private final case class State(text: String, history: Vector[Splice], spans: List[(Int, Int)], count: Int)

  def apply(original: String, edits: List[Edit]): Either[String, Edited] =
    val before = LineIndex(original)
    edits.zipWithIndex
      .foldLeft[Either[String, State]](Right(State(original, Vector.empty, Nil, 0))) { case (acc, (e, i)) =>
        acc.flatMap(st => step(st, before, e).left.map(m => if edits.size > 1 then s"第 ${i + 1} 处修改：$m" else m))
      }
      .map(st => Edited(st.text, st.count, st.spans.sortBy(_._1)))

  private def step(st: State, before: LineIndex, edit: Edit): Either[String, State] =
    edit.locator match
      case Locator.Text(old, all) =>
        occurrences(st.text, old, edit.replacement, all).map { sps =>
          sps.foldLeft(st)(splice(_, _, edit.replacement)).copy(count = st.count + sps.size)
        }
      case Locator.Lines(start, end, anchor) =>
        lineSplice(st, before, start, end, anchor, edit.replacement).map { (sp, text) =>
          splice(st, sp, text).copy(count = st.count + 1)
        }

  /** 原文在当前文本里的各处出现，换算成依次执行的拼接（后一处的位置已计入前一处的长度变化）。 */
  private def occurrences(text: String, old: String, replacement: String, all: Boolean): Either[String, List[Splice]] =
    if old.isEmpty then Left("old_text 不能为空")
    else
      val found = Iterator.iterate(text.indexOf(old))(i => text.indexOf(old, i + old.length)).takeWhile(_ >= 0).toList
      found match
        case Nil => Left("old_text 在文件里找不到：要和文件现在的内容逐字一致（含空白、缩进、换行）；同一次调用里前面的修改可能已经改掉了它")
        case _ :: _ :: _ if !all =>
          Left(s"old_text 在文件里出现了 ${found.size} 次，不唯一：多带几行上下文让它唯一，或设 replace_all 全部替换")
        case _ =>
          val d = replacement.length - old.length
          Right(found.zipWithIndex.map((at, k) => Splice(at + k * d, old.length, replacement.length)))

  /** 按行号的修改：换算成原文件里的区间，再按前面的修改挪到当前文本里的位置；和前面改过的地方重叠就拒绝。 */
  private def lineSplice(st: State, before: LineIndex, start: Int, end: Int, anchor: String, newText: String): Either[String, (Splice, String)] =
    val total = before.count
    if total == 0 then Left("文件是空的，没有可以按行号改的行；用 write_file 写入内容")
    else if start < 1 || end < start || end > total then Left(s"行号范围 $start-$end 不对：要 1 ≤ start_line ≤ end_line ≤ $total（文件共 $total 行）")
    else if before.line(start).trim != anchor.trim then
      Left(s"第 $start 行现在是「${before.line(start).take(200)}」，和 anchor 对不上：行号可能过时了，重新 read_file 看准再改")
    else
      val from       = before.start(start)
      val until      = before.endWithNewline(end)
      val hadNewline = until > before.end(end)
      val body       = if newText.isEmpty then "" else newText.stripSuffix("\n") + (if hadNewline then "\n" else "")
      st.history
        .foldLeft[Either[String, (Int, Int)]](Right((from, until))) { case (acc, h) =>
          acc.flatMap { (f, u) =>
            if u <= h.at then Right((f, u))
            else if f >= h.at + h.removed then Right((f + h.delta, u + h.delta))
            else Left(s"第 $start-$end 行和同一次调用里前面的一处修改重叠：行号指这次调用之前的文件，重叠的改动请合成一处")
          }
        }
        .map((f, u) => (Splice(f, u - f, body.length), body))

  private def splice(st: State, sp: Splice, replacement: String): State =
    val text = st.text.substring(0, sp.at) + replacement + st.text.substring(sp.at + sp.removed)
    State(text, st.history :+ sp, moveSpans(st.spans, sp), st.count)

  /** 新的拼接之后，之前记下的改动区间怎么挪：在它之前的不动，在它之后的平移，和它相交的并进它。 */
  private def moveSpans(spans: List[(Int, Int)], sp: Splice): List[(Int, Int)] =
    val cut = sp.at + sp.removed
    val (touching, apart) = spans.partition((s, e) => e >= sp.at && s <= cut)
    val merged = touching.foldLeft((sp.at, sp.at + sp.inserted)) { case ((ms, me), (s, e)) =>
      (ms.min(s), me.max(if e > cut then e + sp.delta else sp.at + sp.inserted))
    }
    merged :: apart.map((s, e) => if s > cut then (s + sp.delta, e + sp.delta) else (s, e))
