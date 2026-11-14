package muliao.triggers

/** 按行比较两段文字，给出 `- ` 删去、`+ ` 新增的行（只列变化的行，按新文字的顺序穿插）。 */
object LineDiff:
  /** 行数乘积超过这个就不做最长公共子序列，直接整段对比（输出很长时 diff 也没人细看）。 */
  private val MaxCells = 4_000_000L

  def apply(before: String, after: String): String =
    val a = lines(before)
    val b = lines(after)
    val out =
      if a.length.toLong * b.length > MaxCells then a.map("- " + _) ++ b.map("+ " + _)
      else walk(a, b)
    out.mkString("\n")

  private def lines(s: String): Vector[String] = if s.isEmpty then Vector.empty else s.split("\n", -1).toVector

  /** 最长公共子序列回溯。 */
  private def walk(a: Vector[String], b: Vector[String]): Vector[String] =
    val n = a.length
    val m = b.length
    // lcs(i)(j)：a 从 i、b 从 j 起的最长公共子序列长度
    val lcs = Array.ofDim[Int](n + 1, m + 1)
    for i <- (n - 1) to 0 by -1; j <- (m - 1) to 0 by -1 do
      lcs(i)(j) = if a(i) == b(j) then lcs(i + 1)(j + 1) + 1 else lcs(i + 1)(j).max(lcs(i)(j + 1))
    val out = Vector.newBuilder[String]
    var i   = 0
    var j   = 0
    while i < n || j < m do
      if i < n && j < m && a(i) == b(j) then
        i += 1; j += 1
      else if i < n && (j == m || lcs(i + 1)(j) >= lcs(i)(j + 1)) then
        out += "- " + a(i); i += 1
      else
        out += "+ " + b(j); j += 1
    out.result()
