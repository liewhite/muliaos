package muliao.workspace

import muliao.workspace.text.*
import zio.test.*

/** 编辑语义与读文件的分段渲染（纯函数）。 */
object TextSpec extends ZIOSpecDefault:

  private def text(old: String, all: Boolean = false) = Locator.Text(old, all)
  private def lines(s: Int, e: Int, anchor: String)   = Locator.Lines(s, e, anchor)

  private val five = "one\ntwo\nthree\nfour\nfive\n"

  def spec = suite("文本")(
    suite("编辑")(
      test("按原文：恰好一处才替换，找不到或不唯一都拒绝") {
        val ok      = TextEdit("a b a", List(Edit(text("b"), "B")))
        val missing = TextEdit("a b a", List(Edit(text("x"), "y")))
        val twice   = TextEdit("a b a", List(Edit(text("a"), "A")))
        val all     = TextEdit("a b a", List(Edit(text("a", all = true), "AA")))
        val empty   = TextEdit("a", List(Edit(text(""), "b")))
        assertTrue(
          ok.map(_.text) == Right("a B a"),
          missing.left.exists(_.contains("找不到")),
          twice.left.exists(_.contains("2 次")),
          all.map(e => (e.text, e.replacements)) == Right(("AA b AA", 2)),
          empty.isLeft
        )
      },
      test("按行号：整行替换、要 anchor 对得上，删除时连换行一起删，没有换行的末行保持没有") {
        val swap    = TextEdit(five, List(Edit(lines(2, 3, "  two "), "TWO\nTHREE")))
        val wrong   = TextEdit(five, List(Edit(lines(2, 2, "three"), "x")))
        val delete  = TextEdit(five, List(Edit(lines(2, 3, "two"), "")))
        val noEol   = TextEdit("a\nb", List(Edit(lines(2, 2, "b"), "B\n")))
        val outside = TextEdit(five, List(Edit(lines(5, 6, "five"), "x")))
        assertTrue(
          swap.map(_.text) == Right("one\nTWO\nTHREE\nfour\nfive\n"),
          wrong.left.exists(e => e.contains("anchor") && e.contains("two")),
          delete.map(_.text) == Right("one\nfour\nfive\n"),
          noEol.map(_.text) == Right("a\nB"),
          outside.left.exists(_.contains("共 5 行"))
        )
      },
      test("多处：原文在前面改过的内容里找，行号总指改之前的文件，与前面改动重叠的行号被拒绝") {
        val seq = TextEdit(
          five,
          List(
            Edit(text("one\n"), "zero\none\n"), // 前面插一行，后面的行号仍按原文件
            Edit(lines(4, 4, "four"), "FOUR"),
            Edit(text("zero"), "ZERO") // 在改过的内容里找
          )
        )
        val overlap = TextEdit(five, List(Edit(text("two"), "2"), Edit(lines(2, 3, "two"), "x")))
        assertTrue(
          seq.map(_.text) == Right("ZERO\none\ntwo\nthree\nFOUR\nfive\n"),
          seq.map(_.replacements) == Right(3),
          overlap.left.exists(e => e.startsWith("第 2 处修改") && e.contains("重叠"))
        )
      },
      test("回显改动处与上下文，不相连的块用 -- 隔开，行号按改后的文件") {
        val src    = (1 to 20).map(i => s"l$i").mkString("", "\n", "\n")
        val edited = TextEdit(src, List(Edit(text("l3\n"), "l3\nnew\n"), Edit(text("l18"), "L18"))).toOption.get
        val out    = EditReport.render("f.txt", edited)
        val body   = out.linesIterator.drop(1).toList
        assertTrue(
          out.startsWith("edited f.txt (2 replacements; now 21 lines)"),
          body.contains(CatN.line(4, "new")),
          body.contains("--"),
          body.contains(CatN.line(19, "L18")),
          body.head == CatN.line(1, "l1"),
          !body.contains(CatN.line(10, "l9"))
        )
      }
    ),
    suite("读")(
      test("cat -n 格式；到行数上限说明从哪续读；到字符上限停在整行") {
        val idx    = LineIndex((1 to 100).map(i => s"line $i").mkString("\n"))
        val first  = Listing.slice(idx, 1, 10, 100000).toOption.get
        val budget = Listing.slice(idx, 95, 2000, 40).toOption.get
        val tail   = Listing.slice(idx, 99, 10, 100000).toOption.get
        assertTrue(
          first.text.linesIterator.next() == "     1\tline 1",
          Listing.continuation(first).contains("[还有 90 行；从 offset=11 继续读]"),
          budget.last == 96, // 每行约 16 个字符，40 个字符放得下两行
          Listing.continuation(budget).exists(_.contains("offset=97")),
          Listing.continuation(tail).isEmpty,
          Listing.header("a", tail) == "==> a lines 99-100 of 100 <==",
          Listing.slice(idx, 101, 1, 100).isLeft
        )
      },
      test("行尾的 \\r 不显示，空文件没有行") {
        val idx = LineIndex("a\r\nb\r\n")
        assertTrue(idx.count == 2, idx.line(1) == "a", LineIndex("").count == 0, LineIndex("x").count == 1)
      }
    )
  )
