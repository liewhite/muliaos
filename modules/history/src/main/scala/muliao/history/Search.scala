package muliao.history

/** 关键字检索的相关度（纯函数）。按子串匹配，中文不用分词；每个词都要出现，出现在标题、引用里比出现在摘要里权重高、出现次数越多越靠前。 */
object Search:
  private val TitleWeight   = 3
  private val RefWeight     = 3
  private val SummaryWeight = 1

  private def count(text: String, word: String): Int =
    Iterator.iterate(text.indexOf(word))(i => text.indexOf(word, i + word.length)).takeWhile(_ >= 0).size

  /** 相关度；有词没出现时是 None。`words` 已转小写。 */
  def score(e: HistoryEntry, words: List[String]): Option[Int] =
    val title   = e.title.toLowerCase
    val summary = e.summary.toLowerCase
    val refs    = e.refs.mkString(" ").toLowerCase
    words.foldLeft(Option(0)) { (acc, w) =>
      acc.flatMap { total =>
        val s = count(title, w) * TitleWeight + count(refs, w) * RefWeight + count(summary, w) * SummaryWeight
        Option.when(s > 0)(total + s)
      }
    }

  /** 留下每个词都出现的，按相关度、重要程度、日期（新的在前）排，取前 `limit` 条。 */
  def rank(entries: List[HistoryEntry], words: List[String], limit: Int): List[HistoryEntry] =
    entries
      .flatMap(e => score(e, words).map(e -> _))
      .sortBy((e, s) => (-s, -e.importance.level, -e.day.toEpochDay, -e.id))
      .take(limit)
      .map(_._1)
