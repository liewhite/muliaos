package muliao.history

/** 长输出分页（纯函数）：从第 `offset` 项起，拼起来不超过 `budget` 个字符（至少给一项，免得永远读不动），截住时最后单独一行说明从哪继续。 */
object Pages:
  /** 一次工具输出的字符预算。 */
  val Budget = 40000

  /** `unit` 是说明里的量词（「行」「条」）。`offset` 从 1 起。 */
  def slice(items: List[String], offset: Int, unit: String, budget: Int = Budget): Either[String, String] =
    val total = items.size
    if offset < 1 then Left("offset 从 1 起")
    else if offset > total && !(total == 0 && offset == 1) then Left(s"offset=$offset 超出了：共 $total $unit")
    else
      val rest  = items.drop(offset - 1)
      val fits  = rest.scanLeft(0)(_ + _.length + 1).tail.takeWhile(_ <= budget).size
      val taken = rest.take(fits.max(1))
      val last  = offset - 1 + taken.size
      val note  = Option.when(last < total)(s"[到了输出上限，停在第 $last $unit（共 $total $unit）；从 offset=${last + 1} 继续]")
      Right((taken ++ note).mkString("\n"))
