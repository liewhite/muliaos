package muliao.contract.chain

object Eithers:
  /** 按顺序逐个做（读字节流的要按顺序），第一个失败就停。 */
  def traverse[A, B](xs: List[A])(f: A => Either[String, B]): Either[String, List[B]] =
    xs.foldLeft[Either[String, List[B]]](Right(Nil))((acc, a) => acc.flatMap(bs => f(a).map(_ :: bs))).map(_.reverse)
