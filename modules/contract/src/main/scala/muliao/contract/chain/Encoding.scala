package muliao.contract.chain

/** 链上常用的文本编码（前后端共用，#1092）：十六进制、base58（比特币字母表，Solana 用）、base64。 */
object Hex:
  private val Digits = "0123456789abcdef"

  /** 小写、不带 0x。 */
  def encode(a: Array[Byte]): String =
    val sb = new StringBuilder(a.length * 2)
    a.foreach(b => sb.append(Digits((b >> 4) & 0xf)).append(Digits(b & 0xf)))
    sb.toString

  /** 小写、带 0x。 */
  def prefixed(a: Array[Byte]): String = "0x" + encode(a)

  /** 读十六进制，可带 0x，大小写都认；长度要是偶数。 */
  def decode(s: String): Either[String, Array[Byte]] =
    val body = if s.startsWith("0x") || s.startsWith("0X") then s.drop(2) else s
    if body.length % 2 != 0 then Left("十六进制的位数要是偶数")
    else if !body.forall(c => Character.digit(c, 16) >= 0) then Left("不是十六进制")
    else Right(Array.tabulate(body.length / 2)(i => ((Character.digit(body(2 * i), 16) << 4) | Character.digit(body(2 * i + 1), 16)).toByte))

object Base58:
  private val Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  def encode(a: Array[Byte]): String =
    val zeros = a.takeWhile(_ == 0).length
    val digits = Iterator
      .unfold(BigInt(1, a))(n => Option.when(n > 0)((Alphabet((n % 58).toInt), n / 58)))
      .toList
      .reverse
    "1" * zeros + digits.mkString

  def decode(s: String): Either[String, Array[Byte]] =
    s.find(c => Alphabet.indexOf(c) < 0) match
      case Some(_) => Left("不是 base58（含字母表以外的字符）")
      case None =>
        val zeros = s.takeWhile(_ == '1').length
        val n     = s.foldLeft(BigInt(0))((acc, c) => acc * 58 + Alphabet.indexOf(c))
        val body  = if n == 0 then Array.emptyByteArray else n.toByteArray.dropWhile(_ == 0)
        Right(Array.fill[Byte](zeros)(0) ++ body)

object Base64:
  def encode(a: Array[Byte]): String = java.util.Base64.getEncoder.encodeToString(a)

  def decode(s: String): Either[String, Array[Byte]] =
    try Right(java.util.Base64.getDecoder.decode(s.trim))
    catch case _: IllegalArgumentException => Left("不是 base64")

object Units:
  /** 以 10^decimals 为单位的小数写法（去掉末尾的 0）。 */
  def decimal(n: BigInt, decimals: Int): String =
    val d    = BigInt(10).pow(decimals)
    val frac = (n % d).toString.reverse.padTo(decimals, '0').reverse.reverse.dropWhile(_ == '0').reverse
    if frac.isEmpty then (n / d).toString else s"${n / d}.$frac"
