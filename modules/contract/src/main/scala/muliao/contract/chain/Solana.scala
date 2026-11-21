package muliao.contract.chain

import sttp.tapir.Schema
import zio.json.*

/** Solana 交易的线上格式（legacy 与 v0），前后端共用（#1092）：读出要签的消息、按公钥找签名位、填进签名；解成人能看懂的概要。
  * 读的时候就解析好：格式不对的读不进来。
  */
final case class SolanaTx private (signatures: Vector[Array[Byte]], message: Array[Byte], parsed: SolanaTx.Message):
  /** 这把公钥在第几个签名位（只在要求签名的账户里找）。 */
  def signerIndex(publicKey: Array[Byte]): Either[String, Int] =
    parsed.staticKeys.take(parsed.requiredSignatures).indexWhere(java.util.Arrays.equals(_, publicKey)) match
      case -1 => Left(s"这笔交易不需要 ${Base58.encode(publicKey)} 签名（要签名的是：${parsed.staticKeys.take(parsed.requiredSignatures).map(Base58.encode).mkString("、")}）")
      case i  => Right(i)

  /** 把签名填进第 `index` 位，得到交易的线上字节。 */
  def withSignature(index: Int, signature: Array[Byte]): Array[Byte] =
    val sigs = signatures.updated(index, signature)
    SolanaTx.compactU16(sigs.length) ++ sigs.toArray.flatten ++ message

  def bytes: Array[Byte] = SolanaTx.compactU16(signatures.length) ++ signatures.toArray.flatten ++ message

  override def toString: String = s"SolanaTx(${Base58.encode(parsed.blockhash)})"

object SolanaTx:
  /** 一条指令：程序、账户（按账户表的序号）、数据。 */
  final case class Instruction(program: Int, accounts: List[Int], data: Array[Byte])

  /** 地址查找表（v0）：表的地址，取出的可写、只读账户的序号。 */
  final case class Lookup(table: Array[Byte], writable: List[Int], readonly: List[Int])

  /** 消息：`version` 为空是 legacy。账户表先是静态账户，v0 再接查找表里取出的可写、只读账户。 */
  final case class Message(
      version: Option[Int],
      requiredSignatures: Int,
      readonlySigned: Int,
      readonlyUnsigned: Int,
      staticKeys: Vector[Array[Byte]],
      blockhash: Array[Byte],
      instructions: List[Instruction],
      lookups: List[Lookup]
  ):
    /** 账户表里第 `i` 个账户的写法：静态的是 base58 地址，查找表里的写「查找表 <地址> 第 n 项」。 */
    def account(i: Int): String =
      if i < staticKeys.length then Base58.encode(staticKeys(i))
      else
        val loaded = lookups.flatMap(l => l.writable.map(l.table -> _)) ++ lookups.flatMap(l => l.readonly.map(l.table -> _))
        loaded.lift(i - staticKeys.length).fold(s"#$i（越界）")((t, n) => s"查找表 ${Base58.encode(t)} 第 $n 项")

    /** 账户 `i` 是否可写（只对静态账户算得出，查找表里取出的按可写/只读两段算）。 */
    def writable(i: Int): Boolean =
      val n = staticKeys.length
      if i < requiredSignatures then i < requiredSignatures - readonlySigned
      else if i < n then i < n - readonlyUnsigned
      else i - n < lookups.map(_.writable.length).sum

  def parse(wire: Array[Byte]): Either[String, SolanaTx] =
    val r = Reader(wire)
    for
      n    <- r.compactU16
      sigs <- Eithers.traverse(List.fill(n)(()))(_ => r.take(64))
      msgAt = r.at
      msg  <- message(r)
      _    <- Either.cond(r.done, (), "交易末尾有多余的字节")
      _    <- Either.cond(n == msg.requiredSignatures, (), s"签名位 $n 个，消息要求 ${msg.requiredSignatures} 个")
    yield SolanaTx(sigs.toVector, wire.drop(msgAt), msg)

  /** 只读一段消息（不带签名位），要恰好读完。 */
  def parseMessage(b: Array[Byte]): Either[String, Message] =
    val r = Reader(b)
    message(r).flatMap(m => Either.cond(r.done, m, "消息末尾有多余的字节"))

  given JsonCodec[SolanaTx] = JsonCodec.string.transformOrFail(s => Base64.decode(s).flatMap(parse), t => Base64.encode(t.bytes))
  given Schema[SolanaTx]    = Schema.string

  private def message(r: Reader): Either[String, Message] =
    for
      first   <- r.u8
      version  = Option.when((first & 0x80) != 0)(first & 0x7f)
      _       <- Either.cond(version.forall(_ == 0), (), s"不认得的消息版本 ${version.getOrElse(0)}")
      req     <- if version.isDefined then r.u8 else Right(first)
      roSign  <- r.u8
      roUn    <- r.u8
      nk      <- r.compactU16
      keys    <- Eithers.traverse(List.fill(nk)(()))(_ => r.take(32))
      hash    <- r.take(32)
      ni      <- r.compactU16
      ins <- Eithers.traverse(List.fill(ni)(())) { _ =>
               for
                 p    <- r.u8
                 na   <- r.compactU16
                 accs <- Eithers.traverse(List.fill(na)(()))(_ => r.u8)
                 nd   <- r.compactU16
                 data <- r.take(nd)
               yield Instruction(p, accs, data)
             }
      lookups <- if version.isEmpty then Right(Nil)
                 else
                   r.compactU16.flatMap(nl =>
                     Eithers.traverse(List.fill(nl)(())) { _ =>
                       for
                         t  <- r.take(32)
                         nw <- r.compactU16
                         w  <- Eithers.traverse(List.fill(nw)(()))(_ => r.u8)
                         nr <- r.compactU16
                         ro <- Eithers.traverse(List.fill(nr)(()))(_ => r.u8)
                       yield Lookup(t, w, ro)
                     }
                   )
      _ <- Either.cond(req >= 1 && req <= nk && roSign < req && roUn <= nk - req, (), "消息头的签名数与账户数对不上")
      total = nk + lookups.map(l => l.writable.length + l.readonly.length).sum
      _ <- Either.cond(ins.forall(i => i.program < total && i.accounts.forall(_ < total)), (), "指令引用的账户越界")
    yield Message(version, req, roSign, roUn, keys.toVector, hash, ins, lookups)

  /** Solana 的 compact-u16（每字节 7 位，低位在前）。 */
  def compactU16(n: Int): Array[Byte] =
    Iterator.unfold(Option(n))(_.map(v => if v < 0x80 then (v.toByte, None) else (((v & 0x7f) | 0x80).toByte, Some(v >> 7)))).toArray

  private final class Reader(a: Array[Byte]):
    var at = 0
    def done: Boolean = at == a.length
    def take(n: Int): Either[String, Array[Byte]] =
      if n > a.length - at then Left("交易被截断")
      else
        val r = a.slice(at, at + n)
        at += n
        Right(r)
    def u8: Either[String, Int] = take(1).map(_(0) & 0xff)
    def compactU16: Either[String, Int] =
      def go(acc: Int, shift: Int): Either[String, Int] =
        u8.flatMap(b => if shift > 14 then Left("compact-u16 太长") else if (b & 0x80) == 0 then Right(acc | (b << shift)) else go(acc | ((b & 0x7f) << shift), shift + 7))
      go(0, 0)

/** 显示用：认得的几个程序与指令。 */
object SolanaPrograms:
  val System        = "11111111111111111111111111111111"
  val ComputeBudget = "ComputeBudget111111111111111111111111111111"

  private val Names = Map(
    System                                          -> "System",
    ComputeBudget                                   -> "ComputeBudget",
    "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"  -> "SPL Token",
    "TokenzQdBNbLqP5VEhdkAS6EPFLC1PHnBqCXEpPxuEb"  -> "Token-2022",
    "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL" -> "Associated Token",
    "MemoSq4gqABAXKb96qnH8TuSbKY2fQXbt3Mds8eESjx3" -> "Memo",
    "Memo1UhkJRfHyvLMcVucJwxXeuD728EtVDDdjNfjLu7"  -> "Memo v1"
  )

  /** 一条指令的概要：认得的解出含义（System 转账与 durable nonce、ComputeBudget、Memo、SPL Token 的转账/授权/改权限/关账户），
    * 否则给程序、账户、数据的字节数与十六进制。
    */
  def describe(m: SolanaTx.Message, i: SolanaTx.Instruction): String =
    val program = m.account(i.program)
    val accs    = i.accounts.map(a => s"${m.account(a)}${if m.writable(a) then "（可写）" else ""}")
    def le(b: List[Int]): BigInt = BigInt(1, b.reverse.map(_.toByte).toArray)
    def acc(n: Int): String      = i.accounts.lift(n).fold("（缺）")(m.account)
    val token = Names.get(program).exists(n => n == "SPL Token" || n == "Token-2022")
    val known = (Names.get(program), i.data.toList.map(_ & 0xff)) match
      case (Some("System"), 2 :: 0 :: 0 :: 0 :: rest) if rest.length == 8 =>
        val lamports = le(rest)
        Some(s"System 转账 ${Units.decimal(lamports, 9)} SOL（$lamports lamports）\n从 ${acc(0)}\n到 ${acc(1)}")
      case (Some("System"), 4 :: 0 :: 0 :: 0 :: Nil) =>
        Some(s"System 推进 durable nonce：这笔交易不会因区块哈希过期而失效，签了以后随时能被发出\nnonce 账户 ${acc(0)}\n权限 ${acc(2)}")
      case (Some("ComputeBudget"), 2 :: rest) if rest.length == 4 => Some(s"ComputeBudget 设计算单元上限 ${le(rest)}")
      case (Some("ComputeBudget"), 3 :: rest) if rest.length == 8 => Some(s"ComputeBudget 设优先费 ${le(rest)} micro-lamports/单元")
      case (Some(n), _) if n.startsWith("Memo") => Some(s"$n：${String(i.data, java.nio.charset.StandardCharsets.UTF_8)}")
      case (_, 3 :: rest) if token && rest.length == 8 =>
        Some(s"代币转账 ${le(rest)}（最小单位）\n从 ${acc(0)}\n到 ${acc(1)}\n签名者 ${acc(2)}")
      case (_, 12 :: rest) if token && rest.length == 9 =>
        Some(s"代币转账 ${Units.decimal(le(rest.take(8)), rest(8))}（mint ${acc(1)}）\n从 ${acc(0)}\n到 ${acc(2)}\n签名者 ${acc(3)}")
      case (_, 4 :: rest) if token && rest.length == 8 =>
        Some(s"代币授权：允许 ${acc(1)} 动用 ${le(rest)}（最小单位）\n代币账户 ${acc(0)}\n签名者 ${acc(2)}")
      case (_, 13 :: rest) if token && rest.length == 9 =>
        Some(s"代币授权：允许 ${acc(2)} 动用 ${Units.decimal(le(rest.take(8)), rest(8))}（mint ${acc(1)}）\n代币账户 ${acc(0)}\n签名者 ${acc(3)}")
      case (_, 6 :: _) if token => Some(s"代币改权限（SetAuthority）：账户 ${acc(0)} 的权限换人\n数据 ${Hex.prefixed(i.data)}\n签名者 ${acc(1)}")
      case (_, 9 :: Nil) if token => Some(s"代币关账户：关掉 ${acc(0)}，余下的 SOL 给 ${acc(1)}\n签名者 ${acc(2)}")
      case _ => None
    known.getOrElse {
      val shown = Names.get(program).fold(program)(n => s"$n（$program）")
      s"程序 $shown\n账户 ${accs.mkString("\n     ")}\n数据 ${i.data.length} 字节 ${Hex.prefixed(i.data.take(64))}${if i.data.length > 64 then "…" else ""}"
    }
