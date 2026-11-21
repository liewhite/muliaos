package muliao.contract.chain

import sttp.tapir.Schema
import zio.json.*

import java.nio.charset.StandardCharsets.UTF_8

/** 以太坊的地址、数值、字节串与交易编码（前后端共用，#1092）。只有编码与摘要，不含签名：签名由各端的 [[Crypto]] 做。 */

/** 20 字节地址。内部存小写十六进制（不带 0x）。 */
final case class EthAddress private (lower: String):
  def bytes: Array[Byte] = Hex.decode(lower).toOption.get

  /** EIP-55 大小写校验写法。 */
  def checksummed: String =
    val h = Hex.encode(Keccak256(lower.getBytes(UTF_8)))
    "0x" + lower.indices.map(i => if Character.digit(h(i), 16) >= 8 then lower(i).toUpper else lower(i)).mkString

  override def toString: String = checksummed

object EthAddress:
  /** 读 `0x` 加 40 位十六进制；大小写混写的按 EIP-55 核对校验。 */
  def parse(s: String): Either[String, EthAddress] =
    val t = s.trim
    for
      _ <- Either.cond(t.startsWith("0x") && t.length == 42, (), s"地址要是 0x 加 40 位十六进制：$t")
      b <- Hex.decode(t)
      a  = EthAddress(Hex.encode(b))
      _ <- Either.cond(t.drop(2).forall(!_.isUpper) || t.drop(2).forall(!_.isLower) || a.checksummed == t, (), s"地址的大小写校验（EIP-55）不对：$t")
    yield a

  /** 公钥（64 字节 x‖y）对应的地址。 */
  def fromPublic(xy: Array[Byte]): EthAddress = EthAddress(Hex.encode(Keccak256(xy).takeRight(20)))

  def fromBytes(b: Array[Byte]): Either[String, EthAddress] =
    Either.cond(b.length == 20, EthAddress(Hex.encode(b)), s"地址应是 20 字节，实际 ${b.length}")

  given JsonCodec[EthAddress] = JsonCodec.string.transformOrFail(parse, _.checksummed)
  given Schema[EthAddress]    = Schema.string

/** 0 到 2²⁵⁶−1 的整数（金额、gas、nonce、链编号）。JSON 里写十进制字符串；读时也认 `0x` 十六进制。 */
final case class Quantity private (value: BigInt):
  override def toString: String = value.toString

object Quantity:
  val Max: BigInt = BigInt(2).pow(256) - 1

  def of(n: BigInt): Either[String, Quantity] =
    Either.cond(n >= 0 && n <= Max, new Quantity(n), s"数值要在 0 到 2^256-1 之间：$n")

  def parse(s: String): Either[String, Quantity] =
    val t = s.trim
    val n =
      if t.startsWith("0x") || t.startsWith("0X") then
        Either.cond(t.length > 2 && t.drop(2).forall(c => Character.digit(c, 16) >= 0), BigInt(t.drop(2), 16), s"不是十六进制整数：$t")
      else Either.cond(t.nonEmpty && t.forall(_.isDigit), BigInt(t), s"不是非负整数（十进制或 0x 十六进制）：$t")
    n.flatMap(of)

  given JsonCodec[Quantity] = JsonCodec.string.transformOrFail(parse, _.value.toString)
  given Schema[Quantity]    = Schema.string

/** 任意字节串（交易的 data、消息的原始字节）。内部存小写十六进制（不带 0x），JSON 里写 `0x…`。 */
final case class HexData private (lower: String):
  def bytes: Array[Byte] = Hex.decode(lower).toOption.get
  def length: Int        = lower.length / 2
  override def toString: String = "0x" + lower

object HexData:
  val Empty: HexData = HexData("")

  def parse(s: String): Either[String, HexData] = Hex.decode(s.trim).map(of)

  def of(b: Array[Byte]): HexData = HexData(Hex.encode(b))

  given JsonCodec[HexData] = JsonCodec.string.transformOrFail(parse, _.toString)
  given Schema[HexData]    = Schema.string

/** secp256k1 签名：r、s 各 32 字节（s 已在低半区），`recovery` 是 0 或 1。 */
final case class EcdsaSignature(r: BigInt, s: BigInt, recovery: Int):
  /** 65 字节 r‖s‖v（v = 27 + recovery），`personal_sign` 与 `eth_signTypedData` 的写法。 */
  def rsv: Array[Byte] = Eth.word(r) ++ Eth.word(s) ++ Array((27 + recovery).toByte)

/** RLP 编码（以太坊交易的序列化）。 */
enum Rlp:
  case Str(bytes: Array[Byte])
  case Items(items: List[Rlp])

  def encode: Array[Byte] = this match
    case Str(b) if b.length == 1 && (b(0) & 0xff) < 0x80 => b
    case Str(b)                                          => Rlp.head(0x80, b.length) ++ b
    case Items(xs) =>
      val body = xs.toArray.flatMap(_.encode)
      Rlp.head(0xc0, body.length) ++ body

object Rlp:
  /** 无符号整数：去掉前导零的大端字节，0 是空串。 */
  def uint(n: BigInt): Rlp = Str(Eth.minimal(n))

  private def head(base: Int, len: Int): Array[Byte] =
    if len < 56 then Array((base + len).toByte)
    else
      val l = Eth.minimal(BigInt(len))
      Array((base + 55 + l.length).toByte) ++ l

/** EIP-2930 访问列表的一项。 */
final case class AccessEntry(address: EthAddress, storageKeys: List[HexData]) derives JsonCodec, Schema

/** 要签的以太坊交易：EIP-1559（type 2）或 legacy（EIP-155，签名里带链编号）。`to` 为空是建合约。JSON 里 `type` 是分支名。 */
@jsonDiscriminator("type")
enum EthTx derives JsonCodec:
  case Eip1559(
      chainId: Quantity,
      nonce: Quantity,
      maxPriorityFeePerGas: Quantity,
      maxFeePerGas: Quantity,
      gas: Quantity,
      to: Option[EthAddress],
      value: Quantity,
      data: HexData,
      accessList: List[AccessEntry]
  )
  case Legacy(chainId: Quantity, nonce: Quantity, gasPrice: Quantity, gas: Quantity, to: Option[EthAddress], value: Quantity, data: HexData)

  def chainId: Quantity
  def nonce: Quantity
  def gas: Quantity
  def to: Option[EthAddress]
  def value: Quantity
  def data: HexData

  /** 签名覆盖的摘要。 */
  def signingHash: Array[Byte] = Keccak256(payload(None))

  /** 带上签名的交易（可以直接 `eth_sendRawTransaction`）。 */
  def signed(sig: EcdsaSignature): Array[Byte] = payload(Some(sig))

  private def payload(sig: Option[EcdsaSignature]): Array[Byte] =
    import Rlp.uint
    val toItem = Rlp.Str(to.fold(Array.emptyByteArray)(_.bytes))
    this match
      case t: Eip1559 =>
        val access = Rlp.Items(t.accessList.map(e => Rlp.Items(List(Rlp.Str(e.address.bytes), Rlp.Items(e.storageKeys.map(k => Rlp.Str(k.bytes)))))))
        val fields = List(uint(t.chainId.value), uint(t.nonce.value), uint(t.maxPriorityFeePerGas.value), uint(t.maxFeePerGas.value), uint(t.gas.value), toItem, uint(t.value.value), Rlp.Str(t.data.bytes), access)
        val tail   = sig.toList.flatMap(s => List(uint(s.recovery), uint(s.r), uint(s.s)))
        Array(2.toByte) ++ Rlp.Items(fields ++ tail).encode
      case t: Legacy =>
        val fields = List(uint(t.nonce.value), uint(t.gasPrice.value), uint(t.gas.value), toItem, uint(t.value.value), Rlp.Str(t.data.bytes))
        val tail = sig match
          case None    => List(uint(t.chainId.value), uint(0), uint(0))
          case Some(s) => List(uint(t.chainId.value * 2 + 35 + s.recovery), uint(s.r), uint(s.s))
        Rlp.Items(fields ++ tail).encode

object EthTx:
  given Schema[EthTx] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("type")
    Schema.derived

object Eth:
  /** EIP-191 `personal_sign` 的摘要：keccak256("\x19Ethereum Signed Message:\n" + 字节数 + 消息)。 */
  def personalHash(message: Array[Byte]): Array[Byte] =
    Keccak256(s"\u0019Ethereum Signed Message:\n${message.length}".getBytes(UTF_8) ++ message)

  /** 去掉前导零的大端字节，0 是空串。 */
  def minimal(n: BigInt): Array[Byte] = if n == 0 then Array.emptyByteArray else n.toByteArray.dropWhile(_ == 0)

  /** 补成 32 字节的大端无符号整数。 */
  def word(n: BigInt): Array[Byte] =
    val m = minimal(n)
    Array.fill[Byte](32 - m.length)(0) ++ m

  /** 常见链的名字（显示用，没收录的只显示编号）。 */
  def chainName(id: BigInt): Option[String] = ChainNames.get(id)

  private val ChainNames: Map[BigInt, String] = Map(
    BigInt(1)        -> "Ethereum 主网",
    BigInt(10)       -> "Optimism",
    BigInt(56)       -> "BNB Chain",
    BigInt(100)      -> "Gnosis",
    BigInt(137)      -> "Polygon",
    BigInt(8453)     -> "Base",
    BigInt(42161)    -> "Arbitrum One",
    BigInt(43114)    -> "Avalanche C",
    BigInt(59144)    -> "Linea",
    BigInt(80094)    -> "Berachain",
    BigInt(17000)    -> "Holesky 测试网",
    BigInt(11155111) -> "Sepolia 测试网",
    BigInt(999)      -> "HyperEVM"
  )

/** 显示用：认得代币与 NFT 合约最常见的几种调用（ERC20 transfer、approve，ERC20/721 transferFrom，721/1155 setApprovalForAll），金额写最小单位（不知道代币的小数位）。 */
object TokenCalls:
  def describe(data: Array[Byte]): Option[String] =
    val args = data.drop(4).grouped(32).toList
    def addr(w: Array[Byte]) = EthAddress.fromBytes(w.drop(12)).fold(_ => Hex.prefixed(w), _.checksummed)
    def amount(w: Array[Byte]) =
      val n = BigInt(1, w)
      if n == Quantity.Max then "无限" else s"$n（最小单位）"
    Option.when(data.length >= 4 && (data.length - 4) % 32 == 0)(Hex.encode(data.take(4))).flatMap {
      case "a9059cbb" if args.size == 2 => Some(s"代币转账 transfer：给 ${addr(args(0))} ${amount(args(1))}（目标是代币合约）")
      case "095ea7b3" if args.size == 2 => Some(s"代币授权 approve：允许 ${addr(args(0))} 动用 ${amount(args(1))}")
      case "23b872dd" if args.size == 3 => Some(s"代币代转 transferFrom：从 ${addr(args(0))} 给 ${addr(args(1))} ${amount(args(2))}")
      case "a22cb465" if args.size == 2 =>
        Some(s"全部授权 setApprovalForAll：${if BigInt(1, args(1)) != 0 then "允许" else "取消"} ${addr(args(0))} 动用这个合约里你的全部 NFT/代币")
      case _ => None
    }
