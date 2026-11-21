package muliao.contract

import muliao.contract.chain.*
import zio.json.*

import java.nio.charset.StandardCharsets.UTF_8

/** 签名一方（服务端或用户浏览器）自己提供的底层能力：曲线运算、安全随机数、时钟。其余（读私钥、按内容拼待签字节、拼结果）
  * 都在 [[Signing]] 里，两边共用一份（#1092）。
  */
trait Crypto:
  /** ed25519：32 字节种子对应的公钥（32 字节）。 */
  def ed25519Public(seed: Array[Byte]): Array[Byte]
  def ed25519Sign(seed: Array[Byte], message: Array[Byte]): Array[Byte]

  /** secp256k1：32 字节私钥对应的公钥（未压缩，去掉 0x04 前缀的 64 字节 x‖y）。 */
  def secp256k1Public(secret: Array[Byte]): Array[Byte]

  /** 对 32 字节摘要做 ECDSA 签名（RFC 6979 确定性随机数、s 取低半区，同 ethers），带恢复位。 */
  def secp256k1Sign(secret: Array[Byte], digest: Array[Byte]): EcdsaSignature

  def random(n: Int): Array[Byte]

  /** 现在（epoch 秒）。 */
  def now: Long

/** 一段消息的原始字节：文本（按 UTF-8）或十六进制。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum MessageBytes derives JsonCodec:
  case Text(text: String)
  case Bytes(hex: HexData)

  def bytes: Array[Byte] = this match
    case Text(t)  => t.getBytes(UTF_8)
    case Bytes(h) => h.bytes

  /** 显示用：文本原样，字节写十六进制。 */
  def shown: String = this match
    case Text(t)  => t
    case Bytes(h) => s"${h.length} 字节 $h"

object MessageBytes:
  given sttp.tapir.Schema[MessageBytes] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    sttp.tapir.Schema.derived

/** Solana 链下消息（钱包的 signMessage）：ed25519 直接签原始字节，而同一把 ed25519 钥匙也可能是 SSH CA，所以不许是一笔交易的消息，
  * 也不许是一张 SSH 证书的待签字节，免得以「签消息」的名义签了交易或签出一张不受 ssh_certificate 约束的证书。
  */
final case class SolanaOffchain private (message: MessageBytes)

object SolanaOffchain:
  private val SshCertPrefix = SshCert.CertType.getBytes(UTF_8)

  def of(m: MessageBytes): Either[String, SolanaOffchain] =
    val b = m.bytes
    for
      _ <- Either.cond(SolanaTx.parseMessage(b).isLeft, (), "这段字节是一笔 Solana 交易的消息：签交易请用 solana_transaction")
      // 证书待签字节以 string 类型串开头：4 字节长度后接类型名（不论长度字段写的什么都拒）
      _ <- Either.cond(!b.slice(4, 4 + SshCertPrefix.length).sameElements(SshCertPrefix), (), "这段字节是一张 SSH 证书：签证书请用 ssh_certificate")
    yield SolanaOffchain(m)

  given JsonCodec[SolanaOffchain]            = JsonCodec[MessageBytes].transformOrFail(of, _.message)
  given sttp.tapir.Schema[SolanaOffchain]    = summon[sttp.tapir.Schema[MessageBytes]].as[SolanaOffchain]

/** 读私钥：按内容要的算法读，读出来的就能签（公钥与种子对得上）。 */
object SigningKeys:
  /** secp256k1 私钥与它的以太坊地址。toString 不带私钥。 */
  final class Secp256k1Key(secretBytes: Array[Byte], val address: EthAddress):
    def secret: Array[Byte] = secretBytes.clone()
    override def toString: String = s"Secp256k1Key($address)"

  private val N = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  /** ed25519：不带口令的 OpenSSH 私钥文件，或 Solana 私钥（`solana-keygen` 的 JSON 数组、钱包导出的 base58，都是 64 字节种子‖公钥）。 */
  def ed25519(text: String, crypto: Crypto): Either[String, SshCert.PrivateKey] =
    val t = text.trim
    val parsed =
      if t.contains("OPENSSH PRIVATE KEY") then SshCert.readPrivate(t)
      else
        val raw =
          if t.startsWith("[") then
            t.fromJson[List[Int]].left.map(_ => "不是数字数组").flatMap(xs => Either.cond(xs.forall(x => x >= 0 && x < 256), xs.map(_.toByte).toArray, "数组里要是 0–255 的字节"))
          else Base58.decode(t)
        raw.flatMap(b =>
          Either.cond(b.length == 64, SshCert.PrivateKey(b.take(32), SshCert.PublicKey(b.drop(32))), s"Solana 私钥应是 64 字节，实际 ${b.length}")
        )
    parsed
      .left.map(e => s"不是认得的 ed25519 私钥（OpenSSH 私钥文件、Solana 的 JSON 数组或 base58）：$e")
      .flatMap(k => Either.cond(java.util.Arrays.equals(crypto.ed25519Public(k.seed), k.publicKey.bytes), k, "私钥与里面带的公钥对不上"))

  /** secp256k1：64 位十六进制（可带 0x），在曲线阶的范围内。 */
  def secp256k1(text: String, crypto: Crypto): Either[String, Secp256k1Key] =
    for
      b <- Hex.decode(text.trim).left.map(e => s"以太坊私钥要是 64 位十六进制：$e")
      _ <- Either.cond(b.length == 32, (), s"以太坊私钥应是 32 字节（64 位十六进制），实际 ${b.length} 字节")
      n  = BigInt(1, b)
      _ <- Either.cond(n > 0 && n < N, (), "不是有效的 secp256k1 私钥（超出范围）")
    yield Secp256k1Key(b, EthAddress.fromPublic(crypto.secp256k1Public(b)))

  /** 核对是这种算法认得的私钥，算出它的公开身份（设值时存下、给用户看，#1098）。 */
  def identity(alg: SigningAlgorithm, text: String, crypto: Crypto): Either[String, KeyIdentity] = alg match
    case SigningAlgorithm.Ed25519   => ed25519(text, crypto).map(k => KeyIdentity.Ed25519(k.publicKey))
    case SigningAlgorithm.Secp256k1 => secp256k1(text, crypto).map(k => KeyIdentity.Secp256k1(k.address))

/** 签好的结果（给 agent 的 JSON）。 */
object Signed:
  final case class EthTransaction(
      from: EthAddress,
      @jsonField("transaction_hash") hash: HexData,
      @jsonField("raw_transaction") raw: HexData
  ) derives JsonEncoder
  final case class EthSignature(address: EthAddress, signature: HexData, digest: HexData) derives JsonEncoder
  final case class SolanaTransaction(@jsonField("public_key") publicKey: String, signature: String, transaction: String) derives JsonEncoder
  final case class SolanaSignature(@jsonField("public_key") publicKey: String, signature: String) derives JsonEncoder

object Signing:
  /** 用私钥文本签一份内容：先核对私钥就是 secret 记下的那把（`signer`，#1098），再按内容的类型拼出待签的字节、签好拼成结果
    * （SSH 证书是证书文件的一行，其余是给 agent 的 JSON）。
    */
  def sign(content: SignContent, signer: KeyIdentity, keyText: String, crypto: Crypto): Either[String, String] =
    for
      actual <- SigningKeys.identity(content.algorithm, keyText, crypto)
      _      <- Either.cond(actual == signer, (), s"私钥对应 ${actual.address}，和 secret 记下的 ${signer.address} 不一致：不签；请在 Secret 页重新设一次它的值")
      out    <- signWith(content, keyText, crypto)
    yield out

  private def signWith(content: SignContent, keyText: String, crypto: Crypto): Either[String, String] =
    content match
      case SignContent.SshCert(req) =>
        SigningKeys.ed25519(keyText, crypto).flatMap { ca =>
          val serial = crypto.random(8).foldLeft(0L)((acc, b) => (acc << 8) | (b & 0xffL))
          SshCert.issue(ca, req, crypto.now, crypto.random(32), serial)(crypto.ed25519Sign(ca.seed, _))
        }
      case SignContent.EthTransaction(tx) =>
        SigningKeys.secp256k1(keyText, crypto).map { k =>
          val raw = tx.signed(crypto.secp256k1Sign(k.secret, tx.signingHash))
          Signed.EthTransaction(k.address, HexData.of(Keccak256(raw)), HexData.of(raw)).toJsonPretty
        }
      case SignContent.EthMessage(m)     => ethDigest(keyText, crypto, Eth.personalHash(m.bytes))
      case SignContent.EthTypedData(d)   => ethDigest(keyText, crypto, d.digest)
      case SignContent.SolanaTransaction(tx) =>
        for
          k <- SigningKeys.ed25519(keyText, crypto)
          i <- tx.signerIndex(k.publicKey.bytes)
          sig = crypto.ed25519Sign(k.seed, tx.message)
        yield Signed.SolanaTransaction(Base58.encode(k.publicKey.bytes), Base58.encode(sig), Base64.encode(tx.withSignature(i, sig))).toJsonPretty
      case SignContent.SolanaMessage(m) =>
        SigningKeys.ed25519(keyText, crypto).map(k =>
          Signed.SolanaSignature(Base58.encode(k.publicKey.bytes), Base58.encode(crypto.ed25519Sign(k.seed, m.message.bytes))).toJsonPretty
        )

  private def ethDigest(keyText: String, crypto: Crypto, digest: Array[Byte]): Either[String, String] =
    SigningKeys.secp256k1(keyText, crypto).map(k =>
      Signed.EthSignature(k.address, HexData.of(crypto.secp256k1Sign(k.secret, digest).rsv), HexData.of(digest)).toJsonPretty
    )

  /** 给用户看的标题（批准卡片、推送）。 */
  def title(content: SignContent): String = content match
    case SignContent.SshCert(_)           => "签 SSH 证书"
    case SignContent.EthTransaction(_)    => "签以太坊交易"
    case SignContent.EthMessage(_)        => "签以太坊消息（EIP-191）"
    case SignContent.EthTypedData(_)      => "签以太坊结构化数据（EIP-712）"
    case SignContent.SolanaTransaction(_) => "签 Solana 交易"
    case SignContent.SolanaMessage(_)     => "签 Solana 消息"

  /** 给用户看的内容（批准卡片逐行显示）：从内容本身解出来，不采信 agent 写的说明。 */
  def rows(content: SignContent): List[(String, String)] = content match
    case SignContent.SshCert(r) =>
      List("可登" -> r.principals.mkString("  "), "有效" -> s"${r.minutes} 分钟（从签发时算起）", "来源" -> r.sourceAddress.getOrElse("不限"), "记录" -> r.keyId)
    case SignContent.EthTransaction(tx) => ethTx(tx)
    case SignContent.EthMessage(m)      => List("消息" -> m.shown, "摘要" -> Hex.prefixed(Eth.personalHash(m.bytes)))
    case SignContent.EthTypedData(d) =>
      val chain = d.domain.get("chainId").flatMap {
        case zio.json.ast.Json.Num(n) => Some(BigInt(n.toBigInteger))
        case zio.json.ast.Json.Str(s) => Quantity.parse(s).toOption.map(_.value)
        case _                        => None
      }.map(chainLabel)
      ("类型" -> d.typeString.replace(")", ")\n").trim) :: chain.map("链" -> _).toList ++ List(
        "摘要" -> Hex.prefixed(d.digest),
        "域"   -> d.domain.toJsonPretty,
        "内容" -> d.message.toJsonPretty
      )
    case SignContent.SolanaTransaction(tx) =>
      val m = tx.parsed
      ("付费账户" -> m.account(0)) :: m.instructions.zipWithIndex.map((ins, i) => s"指令 ${i + 1}" -> SolanaPrograms.describe(m, ins)) ++ List(
        "要签名的" -> (0 until m.requiredSignatures).map(m.account).mkString("\n"),
        "版本"     -> m.version.fold("legacy")(v => s"v$v"),
        "区块哈希" -> Base58.encode(m.blockhash)
      )
    case SignContent.SolanaMessage(m) => List("消息" -> m.message.shown)

  private def chainLabel(id: BigInt): String = Eth.chainName(id).fold(s"链编号 $id")(n => s"$n（$id）")

  private def ethTx(tx: EthTx): List[(String, String)] =
    def gwei(q: Quantity) = s"${Units.decimal(q.value, 9)} gwei"
    val (kind, fees, maxCost) = tx match
      case t: EthTx.Eip1559 => ("EIP-1559", List("最高费率" -> gwei(t.maxFeePerGas), "小费" -> gwei(t.maxPriorityFeePerGas)), t.gas.value * t.maxFeePerGas.value)
      case t: EthTx.Legacy  => ("legacy", List("gas 价格" -> gwei(t.gasPrice)), t.gas.value * t.gasPrice.value)
    val data = tx.data.length match
      case 0 => "无"
      case n => s"$n 字节，函数选择器 ${Hex.prefixed(tx.data.bytes.take(4))}\n${tx.data}"
    val access = tx match
      case t: EthTx.Eip1559 if t.accessList.nonEmpty =>
        List("访问列表" -> t.accessList.map(e => s"${e.address.checksummed} ${e.storageKeys.mkString(" ")}").mkString("\n"))
      case _ => Nil
    List(
      "链"     -> chainLabel(tx.chainId.value),
      "目标"   -> tx.to.fold("（建合约）")(_.checksummed),
      "金额"   -> s"${Units.decimal(tx.value.value, 18)}（原生币；${tx.value} wei）${TokenCalls.describe(tx.data.bytes).fold("")(c => s"；$c")}",
      "调用数据" -> data,
      "类型"   -> kind,
      "nonce"  -> tx.nonce.toString,
      "gas 上限" -> tx.gas.toString
    ) ++ fees ++ List("最多花费" -> s"${Units.decimal(maxCost + tx.value.value, 18)}（gas 费上限加金额）") ++ access
