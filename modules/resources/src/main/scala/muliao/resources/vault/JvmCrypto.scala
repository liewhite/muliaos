package muliao.resources.vault

import muliao.contract.Crypto
import muliao.contract.chain.EcdsaSignature
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.params.{ECDomainParameters, ECPrivateKeyParameters, Ed25519PrivateKeyParameters}
import org.bouncycastle.crypto.signers.{ECDSASigner, Ed25519Signer, HMacDSAKCalculator}
import org.bouncycastle.math.ec.{ECAlgorithms, FixedPointCombMultiplier}

import java.math.BigInteger

/** 服务端保管的密钥对在服务端签名时的底层能力（#1069、#1092）：Bouncy Castle 的 secp256k1 与 ed25519，签法与浏览器的 @noble/curves 逐字节相同
  * （ECDSA 用 RFC 6979 确定性随机数、s 取低半区）。读私钥、拼待签字节在前后端共用的 [[muliao.contract.Signing]] 里。
  */
object JvmCrypto extends Crypto:
  private val Params = CustomNamedCurves.getByName("secp256k1")
  private val Domain = ECDomainParameters(Params.getCurve, Params.getG, Params.getN, Params.getH)
  private val N      = Params.getN
  private val Random = java.security.SecureRandom()

  def ed25519Public(seed: Array[Byte]): Array[Byte] = Ed25519PrivateKeyParameters(seed, 0).generatePublicKey().getEncoded

  def ed25519Sign(seed: Array[Byte], message: Array[Byte]): Array[Byte] =
    val s = Ed25519Signer()
    s.init(true, Ed25519PrivateKeyParameters(seed, 0))
    s.update(message, 0, message.length)
    s.generateSignature()

  def secp256k1Public(secret: Array[Byte]): Array[Byte] =
    FixedPointCombMultiplier().multiply(Params.getG, BigInteger(1, secret)).normalize().getEncoded(false).drop(1)

  def secp256k1Sign(secret: Array[Byte], digest: Array[Byte]): EcdsaSignature =
    val signer = ECDSASigner(HMacDSAKCalculator(SHA256Digest()))
    signer.init(true, ECPrivateKeyParameters(BigInteger(1, secret), Domain))
    val Array(r, s0) = signer.generateSignature(digest)
    val s            = if s0.compareTo(N.shiftRight(1)) > 0 then N.subtract(s0) else s0
    val pub          = secp256k1Public(secret)
    // 恢复位：试 0、1，哪个恢复出的公钥是自己就是哪个（r 超过曲线阶的 2、3 概率可忽略）
    val recovery = (0 to 1).find(id => java.util.Arrays.equals(recover(id, r, s, digest), pub)).getOrElse(sys.error("secp256k1 签名算不出恢复位"))
    EcdsaSignature(BigInt(r), BigInt(s), recovery)

  /** SEC 1 4.1.6：由签名与摘要恢复公钥（64 字节 x‖y）。 */
  private def recover(id: Int, r: BigInteger, s: BigInteger, digest: Array[Byte]): Array[Byte] =
    val x     = r.toByteArray.dropWhile(_ == 0)
    val point = Params.getCurve.decodePoint(Array((2 + id).toByte) ++ Array.fill[Byte](32 - x.length)(0) ++ x)
    val rInv  = r.modInverse(N)
    val e     = BigInteger(1, digest)
    ECAlgorithms.sumOfTwoMultiplies(Params.getG, e.negate().mod(N).multiply(rInv).mod(N), point, s.multiply(rInv).mod(N)).normalize().getEncoded(false).drop(1)

  def random(n: Int): Array[Byte] =
    val a = Array.ofDim[Byte](n)
    Random.nextBytes(a)
    a

  def now: Long = java.time.Instant.now().getEpochSecond
