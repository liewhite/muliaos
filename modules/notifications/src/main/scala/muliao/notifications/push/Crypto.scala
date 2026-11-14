package muliao.notifications.push

import java.math.BigInteger
import java.security.{AlgorithmParameters, KeyFactory, KeyPair, KeyPairGenerator}
import java.security.interfaces.{ECPrivateKey, ECPublicKey}
import java.security.spec.{ECGenParameterSpec, ECParameterSpec, ECPoint, ECPublicKeySpec}
import java.util.Base64
import javax.crypto.{Cipher, KeyAgreement, Mac}
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

/** Web Push 用到的 base64url（不带填充；解码时带不带都收）。 */
object B64:
  private val enc = Base64.getUrlEncoder.withoutPadding
  private val dec = Base64.getUrlDecoder

  def encode(b: Array[Byte]): String = enc.encodeToString(b)

  def decode(s: String): Either[String, Array[Byte]] =
    try Right(dec.decode(s.trim.stripSuffix("=").stripSuffix("=")))
    catch case _: IllegalArgumentException => Left("不是合法的 base64url")

/** P-256 曲线（secp256r1）：Web Push 的密钥协商与 VAPID 签名都用它。只用 JDK。 */
object P256:
  val params: ECParameterSpec =
    val p = AlgorithmParameters.getInstance("EC")
    p.init(ECGenParameterSpec("secp256r1"))
    p.getParameterSpec(classOf[ECParameterSpec])

  private val prime: BigInteger = params.getCurve.getField match
    case f: java.security.spec.ECFieldFp => f.getP
    case other                           => throw IllegalStateException(s"P-256 应是素数域：$other")
  private val a: BigInteger = params.getCurve.getA
  private val b: BigInteger = params.getCurve.getB

  def generate: KeyPair =
    val g = KeyPairGenerator.getInstance("EC")
    g.initialize(ECGenParameterSpec("secp256r1"))
    g.generateKeyPair()

  /** 未压缩点（0x04 ‖ x ‖ y，65 字节）。 */
  def encode(k: ECPublicKey): Array[Byte] =
    val w = k.getW
    Array(4.toByte) ++ fixed(w.getAffineX) ++ fixed(w.getAffineY)

  /** 从未压缩点读公钥，并核对点在曲线上（浏览器给的密钥不可信）。 */
  def decode(raw: Array[Byte]): Either[String, ECPublicKey] =
    if raw.length != 65 || raw(0) != 4 then Left("公钥应是 65 字节的未压缩点")
    else
      val x = BigInteger(1, raw.slice(1, 33))
      val y = BigInteger(1, raw.slice(33, 65))
      Either.cond(onCurve(x, y), publicKey(ECPoint(x, y)), "公钥不在 P-256 曲线上")

  /** 由私钥算出公钥（d·G）。只在启动时对自己的密钥算一次，不涉及外来输入。 */
  def publicOf(k: ECPrivateKey): ECPublicKey = publicKey(multiply(k.getS, params.getGenerator))

  /** ECDH 共享秘密（x 坐标，32 字节）。 */
  def agree(mine: ECPrivateKey, theirs: ECPublicKey): Array[Byte] =
    val ka = KeyAgreement.getInstance("ECDH")
    ka.init(mine)
    ka.doPhase(theirs, true)
    ka.generateSecret()

  private def publicKey(w: ECPoint): ECPublicKey =
    KeyFactory.getInstance("EC").generatePublic(ECPublicKeySpec(w, params)).asInstanceOf[ECPublicKey]

  private def fixed(n: BigInteger): Array[Byte] =
    val raw = n.toByteArray.dropWhile(_ == 0)
    Array.fill[Byte](32 - raw.length)(0) ++ raw

  private def onCurve(x: BigInteger, y: BigInteger): Boolean =
    x.signum >= 0 && x.compareTo(prime) < 0 && y.signum >= 0 && y.compareTo(prime) < 0 &&
      y.multiply(y).subtract(x.pow(3).add(a.multiply(x)).add(b)).mod(prime).signum == 0

  // 仿射坐标上的点运算；None 是无穷远点
  private def add(p: Option[ECPoint], q: Option[ECPoint]): Option[ECPoint] = (p, q) match
    case (None, _) => q
    case (_, None) => p
    case (Some(u), Some(v)) =>
      val (x1, y1, x2, y2) = (u.getAffineX, u.getAffineY, v.getAffineX, v.getAffineY)
      if x1 == x2 && y1.add(y2).mod(prime).signum == 0 then None
      else
        val slope =
          if x1 == x2 then x1.pow(2).multiply(BigInteger.valueOf(3)).add(a).multiply(y1.shiftLeft(1).modInverse(prime))
          else y2.subtract(y1).multiply(x2.subtract(x1).modInverse(prime))
        val x3 = slope.pow(2).subtract(x1).subtract(x2).mod(prime)
        val y3 = slope.multiply(x1.subtract(x3)).subtract(y1).mod(prime)
        Some(ECPoint(x3, y3))

  private def multiply(k: BigInteger, g: ECPoint): ECPoint =
    (0 until k.bitLength).foldLeft((Option.empty[ECPoint], Option(g))) { case ((acc, base), i) =>
      (if k.testBit(i) then add(acc, base) else acc, add(base, base))
    }._1.getOrElse(throw IllegalArgumentException("私钥不合法"))

/** HKDF-SHA256（RFC 5869），只用到一块输出（≤ 32 字节）。 */
object Hkdf:
  def extract(salt: Array[Byte], ikm: Array[Byte]): Array[Byte] = hmac(salt, ikm)

  def expand(prk: Array[Byte], info: Array[Byte], length: Int): Array[Byte] =
    require(length <= 32, "只取一块输出")
    hmac(prk, info :+ 1.toByte).take(length)

  private def hmac(key: Array[Byte], data: Array[Byte]): Array[Byte] =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(data)

/** 浏览器订阅里给出的接收方密钥：P-256 公钥与 16 字节认证密钥。 */
final case class DeviceKeys(public: ECPublicKey, auth: Array[Byte])

object DeviceKeys:
  def parse(p256dh: String, auth: String): Either[String, DeviceKeys] =
    for
      pk <- B64.decode(p256dh).flatMap(P256.decode).left.map(e => s"p256dh：$e")
      a  <- B64.decode(auth).left.map(e => s"auth：$e")
      _  <- Either.cond(a.length == 16, (), s"auth 应是 16 bytes，收到 ${a.length}")
    yield DeviceKeys(pk, a)

/** Web Push 载荷加密（RFC 8291，内容编码 aes128gcm，RFC 8188）：整条消息一个记录。 */
object Ece:
  /** 请求头里的记录大小。 */
  val RecordSize = 4096

  /** 加密；发送方临时密钥与盐由调用方给（测试用 RFC 的固定值核对）。返回请求体：盐 ‖ 记录大小 ‖ 公钥长度 ‖ 发送方公钥 ‖ 密文。 */
  def encrypt(device: DeviceKeys, sender: KeyPair, salt: Array[Byte], payload: Array[Byte]): Array[Byte] =
    require(salt.length == 16, "盐是 16 字节")
    val senderPriv = sender.getPrivate.asInstanceOf[ECPrivateKey]
    val asPublic   = P256.encode(sender.getPublic.asInstanceOf[ECPublicKey])
    val uaPublic   = P256.encode(device.public)
    val shared     = P256.agree(senderPriv, device.public)

    val keyInfo = "WebPush: info\u0000".getBytes("US-ASCII") ++ uaPublic ++ asPublic
    val ikm     = Hkdf.expand(Hkdf.extract(device.auth, shared), keyInfo, 32)
    val prk     = Hkdf.extract(salt, ikm)
    val cek     = Hkdf.expand(prk, "Content-Encoding: aes128gcm\u0000".getBytes("US-ASCII"), 16)
    val nonce   = Hkdf.expand(prk, "Content-Encoding: nonce\u0000".getBytes("US-ASCII"), 12)

    // 唯一的记录也是最后一个：明文后接分隔符 0x02，不加填充
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(Cipher.ENCRYPT_MODE, SecretKeySpec(cek, "AES"), GCMParameterSpec(128, nonce))
    val sealedRecord = cipher.doFinal(payload :+ 2.toByte)

    val rs = java.nio.ByteBuffer.allocate(4).putInt(RecordSize).array()
    salt ++ rs ++ Array(asPublic.length.toByte) ++ asPublic ++ sealedRecord
