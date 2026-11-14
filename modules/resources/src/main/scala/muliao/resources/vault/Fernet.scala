package muliao.resources.vault

import java.nio.ByteBuffer
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.Mac

/** Fernet 对称加密（https://github.com/fernet/spec），与现有数据兼容：
  * 密钥是 URL-safe base64 的 32 字节，前 16 字节签名（HMAC-SHA256）、后 16 字节加密（AES-128-CBC，PKCS7）；
  * 令牌 = 0x80 | 时间戳(8) | IV(16) | 密文 | HMAC(32)，整体 URL-safe base64。
  */
final class Fernet private (signingKey: Array[Byte], encryptionKey: Array[Byte]):
  private val random = SecureRandom()

  def encrypt(plaintext: String, nowSeconds: Long): String =
    val iv = new Array[Byte](16)
    random.nextBytes(iv)
    encryptWith(plaintext.getBytes("UTF-8"), nowSeconds, iv)

  private[resources] def encryptWith(plain: Array[Byte], nowSeconds: Long, iv: Array[Byte]): String =
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, SecretKeySpec(encryptionKey, "AES"), IvParameterSpec(iv))
    val ct   = cipher.doFinal(plain)
    val body = ByteBuffer.allocate(1 + 8 + 16 + ct.length).put(Fernet.Version).putLong(nowSeconds).put(iv).put(ct).array()
    Base64.getUrlEncoder.encodeToString(body ++ hmac(body))

  /** 解密；令牌格式不对、签名对不上（密钥不对或密文被改）都返回原因。 */
  def decrypt(token: String): Either[String, String] =
    for
      raw <- scala.util.Try(Base64.getUrlDecoder.decode(token.trim)).toEither.left.map(_ => "密文不是合法的 base64")
      _   <- Either.cond(raw.length >= 1 + 8 + 16 + 16 + 32 && raw(0) == Fernet.Version, (), "密文格式不对")
      body = raw.dropRight(32)
      _   <- Either.cond(MessageDigest.isEqual(hmac(body), raw.takeRight(32)), (), "签名对不上（主密钥不对，或密文被改过）")
      iv   = body.slice(9, 25)
      ct   = body.drop(25)
      plain <- scala.util
                 .Try {
                   val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
                   cipher.init(Cipher.DECRYPT_MODE, SecretKeySpec(encryptionKey, "AES"), IvParameterSpec(iv))
                   String(cipher.doFinal(ct), "UTF-8")
                 }
                 .toEither
                 .left
                 .map(_ => "解密失败")
    yield plain

  private def hmac(data: Array[Byte]): Array[Byte] =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(SecretKeySpec(signingKey, "HmacSHA256"))
    mac.doFinal(data)

  override def toString: String = "Fernet(***)"

object Fernet:
  private val Version: Byte = 0x80.toByte

  def fromKey(key: String): Either[String, Fernet] =
    scala.util.Try(Base64.getUrlDecoder.decode(key.trim)).toOption match
      case Some(k) if k.length == 32 => Right(Fernet(k.take(16), k.drop(16)))
      case _                         => Left("vault 主密钥应是 URL-safe base64 编码的 32 字节（Fernet 密钥）")

  /** 新生成一个密钥。 */
  def generateKey(): String =
    val k = new Array[Byte](32)
    SecureRandom().nextBytes(k)
    Base64.getUrlEncoder.encodeToString(k)
