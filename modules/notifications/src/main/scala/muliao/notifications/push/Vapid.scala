package muliao.notifications.push

import muliao.platform.AppError
import zio.*

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.security.{KeyFactory, Signature}
import java.security.interfaces.{ECPrivateKey, ECPublicKey}
import java.security.spec.PKCS8EncodedKeySpec
import java.time.Instant

/** 本服务的 VAPID 密钥（RFC 8292）：推送服务据此认出发送方。私钥存为 PKCS#8 PEM，换了它所有设备都得重新订阅，所以只在第一次
  * 生成，此后一直沿用（迁移时连同文件一起搬）。
  */
final class VapidKeys private (privateKey: ECPrivateKey, publicKey: ECPublicKey):
  /** 浏览器订阅时用的 applicationServerKey（未压缩点的 base64url）。 */
  val publicKeyB64: String = B64.encode(P256.encode(publicKey))

  /** 推到 `endpoint` 时的 Authorization 头：ES256 签名的 JWT，受众是推送服务的源，12 小时有效。 */
  def authorization(endpoint: URI, subject: String, now: Instant): String =
    val origin = s"${endpoint.getScheme}://${endpoint.getRawAuthority}"
    val header = """{"typ":"JWT","alg":"ES256"}"""
    val claims = zio.json.ast.Json
      .Obj("aud" -> zio.json.ast.Json.Str(origin), "exp" -> zio.json.ast.Json.Num(now.getEpochSecond + 12 * 3600), "sub" -> zio.json.ast.Json.Str(subject))
      .toString
    val signing = s"${b64(header)}.${b64(claims)}"
    val s       = Signature.getInstance("SHA256withECDSAinP1363Format")
    s.initSign(privateKey)
    s.update(signing.getBytes(StandardCharsets.US_ASCII))
    s"vapid t=$signing.${B64.encode(s.sign())}, k=$publicKeyB64"

  private def b64(s: String): String = B64.encode(s.getBytes(StandardCharsets.UTF_8))

object VapidKeys:
  def of(k: ECPrivateKey): VapidKeys = VapidKeys(k, P256.publicOf(k))

  /** 读私钥文件；没有就生成一个（权限 600）。文件在但读不出来时报错，不覆盖（覆盖等于让全部设备的订阅失效）。 */
  def loadOrCreate(path: Path): IO[AppError, VapidKeys] =
    ZIO
      .attemptBlocking {
        if Files.exists(path) then
          restrict(path)
          of(parse(Files.readString(path)))
        else
          Files.createDirectories(path.toAbsolutePath.getParent)
          val k   = P256.generate.getPrivate.asInstanceOf[ECPrivateKey]
          val tmp = path.resolveSibling(s"${path.getFileName}.tmp")
          Files.deleteIfExists(tmp)
          Files.createFile(tmp, PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")))
          Files.writeString(tmp, pem(k))
          Files.move(tmp, path, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
          of(k)
      }
      .mapError(e => AppError.internal(s"VAPID 私钥 $path 读不出来：${e.getMessage}", e))

  private def restrict(path: Path): Unit =
    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-------")): Unit

  private def parse(text: String): ECPrivateKey =
    val body = text.linesIterator.map(_.trim).filterNot(l => l.startsWith("-----") || l.isEmpty).mkString
    KeyFactory.getInstance("EC").generatePrivate(PKCS8EncodedKeySpec(java.util.Base64.getDecoder.decode(body))) match
      case k: ECPrivateKey if k.getParams.getCurve == P256.params.getCurve => k
      case _                                                             => throw IllegalArgumentException("不是 P-256 私钥")

  private def pem(k: ECPrivateKey): String =
    val b64 = java.util.Base64.getMimeEncoder(64, "\n".getBytes).encodeToString(k.getEncoded)
    s"-----BEGIN PRIVATE KEY-----\n$b64\n-----END PRIVATE KEY-----\n"
