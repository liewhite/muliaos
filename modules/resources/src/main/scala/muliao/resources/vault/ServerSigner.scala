package muliao.resources.vault

import muliao.contract.{SignContent, SigningAlgorithm, SshCert}
import muliao.platform.AppError
import zio.*

/** 服务端保管的密钥对在服务端签（#1069）：读私钥、按内容的类型拼出待签的字节、用 JDK 签。 */
object ServerSigner:
  def sign(key: String, pem: String, content: SignContent): IO[AppError, String] =
    for
      ca  <- ZIO.fromEither(SigningAlgorithm.readPrivate(content.algorithm, pem)).mapError(e => AppError.invalid(s"secret $key 不是可用的私钥：$e"))
      now <- Clock.currentTime(java.util.concurrent.TimeUnit.SECONDS)
      out <- content match
               case SignContent.SshCert(req) =>
                 ZIO
                   .attempt {
                     val rnd   = java.security.SecureRandom()
                     val nonce = Array.ofDim[Byte](32)
                     rnd.nextBytes(nonce)
                     SshCert.issue(ca, req, now, nonce, rnd.nextLong())(ed25519(ca.seed))
                   }
                   .mapError(e => AppError.internal(s"用 secret $key 签名失败：${e.getMessage}", e))
                   .flatMap(r => ZIO.fromEither(r).mapError(AppError.invalid))
    yield out

  private def ed25519(seed: Array[Byte])(data: Array[Byte]): Array[Byte] =
    import java.security.spec.{EdECPrivateKeySpec, NamedParameterSpec}
    val priv = java.security.KeyFactory.getInstance("Ed25519").generatePrivate(EdECPrivateKeySpec(NamedParameterSpec.ED25519, seed))
    val s    = java.security.Signature.getInstance("Ed25519")
    s.initSign(priv)
    s.update(data)
    s.sign()
