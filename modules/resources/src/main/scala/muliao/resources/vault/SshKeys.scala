package muliao.resources.vault

import muliao.contract.SshCert
import muliao.platform.AppError
import zio.*

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.security.{KeyPairGenerator, SecureRandom}

/** 签 SSH 证书用的临时钥匙（#938）：每张证书一把新的，放在系统临时目录里只有本用户能读的新目录（macOS 几天后自动清掉），
  * 证书过期后钥匙就没用了。
  */
object SshKeys:
  /** 新生成一把 ed25519 钥匙（JDK 自带）。编码里末尾 32 字节就是原始公钥与种子（X.509 与 PKCS#8 的固定前缀之后）。 */
  val generate: IO[AppError, SshCert.PrivateKey] =
    ZIO.attempt {
      val kp = KeyPairGenerator.getInstance("Ed25519").generateKeyPair()
      SshCert.PrivateKey(kp.getPrivate.getEncoded.takeRight(32), SshCert.PublicKey(kp.getPublic.getEncoded.takeRight(32)))
    }.mapError(e => AppError.internal(s"生成 SSH 临时钥匙失败：${e.getMessage}", e))

  /** 把钥匙与证书写进一个新的私有目录，返回钥匙路径（证书在它旁边的 `-cert.pub`，ssh 自动找）。 */
  def save(key: SshCert.PrivateKey, cert: String, comment: String): IO[AppError, Path] =
    ZIO.attemptBlocking {
      val dir  = Files.createTempDirectory("muliao-ssh-", PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------")))
      val file = dir.resolve("id_ed25519")
      Files.writeString(Files.createFile(file, PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))), SshCert.writePrivate(key, SecureRandom().nextInt(), comment))
      Files.writeString(dir.resolve("id_ed25519-cert.pub"), cert.trim + "\n")
      file
    }.mapError(e => AppError.internal(s"写 SSH 钥匙与证书失败：${e.getMessage}", e))
