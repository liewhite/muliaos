package muliao.resources

import muliao.contract.SshCert
import muliao.resources.vault.SshKeys
import zio.*
import zio.test.*

import java.nio.file.{Files, Path}
import java.security.KeyFactory
import java.security.spec.{EdECPrivateKeySpec, NamedParameterSpec}

/** SSH 证书编码（#938）：和本机 ssh-keygen 逐字节比对（ed25519 签名是确定的，随机数取 ssh-keygen 那张的）。 */
object SshCertSpec extends ZIOSpecDefault:
  def sh(dir: Path, cmd: String*): String =
    val p = ProcessBuilder(cmd*).directory(dir.toFile).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes())
    val code = p.waitFor()
    if code != 0 then throw RuntimeException(s"${cmd.mkString(" ")} 退出码 $code：$out")
    out

  /** 用 JDK 按种子签 ed25519（浏览器里是 WebCrypto）。 */
  def sign(seed: Array[Byte], data: Array[Byte]): Array[Byte] =
    val key = KeyFactory.getInstance("Ed25519").generatePrivate(EdECPrivateKeySpec(NamedParameterSpec.ED25519, seed))
    val s = java.security.Signature.getInstance("Ed25519")
    s.initSign(key)
    s.update(data)
    s.sign()

  /** 证书里的 nonce：类型名之后的第一个 string。 */
  def nonceOf(certLine: String): Array[Byte] =
    val blob = java.util.Base64.getDecoder.decode(certLine.trim.split(" ")(1))
    def u32(at: Int) = blob.slice(at, at + 4).foldLeft(0)((a, b) => (a << 8) | (b & 0xff))
    val at = 4 + u32(0)
    blob.slice(at + 4, at + 4 + u32(at))

  val spec = suite("SSH 证书")(
    test("读 ssh-keygen 的 CA 私钥与公钥；拼出的证书与 ssh-keygen 签的逐字节相同；带不带来源限制都对") {
      ZIO.attemptBlocking {
        val dir = Files.createTempDirectory("sshcert")
        sh(dir, "ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-C", "ca", "-f", "ca")
        sh(dir, "ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-C", "", "-f", "user")
        val ca   = SshCert.readPrivate(Files.readString(dir.resolve("ca"))).toOption.get
        val caPub = SshCert.readPublic(Files.readString(dir.resolve("ca.pub"))).toOption.get
        val user = SshCert.readPublic(Files.readString(dir.resolve("user.pub"))).toOption.get
        def both(source: Option[String]) =
          val opts = List("-O", "clear", "-O", "permit-pty", "-O", "permit-port-forwarding") ++ source.toList.flatMap(s => List("-O", s"source-address=$s"))
          sh(dir, (List("ssh-keygen", "-q", "-s", "ca", "-I", "muliao:alice:s1", "-n", "admin@relay,root@jlp", "-z", "1234567890123", "-V", "0x6a000000:0x6a000708") ++ opts ++ List("user.pub"))*)
          val theirs = Files.readString(dir.resolve("user-cert.pub")).trim
          val u = SshCert.Unsigned(nonceOf(theirs), user, 1234567890123L, "muliao:alice:s1", List("admin@relay", "root@jlp"), SshCert.Validity(0x6a000000L, 0x6a000708L), source, ca.publicKey)
          (theirs.split(" ")(1), u.signed(sign(ca.seed, u.toBeSigned)))
        val (t1, o1) = both(None)
        val (t2, o2) = both(Some("203.0.113.7/32,10.0.0.0/8"))
        Files.writeString(dir.resolve("mine-cert.pub"), o2 + "\n")
        val listed = sh(dir, "ssh-keygen", "-L", "-f", "mine-cert.pub")
        (ca, caPub, t1, o1, t2, o2, listed)
      }.map { (ca, caPub, t1, o1, t2, o2, listed) =>
        assertTrue(
          ca.publicKey == caPub,
          o1.split(" ")(1) == t1,
          o2.split(" ")(1) == t2,
          o1.startsWith(SshCert.CertType + " "),
          o1.endsWith(" muliao:alice:s1"),
          listed.contains("source-address 203.0.113.7/32,10.0.0.0/8"),
          listed.contains("admin@relay")
        )
      }
    },
    test("平台生成的临时钥匙写成 OpenSSH 私钥文件，ssh-keygen 认得、导出的公钥一致；钥匙文件只有本用户可读") {
      for
        key  <- SshKeys.generate
        path <- SshKeys.save(key, "ssh-ed25519-cert-v01@openssh.com AAAA x", "muliao:t")
        pub  <- ZIO.attemptBlocking(sh(path.getParent, "ssh-keygen", "-y", "-f", path.toString))
        perm <- ZIO.attemptBlocking(java.nio.file.attribute.PosixFilePermissions.toString(Files.getPosixFilePermissions(path)))
        cert <- ZIO.attemptBlocking(Files.readString(path.resolveSibling("id_ed25519-cert.pub")))
        back  = SshCert.readPrivate(Files.readString(path))
      yield assertTrue(
        SshCert.readPublic(pub) == Right(key.publicKey),
        back.map(_.publicKey) == Right(key.publicKey),
        back.map(_.seed.toSeq) == Right(key.seed.toSeq),
        perm == "rw-------",
        cert == "ssh-ed25519-cert-v01@openssh.com AAAA x\n"
      )
    },
    test("读不了的：带口令的私钥、不是 ed25519 的公钥") {
      ZIO.attemptBlocking {
        val dir = Files.createTempDirectory("sshcert")
        sh(dir, "ssh-keygen", "-q", "-t", "ed25519", "-N", "pass", "-f", "locked")
        sh(dir, "ssh-keygen", "-q", "-t", "ecdsa", "-N", "", "-f", "ec")
        (SshCert.readPrivate(Files.readString(dir.resolve("locked"))), SshCert.readPublic(Files.readString(dir.resolve("ec.pub"))))
      }.map((locked, ec) => assertTrue(locked.left.toOption.exists(_.contains("口令")), ec.isLeft))
    }
  )
