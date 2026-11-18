package muliao.contract

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64

/** OpenSSH 的 ed25519 钥匙与用户证书的编码（PROTOCOL.certkeys、PROTOCOL.key），前后端共用（#938）：
  * 服务端生成临时钥匙、写钥匙文件；浏览器读 CA 私钥、拼出待签的证书，用 WebCrypto 签名后拼成证书行。
  * 只有编码，不含签名与随机数：签名各端自己做（服务端 JDK、浏览器 WebCrypto），随机数由调用方给。
  */
object SshCert:
  val KeyType  = "ssh-ed25519"
  val CertType = "ssh-ed25519-cert-v01@openssh.com"

  /** 证书带的扩展（按名字排序）：只给终端与端口转发，不给 X11、agent 转发与 ~/.ssh/rc。 */
  val Extensions: List[String] = List("permit-port-forwarding", "permit-pty")

  /** ed25519 公钥（32 字节）。 */
  final class PublicKey(raw: Array[Byte]):
    def bytes: Array[Byte] = raw.clone()
    /** 线上格式：string "ssh-ed25519" + string 公钥。 */
    def blob: Array[Byte] = Wire.write(_.str(KeyType).bytes(raw))
    def line(comment: String): String = s"$KeyType ${b64(blob)}${if comment.isEmpty then "" else " " + comment}"
    override def equals(o: Any): Boolean = o match
      case p: PublicKey => java.util.Arrays.equals(raw, p.bytes)
      case _            => false
    override def hashCode: Int = java.util.Arrays.hashCode(raw)
    override def toString: String = s"PublicKey(${b64(raw)})"

  /** ed25519 私钥：32 字节种子与对应公钥。toString 不带种子。 */
  final class PrivateKey(seedBytes: Array[Byte], val publicKey: PublicKey):
    def seed: Array[Byte] = seedBytes.clone()
    override def toString: String = s"PrivateKey($publicKey)"

  /** 有效期（epoch 秒，含头不含尾）。 */
  final case class Validity(after: Long, before: Long)

  /** 一张待签的用户证书。`principals` 是允许登录的名字，`sourceAddress` 是只许从哪些地址用（CIDR，逗号分隔）。 */
  final case class Unsigned(
      nonce: Array[Byte],
      key: PublicKey,
      serial: Long,
      keyId: String,
      principals: List[String],
      validity: Validity,
      sourceAddress: Option[String],
      ca: PublicKey
  ):
    /** 签名覆盖的字节：证书从开头到 CA 公钥为止。 */
    def toBeSigned: Array[Byte] =
      Wire.write { w =>
        w.str(CertType).bytes(nonce).bytes(key.bytes).u64(serial).u32(1) // 1 = 用户证书
          .str(keyId)
          .bytes(Wire.write(x => principals.foldLeft(x)(_.str(_))))
          .u64(validity.after)
          .u64(validity.before)
          .bytes(Wire.write(x => sourceAddress.foldLeft(x)((y, a) => y.str("source-address").bytes(Wire.write(_.str(a))))))
          .bytes(Wire.write(x => Extensions.foldLeft(x)((y, e) => y.str(e).bytes(Array.emptyByteArray))))
          .bytes(Array.emptyByteArray) // reserved
          .bytes(ca.blob)
      }

    /** 带上 CA 的签名（64 字节），得到证书文件的一行（`<keyId>` 作注释）。 */
    def signed(signature: Array[Byte]): String =
      val all = Wire.write(_.raw(toBeSigned).bytes(Wire.write(_.str(KeyType).bytes(signature))))
      s"$CertType ${b64(all)} $keyId"

  /** 读公钥行（`ssh-ed25519 AAAA… 注释`）。 */
  def readPublic(line: String): Either[String, PublicKey] =
    line.trim.split("\\s+").toList match
      case KeyType :: data :: _ =>
        for
          blob <- unb64(data)
          pub  <- Wire.read(blob)(r => r.expect(KeyType).flatMap(_ => r.bytes)).flatMap(key)
        yield pub
      case _ => Left("不是 ssh-ed25519 公钥")

  /** 读 OpenSSH 格式、不带口令的 ed25519 私钥文件（`-----BEGIN OPENSSH PRIVATE KEY-----`）。 */
  def readPrivate(pem: String): Either[String, PrivateKey] =
    // 不按行切：网页的单行输入框会把换行删掉，整份成了一行
    val body = pem.replaceAll("-----(BEGIN|END) OPENSSH PRIVATE KEY-----", "").replaceAll("\\s+", "")
    for
      bin <- unb64(body)
      k <- Wire.read(bin) { r =>
             for
               _      <- r.magic(Magic)
               cipher <- r.string
               _      <- Either.cond(cipher == "none", (), "私钥带了口令：要用不带口令的（ssh-keygen -N ''）")
               _      <- r.string // kdfname
               _      <- r.bytes  // kdfoptions
               n      <- r.u32
               _      <- Either.cond(n == 1, (), s"文件里有 $n 把钥匙，只支持 1 把")
               _      <- r.bytes // 公钥
               priv   <- r.bytes
               k      <- Wire.read(priv) { p =>
                           for
                             c1   <- p.u32
                             c2   <- p.u32
                             _    <- Either.cond(c1 == c2, (), "私钥文件校验不对")
                             _    <- p.expect(KeyType)
                             pub  <- p.bytes.flatMap(key)
                             both <- p.bytes
                             _    <- Either.cond(both.length == 64 && java.util.Arrays.equals(both.drop(32), pub.bytes), (), "私钥与公钥对不上")
                           yield PrivateKey(both.take(32), pub)
                         }
             yield k
           }
    yield k

  /** 写成 OpenSSH 格式、不带口令的私钥文件。`check` 是文件里的校验数（随机）。 */
  def writePrivate(k: PrivateKey, check: Int, comment: String): String =
    val pub = k.publicKey.bytes
    val inner = Wire.write(_.u32(check).u32(check).str(KeyType).bytes(pub).bytes(k.seed ++ pub).str(comment))
    val padded = inner ++ Array.tabulate((8 - inner.length % 8) % 8)(i => (i + 1).toByte)
    val all = Wire.write(_.raw(Magic).str("none").str("none").bytes(Array.emptyByteArray).u32(1).bytes(k.publicKey.blob).bytes(padded))
    b64(all).grouped(70).mkString("-----BEGIN OPENSSH PRIVATE KEY-----\n", "\n", "\n-----END OPENSSH PRIVATE KEY-----\n")

  private val Magic = "openssh-key-v1\u0000".getBytes(UTF_8)

  private def key(raw: Array[Byte]): Either[String, PublicKey] =
    Either.cond(raw.length == 32, PublicKey(raw), s"ed25519 公钥应是 32 字节，实际 ${raw.length}")

  private def b64(a: Array[Byte]): String = Base64.getEncoder.encodeToString(a)

  private def unb64(s: String): Either[String, Array[Byte]] =
    try Right(Base64.getDecoder.decode(s))
    catch case _: IllegalArgumentException => Left("base64 解不开")

  /** SSH 线上格式（RFC 4251 的 uint32、uint64、string）。 */
  private object Wire:
    final class Writer:
      private val out = Array.newBuilder[Byte]
      def raw(a: Array[Byte]): Writer = { out ++= a; this }
      def u32(n: Int): Writer         = raw(Array(24, 16, 8, 0).map(s => (n >>> s).toByte))
      def u64(n: Long): Writer        = raw(Array(56, 48, 40, 32, 24, 16, 8, 0).map(s => (n >>> s).toByte))
      def bytes(a: Array[Byte]): Writer = u32(a.length).raw(a)
      def str(s: String): Writer      = bytes(s.getBytes(UTF_8))
      def result: Array[Byte]         = out.result()

    def write(f: Writer => Writer): Array[Byte] = f(Writer()).result

    final class Reader(a: Array[Byte]):
      private var at = 0
      private def take(n: Int): Either[String, Array[Byte]] =
        if n < 0 || n > a.length - at then Left("数据被截断")
        else
          val r = a.slice(at, at + n)
          at += n
          Right(r)
      def u32: Either[String, Int] = take(4).map(_.foldLeft(0)((acc, b) => (acc << 8) | (b & 0xff)))
      def bytes: Either[String, Array[Byte]] = u32.flatMap(take)
      def string: Either[String, String] = bytes.map(String(_, UTF_8))
      def expect(s: String): Either[String, Unit] = string.flatMap(t => Either.cond(t == s, (), s"应是 $s，实际 $t"))
      def magic(m: Array[Byte]): Either[String, Unit] =
        take(m.length).flatMap(t => Either.cond(java.util.Arrays.equals(t, m), (), "不是 OpenSSH 私钥文件"))

    def read[A](a: Array[Byte])(f: Reader => Either[String, A]): Either[String, A] = f(Reader(a))
