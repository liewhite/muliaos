package muliao.web.core

import muliao.contract.{PasskeyBox, PasskeyCredential, SshCert, SshCertRequest}
import org.scalajs.dom

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

import Http.ec

/** passkey 加密的 secret（#915）：WebAuthn 的 PRF 扩展从 passkey 导出一把固定的密钥，AES-GCM 在浏览器里加解密（附加数据是 secret 名），
  * 服务端只存密文。passkey 绑定当前域名：在别的地址打开解不开。
  */
object Passkey:
  /** PRF 的输入与 HKDF 的 info（固定）：同一把 passkey 每次导出同一把密钥。改了就解不开已有的密文。 */
  private val Salt = "muliao vault prf v1"
  private val Info = "muliao vault aes-gcm v1"

  private def nav  = dom.window.navigator.asInstanceOf[js.Dynamic]
  private def subtle = dom.window.asInstanceOf[js.Dynamic].crypto.subtle
  def host: String = dom.window.location.hostname

  /** 这个页面能不能用 passkey：HTTPS、支持 WebAuthn，且是域名地址（passkey 不能绑 IP；绑到 localhost 以后就只能在本机解开）。 */
  def available: Boolean =
    dom.window.asInstanceOf[js.Dynamic].isSecureContext.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false) &&
      !js.isUndefined(dom.window.asInstanceOf[js.Dynamic].PublicKeyCredential) &&
      host.contains('.') && !host.forall(c => c.isDigit || c == '.')

  /** 这次打开页面新建、还没用来加密过的 passkey（浏览器建时没给出 PRF 结果，要用户再点一次）。 */
  private var created: Option[String] = None

  /** 加密一个新值（要在用户的点击里直接调，中间不发网络请求：Safari 要求 WebAuthn 由点击触发）。
    * `known` 是已用过的 passkey：有同一域名的就用它；都没有就新建一把，浏览器建时给出了 PRF 结果就直接加密，否则请用户再点一次。
    */
  def seal(key: String, plain: String, known: List[PasskeyCredential]): Future[PasskeyBox] =
    for
      (cred, aes) <- known.find(_.rp == host).map(_.credential).orElse(created) match
                       case Some(c) => derive(c).map(c -> _)
                       case None =>
                         known.headOption match
                           case Some(other) => fail(elsewhere(other.rp))
                           case None        => create()
      iv   = random(12)
      ct  <- promise[ArrayBuffer](subtle.encrypt(js.Dynamic.literal(name = "AES-GCM", iv = iv, additionalData = utf8(key)), aes, utf8(plain)))
    yield PasskeyBox(host, cred, b64(iv), b64(new Uint8Array(ct)))

  /** 解开一个 passkey secret（用户在系统弹窗里确认）。 */
  def open(key: String, box: PasskeyBox): Future[String] =
    if box.rp != host then fail(elsewhere(box.rp))
    else
      for
        aes <- derive(box.credential)
        pt <- promise[ArrayBuffer](
                subtle.decrypt(js.Dynamic.literal(name = "AES-GCM", iv = unb64(box.iv), additionalData = utf8(key)), aes, unb64(box.ciphertext))
              ).recoverWith(_ => fail("解不开：选的不是加密时那把 passkey，或密文被改过"))
      yield js.Dynamic.newInstance(js.Dynamic.global.TextDecoder)().decode(new Uint8Array(pt)).asInstanceOf[String]

  /** 用 passkey secret 里的 SSH CA 签一张证书（#938）：解开 CA 私钥、拼出证书、WebCrypto 签名，返回证书文件的一行。CA 私钥不离开这个页面。
    * 有效期从现在起 `minutes` 分钟，开头往前留 5 分钟容服务器时钟偏差。
    */
  def certify(key: String, box: PasskeyBox, req: SshCertRequest): Future[String] =
    for
      pem <- open(key, box)
      ca  <- either(SshCert.readPrivate(pem).left.map(e => s"secret $key 不是可用的 SSH CA：$e"))
      pub <- either(SshCert.readPublic(req.publicKey).left.map(e => s"请求里的公钥不对：$e"))
      now  = (js.Date.now() / 1000).toLong
      unsigned = SshCert.Unsigned(
                   nonce = bytes(random(32)),
                   key = pub,
                   serial = bytes(random(8)).foldLeft(0L)((acc, b) => (acc << 8) | (b & 0xffL)),
                   keyId = req.keyId,
                   principals = req.principals,
                   validity = SshCert.Validity(now - 300, now + req.minutes.toLong * 60),
                   sourceAddress = req.sourceAddress,
                   ca = ca.publicKey
                 )
      signer <- promise[js.Dynamic](
                  subtle.importKey("pkcs8", array(Pkcs8Ed25519 ++ ca.seed), js.Dynamic.literal(name = "Ed25519"), false, js.Array("sign"))
                ).recoverWith(e => fail(s"浏览器不支持 Ed25519 签名（要 Safari 17、Chrome 137 以上）：${e.getMessage}"))
      sig <- promise[ArrayBuffer](subtle.sign(js.Dynamic.literal(name = "Ed25519"), signer, array(unsigned.toBeSigned)))
    yield unsigned.signed(bytes(new Uint8Array(sig)))

  /** ed25519 私钥种子的 PKCS#8 外壳（RFC 8410）：后面接 32 字节种子。 */
  private val Pkcs8Ed25519: Array[Byte] =
    Array(0x30, 0x2e, 0x02, 0x01, 0x00, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x04, 0x22, 0x04, 0x20).map(_.toByte)

  private def either[A](e: Either[String, A]): Future[A] = e.fold(fail, Future.successful)

  private def bytes(a: Uint8Array): Array[Byte] = Array.tabulate(a.length)(i => a(i).toByte)

  private def array(a: Array[Byte]): Uint8Array =
    val u = new Uint8Array(a.length)
    a.indices.foreach(i => u(i) = (a(i) & 0xff).toShort)
    u

  private def elsewhere(rp: String) = s"passkey 绑在 $rp，请在 https://$rp 打开这一页"

  /** 新建一把 passkey（可同步的发现式凭据，要求开着 PRF）：浏览器顺带给出 PRF 结果就返回编号与密钥；只说支持就记下编号，请用户再点一次。 */
  private def create(): Future[(String, js.Dynamic)] =
    val options = js.Dynamic.literal(
      publicKey = js.Dynamic.literal(
        rp = js.Dynamic.literal(id = host, name = "muliao"),
        user = js.Dynamic.literal(id = random(16), name = "muliao vault", displayName = "muliao vault"),
        challenge = random(32),
        pubKeyCredParams = js.Array(js.Dynamic.literal(`type` = "public-key", alg = -7), js.Dynamic.literal(`type` = "public-key", alg = -257)),
        authenticatorSelection = js.Dynamic.literal(residentKey = "required", userVerification = "required"),
        extensions = js.Dynamic.literal(prf = js.Dynamic.literal(eval = js.Dynamic.literal(first = utf8(Salt))))
      )
    )
    promise[js.Dynamic](nav.credentials.create(options)).flatMap { cred =>
      val id  = b64url(new Uint8Array(cred.rawId.asInstanceOf[ArrayBuffer]))
      val prf = cred.getClientExtensionResults().prf.asInstanceOf[js.UndefOr[js.Dynamic]].toOption
      prf.flatMap(firstResult) match
        case Some(out) => keyOf(out).map(id -> _)
        case None if prf.flatMap(_.enabled.asInstanceOf[js.UndefOr[Boolean]].toOption).contains(true) =>
          created = Some(id)
          fail("passkey 已建好，浏览器还要再确认一次才能加密：请再点一次保存")
        case None => fail("这把 passkey 不支持 PRF，不能用来加密；换用系统钥匙串（iCloud 钥匙串、Google 密码管理器）的 passkey")
    }

  private def firstResult(prf: js.Dynamic): Option[ArrayBuffer] =
    prf.results.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.flatMap(_.first.asInstanceOf[js.UndefOr[ArrayBuffer]].toOption)

  /** 用那把 passkey 算出 PRF，再派生出 AES-GCM 密钥。 */
  private def derive(credential: String): Future[js.Dynamic] =
    val options = js.Dynamic.literal(
      publicKey = js.Dynamic.literal(
        challenge = random(32),
        rpId = host,
        allowCredentials = js.Array(js.Dynamic.literal(`type` = "public-key", id = unb64url(credential))),
        userVerification = "required",
        extensions = js.Dynamic.literal(prf = js.Dynamic.literal(eval = js.Dynamic.literal(first = utf8(Salt))))
      )
    )
    for
      cred <- promise[js.Dynamic](nav.credentials.get(options))
      first = cred.getClientExtensionResults().prf.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.flatMap(firstResult)
      prf <- first.fold(fail[ArrayBuffer]("浏览器没给出 PRF 结果：要 Safari 18 / iOS 18、Chrome 116 以上，并用支持 PRF 的 passkey"))(Future.successful)
      aes <- keyOf(prf)
    yield aes

  /** PRF 结果经 HKDF 派生出 AES-GCM 密钥。 */
  private def keyOf(prf: ArrayBuffer): Future[js.Dynamic] =
    for
      hk  <- promise[js.Dynamic](subtle.importKey("raw", prf, "HKDF", false, js.Array("deriveKey")))
      aes <- promise[js.Dynamic](
               subtle.deriveKey(
                 js.Dynamic.literal(name = "HKDF", hash = "SHA-256", salt = new Uint8Array(0), info = utf8(Info)),
                 hk,
                 js.Dynamic.literal(name = "AES-GCM", length = 256),
                 false,
                 js.Array("encrypt", "decrypt")
               )
             )
    yield aes

  private def promise[A](p: js.Dynamic): Future[A] =
    p.asInstanceOf[js.Promise[A]].toFuture.recoverWith {
      case e: js.JavaScriptException => fail(Option(e.exception.asInstanceOf[js.Dynamic].message).map(_.toString).getOrElse(e.toString))
    }

  private def fail[A](msg: String): Future[A] = Future.failed(new Exception(msg))

  private def utf8(s: String): Uint8Array = js.Dynamic.newInstance(js.Dynamic.global.TextEncoder)().encode(s).asInstanceOf[Uint8Array]

  private def random(n: Int): Uint8Array =
    val a = new Uint8Array(n)
    dom.window.asInstanceOf[js.Dynamic].crypto.getRandomValues(a)
    a

  private def b64(a: Uint8Array): String = dom.window.btoa(a.map(_.toChar).mkString)

  private def unb64(s: String): Uint8Array =
    val bin = dom.window.atob(s)
    val a   = new Uint8Array(bin.length)
    bin.indices.foreach(i => a(i) = bin.charAt(i).toShort)
    a

  private def b64url(a: Uint8Array): String = b64(a).replace('+', '-').replace('/', '_').reverse.dropWhile(_ == '=').reverse

  private def unb64url(s: String): Uint8Array =
    val std = s.replace('-', '+').replace('_', '/')
    unb64(std + "=" * ((4 - std.length % 4) % 4))
