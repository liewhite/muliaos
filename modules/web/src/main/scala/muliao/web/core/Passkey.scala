package muliao.web.core

import muliao.contract.{PasskeyAad, PasskeyBox, PasskeyCredential, SignContent, SigningAlgorithm, SshCert, SshCertRequest}
import org.scalajs.dom

import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

import Http.ec

/** passkey 加密的 secret（#915）：WebAuthn 的 PRF 扩展从 passkey 导出一把固定的密钥，AES-GCM 在浏览器里加解密（附加数据见 [[PasskeyAad]]），
  * 服务端只存密文。passkey 绑定当前域名：在别的地址打开解不开。签名钥匙（#938）只在这里签名，不交出明文：它的附加数据与普通的不同，取明文的路解不开它。
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
  def seal(aad: String, plain: String, known: List[PasskeyCredential]): Future[PasskeyBox] =
    for
      (cred, aes) <- known.find(_.rp == host).map(_.credential).orElse(created) match
                       case Some(c) => derive(c).map(c -> _)
                       case None =>
                         known.headOption match
                           case Some(other) => fail(elsewhere(other.rp))
                           case None        => create()
      iv   = random(12)
      ct  <- promise[ArrayBuffer](subtle.encrypt(js.Dynamic.literal(name = "AES-GCM", iv = iv, additionalData = utf8(aad)), aes, utf8(plain)))
    yield PasskeyBox(host, cred, b64(iv), b64(new Uint8Array(ct)))

  /** 用户设签名钥匙：先核对是这种算法认得的私钥，再加密。 */
  def sealSigning(key: String, alg: SigningAlgorithm, plain: String, known: List[PasskeyCredential]): Future[PasskeyBox] =
    either(material(alg, plain)).flatMap(_ => seal(PasskeyAad.signing(key, alg), plain, known))

  /** 把普通 passkey secret 转成签名钥匙：解开、核对是私钥、按签名钥匙重新加密（用同一把 passkey）。 */
  def toSigning(key: String, alg: SigningAlgorithm, box: PasskeyBox, known: List[PasskeyCredential]): Future[PasskeyBox] =
    open(PasskeyAad.plain(key), box).flatMap(v => sealSigning(key, alg, v, known))

  /** 解开普通 passkey secret 交出明文（签名钥匙在这里解不开）。 */
  def reveal(key: String, box: PasskeyBox): Future[String] = open(PasskeyAad.plain(key), box)

  /** 用签名钥匙签一份内容，返回签好的结果。内容按类型在这里拼成待签的字节，私钥不离开这个页面。 */
  def sign(key: String, box: PasskeyBox, content: SignContent): Future[String] =
    for
      pem    <- open(PasskeyAad.signing(key, content.algorithm), box)
      secret <- either(material(content.algorithm, pem).left.map(e => s"secret $key 不是可用的签名钥匙：$e"))
      out <- content match
               case SignContent.SshCert(req) => either(certificate(secret, req))
    yield out

  /** 某种算法的私钥文本读成能签名的东西。ed25519 认 OpenSSH 私钥文件（不带口令）。 */
  private def material(alg: SigningAlgorithm, text: String): Either[String, SshCert.PrivateKey] = alg match
    case SigningAlgorithm.Ed25519 => SshCert.readPrivate(text)

  /** 签一张 SSH 证书：有效期从现在起 `minutes` 分钟，开头往前留 5 分钟容服务器时钟偏差；nonce、serial 取浏览器的安全随机数。 */
  private def certificate(ca: SshCert.PrivateKey, req: SshCertRequest): Either[String, String] =
    SshCert.readPublic(req.publicKey).left.map(e => s"请求里的公钥不对：$e").map { pub =>
      val now = (js.Date.now() / 1000).toLong
      val unsigned = SshCert.Unsigned(
        nonce = bytes(random(32)),
        key = pub,
        serial = bytes(random(8)).foldLeft(0L)((acc, b) => (acc << 8) | (b & 0xffL)),
        keyId = req.keyId,
        principals = req.principals,
        validity = SshCert.Validity(now - 300, now + req.minutes.toLong * 60),
        sourceAddress = req.sourceAddress,
        ca = ca.publicKey
      )
      unsigned.signed(bytes(NobleEd25519.sign(array(unsigned.toBeSigned), array(ca.seed))))
    }

  /** 解开（用户在系统弹窗里确认）。`aad` 不对（比如拿签名钥匙走取明文）就解不开。 */
  private def open(aad: String, box: PasskeyBox): Future[String] =
    if box.rp != host then fail(elsewhere(box.rp))
    else
      for
        aes <- derive(box.credential)
        pt <- promise[ArrayBuffer](
                subtle.decrypt(js.Dynamic.literal(name = "AES-GCM", iv = unb64(box.iv), additionalData = utf8(aad)), aes, unb64(box.ciphertext))
              ).recoverWith(_ => fail("解不开：选的不是加密时那把 passkey，或密文被改过"))
      yield js.Dynamic.newInstance(js.Dynamic.global.TextDecoder)().decode(new Uint8Array(pt)).asInstanceOf[String]

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
