package muliao.web.core

import muliao.contract.{PasskeyBox, PasskeyCredential}
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

  /** 这个页面能不能用 passkey（要 HTTPS 与支持 WebAuthn 的浏览器）。 */
  def available: Boolean =
    dom.window.asInstanceOf[js.Dynamic].isSecureContext.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false) &&
      !js.isUndefined(dom.window.asInstanceOf[js.Dynamic].PublicKeyCredential)

  /** 加密一个新值。已有用过的 passkey（同一域名）就用它，否则先新建一把。 */
  def seal(key: String, plain: String, known: List[PasskeyCredential]): Future[PasskeyBox] =
    for
      cred <- known.find(_.rp == host) match
                case Some(c) => Future.successful(c.credential)
                case None =>
                  known.headOption match
                    case Some(other) => fail(elsewhere(other.rp))
                    case None        => create()
      aes <- derive(cred)
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

  private def elsewhere(rp: String) = s"passkey 绑在 $rp，请在 https://$rp 打开这一页"

  /** 新建一把 passkey（可同步的发现式凭据，要求开着 PRF），返回它的编号。 */
  private def create(): Future[String] =
    val options = js.Dynamic.literal(
      publicKey = js.Dynamic.literal(
        rp = js.Dynamic.literal(id = host, name = "muliao"),
        user = js.Dynamic.literal(id = random(16), name = "muliao vault", displayName = "muliao vault"),
        challenge = random(32),
        pubKeyCredParams = js.Array(js.Dynamic.literal(`type` = "public-key", alg = -7), js.Dynamic.literal(`type` = "public-key", alg = -257)),
        authenticatorSelection = js.Dynamic.literal(residentKey = "required", userVerification = "required"),
        extensions = js.Dynamic.literal(prf = js.Dynamic.literal())
      )
    )
    promise[js.Dynamic](nav.credentials.create(options)).flatMap { cred =>
      val enabled = cred.getClientExtensionResults().prf.asInstanceOf[js.UndefOr[js.Dynamic]].toOption.flatMap(_.enabled.asInstanceOf[js.UndefOr[Boolean]].toOption)
      if enabled.contains(false) then fail("这把 passkey 不支持 PRF，不能用来加密；换用系统钥匙串（iCloud 钥匙串、Google 密码管理器）的 passkey")
      else Future.successful(b64url(new Uint8Array(cred.rawId.asInstanceOf[ArrayBuffer])))
    }

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
      first = cred.getClientExtensionResults().prf.asInstanceOf[js.UndefOr[js.Dynamic]].toOption
                .flatMap(_.results.asInstanceOf[js.UndefOr[js.Dynamic]].toOption)
                .flatMap(_.first.asInstanceOf[js.UndefOr[ArrayBuffer]].toOption)
      prf <- first.fold(fail[ArrayBuffer]("浏览器没给出 PRF 结果：要 Safari 18 / iOS 18、Chrome 116 以上，并用支持 PRF 的 passkey"))(Future.successful)
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
