package muliao.web.core

import muliao.contract.Crypto
import muliao.contract.chain.EcdsaSignature
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

/** 浏览器里签名的底层能力（#1092）：@noble/curves 的 ed25519 与 secp256k1，WebCrypto 的随机数。签法与服务端的 Bouncy Castle 逐字节相同。 */
object BrowserCrypto extends Crypto:
  def ed25519Public(seed: Array[Byte]): Array[Byte] = bytes(NobleEd25519.getPublicKey(array(seed)))

  def ed25519Sign(seed: Array[Byte], message: Array[Byte]): Array[Byte] = bytes(NobleEd25519.sign(array(message), array(seed)))

  def secp256k1Public(secret: Array[Byte]): Array[Byte] = bytes(NobleSecp256k1.getPublicKey(array(secret), false)).drop(1)

  def secp256k1Sign(secret: Array[Byte], digest: Array[Byte]): EcdsaSignature =
    val out = bytes(NobleSecp256k1.sign(array(digest), array(secret), js.Dynamic.literal(prehash = false, lowS = true, format = "recovered", extraEntropy = false)))
    EcdsaSignature(BigInt(1, out.slice(1, 33)), BigInt(1, out.slice(33, 65)), out(0).toInt)

  def random(n: Int): Array[Byte] =
    val a = new Uint8Array(n)
    dom.window.asInstanceOf[js.Dynamic].crypto.getRandomValues(a)
    bytes(a)

  def now: Long = (js.Date.now() / 1000).toLong

  private def bytes(a: Uint8Array): Array[Byte] = Array.tabulate(a.length)(i => a(i).toByte)

  private def array(a: Array[Byte]): Uint8Array =
    val u = new Uint8Array(a.length)
    a.indices.foreach(i => u(i) = (a(i) & 0xff).toShort)
    u
