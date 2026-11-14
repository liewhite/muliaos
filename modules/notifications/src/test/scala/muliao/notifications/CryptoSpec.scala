package muliao.notifications

import muliao.notifications.push.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.math.BigInteger
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.security.{KeyFactory, KeyPair, Signature}
import java.security.interfaces.{ECPrivateKey, ECPublicKey}
import java.security.spec.ECPrivateKeySpec
import java.time.Instant

/** Web Push 的加密与签名：RFC 8291 的例子逐字节核对，VAPID 签名用公钥验证，私钥文件与 openssl 生成的格式互通。 */
object CryptoSpec extends ZIOSpecDefault:

  private def b64(s: String): Array[Byte] = B64.decode(s).toOption.get

  private def privateKey(raw: Array[Byte]): ECPrivateKey =
    KeyFactory.getInstance("EC").generatePrivate(ECPrivateKeySpec(BigInteger(1, raw), P256.params)).asInstanceOf[ECPrivateKey]

  // RFC 8291 第 5 节（附录 A）的例子
  private val uaPublic = "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4"
  private val authSecret = "BTBZMqHH6r4Tts7J_aSIgg"
  private val asPrivate = "yfWPiYE-n46HLnH0KqZOF1fJJU3MYrct3AELtAQ-oRw"
  private val asPublic  = "BP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27mlmlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A8"
  private val salt      = "DGv6ra1nlYgDCS1FRnbzlw"
  private val expected =
    "DGv6ra1nlYgDCS1FRnbzlwAAEABBBP4z9KsN6nGRTbVYI_c7VJSPQTBtkgcy27ml" +
      "mlMoZIIgDll6e3vCYLocInmYWAmS6TlzAC8wEqKK6PBru3jl7A_yl95bQpu6cVPT" +
      "pK4Mqgkf1CXztLVBSt2Ks3oZwbuwXPXLWyouBWLVWGNWQexSgSxsj_Qulcy4a-fN"

  private def tempDir: UIO[Path] = ZIO.succeed(Files.createTempDirectory("muliao-vapid"))

  private def mode(p: Path): String = PosixFilePermissions.toString(Files.getPosixFilePermissions(p))

  def spec = suite("Web Push 加密与签名")(
    test("RFC 8291 的例子：同样的密钥与盐得到逐字节相同的请求体") {
      val device = DeviceKeys.parse(uaPublic, authSecret).toOption.get
      val priv   = privateKey(b64(asPrivate))
      val pub    = P256.publicOf(priv)
      val body   = Ece.encrypt(device, KeyPair(pub, priv), b64(salt), "When I grow up, I want to be a watermelon".getBytes(StandardCharsets.UTF_8))
      assertTrue(B64.encode(P256.encode(pub)) == asPublic, B64.encode(body) == expected)
    },
    test("由私钥算出的公钥与 JDK 生成的一致") {
      val pairs = List.fill(5)(P256.generate)
      assertTrue(pairs.forall(kp => P256.encode(P256.publicOf(kp.getPrivate.asInstanceOf[ECPrivateKey])).sameElements(P256.encode(kp.getPublic.asInstanceOf[ECPublicKey]))))
    },
    test("VAPID：ES256 签名的 JWT，受众是推送服务的源，能用 k 里的公钥验证") {
      for
        dir  <- tempDir
        keys <- VapidKeys.loadOrCreate(dir.resolve("k.pem"))
        now   = Instant.parse("2026-11-15T00:00:00Z")
        h     = keys.authorization(URI.create("https://web.push.apple.com/abc/def?x=1"), "mailto:me@example.com", now)
      yield
        val Array(token, k) = h.stripPrefix("vapid t=").split(", k=")
        val Array(head, claims, sig) = token.split('.')
        val c = String(b64(claims), StandardCharsets.UTF_8).fromJson[Json.Obj].toOption.get
        val v = Signature.getInstance("SHA256withECDSAinP1363Format")
        v.initVerify(P256.decode(b64(k)).toOption.get)
        v.update(s"$head.$claims".getBytes(StandardCharsets.US_ASCII))
        assertTrue(
          h.startsWith("vapid t="),
          k == keys.publicKeyB64,
          c.get("aud").contains(Json.Str("https://web.push.apple.com")),
          c.get("sub").contains(Json.Str("mailto:me@example.com")),
          c.get("exp").contains(Json.Num(now.getEpochSecond + 12 * 3600)),
          v.verify(b64(sig))
        )
    },
    test("私钥只生成一次（权限 600），之后读回同一把；被放宽的权限收回") {
      for
        dir   <- tempDir
        path   = dir.resolve("sub/vapid_private.pem")
        first <- VapidKeys.loadOrCreate(path)
        m1     = mode(path)
        text   = Files.readString(path)
        _     <- ZIO.succeed(Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rw-r--r--")))
        again <- VapidKeys.loadOrCreate(path)
      yield assertTrue(
        m1 == "rw-------",
        text.startsWith("-----BEGIN PRIVATE KEY-----"),
        first.publicKeyB64 == again.publicKeyB64,
        mode(path) == "rw-------",
        b64(first.publicKeyB64).length == 65
      )
    },
    test("读得懂 openssl 生成的 PKCS#8 私钥（与现有密钥文件同一格式），公钥与 openssl 算的一致") {
      for
        dir  <- tempDir
        path  = dir.resolve("k.pem")
        _    <- ZIO.succeed(Files.write(path, getClass.getResourceAsStream("/vapid_openssl.pem").readAllBytes()))
        keys <- VapidKeys.loadOrCreate(path)
      yield assertTrue(keys.publicKeyB64 == "BDks_ZbeioVHmmENr7AXtriyebUxVjbSk7QirYNnU7ms0sRpG18cW-7wDaBnwFFKJeVmNS0oWzfT1AXSLdhmbHM")
    },
    test("坏的私钥文件报错，不被覆盖") {
      for
        dir  <- tempDir
        path  = dir.resolve("k.pem")
        _    <- ZIO.succeed(Files.writeString(path, "-----BEGIN PRIVATE KEY-----\nAAAA\n-----END PRIVATE KEY-----\n"))
        r    <- VapidKeys.loadOrCreate(path).either
      yield assertTrue(r.isLeft, Files.readString(path).contains("AAAA"))
    },
    test("订阅校验：https 地址、曲线上的公钥、16 字节 auth") {
      val ok = Device.parse("https://fcm.googleapis.com/fcm/send/x", uaPublic, authSecret)
      assertTrue(
        ok.map(_.host) == Right("fcm.googleapis.com"),
        Device.parse("http://x/y", uaPublic, authSecret).isLeft,
        Device.parse("not a url", uaPublic, authSecret).isLeft,
        Device.parse("https://x/y", uaPublic, "AAAA").left.exists(_.contains("16 bytes")),
        Device.parse("https://x/y", B64.encode(Array.fill[Byte](65)(4)), authSecret).left.exists(_.contains("曲线"))
      )
    },
    test("推送服务的回应归类：404/410 失效、408/429/5xx 暂时、其余 4xx 拒收") {
      assertTrue(
        PushFailure.ofStatus(201, None, "") == None,
        PushFailure.ofStatus(410, None, "") == Some(PushFailure.Gone(410)),
        PushFailure.ofStatus(404, None, "") == Some(PushFailure.Gone(404)),
        PushFailure.ofStatus(429, Some(5.seconds), "").contains(PushFailure.Transient("推送服务回 HTTP 429", Some(5.seconds))),
        PushFailure.ofStatus(503, None, "").exists(_.isInstanceOf[PushFailure.Transient]),
        PushFailure.ofStatus(403, None, " bad jwt ").contains(PushFailure.Rejected("推送服务拒收，HTTP 403：bad jwt"))
      )
    }
  )
