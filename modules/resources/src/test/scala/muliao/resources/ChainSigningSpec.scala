package muliao.resources

import muliao.contract.*
import muliao.contract.chain.*
import muliao.resources.tree.Owner
import muliao.resources.vault.*
import muliao.toolkit.{ToolContent, ToolOutcome}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** 链上签名（#1092）：服务端（Bouncy Castle）签出的与 ethers、@solana/web3.js、tweetnacl 的逐字节相同；sign 工具的参数校验、两条签名路径、算法不符。 */
object ChainSigningSpec extends ZIOSpecDefault:
  private val v: Json.Obj = ChainVectors.json.fromJson[Json.Obj].toOption.get

  private def s(path: String*): String = path.foldLeft[Json](v)((j, k) => j.asInstanceOf[Json.Obj].get(k).get) match
    case Json.Str(x) => x
    case other       => other.toString

  private val ethKey = s("ethKey")
  private val solKey = s("solana", "secretKeyJson")

  private def signed(c: SignContent, key: String): Json.Obj = Signing.sign(c, key, JvmCrypto).flatMap(_.fromJson[Json.Obj]).toOption.get
  private def field(o: Json.Obj, k: String): String = o.get(k).collect { case Json.Str(x) => x }.getOrElse("")

  private def tool = PasskeyVaultSpec.toolOf("sign")
  private def text(o: ToolOutcome[?]): String = o match
    case ToolOutcome.Completed(cs) => cs.collect { case ToolContent.Text(t) => t }.mkString
    case other                     => other.toString

  private def call(content: Json, key: String) = Json.Obj("key" -> Json.Str(key), "content" -> content)

  private val ethTxArgs = Json.Obj(
    "kind"                     -> Json.Str("eth_transaction"),
    "chain_id"                 -> Json.Str("1"),
    "nonce"                    -> Json.Str("7"),
    "to"                       -> Json.Str("0x3535353535353535353535353535353535353535"),
    "value"                    -> Json.Str("1000000000000000000"),
    "gas"                      -> Json.Str("21000"),
    "max_fee_per_gas"          -> Json.Str("30000000000"),
    "max_priority_fee_per_gas" -> Json.Str("0x59682f00")
  )

  def spec = suite("链上签名")(
    test("以太坊：交易、EIP-191、EIP-712 在服务端签出的同 ethers") {
      val q   = (x: String) => Quantity.parse(x).toOption.get
      val to  = EthAddress.parse("0x3535353535353535353535353535353535353535").toOption
      val tx1 = EthTx.Eip1559(q("1"), q("7"), q("1500000000"), q("30000000000"), q("21000"), to, q("1000000000000000000"), HexData.Empty, Nil)
      val tx0 = EthTx.Legacy(q("1"), q("9"), q("20000000000"), q("21000"), to, q("1000000000000000000"), HexData.Empty)
      val r1  = signed(SignContent.EthTransaction(tx1), ethKey)
      val r0  = signed(SignContent.EthTransaction(tx0), " 0x" + ethKey.drop(2).toUpperCase + "\n")
      val sigOf = (c: SignContent) => field(signed(c, ethKey), "signature")
      assertTrue(
        field(r1, "raw_transaction") == s("eip1559", "raw"),
        field(r1, "transaction_hash") == s("eip1559", "hash"),
        field(r1, "from") == s("ethAddress"),
        field(r0, "raw_transaction") == s("legacy", "raw"),
        sigOf(SignContent.EthMessage(MessageBytes.Text(s("personalText", "message")))) == s("personalText", "sig"),
        sigOf(SignContent.EthMessage(MessageBytes.Bytes(HexData.parse(s("personalHex", "message")).toOption.get))) == s("personalHex", "sig"),
        List("mail", "safe", "complex").forall(n => sigOf(SignContent.EthTypedData(TypedData.parse(s(n, "typedData")).toOption.get)) == s(n, "sig"))
      )
    },
    test("Solana：交易（legacy、v0、多签名）与消息在服务端签出的同 web3.js、tweetnacl；JSON 数组与 base58 私钥都认") {
      val txOf = (n: String) => SignContent.SolanaTransaction(Base64.decode(s(n, "unsigned")).flatMap(SolanaTx.parse).toOption.get)
      val msg  = SignContent.SolanaMessage(SolanaOffchain.of(MessageBytes.Text(s("solanaMessage", "message"))).toOption.get)
      assertTrue(
        List("solanaLegacy", "solanaV0", "solanaMulti").forall(n =>
          val r = signed(txOf(n), solKey)
          field(r, "transaction") == s(n, "signed") && field(r, "signature") == s(n, "signature") && field(r, "public_key") == s("solana", "publicKey")
        ),
        field(signed(msg, s("solana", "secretKeyB58")), "signature") == s("solanaMessage", "signature"),
        SigningKeys.identity(SigningAlgorithm.Ed25519, solKey, JvmCrypto).exists(_.contains(s("solana", "publicKey"))),
        SigningKeys.identity(SigningAlgorithm.Secp256k1, ethKey, JvmCrypto) == Right(s"以太坊地址 ${s("ethAddress")}"),
        // 公钥部分被改过的 Solana 私钥、超出范围的以太坊私钥都不认
        SigningKeys.ed25519(solKey.replaceFirst("121,", "122,"), JvmCrypto).isLeft,
        SigningKeys.secp256k1("0x" + "f" * 64, JvmCrypto).isLeft,
        SigningKeys.secp256k1("0x" + "0" * 64, JvmCrypto).isLeft
      )
    },
    test("sign 工具：参数 schema 是按 kind 分支的 oneOf；服务端保管的直接签；passkey 的等用户签好交回；参数不对点名字段、不发请求；算法不符报错") {
      for
        a     <- ZIO.serviceWithZIO[agents.Agents](_.create(agents.NewAgent("alice", "x")))
        vault <- ZIO.service[Vault]
        signOnly = (alg: SigningAlgorithm) => SecretContent.KeyPair(alg, KeyExposure.SignOnly)
        mine     = Owner.Agent(a.name)
        _     <- vault.set("ETH_KEY", SecretInput.ByUser(SecretValue.Server(ethKey, signOnly(SigningAlgorithm.Secp256k1)), mine), Some("eth"))
        _     <- vault.set("SOL_KEY", SecretInput.ByUser(SecretValue.Server(solKey, signOnly(SigningAlgorithm.Ed25519)), mine), Some("sol"))
        _     <- vault.set("ETH_PK", SecretInput.ByUser(SecretValue.Passkey(PasskeyVaultSpec.box, signOnly(SigningAlgorithm.Secp256k1)), mine), Some("eth"))
        badKey <- vault.set("BAD", SecretInput.ByUser(SecretValue.Server("0x1234", signOnly(SigningAlgorithm.Secp256k1)), mine), None).either
        t      <- tool
        ctx     = PasskeyVaultSpec.Ctx(a.id)
        server <- t.invoke(call(ethTxArgs, "ETH_KEY"), ctx)
        noTo   <- t.invoke(call(Json.Obj(ethTxArgs.fields.filterNot(_._1 == "to")), "ETH_KEY"), ctx).either
        badTo  <- t.invoke(call(Json.Obj(ethTxArgs.fields.map((k, x) => if k == "to" then k -> Json.Str("0x12") else k -> x)), "ETH_KEY"), ctx).either
        wrong  <- t.invoke(call(ethTxArgs, "SOL_KEY"), ctx).either
        txMsg  <- t.invoke(call(Json.Obj("kind" -> Json.Str("solana_message"), "message" -> Json.Str(Hex.prefixed(Base64.decode(s("solanaLegacy", "unsigned")).flatMap(SolanaTx.parse).toOption.get.message)), "encoding" -> Json.Str("hex")), "SOL_KEY"), ctx).either
        pending <- ZIO.serviceWithZIO[Grants](_.view.map(_._1))
        f      <- t.invoke(call(Json.Obj("kind" -> Json.Str("eth_message"), "message" -> Json.Str("hi")), "ETH_PK"), ctx).fork
        w      <- PasskeyVaultSpec.asks(1)
        _      <- PasskeyVaultSpec.answer(w.head.id, AskDecision.Signed("{\"signature\":\"0xabc\"}"))
        viaPk  <- f.join
      yield
        val schema = t.spec.inputSchema.toJson
        assertTrue(
          schema.contains("\"oneOf\""),
          List("eth_transaction", "eth_legacy_transaction", "eth_message", "eth_typed_data", "solana_transaction", "solana_message").forall(k => schema.contains(s"\"const\":\"$k\"")),
          text(server).contains(s("eip1559", "raw")),
          PasskeyVaultSpec.reason(badKey).contains("不是认得的私钥"),
          noTo.left.toOption.exists(_.toString.contains("没填收款地址")),
          badTo.left.toOption.exists(_.toString.contains("to：")),
          wrong.left.toOption.exists(_.toString.contains("不是 secp256k1 密钥对")),
          txMsg.left.toOption.exists(_.toString.contains("solana_transaction")),
          pending.isEmpty,
          w.head.want == AskWant.Sign(SignContent.EthMessage(MessageBytes.Text("hi"))),
          w.head.purpose.what == "签以太坊消息（EIP-191）",
          text(viaPk).contains("0xabc")
        )
    }.provideSomeLayer[Scope](TestEnv.layer)
  ) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
