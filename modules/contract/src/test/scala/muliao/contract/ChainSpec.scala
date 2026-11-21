package muliao.contract

import muliao.contract.chain.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** 链上编码（#1092）：与 ethers、@solana/web3.js 生成的向量逐字节比对。JVM 与浏览器（Scala.js）各跑一遍。 */
object ChainSpec extends ZIOSpecDefault:
  private val v: Json.Obj = ChainVectors.json.fromJson[Json.Obj].toOption.get

  private def at(path: String*): Json = path.foldLeft[Json](v)((j, k) => j.asInstanceOf[Json.Obj].get(k).get)
  private def s(path: String*): String = at(path*) match
    case Json.Str(x) => x
    case other       => other.toString
  private def hex(path: String*): Array[Byte] = Hex.decode(s(path*)).toOption.get
  private def same(a: Array[Byte], b: Array[Byte]): Boolean = java.util.Arrays.equals(a, b)

  private def q(x: String) = Quantity.parse(x).toOption.get
  private def addr(x: String) = EthAddress.parse(x).toOption.get
  private def data(x: String) = HexData.parse(x).toOption.get

  private val to35 = Some(addr("0x3535353535353535353535353535353535353535"))
  private val txs: List[(String, EthTx)] = List(
    "eip1559" -> EthTx.Eip1559(q("1"), q("7"), q("1500000000"), q("30000000000"), q("21000"), to35, q("1000000000000000000"), HexData.Empty, Nil),
    "eip1559b" -> EthTx.Eip1559(
      q("8453"), q("0"), q("1"), q("2000000"), q("100000"), Some(addr("0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48")), q("0"),
      data("0xa9059cbb0000000000000000000000003535353535353535353535353535353535353535000000000000000000000000000000000000000000000000000000000000000a"),
      List(AccessEntry(addr("0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"), List(data("0x0000000000000000000000000000000000000000000000000000000000000001"))))
    ),
    "eip1559c" -> EthTx.Eip1559(q("11155111"), q("300"), q("0"), q("1"), q("53000"), None, q("0"), data("0x6000"), Nil),
    "legacy"   -> EthTx.Legacy(q("1"), q("9"), q("20000000000"), q("21000"), to35, q("1000000000000000000"), HexData.Empty)
  )

  def spec = suite("链上编码")(
    test("keccak256：各长度（含块边界）") {
      val ks = at("keccak").asInstanceOf[Json.Obj].fields.toList
      assertTrue(ks.size == 6, ks.forall((n, h) => Hex.prefixed(Keccak256(Array.fill(n.toInt)('a'.toByte))) == h.asInstanceOf[Json.Str].value))
    },
    test("地址：EIP-55 写法，大小写校验错的拒绝") {
      val a = addr("0xfb6916095ca1df60bb79ce92ce3ea74c37c5d359")
      assertTrue(
        a.checksummed == s("checksum"),
        EthAddress.parse(s("checksum")) == Right(a),
        EthAddress.parse("0xFB6916095CA1DF60BB79CE92CE3EA74C37C5D359") == Right(a),
        EthAddress.parse("0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d35A").isLeft,
        EthAddress.parse("0x1234").isLeft
      )
    },
    test("数值：十进制与 0x，负数、越界、空的拒绝") {
      assertTrue(
        q("0x10").value == 16, q("255").value == 255,
        Quantity.parse("-1").isLeft, Quantity.parse("").isLeft, Quantity.parse("0x").isLeft, Quantity.parse("1.5").isLeft,
        Quantity.parse(Quantity.Max.toString).isRight, Quantity.parse((Quantity.Max + 1).toString).isLeft
      )
    },
    test("以太坊交易：待签摘要与带签名的线上字节同 ethers") {
      val checks = txs.map { (name, tx) =>
        val sig = EcdsaSignature(BigInt(s("sigs", name, "r").drop(2), 16), BigInt(s("sigs", name, "s").drop(2), 16), s("sigs", name, "yParity").toInt)
        val raw = tx.signed(sig)
        (name, Hex.prefixed(tx.signingHash) == s(name, "unsignedHash"), Hex.prefixed(raw) == s(name, "raw"), Hex.prefixed(Keccak256(raw)) == s(name, "hash"))
      }
      assertTrue(checks.forall((_, a, b, c) => a && b && c))
    },
    test("交易 JSON 往返：数值写十进制字符串，读回相等") {
      val back = txs.map(_._2).map(t => t.toJson.fromJson[EthTx])
      assertTrue(back == txs.map(t => Right(t._2)), """{"type":"Legacy","chainId":"1","nonce":"-1","gasPrice":"1","gas":"1","value":"0","data":"0x"}""".fromJson[EthTx].isLeft)
    },
    test("EIP-191：文本与十六进制的摘要同 ethers") {
      assertTrue(
        Hex.prefixed(Eth.personalHash(MessageBytes.Text(s("personalText", "message")).bytes)) == s("personalText", "hash"),
        Hex.prefixed(Eth.personalHash(MessageBytes.Bytes(data(s("personalHex", "message"))).bytes)) == s("personalHex", "hash")
      )
    },
    test("EIP-712：规范例子、Safe 交易、数组与嵌套结构的摘要同 ethers") {
      val names = List("mail", "safe", "complex")
      val got   = names.map(n => TypedData.parse(s(n, "typedData")).map(d => Hex.prefixed(d.digest)))
      assertTrue(got == names.map(n => Right(s(n, "digest"))))
    },
    test("EIP-712：没声明 EIP712Domain 时按 domain 里有的字段推出") {
      val j    = s("mail", "typedData").fromJson[Json.Obj].toOption.get
      val types = j.get("types").get.asInstanceOf[Json.Obj]
      val drop = Json.Obj(j.fields.map((k, x) => if k == "types" then k -> Json.Obj(types.fields.filterNot(_._1 == "EIP712Domain")) else k -> x))
      assertTrue(TypedData.fromJson(drop).map(d => Hex.prefixed(d.digest)) == Right(s("mail", "digest")))
    },
    test("EIP-712：缺字段、多字段、越界、类型不认得的读不进来") {
      def mutate(f: String => String) = TypedData.parse(f(s("mail", "typedData")))
      assertTrue(
        mutate(_.replace("\"contents\":\"Hello, Bob!\"", "\"contents\":\"Hello, Bob!\",\"extra\":1")).left.exists(_.contains("extra")),
        mutate(_.replace(",\"contents\":\"Hello, Bob!\"", "")).left.exists(_.contains("contents")),
        mutate(_.replace("\"type\":\"string\"}]", "\"type\":\"strin\"}]")).isLeft,
        mutate(_.replace("\"chainId\":1", "\"chainId\":-1")).isLeft,
        mutate(_.replace("\"contents\":\"Hello, Bob!\"", "\"contents\":\"Hello, Bob!\",\"contents\":\"x\"")).left.exists(_.contains("重复")),
        TypedData.parse("""{"types":{"A":[{"name":"x","type":"uint8"}]},"primaryType":"A","domain":{},"message":{"x":256}}""").isLeft,
        TypedData.parse("""{"types":{"A":[{"name":"x","type":"int8"}]},"primaryType":"A","domain":{},"message":{"x":"-129"}}""").isLeft,
        TypedData.parse("""{"types":{"A":[{"name":"x","type":"bytes4"}]},"primaryType":"A","domain":{},"message":{"x":"0x0102"}}""").isLeft
      )
    },
    test("Solana：读出 legacy、v0、多签名的交易，填进签名同 web3.js") {
      val cases = List("solanaLegacy", "solanaV0", "solanaMulti")
      val pub   = Base58.decode(s("solana", "publicKey")).toOption.get
      val got = cases.map { n =>
        for
          tx  <- Base64.decode(s(n, "unsigned")).flatMap(SolanaTx.parse)
          i   <- tx.signerIndex(pub)
          sig <- Base58.decode(s(n, "signature"))
        yield Base64.encode(tx.withSignature(i, sig))
      }
      assertTrue(got == cases.map(n => Right(s(n, "signed"))))
    },
    test("Solana：不需要这把钥匙签的报错，截断的读不进来，概要认得转账") {
      val tx     = Base64.decode(s("solanaLegacy", "unsigned")).flatMap(SolanaTx.parse).toOption.get
      val wire   = Base64.decode(s("solanaV0", "unsigned")).toOption.get
      val rows   = Signing.rows(SignContent.SolanaTransaction(tx)).map(_._2).mkString("\n")
      val v0rows = Signing.rows(SignContent.SolanaTransaction(SolanaTx.parse(wire).toOption.get)).map(_._2).mkString("\n")
      assertTrue(
        tx.signerIndex(Array.fill(32)(1)).isLeft,
        SolanaTx.parse(wire.dropRight(1)).isLeft,
        SolanaTx.parse(wire :+ 0.toByte).isLeft,
        rows.contains("System 转账 1.23456789 SOL（1234567890 lamports）"),
        rows.contains("ComputeBudget 设优先费 1000"),
        v0rows.contains("查找表")
      )
    },
    test("Solana 消息：是一笔交易的消息、一张 SSH 证书的待签字节的拒绝") {
      val tx  = Base64.decode(s("solanaLegacy", "unsigned")).flatMap(SolanaTx.parse).toOption.get
      val pub = SshCert.PublicKey(Array.fill(32)(1))
      val tbs = SshCert.Unsigned(Array.fill(32)(0), pub, 1, "x", List("root@a"), SshCert.Validity(0, Long.MaxValue), None, pub).toBeSigned
      assertTrue(
        SolanaOffchain.of(MessageBytes.Bytes(HexData.of(tx.message))).isLeft,
        SolanaOffchain.of(MessageBytes.Bytes(HexData.of(tbs))).left.exists(_.contains("ssh_certificate")),
        SolanaOffchain.of(MessageBytes.Text(s("solanaMessage", "message"))).isRight
      )
    },
    test("显示：ERC20 transfer/approve 解出收款人与金额，无限授权写「无限」") {
      val rows = Signing.rows(SignContent.EthTransaction(txs(1)._2)).toMap
      val approve = TokenCalls.describe(Hex.decode("095ea7b3" + "00" * 12 + "35" * 20 + "ff" * 32).toOption.get)
      assertTrue(
        rows("金额").contains("transfer：给 0x3535353535353535353535353535353535353535 10（最小单位）"),
        approve.exists(_.contains("动用 无限"))
      )
    },
    test("显示：SPL Token TransferChecked、Approve 与 durable nonce 解出含义") {
      val keys = Vector.tabulate(5)(i => Array.fill[Byte](32)((i + 1).toByte)) ++
        Vector("TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA", SolanaPrograms.System).map(Base58.decode(_).toOption.get)
      val le8 = (n: Long) => Array.tabulate(8)(i => (n >>> (8 * i)).toByte)
      val ins = List(
        SolanaTx.Instruction(5, List(1, 2, 3, 0), Array[Byte](12) ++ le8(1500000) ++ Array[Byte](6)),
        SolanaTx.Instruction(5, List(1, 4, 0), Array[Byte](4) ++ le8(7)),
        SolanaTx.Instruction(6, List(2, 3, 0), Array[Byte](4, 0, 0, 0))
      )
      val m    = SolanaTx.Message(None, 1, 0, 2, keys, Array.fill(32)(9), ins, Nil)
      val text = ins.map(SolanaPrograms.describe(m, _))
      assertTrue(
        text(0).startsWith(s"代币转账 1.5（mint ${Base58.encode(keys(2))}）"),
        text(0).contains(s"到 ${Base58.encode(keys(3))}"),
        text(1).startsWith(s"代币授权：允许 ${Base58.encode(keys(4))} 动用 7"),
        text(2).contains("durable nonce")
      )
    },
    test("base58：前导零与往返") {
      val b = Array[Byte](0, 0, 1, 2, -1)
      assertTrue(same(Base58.decode(Base58.encode(b)).toOption.get, b), Base58.encode(Array[Byte](0, 0)) == "11", Base58.decode("0OIl").isLeft)
    },
    test("签名内容 JSON 往返：各分支读回相等") {
      val td = TypedData.parse(s("safe", "typedData")).toOption.get
      val tx = Base64.decode(s("solanaV0", "unsigned")).flatMap(SolanaTx.parse).toOption.get
      val all = List[SignContent](
        SignContent.EthTransaction(txs.head._2),
        SignContent.EthMessage(MessageBytes.Text("hi")),
        SignContent.EthTypedData(td),
        SignContent.SolanaTransaction(tx),
        SignContent.SolanaMessage(SolanaOffchain.of(MessageBytes.Bytes(data("0x0102"))).toOption.get)
      )
      val back = all.map(c => c.toJson.fromJson[SignContent].map(_.toJson))
      assertTrue(back == all.map(c => Right(c.toJson)), all.map(_.algorithm.label).distinct == List("secp256k1", "ed25519"))
    }
  )
