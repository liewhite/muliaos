package muliao.resources.tools

import muliao.contract.{AskPurpose, Labeled, Labels, MessageBytes, SignContent, Signing, SolanaOffchain}
import muliao.contract.chain.*
import muliao.platform.AppError
import muliao.resources.vault.Vault
import muliao.toolkit.*
import zio.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{caseName, description, discriminatorName, fieldName}

/** 工具 sign（#1092）：用密钥对类 secret 签以太坊交易、EIP-191、EIP-712，Solana 交易与消息。agent 拿不到私钥，只拿到签好的结果；
  * passkey 加密的由用户在浏览器里看着解出的内容签，服务端保管的在服务端直接签。参数按内容类型分开，读进来先校验成 [[SignContent]]，不对的不发请求。
  */
object SignTool:

  /** 消息的写法：文本按 UTF-8，或十六进制字节。 */
  enum MessageEncoding(val label: String) extends Labeled:
    case Utf8 extends MessageEncoding("utf8")
    case Hex  extends MessageEncoding("hex")

  object MessageEncoding extends Labels[MessageEncoding]("消息写法", MessageEncoding.values)

  final case class AccessItem(address: String, @fieldName("storage_keys") storageKeys: List[String] = Nil)
  given Schema[AccessItem] = DeriveSchema.gen[AccessItem]

  private final val Num = "十进制或 0x 十六进制字符串"

  @discriminatorName("kind")
  enum Content:
    @caseName("eth_transaction") @description("以太坊 EIP-1559（type 2）交易")
    case EthTransaction(
        @fieldName("chain_id") @description(s"链编号（$Num）") chainId: String,
        @description(Num) nonce: String,
        @description("收款或合约地址；建合约不填") to: Option[String] = None,
        @description(s"金额，单位 wei（$Num）") value: String = "0",
        @description("调用数据，0x 十六进制") data: String = "0x",
        @description(s"gas 上限（$Num）") gas: String,
        @fieldName("max_fee_per_gas") @description(s"每单位 gas 最高费用，wei（$Num）") maxFeePerGas: String,
        @fieldName("max_priority_fee_per_gas") @description(s"每单位 gas 小费，wei（$Num）") maxPriorityFeePerGas: String,
        @fieldName("access_list") @description("EIP-2930 访问列表") accessList: List[AccessItem] = Nil
    )
    @caseName("eth_legacy_transaction") @description("以太坊 legacy 交易（EIP-155，签名带链编号）：只有不支持 EIP-1559 的链才用")
    case EthLegacyTransaction(
        @fieldName("chain_id") @description(s"链编号（$Num）") chainId: String,
        @description(Num) nonce: String,
        @description("收款或合约地址；建合约不填") to: Option[String] = None,
        @description(s"金额，单位 wei（$Num）") value: String = "0",
        @description("调用数据，0x 十六进制") data: String = "0x",
        @description(s"gas 上限（$Num）") gas: String,
        @fieldName("gas_price") @description(s"gas 价格，wei（$Num）") gasPrice: String
    )
    @caseName("eth_message") @description("EIP-191 personal_sign 消息")
    case EthMessage(@description("消息") message: String, @description("message 的写法") encoding: MessageEncoding = MessageEncoding.Utf8)
    @caseName("eth_typed_data") @description("EIP-712 结构化数据（eth_signTypedData_v4）")
    case EthTypedData(@fieldName("typed_data") @description("完整的 JSON 文本：types、primaryType、domain、message") typedData: String)
    @caseName("solana_transaction") @description("Solana 交易（legacy 或 v0）")
    case SolanaTransaction(
        @description("序列化的交易，base64（如 web3.js 的 `tx.serialize({requireAllSignatures:false})`），签名位可空可已有别人的签名") transaction: String
    )
    @caseName("solana_message") @description("Solana 链下消息（钱包的 signMessage）；不能是交易的消息")
    case SolanaMessage(@description("消息") message: String, @description("message 的写法") encoding: MessageEncoding = MessageEncoding.Utf8)

  given Schema[Content] = DeriveSchema.gen[Content]

  @description("用密钥对签名")
  final case class Sign(
      @description("密钥对类 secret 名：以太坊的要 secp256k1 密钥，Solana 的要 ed25519 密钥") key: String,
      @description("要签什么：`kind` 定类型，其余参数随类型") content: Content
  )
  given Schema[Sign] = DeriveSchema.gen[Sign]

  val SignDef = ToolDef[ToolContext, Sign](
    "sign",
    "用密钥对类 secret 签名，你拿到签好的结果、拿不到私钥：以太坊交易（返回 raw_transaction，自己广播）、EIP-191 消息、EIP-712 结构化数据，Solana 交易（返回填好签名的交易，base64）与消息。只签不广播。secret 是 passkey 加密的等用户在界面上看着解出的内容批准（每次都要批，期间这个调用一直等着），服务端保管的直接签。签交易前自己查好 nonce、gas 和费率。",
    Access.Write
  )

  /** 参数读成签名内容：哪个字段不对就点名哪个。 */
  def content(c: Content): Either[String, SignContent] =
    def field[A](name: String, r: Either[String, A]) = r.left.map(e => s"$name：$e")
    def num(name: String, s: String)                 = field(name, Quantity.parse(s))
    def to(s: Option[String])                        = s.map(_.trim).filter(_.nonEmpty).fold(Right(None))(a => field("to", EthAddress.parse(a)).map(Some(_)))
    def data(s: String)                              = field("data", HexData.parse(if s.trim.isEmpty then "0x" else s))
    // 不带 to 是建合约；没有初始化代码的建合约只会是漏填了收款地址，签出来金额会进一个取不出的空合约
    def target(t: Option[String], d: HexData) =
      to(t).filterOrElse(dest => dest.isDefined || d.length > 0, "to：没填收款地址且没有 data（建合约要带初始化代码）")
    def message(m: String, e: MessageEncoding) = e match
      case MessageEncoding.Utf8 => Right(MessageBytes.Text(m))
      case MessageEncoding.Hex  => field("message", HexData.parse(m)).map(MessageBytes.Bytes(_))
    c match
      case t: Content.EthTransaction =>
        for
          chain <- num("chain_id", t.chainId)
          nonce <- num("nonce", t.nonce)
          prio  <- num("max_priority_fee_per_gas", t.maxPriorityFeePerGas)
          max   <- num("max_fee_per_gas", t.maxFeePerGas)
          gas   <- num("gas", t.gas)
          value <- num("value", t.value)
          d     <- data(t.data)
          dest  <- target(t.to, d)
          acl <- Eithers.traverse(t.accessList)(e =>
                   for
                     a  <- field("access_list.address", EthAddress.parse(e.address))
                     ks <- Eithers.traverse(e.storageKeys)(k => field("access_list.storage_keys", HexData.parse(k).filterOrElse(_.length == 32, "要是 32 字节")))
                   yield AccessEntry(a, ks)
                 )
        yield SignContent.EthTransaction(EthTx.Eip1559(chain, nonce, prio, max, gas, dest, value, d, acl))
      case t: Content.EthLegacyTransaction =>
        for
          chain <- num("chain_id", t.chainId)
          nonce <- num("nonce", t.nonce)
          price <- num("gas_price", t.gasPrice)
          gas   <- num("gas", t.gas)
          value <- num("value", t.value)
          d     <- data(t.data)
          dest  <- target(t.to, d)
        yield SignContent.EthTransaction(EthTx.Legacy(chain, nonce, price, gas, dest, value, d))
      case Content.EthMessage(m, e)       => message(m, e).map(SignContent.EthMessage(_))
      case Content.EthTypedData(text)     => field("typed_data", TypedData.parse(text)).map(SignContent.EthTypedData(_))
      case Content.SolanaTransaction(b64) => field("transaction", Base64.decode(b64).flatMap(SolanaTx.parse)).map(SignContent.SolanaTransaction(_))
      case Content.SolanaMessage(m, e)    => message(m, e).flatMap(SolanaOffchain.of).map(SignContent.SolanaMessage(_))

  def tool(vault: Vault): Tool[ToolContext, Nothing] =
    SignDef { (in, ctx) =>
      (for
        c   <- ZIO.fromEither(content(in.content)).mapError(AppError.invalid)
        out <- vault.sign(ctx.agent, in.key, c, AskPurpose(Some(ctx.session), Signing.title(c)))
      yield ToolOutcome.text(s"已签好（只签名，没有广播）：\n$out")).mapError(ToolFailure.fromAppError)
    }
