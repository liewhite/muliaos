package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- passkey secret 的授权（#915） ----

/** passkey 加密的 secret 的密文：只有浏览器用那把 passkey（WebAuthn PRF 扩展）导出的密钥能解开，服务端只存不解。
  * `rp` 是绑定的域名，`credential` 是 passkey 的编号（base64url），`iv`、`ciphertext` 是 AES-GCM 的（base64，附加数据是 secret 名）。
  */
final case class PasskeyBox(rp: String, credential: String, iv: String, ciphertext: String) derives JsonCodec, Schema:
  override def toString: String = s"PasskeyBox($rp)"

/** 密钥对的算法（#938）。 */
enum SigningAlgorithm(val label: String) extends Labeled:
  case Ed25519 extends SigningAlgorithm("ed25519")

object SigningAlgorithm extends Labels[SigningAlgorithm]("签名算法", SigningAlgorithm.values)

// ---- secret 的三个维度（#1069）：谁能解开（服务端主密钥或用户的 passkey，见 [[SecretValue]]）× 内容 × 能否取出明文（[[SecretContent]]） ----

/** 解开后的明文能交给谁：任意命令（run_shell、触发器检查与常驻命令），或只给 skill 脚本（skill_run）和 MCP 配置。 */
enum RevealScope(val label: String) extends Labeled:
  case Shell   extends RevealScope("shell")
  case Scripts extends RevealScope("scripts")

object RevealScope extends Labels[RevealScope]("明文范围", RevealScope.values)

/** 密钥对能否取出明文：能（交给哪些地方），或只能签名（私钥只在解开它的一方用：服务端或用户浏览器）。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum KeyExposure derives JsonCodec:
  case Reveal(scope: RevealScope)
  case SignOnly

object KeyExposure:
  given Schema[KeyExposure] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

/** secret 的内容与能否取出明文：口令（总能取，只分范围），或密钥对（还可以只能签名）。「口令 + 只能签名」写不出来。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum SecretContent derives JsonCodec:
  case Opaque(scope: RevealScope)
  case KeyPair(algorithm: SigningAlgorithm, exposure: KeyExposure)

  /** 明文能交给谁；只能签名的没有。 */
  def revealScope: Option[RevealScope] = this match
    case Opaque(s)                         => Some(s)
    case KeyPair(_, KeyExposure.Reveal(s)) => Some(s)
    case KeyPair(_, KeyExposure.SignOnly)  => None

  /** 只改明文范围（内容类型与「只能签名」不变）；只能签名的没有范围可改。 */
  def withScope(s: RevealScope): Either[String, SecretContent] = this match
    case Opaque(_)                        => Right(Opaque(s))
    case KeyPair(a, KeyExposure.Reveal(_)) => Right(KeyPair(a, KeyExposure.Reveal(s)))
    case KeyPair(_, KeyExposure.SignOnly) => Left("只能签名的 secret 取不出明文，没有明文范围可改；要改成能取明文须重新设值")

  /** passkey 加密时 AES-GCM 的附加数据，见 [[PasskeyAad]]。 */
  def aad(key: String): String = this match
    case KeyPair(a, KeyExposure.SignOnly) => PasskeyAad.signing(key, a)
    case _                                => PasskeyAad.plain(key)

object SecretContent:
  /** 没指定时（agent 新建、只给值）：任意命令都能用的口令。 */
  val Default: SecretContent = Opaque(RevealScope.Shell)

  given Schema[SecretContent] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

/** passkey 加密时 AES-GCM 的附加数据：能取明文的是 secret 名；只能签名的带上「签名、算法」，于是取明文的那条路（按 secret 名解）解不开它，
  * 服务端被攻破、把只能签名的钥匙冒充成普通 secret 发来取明文，浏览器也解不开。
  */
object PasskeyAad:
  def plain(key: String): String                           = key
  def signing(key: String, alg: SigningAlgorithm): String = s"signing:${alg.label}:$key"

/** secret 的新值，也就定了谁能解开：明文（服务端用主密钥加密），或浏览器里已用 passkey 加密好的密文（附加数据按 [[SecretContent.aad]]）。
  * JSON 里 `kind` 是分支名。
  */
@jsonDiscriminator("kind")
enum SecretValue derives JsonCodec:
  case Plain(text: String)
  case Passkey(box: PasskeyBox)

  override def toString: String = this match
    case Plain(_)   => "SecretValue.Plain(***)"
    case Passkey(b) => s"SecretValue.Passkey($b)"

object SecretValue:
  given Schema[SecretValue] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

/** 要 secret 做什么：哪个会话（触发器等没有会话的为空）、拿去跑什么（命令全文、skill 脚本与参数、要连的 MCP）。给用户判断批不批。 */
final case class AskPurpose(session: Option[SessionId], what: String) derives JsonCodec, Schema

/** 请 SSH CA 签一张用户证书（#938）：`publicKey` 是平台现生成的临时公钥（`ssh-ed25519 …` 一行），`principals` 是能登录的「账户@主机」，
  * 有效 `minutes` 分钟（从用户批准时算起），`sourceAddress` 是只许从哪些地址用（CIDR，逗号分隔），`keyId` 记在证书里、服务器日志可见。
  */
final case class SshCertRequest(publicKey: String, principals: List[String], minutes: Int, sourceAddress: Option[String], keyId: String)
    derives JsonCodec, Schema

/** 要密钥对签的内容，带类型：签名的一方（浏览器或服务端）按类型自己拼出待签的字节，浏览器还解码成人能看懂的样子显示，不采信 agent 写的说明（#938）。
  * 加一种内容就加一个分支。JSON 里 `kind` 是分支名。
  */
@jsonDiscriminator("kind")
enum SignContent derives JsonCodec:
  /** SSH 用户证书（ed25519 CA）。 */
  case SshCert(request: SshCertRequest)

  /** 这种内容要哪种算法的钥匙。 */
  def algorithm: SigningAlgorithm = this match
    case SshCert(_) => SigningAlgorithm.Ed25519

object SignContent:
  given Schema[SignContent] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

/** 要 passkey secret 做什么：解开交出明文，或用密钥对签一份内容（私钥只在浏览器里用、不交回）。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum AskWant derives JsonCodec:
  case Reveal
  // 钥匙的算法就是 `content.algorithm`（服务端发请求前核对过）
  case Sign(content: SignContent)

object AskWant:
  given Schema[AskWant] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

/** 一次调用要用某个 passkey secret，正等用户授权（每次调用各自一个，`id` 全局唯一、不随重启重复，用来答复）。`box` 给浏览器解密用。 */
final case class SecretAsk(id: AskId, agent: AgentId, key: String, want: AskWant, purpose: AskPurpose, since: Long, box: PasskeyBox) derives JsonCodec, Schema

/** 一段时间的授权：到 `until`（epoch 毫秒）为止这个 agent 用这个 secret 不再问。 */
final case class SecretGrant(agent: AgentId, key: String, until: Long) derives JsonCodec, Schema

/** 已用过的 passkey（来自现有 passkey secret 的密文）：新设值时用同一把加密。 */
final case class PasskeyCredential(rp: String, credential: String) derives JsonCodec, Schema

final case class VaultState(asks: List[SecretAsk], grants: List[SecretGrant], credentials: List[PasskeyCredential]) derives JsonCodec, Schema

/** 用户对一次请求的答复：解开的明文用一次、用一段时间（分钟），签好的结果（答 `AskWant.Sign`：SSH 证书是证书文件的一行），或拒绝。
  * JSON 里 `kind` 是分支名。
  */
@jsonDiscriminator("kind")
enum AskDecision derives JsonCodec:
  case Once(value: String)
  case For(value: String, minutes: Int)
  case Signed(result: String)
  case Deny

  override def toString: String = this match
    case Once(_)   => "AskDecision.Once(***)"
    case For(_, m) => s"AskDecision.For(***, $m)"
    case Signed(r) => s"AskDecision.Signed($r)"
    case Deny      => "AskDecision.Deny"

object AskDecision:
  given Schema[AskDecision] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

final case class AskAnswer(id: AskId, decision: AskDecision) derives JsonCodec, Schema

final case class GrantRevoke(agent: AgentId, key: String) derives JsonCodec, Schema

object VaultApi:
  private val vault = Api.secure.in("vault")

  val vaultState = vault.get.out(jsonBody[VaultState]).name("vaultState").refreshedBy(Topic.Vault, Topic.Assets)
    .description("等授权的请求、生效中的授权、已用过的 passkey。")

  val answerAsk =
    vault.post.in("answer").in(jsonBody[AskAnswer]).out(statusCode(StatusCode.NoContent)).name("answerAsk")
      .description("答复一个等授权的请求：浏览器解开的明文用一次（只给这次调用）或一段时间（这个 agent 这个 secret 全部在等的一并给），签名请求交回签好的结果，或拒绝。请求已没了回 NotFound，答复与请求种类不符回 BadRequest。")

  val revokeGrant =
    vault.post.in("revoke").in(jsonBody[GrantRevoke]).out(statusCode(StatusCode.NoContent)).name("revokeGrant")
      .description("提前收回一段时间的授权（明文从服务内存里删掉）。")

  val all = List(vaultState, answerAsk, revokeGrant)
