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

/** 签名钥匙的算法（#938）。 */
enum SigningAlgorithm(val label: String) extends Labeled:
  case Ed25519 extends SigningAlgorithm("ed25519")

object SigningAlgorithm extends Labels[SigningAlgorithm]("签名算法", SigningAlgorithm.values)

/** passkey 加密时 AES-GCM 的附加数据：普通的是 secret 名；签名钥匙带上「签名钥匙、算法」，于是取明文的那条路（按 secret 名解）解不开它，
  * 服务端被攻破、把签名钥匙冒充成普通 secret 发来取明文，浏览器也解不开。
  */
object PasskeyAad:
  def plain(key: String): String                           = key
  def signing(key: String, alg: SigningAlgorithm): String = s"signing:${alg.label}:$key"

/** 设 secret 的新值：明文（服务端用主密钥加密）、浏览器里已用 passkey 加密好的密文，或签名钥匙（passkey 加密的私钥，只能在浏览器里签名，#938）。
  * JSON 里 `kind` 是分支名。
  */
@jsonDiscriminator("kind")
enum SecretValue derives JsonCodec:
  case Plain(text: String)
  case Passkey(box: PasskeyBox)
  case Signing(algorithm: SigningAlgorithm, box: PasskeyBox)

  override def toString: String = this match
    case Plain(_)      => "SecretValue.Plain(***)"
    case Passkey(b)    => s"SecretValue.Passkey($b)"
    case Signing(a, b) => s"SecretValue.Signing(${a.label}, $b)"

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

/** 要签名钥匙签的内容，带类型：浏览器按类型自己解码成人能看懂的样子显示、自己拼出待签的字节，不采信 agent 写的说明（#938）。
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

/** 要 passkey secret 做什么：解开交出明文，或用签名钥匙签一份内容（私钥只在浏览器里用、不交回）。JSON 里 `kind` 是分支名。 */
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
