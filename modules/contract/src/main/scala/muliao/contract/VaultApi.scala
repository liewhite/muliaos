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

/** 设 secret 的新值：明文（服务端用主密钥加密）或浏览器里已用 passkey 加密好的密文。JSON 里 `kind` 是分支名。 */
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

/** 要 passkey secret 做什么：解开交出明文，或当 SSH CA 签一张证书（CA 私钥只在浏览器里用、不交回）。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum AskWant derives JsonCodec:
  case Reveal
  case SshCert(request: SshCertRequest)

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

/** 用户对一次请求的答复：解开的明文用一次、用一段时间（分钟），签好的证书（证书文件的一行，答 `AskWant.SshCert`），或拒绝。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum AskDecision derives JsonCodec:
  case Once(value: String)
  case For(value: String, minutes: Int)
  case Certificate(cert: String)
  case Deny

  override def toString: String = this match
    case Once(_)        => "AskDecision.Once(***)"
    case For(_, m)      => s"AskDecision.For(***, $m)"
    case Certificate(c) => s"AskDecision.Certificate($c)"
    case Deny           => "AskDecision.Deny"

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
      .description("答复一个等授权的请求：浏览器解开的明文用一次（只给这次调用）或一段时间（这个 agent 这个 secret 全部在等的一并给），签证书的请求交回签好的证书，或拒绝。请求已没了回 NotFound，答复与请求种类不符回 BadRequest。")

  val revokeGrant =
    vault.post.in("revoke").in(jsonBody[GrantRevoke]).out(statusCode(StatusCode.NoContent)).name("revokeGrant")
      .description("提前收回一段时间的授权（明文从服务内存里删掉）。")

  val all = List(vaultState, answerAsk, revokeGrant)
