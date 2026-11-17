package muliao.contract

import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

// ---- passkey secret 的授权（#915） ----

/** secret 用在哪：自写命令（run_shell、触发器检查与常驻命令）、skill 脚本、MCP 配置。 */
enum SecretUse(val label: String) extends Labeled:
  case Shell  extends SecretUse("shell")
  case Script extends SecretUse("script")
  case Mcp    extends SecretUse("mcp")

object SecretUse extends Labels[SecretUse]("secret 用途", SecretUse.values)

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

/** 某 agent 要用某个 passkey secret，正等用户授权。`box` 给浏览器解密用。 */
final case class SecretAsk(agent: AgentId, key: String, use: SecretUse, since: Long, box: PasskeyBox) derives JsonCodec, Schema

/** 一段时间的授权：到 `until`（epoch 毫秒）为止这个 agent 用这个 secret 不再问。 */
final case class SecretGrant(agent: AgentId, key: String, until: Long) derives JsonCodec, Schema

/** 已用过的 passkey（来自现有 passkey secret 的密文）：新设值时用同一把加密。 */
final case class PasskeyCredential(rp: String, credential: String) derives JsonCodec, Schema

final case class VaultState(asks: List[SecretAsk], grants: List[SecretGrant], credentials: List[PasskeyCredential]) derives JsonCodec, Schema

/** 用户对一次请求的答复：解开的明文用一次、用一段时间（分钟），或拒绝。JSON 里 `kind` 是分支名。 */
@jsonDiscriminator("kind")
enum AskDecision derives JsonCodec:
  case Once(value: String)
  case For(value: String, minutes: Int)
  case Deny

  override def toString: String = this match
    case Once(_)       => "AskDecision.Once(***)"
    case For(_, m)     => s"AskDecision.For(***, $m)"
    case Deny          => "AskDecision.Deny"

object AskDecision:
  given Schema[AskDecision] =
    given sttp.tapir.generic.Configuration = sttp.tapir.generic.Configuration.default.withDiscriminator("kind")
    Schema.derived

final case class AskAnswer(agent: AgentId, key: String, decision: AskDecision) derives JsonCodec, Schema

final case class GrantRevoke(agent: AgentId, key: String) derives JsonCodec, Schema

object VaultApi:
  private val vault = Api.secure.in("vault")

  val vaultState = vault.get.out(jsonBody[VaultState]).name("vaultState").refreshedBy(Topic.Vault, Topic.Assets)
    .description("等授权的请求、生效中的授权、已用过的 passkey。")

  val answerAsk =
    vault.post.in("answer").in(jsonBody[AskAnswer]).out(statusCode(StatusCode.NoContent)).name("answerAsk")
      .description("答复一个等授权的请求：浏览器解开的明文用一次或一段时间，或拒绝。请求已经没了（agent 那边中断了）回 NotFound。")

  val revokeGrant =
    vault.post.in("revoke").in(jsonBody[GrantRevoke]).out(statusCode(StatusCode.NoContent)).name("revokeGrant")
      .description("提前收回一段时间的授权（明文从服务内存里删掉）。")

  val all = List(vaultState, answerAsk, revokeGrant)
