package muliao.resources.vault

import muliao.contract.{AgentId, AgentName, AskPurpose, AssetKind, RevealScope, SecretContent, SecretShape, SecretValue, SignContent, Signing}
import muliao.platform.AppError
import muliao.resources.VaultSettings
import muliao.resources.access.{Action, Decision, Permissions}
import muliao.resources.agents.Agents
import muliao.resources.tree.*
import zio.*

/** 谁在操作：某个 agent，或用户本人（界面）。用户能改任何资源。 */
enum Actor:
  case Agent(id: AgentId)
  case User

/** secret 用在哪：自写命令（run_shell、触发器检查与常驻命令）、skill 脚本、MCP 配置。 */
enum SecretUse:
  case Shell, Script, Mcp

/** 谁在设、设成什么。 */
enum SecretInput:
  /** agent 的 vault_set：只给明文与可选的明文范围，保存方式与内容类型沿用原来的（新建的服务端加密、是口令，范围缺省任意命令）。 */
  case ByAgent(id: AgentId, text: String, scope: Option[RevealScope])
  /** 用户在界面上：保存方式、内容与能否取明文整个指定；新建的归 `owner`（改已有的归属不变）。 */
  case ByUser(value: SecretValue, owner: Owner)

  override def toString: String = this match
    case ByAgent(id, _, s) => s"SecretInput.ByAgent($id, ***, $s)"
    case ByUser(v, o)      => s"SecretInput.ByUser($v, ${o.label})"

/** secret 的保存与使用。值只在这里出现：不进日志、不进错误信息。
  * 三种保存方式（[[SecretBody]]）：服务端主密钥加密的用时直接解、在服务端签；passkey 的用时经 [[Grants]] 等用户在界面上解开或签好；
  * 明文的照原样交出。
  */
trait Vault:
  /** 存或改一个 secret 的值。agent 改已有的要有改它的权限（它自己的、或规则给了 write），新建的归它自己。
    * `description` 为空时沿用原来的。passkey 加密的 agent 改不了；服务端保管的密钥对先核对是认得的私钥。返回是否新建。
    */
  def set(key: String, input: SecretInput, description: Option[String]): IO[AppError, Boolean]

  /** 解出某 agent 能用的一组 secret。不能用的、只能签名的、只给脚本却要注入自写命令的、解不开的都报错并点名是哪个 key。
    * 其中 passkey 加密的没有生效的授权时，等用户授权（几个同时等），被拒绝报错；`purpose` 给用户看拿去做什么、在哪个会话里批。
    */
  def reveal(agent: AgentId, keys: List[String], use: SecretUse, purpose: AskPurpose): IO[AppError, Map[String, String]]

  /** 用密钥对 `key` 签一份内容，返回签好的结果（SSH 证书是证书文件的一行，链上的是给 agent 的 JSON，见 [[Signing.sign]]）。服务端保管的在服务端直接签；
    * passkey 加密的等用户在浏览器里解开并签好交回（#938），私钥不出浏览器。要有 use 权限；不是这种算法的密钥对报错。
    */
  def sign(agent: AgentId, key: String, content: SignContent, purpose: AskPurpose): IO[AppError, String]

object Vault:
  val layer: ZLayer[VaultSettings & AssetStore & Agents & Grants, AppError, Vault] =
    ZLayer {
      for
        settings <- ZIO.service[VaultSettings]
        fernet   <- ZIO.fromEither(Fernet.fromKey(settings.key)).mapError(AppError.invalid)
        store    <- ZIO.service[AssetStore]
        agents   <- ZIO.service[Agents]
        grants   <- ZIO.service[Grants]
        snap     <- store.snapshot
        _        <- verifyKey(fernet, snap)
        lock     <- Semaphore.make(1)
      yield Live(fernet, store, agents, grants, lock)
    }

  /** 主密钥不对就拒绝启动：已有服务端加密的 secret 却一个都解不开。个别解不开的（密文被改坏）只在用到时报错。 */
  def verifyKey(fernet: Fernet, snap: Snapshot): IO[AppError, Unit] =
    val all = snap.secrets.map(_.content.body).collect { case SecretBody.Server(token, _) => token }
    ZIO.when(all.nonEmpty && all.forall(t => fernet.decrypt(t).isLeft))(
      ZIO.fail(AppError.invalid(s"vault 主密钥不对：现有 ${all.size} 个 secret 一个都解不开"))
    ).unit

  private final case class Live(fernet: Fernet, store: AssetStore, agents: Agents, grants: Grants, lock: Semaphore) extends Vault:

    /** agent 的名字与权限。 */
    private def access(id: AgentId): IO[AppError, (AgentName, Permissions)] =
      for
        a   <- agents.get(id)
        cfg <- store.access
      yield (a.name, Permissions.of(a.id, a.name, cfg.config))

    private def permissions(id: AgentId): IO[AppError, Permissions] = access(id).map(_._2)

    def set(key: String, input: SecretInput, description: Option[String]): IO[AppError, Boolean] =
      lock.withPermit {
        for
          _    <- ZIO.unless(Formats.validSecretKey(key))(ZIO.fail(AppError.invalid(s"secret 名 $key 不合法：大写字母开头，只用大写字母、数字和下划线")))
          snap <- store.snapshot
          // agent 改已有的要看权限，新建的归它自己；用户新建的归他选的主人
          who <- input match
                   case SecretInput.ByAgent(id, _, _) => access(id).map((name, p) => (Some(p), Owner.Agent(name)))
                   case SecretInput.ByUser(_, o) => ZIO.succeed((None, o))
          (perms, newOwner) = who
          now <- Clock.currentTime(java.util.concurrent.TimeUnit.SECONDS)
          created <- snap.secret(key) match
                       case Some(doc) =>
                         for
                           _ <- ZIO.foreachDiscard(perms)(p =>
                                  p.decide(Action.Write, AssetKind.Secret, key, doc.owner) match
                                    case Decision.Allowed(_) => ZIO.unit
                                    case _ =>
                                      ZIO.fail(AppError.forbidden(s"secret $key 归 ${doc.owner.label}，你没有改它的权限；请它的主人改，或找 admin 授权"))
                                )
                           body <- seal(key, input, Some(doc.content.body), now)
                           next  = SecretFile(description.map(_.trim).filter(_.nonEmpty).getOrElse(doc.content.description), body)
                           _ <- store.write(AssetPath.Secret(doc.owner, key), Formats.renderSecret(next))
                         yield false
                       case None =>
                         val owners = snap.occupied.getOrElse(AssetKind.Secret -> key, Nil)
                         for
                           _ <- ZIO.when(owners.nonEmpty)(
                                  ZIO.fail(AppError.conflict(s"${owners.map(_.label).mkString("、")} 已有名为 $key 的 secret 文件（没加载：见资源问题），先处理它"))
                                )
                           body <- seal(key, input, None, now)
                           next  = SecretFile(description.map(_.trim).getOrElse(""), body)
                           _ <- store.write(AssetPath.Secret(newOwner, key), Formats.renderSecret(next))
                         yield true
          _ <- ZIO.logInfo(s"secret $key 已${if created then "新建" else "更新"}")
        yield created
      }

    /** 新值存成什么。用户的照指定的存：明文的原样存、交明文要加密的用主密钥加密（密钥对先核对私钥认得）、passkey 的密文浏览器里已加密好原样存。
      * agent 的沿用原来的保存方式与内容类型（新建的服务端加密的口令），可改明文范围；passkey 加密的改不了。
      */
    private def seal(key: String, input: SecretInput, before: Option[SecretBody], now: Long): IO[AppError, SecretBody] =
      input match
        case SecretInput.ByUser(SecretValue.Server(text, s), _) => encrypt(key, text, s, now)
        case SecretInput.ByUser(SecretValue.Passkey(box, c), _) => ZIO.succeed(SecretBody.Passkey(box, c))
        case SecretInput.ByUser(SecretValue.Plain(text, s), _)  => nonEmpty(text).as(SecretBody.Plain(text, s))
        case SecretInput.ByAgent(_, text, scope) =>
          before match
            case None => encrypt(key, text, SecretShape.Opaque(scope.getOrElse(RevealScope.Shell)), now)
            case Some(SecretBody.Server(_, c)) =>
              ZIO.fromEither(scope.fold(Right(c))(c.withScope)).mapError(AppError.invalid).flatMap(c => encrypt(key, text, SecretShape.of(c), now))
            case Some(SecretBody.Plain(_, s)) => nonEmpty(text).as(SecretBody.Plain(text, scope.getOrElse(s)))
            case Some(SecretBody.Passkey(_, _)) =>
              ZIO.fail(AppError.forbidden(s"secret $key 是 passkey 加密的，只能由用户在界面上改值"))

    private def nonEmpty(text: String): IO[AppError, Unit] =
      ZIO.when(text.isEmpty)(ZIO.fail(AppError.invalid("secret 的值不能为空"))).unit

    /** 密钥对的公开身份从这次的私钥算出（#1098）。 */
    private def encrypt(key: String, text: String, shape: SecretShape, now: Long): IO[AppError, SecretBody] =
      for
        _ <- nonEmpty(text)
        c <- ZIO.fromEither(shape.content(text, JvmCrypto)).mapError(e => AppError.invalid(s"secret $key 的值不是认得的私钥：$e"))
      yield SecretBody.Server(fernet.encrypt(text, now), c)

    /** 这个 agent 能用的 secret 文件：不存在、没加载或没有 use 权限都报错。 */
    private def usable(perms: Permissions, snap: Snapshot, key: String): IO[AppError, SecretDoc] =
      for
        doc <- ZIO
                 .fromOption(snap.secret(key))
                 .orElseFail(AppError.invalid(s"没有可用的 secret $key（不存在或文件没加载）"))
        _ <- perms.decide(Action.Use, AssetKind.Secret, key, doc.owner) match
               case Decision.Allowed(_) => ZIO.unit
               case _                   => ZIO.fail(AppError.forbidden(s"你不能用 secret $key（归 ${doc.owner.label}）；找 admin 授权"))
      yield doc

    def reveal(agent: AgentId, keys: List[String], use: SecretUse, purpose: AskPurpose): IO[AppError, Map[String, String]] =
      for
        perms <- permissions(agent)
        snap  <- store.snapshot
        docs <- ZIO.foreach(keys.distinct) { key =>
                  for
                    doc <- usable(perms, snap, key)
                    _ <- (doc.content.body.content.revealScope, use) match
                           case (None, _) =>
                             ZIO.fail(AppError.forbidden(s"secret $key 只能签名（如 ssh_certificate），取不出明文"))
                           case (Some(RevealScope.Scripts), SecretUse.Shell) =>
                             ZIO.fail(AppError.forbidden(s"secret $key 只能经 skill 脚本（skill_run）或 MCP 使用，不能注入自写命令"))
                           case _ => ZIO.unit
                  yield doc
                }
        // 权限都过了才开始解：passkey 的几个同时等用户授权，一个被拒绝就不再等其余的
        pairs <- ZIO.foreachPar(docs)(doc => open(agent, doc, purpose).map(doc.key -> _))
      yield pairs.toMap

    def sign(agent: AgentId, key: String, content: SignContent, purpose: AskPurpose): IO[AppError, String] =
      for
        perms <- permissions(agent)
        snap  <- store.snapshot
        doc   <- usable(perms, snap, key)
        out <- doc.content.body match
                 case SecretBody.Server(token, SecretContent.KeyPair(signer, _)) if signer.algorithm == content.algorithm =>
                   decrypt(key, token).flatMap(text =>
                     ZIO.attempt(Signing.sign(content, signer, text, JvmCrypto))
                       .mapError(e => AppError.internal(s"用 secret $key 签名失败：${e.getMessage}", e))
                       .flatMap(r => ZIO.fromEither(r).mapError(e => AppError.invalid(s"用 secret $key 签名失败：$e")))
                   )
                 case SecretBody.Passkey(box, SecretContent.KeyPair(signer, _)) if signer.algorithm == content.algorithm =>
                   grants.sign(Slot(agent, key), content, signer, purpose, box)
                 case _ =>
                   ZIO.fail(AppError.invalid(s"secret $key 不是 ${content.algorithm.label} 密钥对：要由用户在界面 Secret 页把它设成 ${content.algorithm.label} 密钥"))
      yield out

    private def decrypt(key: String, token: String): IO[AppError, String] =
      ZIO.fromEither(fernet.decrypt(token)).mapError(r => AppError.invalid(s"secret $key 解不开（$r），请重新存一次它的值"))

    private def open(agent: AgentId, doc: SecretDoc, purpose: AskPurpose): IO[AppError, String] =
      doc.content.body match
        case SecretBody.Server(token, _) => decrypt(doc.key, token)
        case SecretBody.Passkey(box, _)  => grants.obtain(Slot(agent, doc.key), purpose, box)
        case SecretBody.Plain(text, _)   => ZIO.succeed(text)
