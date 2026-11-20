package muliao.resources.vault

import muliao.contract.{AgentId, AskPurpose, AssetKind, RevealScope, SecretContent, SecretValue, SignContent, SigningAlgorithm, SshCert}
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

/** 设值时内容与能否取明文怎么定：沿用原来的（新建的取 [[SecretContent.Default]]）、只改明文范围，或整个指定（用户在界面上）。 */
enum ContentChange:
  case Keep
  case Scope(scope: RevealScope)
  case Replace(content: SecretContent)

  def from(before: Option[SecretContent]): Either[String, SecretContent] = this match
    case Keep       => Right(before.getOrElse(SecretContent.Default))
    case Scope(s)   => before.fold(Right(SecretContent.Opaque(s)))(_.withScope(s))
    case Replace(c) => Right(c)

/** secret 的加密保存与解密使用。值只在这里出现：不进日志、不进错误信息。
  * 两种解锁方（[[SecretLock]]）：服务端主密钥的用时直接解、在服务端签；passkey 的用时经 [[Grants]] 等用户在界面上解开或签好。
  */
trait Vault:
  /** 存或改一个 secret 的值。已有的要有改它的权限（agent 自己的、或规则给了 write）；新的归 `newOwner`（agent 调用时就是它自己）。
    * `description` 为空时沿用原来的。passkey 加密的只有用户能设、能改值；服务端保管的密钥对先核对是认得的私钥。返回是否新建。
    */
  def set(
      actor: Actor,
      key: String,
      value: SecretValue,
      content: ContentChange,
      description: Option[String],
      newOwner: Owner
  ): IO[AppError, Boolean]

  /** 解出某 agent 能用的一组 secret。不能用的、只能签名的、只给脚本却要注入自写命令的、解不开的都报错并点名是哪个 key。
    * 其中 passkey 加密的没有生效的授权时，等用户授权（几个同时等），被拒绝报错；`purpose` 给用户看拿去做什么、在哪个会话里批。
    */
  def reveal(agent: AgentId, keys: List[String], use: SecretUse, purpose: AskPurpose): IO[AppError, Map[String, String]]

  /** 用密钥对 `key` 签一份内容，返回签好的结果（SSH 证书是证书文件的一行）。服务端保管的在服务端直接签；
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
    val all = snap.secrets.map(_.content.lock).collect { case SecretLock.Server(token) => token }
    ZIO.when(all.nonEmpty && all.forall(t => fernet.decrypt(t).isLeft))(
      ZIO.fail(AppError.invalid(s"vault 主密钥不对：现有 ${all.size} 个 secret 一个都解不开"))
    ).unit

  private final case class Live(fernet: Fernet, store: AssetStore, agents: Agents, grants: Grants, lock: Semaphore) extends Vault:

    private def permissions(id: AgentId): IO[AppError, Permissions] =
      for
        a      <- agents.get(id)
        access <- store.access
      yield Permissions.of(a.id, a.name, access.config)

    def set(
        actor: Actor,
        key: String,
        value: SecretValue,
        content: ContentChange,
        description: Option[String],
        newOwner: Owner
    ): IO[AppError, Boolean] =
      lock.withPermit {
        for
          _    <- ZIO.unless(Formats.validSecretKey(key))(ZIO.fail(AppError.invalid(s"secret 名 $key 不合法：大写字母开头，只用大写字母、数字和下划线")))
          snap <- store.snapshot
          perms <- actor match
                     case Actor.Agent(id) => permissions(id).map(Some(_))
                     case Actor.User      => ZIO.none
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
                           shape     <- ZIO.fromEither(content.from(Some(doc.content.content))).mapError(AppError.invalid)
                           sealedNow <- seal(actor, key, value, shape, Some(doc.content.lock), now)
                           next = SecretFile(description.map(_.trim).filter(_.nonEmpty).getOrElse(doc.content.description), sealedNow, shape)
                           _ <- store.write(AssetPath.Secret(doc.owner, key), Formats.renderSecret(next))
                         yield false
                       case None =>
                         val owners = snap.occupied.getOrElse(AssetKind.Secret -> key, Nil)
                         for
                           _ <- ZIO.when(owners.nonEmpty)(
                                  ZIO.fail(AppError.conflict(s"${owners.map(_.label).mkString("、")} 已有名为 $key 的 secret 文件（没加载：见资源问题），先处理它"))
                                )
                           shape     <- ZIO.fromEither(content.from(None)).mapError(AppError.invalid)
                           sealedNow <- seal(actor, key, value, shape, None, now)
                           owner = perms.flatMap(_.name).fold(newOwner)(Owner.Agent(_))
                           next  = SecretFile(description.map(_.trim).getOrElse(""), sealedNow, shape)
                           _ <- store.write(AssetPath.Secret(owner, key), Formats.renderSecret(next))
                         yield true
          _ <- ZIO.logInfo(s"secret $key 已${if created then "新建" else "更新"}")
        yield created
      }

    /** 新值锁成什么：明文用主密钥加密（密钥对先核对私钥认得）；passkey 的密文浏览器里已加密好，原样存。passkey 加密的只有用户能设、能改。 */
    private def seal(actor: Actor, key: String, value: SecretValue, shape: SecretContent, before: Option[SecretLock], now: Long): IO[AppError, SecretLock] =
      (actor, before, value) match
        case (Actor.Agent(_), Some(_: SecretLock.Passkey), _) =>
          ZIO.fail(AppError.forbidden(s"secret $key 是 passkey 加密的，只能由用户在界面上改值"))
        case (Actor.Agent(_), _, _: SecretValue.Passkey) =>
          ZIO.fail(AppError.forbidden("passkey 加密的 secret 只能由用户在界面上设值"))
        case (_, _, SecretValue.Plain(text)) =>
          for
            _ <- ZIO.when(text.isEmpty)(ZIO.fail(AppError.invalid("secret 的值不能为空")))
            _ <- shape match
                   case SecretContent.KeyPair(alg, _) =>
                     ZIO.fromEither(ServerSigner.material(alg, text)).mapError(e => AppError.invalid(s"secret $key 是 ${alg.label} 密钥对，值不是认得的私钥：$e"))
                   case SecretContent.Opaque(_) => ZIO.unit
          yield SecretLock.Server(fernet.encrypt(text, now))
        case (_, _, SecretValue.Passkey(box)) => ZIO.succeed(SecretLock.Passkey(box))

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
                    _ <- (doc.content.content.revealScope, use) match
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
        _ <- doc.content.content match
               case SecretContent.KeyPair(alg, _) if alg == content.algorithm => ZIO.unit
               case _ =>
                 ZIO.fail(AppError.invalid(s"secret $key 不是 ${content.algorithm.label} 密钥对：要由用户在界面 Secret 页把它设成 ${content.algorithm.label} 密钥"))
        out <- doc.content.lock match
                 case SecretLock.Server(token) => decrypt(key, token).flatMap(pem => ServerSigner.sign(key, pem, content))
                 case SecretLock.Passkey(box)  => grants.sign(Slot(agent, key), content, purpose, box)
      yield out

    private def decrypt(key: String, token: String): IO[AppError, String] =
      ZIO.fromEither(fernet.decrypt(token)).mapError(r => AppError.invalid(s"secret $key 解不开（$r），请重新存一次它的值"))

    private def open(agent: AgentId, doc: SecretDoc, purpose: AskPurpose): IO[AppError, String] =
      doc.content.lock match
        case SecretLock.Server(token) => decrypt(doc.key, token)
        case SecretLock.Passkey(box)  => grants.obtain(Slot(agent, doc.key), purpose, box)

/** 服务端保管的密钥对在服务端签（#1069）：读私钥、按内容的类型拼出待签的字节、用 JDK 签。 */
object ServerSigner:
  /** 某种算法的私钥文本读成能签名的东西。ed25519 认 OpenSSH 私钥文件（不带口令）。 */
  def material(alg: SigningAlgorithm, text: String): Either[String, SshCert.PrivateKey] = alg match
    case SigningAlgorithm.Ed25519 => SshCert.readPrivate(text)

  def sign(key: String, pem: String, content: SignContent): IO[AppError, String] =
    for
      ca  <- ZIO.fromEither(material(content.algorithm, pem)).mapError(e => AppError.invalid(s"secret $key 不是可用的私钥：$e"))
      now <- Clock.currentTime(java.util.concurrent.TimeUnit.SECONDS)
      out <- content match
               case SignContent.SshCert(req) =>
                 ZIO
                   .attempt {
                     val rnd   = java.security.SecureRandom()
                     val nonce = Array.ofDim[Byte](32)
                     rnd.nextBytes(nonce)
                     SshCert.issue(ca, req, now, nonce, rnd.nextLong())(ed25519(ca.seed))
                   }
                   .mapError(e => AppError.internal(s"用 secret $key 签名失败：${e.getMessage}", e))
                   .flatMap(r => ZIO.fromEither(r).mapError(AppError.invalid))
    yield out

  private def ed25519(seed: Array[Byte])(data: Array[Byte]): Array[Byte] =
    import java.security.spec.{EdECPrivateKeySpec, NamedParameterSpec}
    val priv = java.security.KeyFactory.getInstance("Ed25519").generatePrivate(EdECPrivateKeySpec(NamedParameterSpec.ED25519, seed))
    val s    = java.security.Signature.getInstance("Ed25519")
    s.initSign(priv)
    s.update(data)
    s.sign()
