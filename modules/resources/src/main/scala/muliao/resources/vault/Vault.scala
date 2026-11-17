package muliao.resources.vault

import muliao.contract.{AgentId, AssetKind, SecretUse, SecretValue}
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

/** secret 的加密保存与解密使用。值只在这里出现：不进日志、不进错误信息。
  * 两种锁法（[[SecretLock]]）：服务端主密钥的用时直接解；passkey 的用时经 [[Grants]] 等用户在界面上解开。
  */
trait Vault:
  /** 存或改一个 secret 的值。已有的要有改它的权限（agent 自己的、或规则给了 write）；新的归 `newOwner`（agent 调用时就是它自己）。
    * `description` 为空时沿用原来的；`allowShell` 省略时新的为 true、已有的不变。passkey 加密的只有用户能改值。返回是否新建。
    */
  def set(
      actor: Actor,
      key: String,
      value: SecretValue,
      description: Option[String],
      allowShell: Option[Boolean],
      newOwner: Owner
  ): IO[AppError, Boolean]

  /** 解出某 agent 能用的一组 secret。不能用的、`allow_shell = false` 却要注入自写命令的、解不开的都报错并点名是哪个 key。
    * 其中 passkey 加密的没有生效的授权时，等用户授权（几个同时等），被拒绝报错。
    */
  def reveal(agent: AgentId, keys: List[String], use: SecretUse): IO[AppError, Map[String, String]]

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
        description: Option[String],
        allowShell: Option[Boolean],
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
                           sealedNow <- seal(actor, key, value, Some(doc.content.lock), now)
                           next = SecretFile(
                                    description.map(_.trim).filter(_.nonEmpty).getOrElse(doc.content.description),
                                    sealedNow,
                                    allowShell.getOrElse(doc.content.allowShell)
                                  )
                           _ <- store.write(AssetPath.Secret(doc.owner, key), Formats.renderSecret(next))
                         yield false
                       case None =>
                         val owners = snap.occupied.getOrElse(AssetKind.Secret -> key, Nil)
                         for
                           _ <- ZIO.when(owners.nonEmpty)(
                                  ZIO.fail(AppError.conflict(s"${owners.map(_.label).mkString("、")} 已有名为 $key 的 secret 文件（没加载：见资源问题），先处理它"))
                                )
                           sealedNow <- seal(actor, key, value, None, now)
                           owner = perms.flatMap(_.name).fold(newOwner)(Owner.Agent(_))
                           next  = SecretFile(description.map(_.trim).getOrElse(""), sealedNow, allowShell.getOrElse(true))
                           _ <- store.write(AssetPath.Secret(owner, key), Formats.renderSecret(next))
                         yield true
          _ <- ZIO.logInfo(s"secret $key 已${if created then "新建" else "更新"}")
        yield created
      }

    /** 新值锁成什么：明文用主密钥加密；passkey 的密文浏览器里已加密好，原样存。passkey 加密的只有用户能设、能改。 */
    private def seal(actor: Actor, key: String, value: SecretValue, before: Option[SecretLock], now: Long): IO[AppError, SecretLock] =
      (actor, before, value) match
        case (Actor.Agent(_), Some(_: SecretLock.Passkey), _) =>
          ZIO.fail(AppError.forbidden(s"secret $key 是 passkey 加密的，只能由用户在界面上改值"))
        case (Actor.Agent(_), _, _: SecretValue.Passkey) =>
          ZIO.fail(AppError.forbidden("passkey 加密的 secret 只能由用户在界面上设值"))
        case (_, _, SecretValue.Plain(text)) =>
          if text.isEmpty then ZIO.fail(AppError.invalid("secret 的值不能为空")) else ZIO.succeed(SecretLock.Server(fernet.encrypt(text, now)))
        case (_, _, SecretValue.Passkey(box)) =>
          if List(box.rp, box.credential, box.iv, box.ciphertext).exists(_.isEmpty) then ZIO.fail(AppError.invalid("passkey 密文缺字段"))
          else ZIO.succeed(SecretLock.Passkey(box))

    def reveal(agent: AgentId, keys: List[String], use: SecretUse): IO[AppError, Map[String, String]] =
      for
        perms <- permissions(agent)
        snap  <- store.snapshot
        docs <- ZIO.foreach(keys.distinct) { key =>
                  for
                    doc <- ZIO
                             .fromOption(snap.secret(key))
                             .orElseFail(AppError.invalid(s"没有可用的 secret $key（不存在或文件没加载）"))
                    _ <- perms.decide(Action.Use, AssetKind.Secret, key, doc.owner) match
                           case Decision.Allowed(_) => ZIO.unit
                           case _                   => ZIO.fail(AppError.forbidden(s"你不能用 secret $key（归 ${doc.owner.label}）；找 admin 授权"))
                    _ <- ZIO.when(use == SecretUse.Shell && !doc.content.allowShell)(
                           ZIO.fail(AppError.forbidden(s"secret $key 只能经 skill 脚本（skill_run）或 MCP 使用，不能注入自写命令"))
                         )
                  yield doc
                }
        // 权限都过了才开始解：passkey 的几个同时等用户授权，一个被拒绝其余随之撤下
        pairs <- ZIO.foreachPar(docs)(doc => open(agent, doc, use).map(doc.key -> _))
      yield pairs.toMap

    private def open(agent: AgentId, doc: SecretDoc, use: SecretUse): IO[AppError, String] =
      doc.content.lock match
        case SecretLock.Server(token) =>
          ZIO.fromEither(fernet.decrypt(token)).mapError(r => AppError.invalid(s"secret ${doc.key} 解不开（$r），请重新存一次它的值"))
        case SecretLock.Passkey(box) => grants.obtain(Slot(agent, doc.key), use, box)
