package muliao.resources.vault

import muliao.contract.{AgentId, AssetKind}
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

/** secret 的加密保存与解密使用。值只在这里出现：不进日志、不进错误信息。 */
trait Vault:
  /** 存或改一个 secret 的值。已有的要有改它的权限（agent 自己的、或规则给了 write）；新的归 `newOwner`（agent 调用时就是它自己）。
    * `description` 为空时沿用原来的；`allowShell` 省略时新的为 true、已有的不变。返回是否新建。
    */
  def set(
      actor: Actor,
      key: String,
      value: String,
      description: Option[String],
      allowShell: Option[Boolean],
      newOwner: Owner
  ): IO[AppError, Boolean]

  /** 解出某 agent 能用的一组 secret。不能用的、`allow_shell = false` 却要注入自写命令的、解不开的都报错并点名是哪个 key。 */
  def reveal(agent: AgentId, keys: List[String], use: SecretUse): IO[AppError, Map[String, String]]

object Vault:
  val layer: ZLayer[VaultSettings & AssetStore & Agents, AppError, Vault] =
    ZLayer {
      for
        settings <- ZIO.service[VaultSettings]
        fernet   <- ZIO.fromEither(Fernet.fromKey(settings.key)).mapError(AppError.invalid)
        store    <- ZIO.service[AssetStore]
        agents   <- ZIO.service[Agents]
        snap     <- store.snapshot
        _        <- verifyKey(fernet, snap)
        lock     <- Semaphore.make(1)
      yield Live(fernet, store, agents, lock)
    }

  /** 主密钥不对就拒绝启动：已有 secret 却一个都解不开。个别解不开的（密文被改坏）只在用到时报错。 */
  def verifyKey(fernet: Fernet, snap: Snapshot): IO[AppError, Unit] =
    val all = snap.secrets
    ZIO.when(all.nonEmpty && all.forall(s => fernet.decrypt(s.content.ciphertext).isLeft))(
      ZIO.fail(AppError.invalid(s"vault 主密钥不对：现有 ${all.size} 个 secret 一个都解不开"))
    ).unit

  private final case class Live(fernet: Fernet, store: AssetStore, agents: Agents, lock: Semaphore) extends Vault:

    private def permissions(id: AgentId): IO[AppError, Permissions] =
      for
        a      <- agents.get(id)
        access <- store.access
      yield Permissions.of(a.id, a.name, access.config)

    def set(
        actor: Actor,
        key: String,
        value: String,
        description: Option[String],
        allowShell: Option[Boolean],
        newOwner: Owner
    ): IO[AppError, Boolean] =
      lock.withPermit {
        for
          _    <- ZIO.unless(Formats.validSecretKey(key))(ZIO.fail(AppError.invalid(s"secret 名 $key 不合法：大写字母开头，只用大写字母、数字和下划线")))
          _    <- ZIO.when(value.isEmpty)(ZIO.fail(AppError.invalid("secret 的值不能为空")))
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
                           next = SecretFile(
                                    description.map(_.trim).filter(_.nonEmpty).getOrElse(doc.content.description),
                                    fernet.encrypt(value, now),
                                    allowShell.getOrElse(doc.content.allowShell)
                                  )
                           _ <- store.write(doc.file, Formats.renderSecret(next), privateFile = true)
                         yield false
                       case None =>
                         val owners = snap.occupied.getOrElse(AssetKind.Secret -> key, Nil)
                         for
                           _ <- ZIO.when(owners.nonEmpty)(
                                  ZIO.fail(AppError.conflict(s"${owners.map(_.label).mkString("、")} 已有名为 $key 的 secret 文件（没加载：见资源问题），先处理它"))
                                )
                           owner = perms.fold(newOwner)(p => Owner.Agent(p.name))
                           file  = store.root.resolve(owner.label).resolve("secrets").resolve(s"$key.toml")
                           next  = SecretFile(description.map(_.trim).getOrElse(""), fernet.encrypt(value, now), allowShell.getOrElse(true))
                           _ <- store.write(file, Formats.renderSecret(next), privateFile = true)
                         yield true
          _ <- ZIO.logInfo(s"secret $key 已${if created then "新建" else "更新"}")
        yield created
      }

    def reveal(agent: AgentId, keys: List[String], use: SecretUse): IO[AppError, Map[String, String]] =
      for
        perms <- permissions(agent)
        snap  <- store.snapshot
        pairs <- ZIO.foreach(keys.distinct) { key =>
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
                     value <- ZIO
                                .fromEither(fernet.decrypt(doc.content.ciphertext))
                                .mapError(r => AppError.invalid(s"secret $key 解不开（$r），请重新存一次它的值"))
                   yield key -> value
                 }
      yield pairs.toMap
