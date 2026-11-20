package muliao.resources.guard

import muliao.contract.AssetKind
import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.resources.*
import muliao.resources.access.*
import muliao.resources.agents.{AgentRecord, AgentRepo}
import muliao.resources.tree.*
import muliao.resources.vault.Actor
import zio.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest

/** 一个文件内容的指纹（读的时候记下，写的时候比对，防止覆盖别人在这期间的改动）。 */
final case class FileFingerprint(value: String)

object FileFingerprint:
  def of(content: String): FileFingerprint =
    val d = MessageDigest.getInstance("SHA-256").digest(content.getBytes(StandardCharsets.UTF_8))
    FileFingerprint(d.take(16).map("%02x".format(_)).mkString)

/** 覆盖已有文件前要满足什么。 */
enum Expectation:
  /** 不要求（按原文定位的局部修改，原文对不上自然失败） */
  case Unconditional
  /** 整体覆盖：本次运行读过它，且读到的是这个指纹 */
  case ReadAs(fingerprint: FileFingerprint)
  /** 整体覆盖，但本次运行没读过它：文件已存在就拒绝 */
  case NotRead

/** 写文件的入口校验：文件工具和界面写文件前都先过这里，得到 [[Vetted]] 才能写。不在资源目录里的路径原样放行；
  * 写权限本身由写沙箱与可写范围管，这里只管资源文件的格式、位置、重名、记忆长度与并发覆盖。
  */
trait AssetGuard:
  /** 文件现在的指纹（不存在为 None）。 */
  def fingerprint(path: Path): IO[AppError, Option[FileFingerprint]]

  /** 校验一次写入，得到真正要写的（记忆会被规整空白）。不合格时说明原因和该看的说明一节。 */
  def check(actor: Actor, path: Path, content: String, expect: Expectation): IO[AppError, Vetted]

object AssetGuard:
  val layer: ZLayer[AssetStore & Resources & ModelCheck & ToolCatalog & Database & ResourcesSettings, Nothing, AssetGuard] =
    ZLayer.derive[Live]

  /** 报错末尾指向 skill assets-files 的哪一节。 */
  private def refuse(section: String)(msg: String): AppError =
    AppError.invalid(s"$msg。没有写入，磁盘上还是原样（见 skill assets-files 的 section '$section'）")

  /** 每种文件的说明在 skill assets-files 的哪一节。 */
  private def section(f: ResourceFile): String = f match
    case AssetPath.AccessFile                                     => "Roles and permissions"
    case AssetPath.SharedMemory(_) | AssetPath.PrivateMemory(_)   => "Memory"
    case AssetPath.AgentFile(_)                                   => "agent.toml"
    case AssetPath.SkillMain(_, _) | AssetPath.SkillAttachment(_, _, _) => "Skills"
    case AssetPath.Mcp(_, _)                                      => "MCP servers"
    case AssetPath.Secret(_, _)                                   => "Secrets"

  final case class Live(
      store: AssetStore,
      resources: Resources,
      models: ModelCheck,
      tools: ToolCatalog,
      db: Database,
      settings: ResourcesSettings
  ) extends AssetGuard:

    private def read(path: Path): IO[AppError, Option[String]] =
      ZIO
        .attemptBlocking(if Files.isRegularFile(path) then Some(Files.readString(path)) else None)
        .mapError(e => AppError.internal(s"读 $path 失败", e))

    def fingerprint(path: Path): IO[AppError, Option[FileFingerprint]] = read(path).map(_.map(FileFingerprint.of))

    def check(actor: Actor, path: Path, content: String, expect: Expectation): IO[AppError, Vetted] =
      store.locate(path).flatMap {
        case Location.Outside(real)                 => ZIO.succeed(Vetted.Plain(real, content))
        case Location.Inside(real, AssetPath.Hidden) => ZIO.succeed(Vetted.Asset(real, content, privateFile = false))
        case Location.Inside(real, AssetPath.Directory(_) | AssetPath.Misplaced) =>
          ZIO.fail(refuse("Layout")(s"${real.path} 不是资源文件的规定位置。${AssetPath.layoutHint}"))
        case Location.Inside(real, file: ResourceFile) =>
          for
            old <- read(real.path)
            _   <- concurrency(real.path, old, expect)
            _   <- ownerExists(file)
            v   <- validate(actor, real.path, file, content, old)
          yield Vetted.Asset(real, v, file.privateFile)
      }

    private def concurrency(path: Path, old: Option[String], expect: Expectation): IO[AppError, Unit] =
      (old, expect) match
        case (Some(_), Expectation.NotRead) =>
          ZIO.fail(refuse("Skills")(s"$path 已存在，整体覆盖前要先在本次运行里 read_file 它（或改用 edit_file）"))
        case (Some(o), Expectation.ReadAs(fp)) if FileFingerprint.of(o) != fp =>
          ZIO.fail(AppError.conflict(s"$path 在你读过之后被改过了，重新 read_file 再写（或改用 edit_file）。没有写入"))
        case _ => ZIO.unit

    private def ownerExists(file: AssetPath): IO[AppError, Unit] =
      file.ownedBy match
        case Some(Owner.Agent(name)) =>
          db.read(AgentRepo.liveByKey(AgentRecord.keyOf(name.value))).flatMap {
            case Some(a) if a.name == name => ZIO.unit
            case _ =>
              ZIO.fail(refuse("Layout")(s"${name.value} 不是现有的 agent：agent 目录只能由 create_agent 建，目录名就是 agent 名"))
          }
        case _ => ZIO.unit

    private def nameTaken(kind: AssetKind, name: String, owner: Owner): IO[AppError, Unit] =
      store.snapshot.flatMap { snap =>
        val others = snap.occupied.getOrElse(kind -> name, Nil).filterNot(_ == owner)
        ZIO.when(others.nonEmpty)(
          ZIO.fail(refuse("Layout")(s"${others.map(_.label).mkString("、")} 已有名为 $name 的 ${kind.label}：每种资源的名字全平台唯一"))
        ).unit
      }

    private def validate(actor: Actor, path: Path, file: AssetPath & ResourceFile, content: String, old: Option[String]): IO[AppError, String] =
      val refused = refuse(section(file))
      file match
        case AssetPath.SkillAttachment(o, n, _) =>
          val main = store.root.resolve(AssetPath.render(AssetPath.SkillMain(o, n)))
          ZIO
            .whenZIO(ZIO.attemptBlocking(!Files.isRegularFile(main)).orDie)(ZIO.fail(refused(s"先写 $main，再写附件")))
            .as(content)
        case AssetPath.Secret(_, key) if old.isEmpty =>
          ZIO.fail(refused(s"secret 文件不能手写新建：用 vault_set 存 $key 的值"))
        case f: ParsedFile =>
          for
            _ <- f.subject match
                   case Some((kind, name)) if old.isEmpty && kind != AssetKind.Memory => nameTaken(kind, name, f.ownedBy.getOrElse(Owner.Platform))
                   case _                                                          => ZIO.unit
            dir     = path.getParent
            parsed <- ZIO
                        .attemptBlocking(FileSpec.load(f, path, content, rel => Files.isRegularFile(dir.resolve(rel))))
                        .orDie
                        .flatMap(r => ZIO.fromEither(r).mapError(refused))
            out <- extra(actor, parsed, content, old, refused)
          yield out

    /** 规格以外、要看平台现状的校验。 */
    private def extra(actor: Actor, parsed: Parsed, content: String, old: Option[String], refused: String => AppError): IO[AppError, String] =
      parsed match
        case Parsed.Access(next) =>
          for
            live <- db.read(AgentRepo.grantable).map(_.map(_.id).toSet)
            snap <- store.snapshot
            before = old.flatMap(o => AccessConfig.parse(o).toOption)
            _ <- ZIO.fromEither(AccessValidation.check(before, next, live, snap, tools.tools.map(_.name))).mapError(refused)
          yield content
        case Parsed.Memory(_) => memory(content, old, refused)
        case Parsed.Agent(p) =>
          ZIO.foreachDiscard(models.check(p.model))(r => ZIO.fail(refused(s"模型 ${p.model} 不能用：$r"))).as(content)
        case Parsed.Skill(_) => ZIO.succeed(content)
        case Parsed.Mcp(doc) => secretRefs(actor, doc.config.secretRefs, refused).as(content)
        case Parsed.Secret(doc) =>
          for
            prev <- ZIO
                      .fromOption(old.flatMap(o => Formats.parseSecret(o).toOption))
                      .orElseFail(refused("原文件已经坏了，用 vault_set 重新存一次值"))
            // 手改只能动说明与明文范围：密文、内容类型、「只能签名」都不变（否则改一行就能把只能签名的私钥变成可取明文）
            sameShape = doc.content.content.revealScope.flatMap(s => prev.content.withScope(s).toOption).contains(doc.content.content)
            _ <- ZIO.when(doc.content.lock != prev.lock || !(sameShape || doc.content.content == prev.content))(
                   ZIO.fail(refused("只能手改 description 和 usage（shell / scripts）；改值用 vault_set，passkey 加密的、改内容类型或「只能签名」的只能由用户在界面上重新设值"))
                 )
          yield content

    private def memory(content: String, old: Option[String], refused: String => AppError): IO[AppError, String] =
      val normalized = Formats.normalizeMemory(content)
      val newLen     = Formats.memoryLength(normalized)
      val oldLen     = old.map(Formats.memoryLength).getOrElse(0)
      if newLen > settings.memoryLimit && newLen > oldLen then
        ZIO.fail(
          refused(
            s"写成后 $newLen 字，超过上限 ${settings.memoryLimit}" +
              (if oldLen > settings.memoryLimit then s"（原来 $oldLen 字，可以分几次删短，但不能变长）" else "")
          )
        )
      else ZIO.succeed(normalized)

    private def secretRefs(actor: Actor, keys: Set[String], refused: String => AppError): IO[AppError, Unit] =
      ZIO.when(keys.nonEmpty) {
        for
          snap <- store.snapshot
          missing = keys.filterNot(k => snap.secret(k).isDefined)
          _ <- ZIO.when(missing.nonEmpty)(ZIO.fail(refused(s"引用的 secret 不存在：${missing.toList.sorted.mkString("、")}")))
          _ <- actor match
                 case Actor.User => ZIO.unit
                 case Actor.Agent(id) =>
                   resources.permissions(id).flatMap { p =>
                     val denied = keys.filterNot(k => snap.secret(k).exists(d => p.decide(Action.Use, AssetKind.Secret, k, d.owner).allowed))
                     ZIO.when(denied.nonEmpty)(
                       ZIO.fail(refused(s"你不能用这些 secret：${denied.toList.sorted.mkString("、")}"))
                     )
                   }
        yield ()
      }.unit
