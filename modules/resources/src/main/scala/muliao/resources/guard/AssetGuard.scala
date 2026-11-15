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

/** 资源目录的写入口校验：文件工具和界面写资源文件前都先过这里。不在资源目录里的路径原样放行；
  * 写权限本身由写沙箱与可写范围管，这里只管格式、位置、重名、记忆长度与并发覆盖。
  */
trait AssetGuard:
  /** 文件现在的指纹（不存在为 None）。 */
  def fingerprint(path: Path): IO[AppError, Option[FileFingerprint]]

  /** 校验一次写入，返回真正要写的内容（记忆会被规整空白）。不合格时说明原因和该看的说明一节。 */
  def check(actor: Actor, path: Path, content: String, expect: Expectation): IO[AppError, String]

object AssetGuard:
  val layer: ZLayer[AssetStore & Resources & ModelCheck & ToolCatalog & Database & ResourcesSettings, Nothing, AssetGuard] =
    ZLayer.derive[Live]

  /** 报错末尾指向 skill assets-files 的哪一节。 */
  private def refuse(section: String)(msg: String): AppError =
    AppError.invalid(s"$msg。没有写入，磁盘上还是原样（见 skill assets-files 的 section '$section'）")

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

    def check(actor: Actor, path: Path, content: String, expect: Expectation): IO[AppError, String] =
      for
        root <- ZIO.attemptBlocking(store.root.toRealPath()).mapError(e => AppError.internal("资源目录不存在", e))
        real <- muliao.exec.RealPath.resolve(path).map(_.path)
        out <-
          if !real.startsWith(root) then ZIO.succeed(content)
          else
            val where = AssetPath.classify(root.relativize(real))
            for
              _ <- where match
                     case AssetPath.Directory(_) | AssetPath.Misplaced =>
                       ZIO.fail(refuse("Layout")(s"$real 不是资源文件的规定位置。${AssetPath.layoutHint}"))
                     case _ => ZIO.unit
              old <- read(real)
              _   <- concurrency(real, old, expect)
              _   <- ownerExists(where)
              v   <- validate(actor, real, where, content, old)
            yield v
      yield out

    private def concurrency(path: Path, old: Option[String], expect: Expectation): IO[AppError, Unit] =
      (old, expect) match
        case (Some(_), Expectation.NotRead) =>
          ZIO.fail(refuse("Skills")(s"$path 已存在，整体覆盖前要先在本次运行里 read_file 它（或改用 edit_file）"))
        case (Some(o), Expectation.ReadAs(fp)) if FileFingerprint.of(o) != fp =>
          ZIO.fail(AppError.conflict(s"$path 在你读过之后被改过了，重新 read_file 再写（或改用 edit_file）。没有写入"))
        case _ => ZIO.unit

    private def ownerExists(where: AssetPath): IO[AppError, Unit] =
      where.ownedBy match
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

    private def validate(actor: Actor, path: Path, where: AssetPath, content: String, old: Option[String]): IO[AppError, String] =
      where match
        case AssetPath.Hidden | AssetPath.Directory(_) | AssetPath.Misplaced => ZIO.succeed(content) // 位置已在前面查过

        case AssetPath.AccessFile =>
          for
            next   <- ZIO.fromEither(AccessConfig.parse(content)).mapError(refuse("Roles and permissions"))
            before  = old.flatMap(o => AccessConfig.parse(o).toOption)
            all    <- db.read(AgentRepo.all)
            snap   <- store.snapshot
            _ <- ZIO
                   .fromEither(AccessValidation.check(before, next, all.filter(_.live).map(_.id).toSet, snap, tools.tools.map(_.name)))
                   .mapError(refuse("Roles and permissions"))
          yield content

        case AssetPath.SharedMemory(name) =>
          for
            _ <- ZIO.unless(Formats.validSlug(name))(ZIO.fail(refuse("Memory")("共享记忆名只能用小写字母、数字和 -")))
            v <- memory(content, old)
          yield v

        case AssetPath.PrivateMemory(_) => memory(content, old)

        case AssetPath.AgentFile(_) =>
          for
            p <- ZIO.fromEither(Formats.parseAgent(content)).mapError(refuse("agent.toml"))
            _ <- ZIO.foreachDiscard(models.check(p.model))(r => ZIO.fail(refuse("agent.toml")(s"模型 ${p.model} 不能用：$r")))
          yield content

        case AssetPath.SkillMain(owner, name) =>
          val dir = path.getParent
          for
            _ <- ZIO.unless(Formats.validSlug(name))(ZIO.fail(refuse("Skills")("skill 名（目录名）只能用小写字母、数字和 -")))
            _ <- ZIO.when(old.isEmpty)(nameTaken(AssetKind.Skill, name, owner))
            _ <- ZIO
                   .attemptBlocking(Formats.parseSkill(content, name, rel => Files.isRegularFile(dir.resolve(rel))))
                   .orDie
                   .flatMap(r => ZIO.fromEither(r).mapError(refuse("Skills")))
          yield content

        case AssetPath.SkillAttachment(_, _, _) =>
          val main = skillDir(path, where).resolve(SkillDoc.Main)
          ZIO
            .whenZIO(ZIO.attemptBlocking(!Files.isRegularFile(main)).orDie)(
              ZIO.fail(refuse("Skills")(s"先写 ${main}，再写附件"))
            )
            .as(content)

        case AssetPath.Mcp(owner, name) =>
          for
            _   <- ZIO.unless(Formats.validSlug(name))(ZIO.fail(refuse("MCP servers")("MCP 名只能用小写字母、数字和 -")))
            _   <- ZIO.when(old.isEmpty)(nameTaken(AssetKind.Mcp, name, owner))
            cfg <- ZIO.fromEither(Formats.parseMcp(content)).mapError(refuse("MCP servers"))
            _   <- secretRefs(actor, cfg.secretRefs)
          yield content

        case AssetPath.Secret(_, key) =>
          for
            before <- ZIO
                        .fromOption(old)
                        .orElseFail(refuse("Secrets")(s"secret 文件不能手写新建：用 vault_set 存 $key 的值"))
            prev <- ZIO.fromEither(Formats.parseSecret(before)).orElseFail(refuse("Secrets")("原文件已经坏了，用 vault_set 重新存一次值"))
            next <- ZIO.fromEither(Formats.parseSecret(content)).mapError(refuse("Secrets"))
            _ <- ZIO.when(next.ciphertext != prev.ciphertext)(
                   ZIO.fail(refuse("Secrets")("只能手改 description 和 allow_shell；改值用 vault_set"))
                 )
          yield content

    private def skillDir(path: Path, where: AssetPath): Path = where match
      case AssetPath.SkillAttachment(_, _, rel) =>
        (1 to rel.split('/').length).foldLeft(path)((p, _) => p.getParent)
      case _ => path.getParent

    private def memory(content: String, old: Option[String]): IO[AppError, String] =
      val normalized = Formats.normalizeMemory(content)
      val newLen     = Formats.memoryLength(normalized)
      val oldLen     = old.map(Formats.memoryLength).getOrElse(0)
      if newLen > settings.memoryLimit && newLen > oldLen then
        ZIO.fail(
          refuse("Memory")(
            s"写成后 $newLen 字，超过上限 ${settings.memoryLimit}" +
              (if oldLen > settings.memoryLimit then s"（原来 $oldLen 字，可以分几次删短，但不能变长）" else "")
          )
        )
      else ZIO.succeed(normalized)

    private def secretRefs(actor: Actor, keys: Set[String]): IO[AppError, Unit] =
      ZIO.when(keys.nonEmpty) {
        for
          snap <- store.snapshot
          missing = keys.filterNot(k => snap.secret(k).isDefined)
          _ <- ZIO.when(missing.nonEmpty)(ZIO.fail(refuse("MCP servers")(s"引用的 secret 不存在：${missing.toList.sorted.mkString("、")}")))
          _ <- actor match
                 case Actor.User => ZIO.unit
                 case Actor.Agent(id) =>
                   resources.permissions(id).flatMap { p =>
                     val denied = keys.filterNot(k => snap.secret(k).exists(d => p.decide(Action.Use, AssetKind.Secret, k, d.owner).allowed))
                     ZIO.when(denied.nonEmpty)(
                       ZIO.fail(refuse("MCP servers")(s"你不能用这些 secret：${denied.toList.sorted.mkString("、")}"))
                     )
                   }
        yield ()
      }.unit
