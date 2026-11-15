package muliao.resources.tree

import muliao.contract.{AgentName, AssetKind, SkillName}
import muliao.resources.access.AccessConfig

import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** 资源目录的指纹：全部文件（不含隐藏的）的相对路径、大小、修改时间。指纹不变就不必重新解析。 */
final case class TreeFingerprint(value: String)

/** 资源目录某一时刻的完整解析结果。读坏的文件不在各列表里，在 [[problems]] 里；同名冲突按规则只留一份或都不留。 */
final case class Snapshot(
    root: Path,
    fingerprint: TreeFingerprint,
    /** access.toml：不存在为 None，存在则是解析结果（失败时是原因）。 */
    access: Option[Either[String, AccessConfig]],
    agentDirs: Set[AgentName],
    profiles: Map[AgentName, AgentProfile],
    memories: List[MemoryDoc],
    skills: List[SkillDoc],
    mcps: List[McpDoc],
    secrets: List[SecretDoc],
    problems: List[Problem],
    /** 磁盘上出现过的每个按名字分发的资源及其主人（含没加载的），用于新建时查重名。 */
    occupied: Map[(AssetKind, String), List[Owner]]
):
  def sharedMemory(name: String): Option[MemoryDoc] =
    memories.find(_.scope == MemoryScope.Shared(name))

  def privateMemory(agent: AgentName): Option[MemoryDoc] =
    memories.find(_.scope == MemoryScope.Private(agent))

  def skill(name: SkillName): Option[SkillDoc] = skills.find(_.name == name)
  def mcp(name: String): Option[McpDoc]       = mcps.find(_.name == name)
  def secret(key: String): Option[SecretDoc]  = secrets.find(_.key == key)

  /** 已加载的、按名字分发的资源：种类、名字、主人。 */
  def named: List[NamedAsset] =
    memories.collect { case MemoryDoc(MemoryScope.Shared(n), _, _) => NamedAsset(AssetKind.Memory, n, Owner.Platform) } ++
      skills.map(s => NamedAsset(AssetKind.Skill, s.name.value, s.owner)) ++
      mcps.map(m => NamedAsset(AssetKind.Mcp, m.name, m.owner)) ++
      secrets.map(s => NamedAsset(AssetKind.Secret, s.key, s.owner))

/** 按名字分发的一项资源：种类、名字、主人。 */
final case class NamedAsset(kind: AssetKind, name: String, owner: Owner)

/** 扫描资源目录（阻塞 IO，调用方放到阻塞线程上）。 */
object Scanner:

  def fingerprint(root: Path): TreeFingerprint =
    val md = MessageDigest.getInstance("SHA-256")
    files(root).foreach { p =>
      val a = Files.readAttributes(p, classOf[java.nio.file.attribute.BasicFileAttributes])
      md.update(s"${root.relativize(p)}\u0000${a.size}\u0000${a.lastModifiedTime.to(java.util.concurrent.TimeUnit.NANOSECONDS)}\u0000${a.fileKey}\n".getBytes(StandardCharsets.UTF_8))
    }
    TreeFingerprint(md.digest().map("%02x".format(_)).mkString)

  /** 资源目录下全部普通文件（跳过 `.` 开头的文件与目录），按路径排序。 */
  def files(root: Path): List[Path] =
    if !Files.isDirectory(root) then Nil
    else
      Using.resource(Files.walk(root)) { s =>
        s.iterator.asScala
          .filter(p => Files.isRegularFile(p))
          .filterNot(p => root.relativize(p).iterator.asScala.exists(_.toString.startsWith(".")))
          .toList
          .sortBy(_.toString)
      }

  /** 严格按 UTF-8 读：编码不对算坏文件。 */
  def readText(p: Path): Either[String, String] =
    try
      val bytes = Files.readAllBytes(p)
      Right(
        StandardCharsets.UTF_8.newDecoder
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
          .decode(ByteBuffer.wrap(bytes))
          .toString
      )
    catch
      case _: CharacterCodingException => Left("不是 UTF-8 文本")
      case e: java.io.IOException      => Left(s"读不了：${e.getMessage}")

  /** 磁盘上出现的一个按名字分发的资源（解析成功或失败）。 */
  private final case class Candidate(kind: AssetKind, name: String, owner: Owner, path: Path, parsed: Either[String, Parsed])

  def scan(root: Path): Snapshot =
    val fp      = fingerprint(root)
    val all     = files(root)
    val classes = all.map(p => p -> AssetPath.classify(root.relativize(p)))
    val agentDirs =
      if !Files.isDirectory(root) then Set.empty[AgentName]
      else
        Using.resource(Files.list(root)) { s =>
          s.iterator.asScala
            .filter(Files.isDirectory(_))
            .map(_.getFileName.toString)
            .filterNot(n => n.startsWith(".") || n == Owner.PlatformDir)
            .map(AgentName.wrap)
            .toSet
        }
    val problems = List.newBuilder[Problem]
    def problem(p: Path, reason: String, ap: AssetPath) = problems += Problem(p, reason, ap.ownedBy, ap.subject)

    // 逐个按规格解析（skill 附件不解析）
    val parsed = classes.collect { case (p, f: ParsedFile) =>
      (p, f, readText(p).flatMap(FileSpec.load(f, p, _, rel => Files.isRegularFile(p.getParent.resolve(rel)))))
    }

    // 不按名字分发的：access.toml、agent.toml、私有记忆
    parsed.foreach {
      case (p, f, Left(r)) if f.subject.isEmpty => problem(p, r, f)
      case _                                    => ()
    }
    val access = parsed.collectFirst {
      case (_, AssetPath.AccessFile, Left(r))   => Left(r)
      case (_, _, Right(Parsed.Access(config))) => Right(config)
    }
    val profiles = parsed.collect { case (_, AssetPath.AgentFile(a), Right(Parsed.Agent(prof))) => a -> prof }.toMap
    val privateMemories = parsed.collect { case (_, _, Right(Parsed.Memory(d @ MemoryDoc(MemoryScope.Private(_), _, _)))) => d }

    // 按名字分发的资源：先逐个解析，再处理同名冲突
    val skillDirs = classes.collect { case (_, AssetPath.SkillMain(o, n)) => (o, n) }.toSet
    val orphanAttachments = classes.collect {
      case (p, ap @ AssetPath.SkillAttachment(o, n, _)) if !skillDirs((o, n)) => (p, ap)
    }.groupBy { case (_, ap) => ap.ownedBy -> ap.subject }.values.map(_.head)
    orphanAttachments.foreach { (p, ap) => problem(p.getParent, s"skill 目录里没有 ${SkillDoc.Main}", ap) }

    val candidates = parsed.flatMap { (p, f, r) =>
      (f.subject, f.ownedBy) match
        case (Some((kind, name)), Some(owner)) => Some(Candidate(kind, name, owner, p, r))
        case _                                 => None
    }

    val occupied = candidates.groupMap(c => (c.kind, c.name))(_.owner).view.mapValues(_.distinct).toMap

    val loaded: List[Parsed] = candidates
      .groupBy(c => (c.kind, c.name))
      .toList
      .sortBy(_._1._2)
      .flatMap { case ((kind, name), cs) =>
        cs.foreach(c => c.parsed.left.foreach(r => problems += Problem(c.path, r, Some(c.owner), Some(kind -> name))))
        val (platform, agents) = cs.partition(_.owner == Owner.Platform)
        val agentOwners        = agents.map(_.owner.label).distinct.sorted
        if platform.nonEmpty then
          agents.foreach(a => problems += Problem(a.path, s"平台已有同名的 ${kind.label} $name，这份不加载", Some(a.owner), Some(kind -> name)))
          platform.flatMap(_.parsed.toOption)
        else if agentOwners.size > 1 then
          agents.foreach(a =>
            problems += Problem(a.path, s"${agentOwners.mkString("、")} 都有名为 $name 的 ${kind.label}，都不加载", Some(a.owner), Some(kind -> name))
          )
          Nil
        else agents.flatMap(_.parsed.toOption)
      }

    val misplaced = classes.collect { case (p, AssetPath.Misplaced) => p }
    misplaced.foreach(p => problems += Problem(p, s"不在规定位置，不加载。${AssetPath.layoutHint}", AssetPath.ownerOf(root.relativize(p)), None))

    Snapshot(
      root = root,
      fingerprint = fp,
      access = access,
      agentDirs = agentDirs,
      profiles = profiles,
      memories = loaded.collect { case Parsed.Memory(d) => d } ++ privateMemories,
      skills = loaded.collect { case Parsed.Skill(d) => d },
      mcps = loaded.collect { case Parsed.Mcp(d) => d },
      secrets = loaded.collect { case Parsed.Secret(d) => d },
      problems = problems.result().sortBy(_.path.toString),
      occupied = occupied
    )
