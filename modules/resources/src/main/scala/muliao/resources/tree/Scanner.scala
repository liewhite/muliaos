package muliao.resources.tree

import muliao.contract.AgentName
import muliao.resources.access.{AccessConfig, Kind}

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
    occupied: Map[(Kind, String), List[Owner]]
):
  def sharedMemory(name: String): Option[MemoryDoc] =
    memories.find(_.scope == MemoryScope.Shared(name))

  def privateMemory(agent: AgentName): Option[MemoryDoc] =
    memories.find(_.scope == MemoryScope.Private(agent))

  def skill(name: String): Option[SkillDoc]   = skills.find(_.name == name)
  def mcp(name: String): Option[McpDoc]       = mcps.find(_.name == name)
  def secret(key: String): Option[SecretDoc]  = secrets.find(_.key == key)

  /** 已加载的、按名字分发的资源：种类、名字、主人。 */
  def named: List[(Kind, String, Owner)] =
    memories.collect { case MemoryDoc(MemoryScope.Shared(n), _, _) => (Kind.Memory, n, Owner.Platform) } ++
      skills.map(s => (Kind.Skill, s.name, s.owner)) ++
      mcps.map(m => (Kind.Mcp, m.name, m.owner)) ++
      secrets.map(s => (Kind.Secret, s.key, s.owner))

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

  /** 解析成功的按名字分发的资源。 */
  private enum Loaded:
    case Memory(doc: MemoryDoc)
    case Skill(doc: SkillDoc)
    case Mcp(doc: McpDoc)
    case Secret(doc: SecretDoc)

  private sealed trait Candidate:
    def kind: Kind
    def name: String
    def owner: Owner
    def path: Path
  private final case class Ok(kind: Kind, name: String, owner: Owner, path: Path, value: Loaded) extends Candidate
  private final case class Bad(kind: Kind, name: String, owner: Owner, path: Path, reason: String) extends Candidate

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

    // access.toml
    val access = classes.collectFirst { case (p, AssetPath.AccessFile) => readText(p).flatMap(AccessConfig.parse) }
    access.foreach(_.left.foreach(r => problem(root.resolve("platform/access.toml"), r, AssetPath.AccessFile)))

    // agent.toml 与私有记忆
    val profiles = classes.flatMap {
      case (p, ap @ AssetPath.AgentFile(a)) =>
        readText(p).flatMap(Formats.parseAgent) match
          case Right(prof) => Some(a -> prof)
          case Left(r)     => problem(p, r, ap); None
      case _ => None
    }.toMap
    val privateMemories = classes.flatMap {
      case (p, ap @ AssetPath.PrivateMemory(a)) =>
        readText(p) match
          case Right(t) => Some(MemoryDoc(MemoryScope.Private(a), p, t))
          case Left(r)  => problem(p, r, ap); None
      case _ => None
    }

    // 按名字分发的资源：先逐个解析，再处理同名冲突
    val skillDirs = classes.collect { case (_, AssetPath.SkillMain(o, n)) => (o, n) }.toSet
    val orphanAttachments = classes.collect {
      case (p, ap @ AssetPath.SkillAttachment(o, n, _)) if !skillDirs((o, n)) => (p, ap)
    }.groupBy { case (_, ap) => ap.ownedBy -> ap.subject }.values.map(_.head)
    orphanAttachments.foreach { (p, ap) => problem(p.getParent, s"skill 目录里没有 ${SkillDoc.Main}", ap) }

    val candidates: List[Candidate] = classes.flatMap {
      case (p, AssetPath.SharedMemory(n)) =>
        Some(
          if !Formats.validSlug(n) then Bad(Kind.Memory, n, Owner.Platform, p, "共享记忆名只能用小写字母、数字和 -")
          else readText(p).fold(Bad(Kind.Memory, n, Owner.Platform, p, _), t => Ok(Kind.Memory, n, Owner.Platform, p, Loaded.Memory(MemoryDoc(MemoryScope.Shared(n), p, t))))
        )
      case (p, AssetPath.SkillMain(o, n)) =>
        val dir = p.getParent
        Some(
          if !Formats.validSlug(n) then Bad(Kind.Skill, n, o, p, "skill 名（目录名）只能用小写字母、数字和 -")
          else
            readText(p).flatMap(t => Formats.parseSkill(t, n, rel => Files.isRegularFile(dir.resolve(rel)))) match
              case Right(m) => Ok(Kind.Skill, n, o, p, Loaded.Skill(SkillDoc(n, o, dir, m.description, m.approval)))
              case Left(r)  => Bad(Kind.Skill, n, o, p, r)
        )
      case (p, AssetPath.Mcp(o, n)) =>
        Some(
          if !Formats.validSlug(n) then Bad(Kind.Mcp, n, o, p, "MCP 名只能用小写字母、数字和 -")
          else readText(p).flatMap(Formats.parseMcp).fold(Bad(Kind.Mcp, n, o, p, _), c => Ok(Kind.Mcp, n, o, p, Loaded.Mcp(McpDoc(n, o, p, c))))
        )
      case (p, AssetPath.Secret(o, k)) =>
        Some(
          if !Formats.validSecretKey(k) then Bad(Kind.Secret, k, o, p, "secret 名只能用大写字母、数字和下划线，字母开头")
          else readText(p).flatMap(Formats.parseSecret).fold(Bad(Kind.Secret, k, o, p, _), s => Ok(Kind.Secret, k, o, p, Loaded.Secret(SecretDoc(k, o, p, s))))
        )
      case _ => None
    }

    val occupied = candidates.groupMap(c => (c.kind, c.name))(_.owner).view.mapValues(_.distinct).toMap

    val loaded: List[Loaded] = candidates
      .groupBy(c => (c.kind, c.name))
      .toList
      .sortBy(_._1._2)
      .flatMap { case ((kind, name), cs) =>
        cs.collect { case b: Bad => b }.foreach(b => problems += Problem(b.path, b.reason, Some(b.owner), Some(kind -> name)))
        val (platform, agents) = cs.partition(_.owner == Owner.Platform)
        val agentOwners        = agents.map(_.owner.label).distinct.sorted
        if platform.nonEmpty then
          agents.foreach(a => problems += Problem(a.path, s"平台已有同名的 ${kind.label} $name，这份不加载", Some(a.owner), Some(kind -> name)))
          platform.collect { case o: Ok => o.value }
        else if agentOwners.size > 1 then
          agents.foreach(a =>
            problems += Problem(a.path, s"${agentOwners.mkString("、")} 都有名为 $name 的 ${kind.label}，都不加载", Some(a.owner), Some(kind -> name))
          )
          Nil
        else agents.collect { case o: Ok => o.value }
      }

    val misplaced = classes.collect { case (p, AssetPath.Misplaced) => p }
    misplaced.foreach(p => problems += Problem(p, s"不在规定位置，不加载。${AssetPath.layoutHint}", AssetPath.ownerOf(root.relativize(p)), None))

    Snapshot(
      root = root,
      fingerprint = fp,
      access = access,
      agentDirs = agentDirs,
      profiles = profiles,
      memories = loaded.collect { case Loaded.Memory(d) => d } ++ privateMemories,
      skills = loaded.collect { case Loaded.Skill(d) => d },
      mcps = loaded.collect { case Loaded.Mcp(d) => d },
      secrets = loaded.collect { case Loaded.Secret(d) => d },
      problems = problems.result().sortBy(_.path.toString),
      occupied = occupied
    )
