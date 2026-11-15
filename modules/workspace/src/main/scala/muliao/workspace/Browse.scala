package muliao.workspace

import muliao.contract.{AgentId, DirEntry, DirListing, SkillName}
import muliao.exec.RealPath
import muliao.platform.AppError
import muliao.resources.ResourcePaths
import muliao.resources.agents.Agents
import muliao.resources.tree.AssetStore
import zio.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** 下载的内容与响应头。 */
final case class Download(contentType: String, disposition: String, bytes: Array[Byte])

/** 界面浏览 agent 工作目录与 skill 目录：列目录、取文件。路径不能走出浏览的根（按真实路径比，符号链接指出去的也不行）；
  * 取到的文件一律按不可信内容处理：只有常见图片内联显示，其余作为下载。
  */
trait Browse:
  def listDir(agent: AgentId, path: Option[String]): IO[AppError, DirListing]
  def workspaceFile(agent: AgentId, path: String): IO[AppError, Download]
  def skillFile(skill: SkillName, path: String): IO[AppError, Download]

object Browse:
  val layer: URLayer[Agents & ResourcePaths & AssetStore, Browse] = ZLayer.derive[Live]

  /** 一个目录最多列多少条。 */
  val MaxEntries = 2000

  /** 下载最大多少字节。 */
  val MaxBytes: Long = 200L * 1024 * 1024

  def download(name: String, bytes: Array[Byte]): Download =
    val encoded = URLEncoder.encode(name, StandardCharsets.UTF_8).replace("+", "%20")
    FileContent.imageType(bytes) match
      case Some(t) => Download(t, s"inline; filename*=UTF-8''$encoded", bytes)
      case None    => Download("application/octet-stream", s"attachment; filename*=UTF-8''$encoded", bytes)

  final case class Live(agents: Agents, paths: ResourcePaths, store: AssetStore) extends Browse:
    private val outside = AppError.notFound("路径不存在，或走出了可浏览的目录")

    /** `rel` 在 `root` 里的真实路径；走出去的当作不存在。 */
    private def inside(root: Path, rel: String): IO[AppError, RealPath] =
      val p = Path.of(rel)
      if p.isAbsolute then ZIO.fail(outside)
      else
        for
          r    <- RealPath.resolve(root)
          real <- RealPath.resolve(root.resolve(p).normalize)
          _    <- ZIO.unless(RealPath.within(real, r))(ZIO.fail(outside))
        yield real

    def listDir(agent: AgentId, path: Option[String]): IO[AppError, DirListing] =
      val rel = path.map(_.stripPrefix("/")).getOrElse("")
      for
        _    <- agents.get(agent)
        root  = paths.workspace(agent)
        _    <- ZIO.attemptBlocking(Files.createDirectories(root)).mapError(e => AppError.internal("建工作目录失败", e))
        real <- inside(root, rel)
        out <- ZIO
                 .attemptBlocking {
                   if !Files.isDirectory(real.path) then None
                   else
                     val all = scala.util.Using.resource(Files.list(real.path))(_.toArray.toList.map(_.asInstanceOf[Path]))
                     val entries = all.map { p =>
                       val dir = Files.isDirectory(p)
                       DirEntry(p.getFileName.toString, dir, if dir then 0L else Files.size(p), Files.getLastModifiedTime(p).toMillis)
                     }.sortBy(e => (!e.dir, e.name))
                     Some(DirListing(rel, entries.take(MaxEntries), entries.size > MaxEntries))
                 }
                 .mapError(e => AppError.internal(s"列目录失败：${e.getMessage}", e))
                 .someOrFail(AppError.notFound(s"$rel 不是目录"))
      yield out

    private def fetch(root: Path, rel: String): IO[AppError, Download] =
      for
        real <- inside(root, rel)
        bytes <- ZIO
                   .attemptBlocking {
                     if !Files.isRegularFile(real.path) then Left(outside)
                     else if Files.size(real.path) > MaxBytes then Left(AppError.invalid(s"文件太大（超过 ${MaxBytes / 1024 / 1024} MB），不能从界面取"))
                     else Right(Files.readAllBytes(real.path))
                   }
                   .mapError(e => AppError.internal(s"读文件失败：${e.getMessage}", e))
                   .flatMap(ZIO.fromEither(_))
      yield download(real.path.getFileName.toString, bytes)

    def workspaceFile(agent: AgentId, path: String): IO[AppError, Download] =
      agents.get(agent) *> fetch(paths.workspace(agent), path.stripPrefix("/"))

    def skillFile(skill: SkillName, path: String): IO[AppError, Download] =
      store.snapshot.flatMap(snap => ZIO.fromOption(snap.skill(skill)).orElseFail(AppError.notFound(s"没有 skill ${skill.value}"))).flatMap(d =>
        fetch(d.dir, path.stripPrefix("/"))
      )
