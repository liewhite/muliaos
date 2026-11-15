package muliao.resources.tree

import muliao.contract.Topic
import muliao.exec.RealPath
import muliao.platform.AppError
import muliao.platform.events.ChangeBus
import muliao.resources.access.AccessConfig
import zio.*

import java.nio.file.{Files, Path, StandardCopyOption}
import java.nio.file.attribute.PosixFilePermissions

/** 生效的权限规则：access.toml 写坏时沿用上一份有效的，`broken` 说明现在这份为什么不生效。 */
final case class AccessState(config: AccessConfig, broken: Option[String])

/** 一个路径相对资源目录在哪（按真实路径判断，符号链接指进来、指出去都按指向的地方算）。 */
enum Location:
  case Outside(real: RealPath)
  case Inside(real: RealPath, at: AssetPath)

/** 过了写入口校验（[[muliao.resources.guard.AssetGuard]]）的一次写，只能由它得到。 */
sealed trait Vetted:
  def content: String

object Vetted:
  /** 不在资源目录里：原样写 */
  final case class Plain private[resources] (real: RealPath, content: String) extends Vetted
  /** 资源目录里的文件：经 [[AssetStore.commit]] 原子写 */
  final case class Asset private[resources] (real: RealPath, content: String, privateFile: Boolean) extends Vetted

/** 资源目录：带指纹缓存的快照、生效的权限规则、平台自己写资源文件的入口（原子写）、变化监听。
  *
  * 快照只在目录指纹变了才重新解析；任何一方（文件工具、shell、界面、平台）改了文件，下次取快照或监听轮询时都能发现。
  */
trait AssetStore:
  def root: Path

  /** 当前快照（目录变了就重新解析）。 */
  def snapshot: IO[AppError, Snapshot]

  /** 生效的权限规则。 */
  def access: IO[AppError, AccessState]

  /** 路径在不在资源目录里、在哪个位置。 */
  def locate(path: Path): IO[AppError, Location]

  /** 平台自己写资源文件（内容由平台生成，不经写入口校验）。写法同 [[commit]]。 */
  def write(file: ResourceFile, content: String): IO[AppError, Unit]

  /** 写一个校验过的资源文件：原子写（先写同目录的隐藏临时文件再改名），自动建父目录；覆盖已有文件时沿用原文件的权限位，新的私密文件只有主人可读写。 */
  def commit(v: Vetted.Asset): IO[AppError, Unit]

  /** 删除文件或整个目录（不存在不算错）。 */
  def delete(path: Path): IO[AppError, Unit]

  /** 挪动文件或目录（目标的父目录自动建；目标已存在报冲突）。 */
  def move(from: Path, to: Path): IO[AppError, Unit]

object AssetStore:
  /** @param lastGoodAccess 上一份有效 access.toml 的副本存放处（在数据目录里，重启后仍沿用） */
  final case class Settings(root: Path, lastGoodAccess: Path, pollInterval: Duration = 2.seconds)

  /** 起动时确保全局记忆存在，并开始轮询监听目录变化（变了就广播「数据变了」）。 */
  def layer(settings: Settings): ZLayer[ChangeBus, AppError, AssetStore] =
    ZLayer.scoped {
      for
        bus   <- ZIO.service[ChangeBus]
        cache <- Ref.make(Option.empty[Snapshot])
        lock  <- Semaphore.make(1)
        store = Live(settings, bus, cache, lock)
        _    <- store.ensureGlobalMemory
        _    <- store.snapshot
        _    <- store.refresh.ignoreLogged.repeat(Schedule.spaced(settings.pollInterval)).forkScoped
      yield store
    }

  private final case class Live(settings: Settings, bus: ChangeBus, cache: Ref[Option[Snapshot]], lock: Semaphore)
      extends AssetStore:
    val root: Path = settings.root

    private def blocking[A](what: String)(f: => A): IO[AppError, A] =
      ZIO.attemptBlocking(f).mapError(e => AppError.internal(s"$what：${e.getMessage}", e))

    def ensureGlobalMemory: IO[AppError, Unit] =
      val g = root.resolve(AssetPath.render(AssetPath.SharedMemory(MemoryDoc.GlobalName)))
      blocking("建全局记忆失败") {
        if !Files.exists(g) then
          Files.createDirectories(g.getParent)
          Files.writeString(g, ""): Unit
      }

    /** 重新取指纹，变了就重新解析并广播。返回最新快照。 */
    def refresh: IO[AppError, Snapshot] =
      lock.withPermit {
        for
          fp  <- blocking("读资源目录失败")(Scanner.fingerprint(root))
          old <- cache.get
          snap <- old match
                    case Some(s) if s.fingerprint == fp => ZIO.succeed(s)
                    case _ =>
                      for
                        s <- blocking("解析资源目录失败")(Scanner.scan(root))
                        _ <- cache.set(Some(s))
                        _ <- rememberAccess(s)
                        _ <- ZIO.when(old.isDefined)(bus.publish(Topic.Assets))
                      yield s
        yield snap
      }

    def snapshot: IO[AppError, Snapshot] = refresh

    private def rememberAccess(s: Snapshot): IO[AppError, Unit] =
      s.access match
        case Some(Right(_)) =>
          val src = root.resolve(AssetPath.render(AssetPath.AccessFile))
          blocking("保存权限规则副本失败") {
            Files.createDirectories(settings.lastGoodAccess.getParent)
            Files.copy(src, settings.lastGoodAccess, StandardCopyOption.REPLACE_EXISTING): Unit
          }
        case Some(Left(reason)) => ZIO.logWarning(s"access.toml 写坏了，沿用上一份有效的：$reason")
        case None               => ZIO.unit

    def access: IO[AppError, AccessState] =
      snapshot.flatMap { s =>
        s.access match
          case None              => ZIO.succeed(AccessState(AccessConfig.empty, None))
          case Some(Right(c))    => ZIO.succeed(AccessState(c, None))
          case Some(Left(reason)) =>
            blocking("读权限规则副本失败") {
              if Files.exists(settings.lastGoodAccess) then Scanner.readText(settings.lastGoodAccess).flatMap(AccessConfig.parse).toOption
              else None
            }.map(last => AccessState(last.getOrElse(AccessConfig.empty), Some(reason)))
      }

    def locate(path: Path): IO[AppError, Location] =
      RealPath.resolve(root).zipWith(RealPath.resolve(path)) { (r, real) =>
        if RealPath.within(real, r) then Location.Inside(real, AssetPath.classify(r.path.relativize(real.path))) else Location.Outside(real)
      }

    def write(file: ResourceFile, content: String): IO[AppError, Unit] =
      writeAt(root.resolve(AssetPath.render(file)), content, file.privateFile)

    def commit(v: Vetted.Asset): IO[AppError, Unit] = writeAt(v.real.path, v.content, v.privateFile)

    private def writeAt(path: Path, content: String, privateFile: Boolean): IO[AppError, Unit] =
      blocking(s"写 $path 失败") {
        Files.createDirectories(path.getParent)
        val tmp = Files.createTempFile(path.getParent, s".${path.getFileName}.", ".tmp")
        try
          // 覆盖已有文件时沿用它的权限（skill 脚本的可执行位不能因为改了内容就丢掉）
          if Files.exists(path) then Files.setPosixFilePermissions(tmp, Files.getPosixFilePermissions(path)): Unit
          else if privateFile then Files.setPosixFilePermissions(tmp, PosixFilePermissions.fromString("rw-------")): Unit
          Files.writeString(tmp, content)
          Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE): Unit
        finally Files.deleteIfExists(tmp): Unit
      } *> refresh.unit

    def delete(path: Path): IO[AppError, Unit] =
      blocking(s"删 $path 失败") {
        if Files.exists(path, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
          if Files.isDirectory(path, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
            scala.util.Using.resource(Files.walk(path)) { s =>
              s.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
            }
          else Files.delete(path)
      } *> refresh.unit

    def move(from: Path, to: Path): IO[AppError, Unit] =
      ZIO.whenZIO(blocking("检查目标路径失败")(Files.exists(to)))(
        ZIO.fail(AppError.conflict(s"$to 已存在"))
      ) *> blocking(s"把 $from 挪到 $to 失败") {
        Files.createDirectories(to.getParent)
        Files.move(from, to, StandardCopyOption.ATOMIC_MOVE): Unit
      } *> refresh.unit
