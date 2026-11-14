package muliao.workspace

import muliao.contract.{AgentId, RunId}
import muliao.exec.RealPath
import muliao.platform.AppError
import muliao.resources.{Resources, WritableScope}
import muliao.resources.guard.{AssetGuard, Expectation, FileFingerprint}
import muliao.resources.tree.{AssetPath, AssetStore}
import muliao.resources.vault.Actor
import zio.*

import java.nio.file.{Files, Path}
import scala.collection.immutable.VectorMap

/** 写文件的方式：决定覆盖资源文件前要满足什么。 */
enum WriteMode:
  /** 整体覆盖（write_file）：已有的资源文件要本次运行读过、且之后没被改 */
  case Overwrite
  /** 在当前内容上局部修改（edit_file）：原文对不上自然失败，不另要求读过 */
  case Patch

/** 文件工具与界面共用的读写：按 agent 的可写范围检查（平台私有文件对非 admin 读写都不行），资源目录里的文件经资源领域的写入口
  * （格式校验、防并发覆盖、原子写）。记下每次运行读过的资源文件的指纹，供整体覆盖时比对。
  */
trait FileAccess:
  /** 读一个文件。 */
  def load(agent: AgentId, run: Option[RunId], path: Path): IO[AppError, FileContent]

  /** 只取字节（分享文件）。 */
  def bytes(agent: AgentId, path: Path): IO[AppError, Array[Byte]]

  /** 写一组文件：先全部检查（权限、资源文件的校验），都通过才写；返回各文件实际写入的内容（资源文件可能被规整）。 */
  def store(agent: AgentId, run: RunId, files: List[(Path, String)], mode: WriteMode): IO[AppError, List[String]]

object FileAccess:
  val layer: URLayer[Resources & AssetGuard & AssetStore, FileAccess] =
    ZLayer {
      for
        resources <- ZIO.service[Resources]
        guard     <- ZIO.service[AssetGuard]
        assets    <- ZIO.service[AssetStore]
        ledger    <- Ref.make(VectorMap.empty[(RunId, Path), FileFingerprint])
      yield Live(resources, guard, assets, ledger)
    }

  /** 读多大以内的文件（更大的用 shell 的 head/tail/rg 看）。 */
  val MaxBytes: Long = 50L * 1024 * 1024

  /** 记多少条「读过」：只用于同一次运行里先读后写，旧的丢掉无妨。 */
  private val LedgerSize = 10000

  /** 相对路径按 `base` 解析，`~/` 按用户主目录展开。 */
  def resolve(base: Path, raw: String): Path =
    val p =
      if raw == "~" then Path.of(java.lang.System.getProperty("user.home"))
      else if raw.startsWith("~/") then Path.of(java.lang.System.getProperty("user.home")).resolve(raw.drop(2))
      else Path.of(raw)
    (if p.isAbsolute then p else base.resolve(p)).normalize

  private final case class Live(resources: Resources, guard: AssetGuard, assets: AssetStore, ledger: Ref[VectorMap[(RunId, Path), FileFingerprint]])
      extends FileAccess:

    private def blocking[A](what: String)(f: => A): IO[AppError, A] =
      ZIO.attemptBlocking(f).mapError(e => AppError.internal(s"$what：${e.getMessage}", e))

    private def assetRoot: IO[AppError, RealPath] = RealPath.resolve(assets.root)

    private def readable(agent: AgentId, path: Path): IO[AppError, (RealPath, WritableScope)] =
      for
        scope <- resources.writableScope(agent)
        real  <- RealPath.resolve(path)
        _ <- ZIO.when(scope.hidden.exists(RealPath.within(real, _)))(
               resources.explainDenied(agent, path).flatMap(why => ZIO.fail(AppError.forbidden(s"读不了 $path：$why")))
             )
      yield (real, scope)

    private def readBytes(real: RealPath, shown: Path): IO[AppError, Array[Byte]] =
      blocking(s"读 $shown 失败") {
        val p = real.path
        if !Files.exists(p) then Left(s"$shown 不存在")
        else if Files.isDirectory(p) then Left(s"$shown 是目录，不是文件（列目录用 run_shell 的 ls）")
        else if Files.size(p) > MaxBytes then Left(s"$shown 有 ${Files.size(p) / 1024 / 1024} MB，太大了：用 run_shell 的 head、tail、rg 看需要的部分")
        else Right(Files.readAllBytes(p))
      }.flatMap(ZIO.fromEither(_).mapError(AppError.invalid))

    def load(agent: AgentId, run: Option[RunId], path: Path): IO[AppError, FileContent] =
      for
        (real, _) <- readable(agent, path)
        bytes     <- readBytes(real, path)
        content    = FileContent.of(bytes)
        root      <- assetRoot
        _ <- (run, content) match
               case (Some(r), FileContent.Text(t)) if RealPath.within(real, root) => remember(r, real.path, FileFingerprint.of(t))
               case _                                                             => ZIO.unit
      yield content

    def bytes(agent: AgentId, path: Path): IO[AppError, Array[Byte]] =
      readable(agent, path).flatMap((real, _) => readBytes(real, path))

    private def remember(run: RunId, path: Path, fp: FileFingerprint): UIO[Unit] =
      ledger.update { m =>
        val next = (m - (run -> path)).updated(run -> path, fp)
        if next.size > LedgerSize then next.drop(next.size - LedgerSize) else next
      }

    /** 一个文件要怎么写：普通文件直接写，资源文件经写入口。 */
    private enum Target:
      case Plain(real: RealPath)
      case Asset(real: RealPath, content: String, secret: Boolean)

    def store(agent: AgentId, run: RunId, files: List[(Path, String)], mode: WriteMode): IO[AppError, List[String]] =
      for
        scope <- resources.writableScope(agent)
        root  <- assetRoot
        // 先全部检查，任何一个不行都不写
        targets <- ZIO.foreach(files) { (path, content) =>
                     for
                       real <- RealPath.resolve(path)
                       _ <- ZIO.unless(scope.permits(real))(
                              resources.explainDenied(agent, path).flatMap(why => ZIO.fail(AppError.forbidden(s"写不了 $path：$why")))
                            )
                       t <-
                         if !RealPath.within(real, root) then ZIO.succeed(Target.Plain(real))
                         else
                           for
                             expect <- expectation(run, real.path, mode)
                             out    <- guard.check(Actor.Agent(agent), real.path, content, expect)
                             secret  = AssetPath.classify(root.path.relativize(real.path)).isInstanceOf[AssetPath.Secret]
                           yield Target.Asset(real, out, secret)
                     yield (t, content)
                   }
        written <- ZIO.foreach(targets) {
                     case (Target.Plain(real), content) =>
                       blocking(s"写 ${real.path} 失败") {
                         Option(real.path.getParent).foreach(Files.createDirectories(_))
                         Files.writeString(real.path, content)
                       }.as(content)
                     case (Target.Asset(real, out, secret), _) =>
                       assets.write(real.path, out, privateFile = secret) *> remember(run, real.path, FileFingerprint.of(out)).as(out)
                   }
      yield written

    private def expectation(run: RunId, path: Path, mode: WriteMode): UIO[Expectation] = mode match
      case WriteMode.Patch => ZIO.succeed(Expectation.Unconditional)
      case WriteMode.Overwrite =>
        ledger.get.map(_.get(run -> path).fold(Expectation.NotRead)(Expectation.ReadAs(_)))
