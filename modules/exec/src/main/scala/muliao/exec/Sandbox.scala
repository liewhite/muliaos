package muliao.exec

import muliao.platform.AppError
import zio.*

import java.nio.file.{Files, Path}

/** 磁盘上的真实路径：符号链接展开、按磁盘上的大小写（macOS 不分大小写）。写沙箱只接受这种路径，防止用别名或大小写绕过。 */
opaque type RealPath = Path

object RealPath:
  extension (r: RealPath) def path: Path = r

  /** 解析成真实路径。不存在的路径按最近一级存在的祖先解析后再接上余下部分。
    * 解析可能触发系统授权弹窗而卡住，所以限时，超时报错而不是挂死。
    */
  def resolve(p: Path, timeout: Duration = 10.seconds): IO[AppError, RealPath] =
    ZIO
      .attemptBlockingInterrupt(resolveUnsafe(p.toAbsolutePath.normalize))
      .mapError(e => AppError.internal(s"解析路径 $p 失败：${e.getMessage}", e))
      .timeoutFail(AppError.internal(s"解析路径 $p 超时（可能在等系统授权弹窗）", new RuntimeException("timeout")))(timeout)

  private def resolveUnsafe(p: Path): Path =
    if Files.exists(p) then p.toRealPath()
    else
      Option(p.getParent) match
        case Some(parent) => resolveUnsafe(parent).resolve(p.getFileName)
        case None         => p

  /** `a` 是否在 `b` 之内（含相等）。 */
  def within(a: RealPath, b: RealPath): Boolean = a.startsWith(b)

/** 子进程的文件权限。网络不限制。 */
enum Sandbox:
  /** 不限制（平台自己的命令、admin）。 */
  case Unrestricted

  /** 只能写 `writable` 里的目录（及其下所有文件）和系统的几个设备文件；`hidden` 里的读写都不行（平台私有文件），优先于 `writable`。
    * 路径由调用方按权限算好。
    */
  case Confined(writable: Chunk[RealPath], hidden: Chunk[RealPath] = Chunk.empty)

object Sandbox:
  private def quote(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def subpaths(ps: Chunk[RealPath]) = ps.map(p => s"  (subpath ${quote(p.path.toString)})").mkString("\n")

  /** macOS sandbox-exec 的规则：先禁止全部写，再放开可写路径，最后禁止读写私有路径（后写的规则优先）。 */
  def profile(writable: Chunk[RealPath], hidden: Chunk[RealPath] = Chunk.empty): String =
    val paths = subpaths(writable)
    val deny =
      if hidden.isEmpty then ""
      else s"(deny file-read* file-write*\n${subpaths(hidden)})\n"
    s"""(version 1)
       |(allow default)
       |(deny file-write*)
       |(allow file-write*
       |  (literal "/dev/null")
       |  (literal "/dev/zero")
       |  (literal "/dev/tty")
       |  (literal "/dev/dtracehelper")
       |  (regex #"^/dev/fd/")
       |  (regex #"^/dev/ttys[0-9]+$$")
       |$paths)
       |$deny""".stripMargin
