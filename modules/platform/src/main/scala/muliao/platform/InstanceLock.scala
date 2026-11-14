package muliao.platform

import zio.*

import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}

/** 单实例：同一份数据目录同时只允许一个进程。进程退出（含被杀）时操作系统自动释放锁。 */
object InstanceLock:
  def acquire(dataDir: Path): ZIO[Scope, AppError, Unit] =
    val file = dataDir.resolve(".lock")
    ZIO
      .acquireRelease(
        ZIO
          .attemptBlocking {
            Files.createDirectories(dataDir)
            val ch = FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
            val lock =
              try ch.tryLock()
              catch case _: java.nio.channels.OverlappingFileLockException => null // 本进程已持有
            Option(lock) match
              case Some(lock) => Right((ch, lock))
              case None       => ch.close(); Left(())
          }
          .mapError(e => AppError.internal(s"无法锁定数据目录 $dataDir：${e.getMessage}", e))
          .flatMap {
            case Right(held) => ZIO.succeed(held)
            case Left(_)     => ZIO.fail(AppError.conflict(s"数据目录 $dataDir 已有另一个 muliao 进程在用"))
          }
      ) { case (ch, lock) => ZIO.succeed { lock.release(); ch.close() } }
      .unit
