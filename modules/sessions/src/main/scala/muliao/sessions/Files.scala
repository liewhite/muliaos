package muliao.sessions

import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.sessions.store.{Repo, UploadRow}
import zio.*

import java.nio.file.{Files as JFiles, Path, StandardCopyOption}
import java.security.MessageDigest

/** 附件存储：内容按 sha256 存一份（`<files>/<前两位>/<sha256>`），上传记录各一条。附件与会话独立，删会话不删附件。 */
trait Files:
  /** 存一段内容，返回 sha256（已有就不再写）。 */
  def put(bytes: Chunk[Byte]): IO[AppError, String]
  def read(sha256: String): IO[AppError, Chunk[Byte]]
  def path(sha256: String): Path

  /** 上传一个文件：存内容、记一条上传记录。 */
  def upload(name: String, mediaType: String, bytes: Chunk[Byte]): IO[AppError, UploadRow]
  def find(ids: List[FileId]): IO[AppError, List[UploadRow]]

object Files:
  val layer: URLayer[SessionPaths & Database & SessionSettings, Files] = ZLayer.derive[Live]

  def sha256(bytes: Chunk[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes.toArray).map(b => f"${b & 0xff}%02x").mkString

  final case class Live(paths: SessionPaths, db: Database, settings: SessionSettings) extends Files:
    def path(sha: String): Path = paths.files.resolve(sha.take(2)).resolve(sha)

    def put(bytes: Chunk[Byte]): IO[AppError, String] =
      val sha = sha256(bytes)
      val p   = path(sha)
      ZIO
        .attemptBlocking {
          if !JFiles.exists(p) then
            JFiles.createDirectories(p.getParent)
            val tmp = JFiles.createTempFile(p.getParent, ".up-", ".tmp")
            JFiles.write(tmp, bytes.toArray)
            JFiles.move(tmp, p, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING): Unit
          sha
        }
        .mapError(e => AppError.internal(s"存附件失败：${e.getMessage}", e))

    def read(sha: String): IO[AppError, Chunk[Byte]] =
      ZIO
        .attemptBlocking(Chunk.fromArray(JFiles.readAllBytes(path(sha))))
        .mapError {
          case _: java.nio.file.NoSuchFileException => AppError.notFound(s"附件内容不存在：$sha")
          case e                                    => AppError.internal(s"读附件失败：${e.getMessage}", e)
        }

    def upload(name: String, mediaType: String, bytes: Chunk[Byte]): IO[AppError, UploadRow] =
      for
        _   <- ZIO.when(bytes.size.toLong > settings.uploadMaxBytes)(ZIO.fail(AppError.invalid(s"文件太大：最多 ${settings.uploadMaxBytes} 字节")))
        _   <- ZIO.when(name.isBlank)(ZIO.fail(AppError.invalid("文件名不能为空")))
        sha <- put(bytes)
        now <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        row  = UploadRow(Ids.file, sha, Path.of(name).getFileName.toString, mediaType, bytes.size.toLong, now)
        _   <- db.write(Repo.insertUpload(row))
      yield row

    def find(ids: List[FileId]): IO[AppError, List[UploadRow]] =
      if ids.isEmpty then ZIO.succeed(Nil)
      else
        db.read(Repo.uploadsOf(ids)).flatMap { rows =>
          val found   = rows.map(r => r.id -> r).toMap
          val missing = ids.filterNot(found.contains)
          if missing.isEmpty then ZIO.succeed(ids.map(found))
          else ZIO.fail(AppError.invalid(s"附件不存在：${missing.map(_.value).mkString("、")}（先上传再引用）"))
        }
