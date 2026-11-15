package muliao.migrate

import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.contract.{Assignee, Labeled}
import zio.*

import java.nio.file.Path
import java.sql.{Connection, DriverManager, PreparedStatement, Types}

/** 新版的库：先用各领域自己的迁移建表（与新版启动时完全一样），再在一个事务里批量写入；中途失败整体回滚。 */
object NewDb:
  /** 新版全部有表的领域（与组装入口 `Assembly` 的领域列表一致）。 */
  private val domains = List(
    muliao.resources.Module,
    muliao.models.Module,
    muliao.sessions.Module,
    muliao.workspace.Module,
    muliao.notifications.Module,
    muliao.board.Module,
    muliao.triggers.Module,
    muliao.history.Module
  )

  def create(file: Path): IO[AppError, Unit] =
    ZIO.scoped(Database.layer(Database.Settings(file), domains.map(_.migrations)).build).unit

  def open(file: Path): Writer =
    val c = DriverManager.getConnection(s"jdbc:sqlite:${file.toAbsolutePath}")
    c.setAutoCommit(false)
    Writer(c)

  /** 按行类型写：表的列名就是行字段名的蛇形（与新版 Quill 的命名一致），编号类型在运行时就是底层值。 */
  final class Writer(conn: Connection) extends AutoCloseable:
    def close(): Unit = conn.close()
    def commit(): Unit = conn.commit()

    def insert[A <: Product](table: String, rows: Iterable[A]): Long =
      rows.headOption.fold(0L) { first =>
        val cols = first.productElementNames.map(snake).toList
        val sql  = s"INSERT INTO $table (${cols.mkString(", ")}) VALUES (${cols.map(_ => "?").mkString(", ")})"
        val st   = conn.prepareStatement(sql)
        try
          var n = 0L
          rows.foreach { r =>
            r.productIterator.zipWithIndex.foreach((v, i) => set(st, i + 1, v))
            st.addBatch()
            n += 1
            if n % 2000 == 0 then st.executeBatch(): Unit
          }
          st.executeBatch(): Unit
          n
        finally st.close()
      }

    def count(table: String): Long =
      val st = conn.createStatement()
      try
        val rs = st.executeQuery(s"SELECT count(*) FROM $table")
        rs.next()
        rs.getLong(1)
      finally st.close()

    private def set(st: PreparedStatement, i: Int, v: Any): Unit = v match
      case None          => st.setNull(i, Types.NULL)
      case Some(x)       => set(st, i, x)
      case null          => st.setNull(i, Types.NULL)
      case b: Boolean    => st.setInt(i, if b then 1 else 0)
      case s: Labeled    => st.setString(i, s.label)
      case a: Assignee   => st.setString(i, Assignee.encode(a))
      case x             => st.setObject(i, x)

  private def snake(name: String): String = name.flatMap(c => if c.isUpper then s"_${c.toLower}" else c.toString)
