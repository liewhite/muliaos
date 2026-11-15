package muliao.platform.db

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import io.getquill.{MappedEncoding, SnakeCase, SqliteZioJdbcContext}
import muliao.contract.{IsLabeled, IsNewtype}
import muliao.platform.AppError
import org.flywaydb.core.Flyway
import zio.*

import java.nio.file.Path
import java.sql.SQLException
import javax.sql.DataSource

/** 全系统共用的 Quill 上下文（SQLite、蛇形命名）。领域代码 `import io.getquill.*` 与 `import Sql.*` 后写查询，再交给 [[Database]] 执行。 */
object Sql extends SqliteZioJdbcContext(SnakeCase):
  /** 所有 [[muliao.contract.Newtype]] 按底层类型存取，不必逐个类型写映射。 */
  // 用 implicit 而不是 given：领域代码 `import Sql.*` 就能带上，不必再 `import Sql.given`
  implicit def newtypeEncode[T, A](implicit n: IsNewtype[T, A]): MappedEncoding[T, A] = MappedEncoding(n.unwrap)
  implicit def newtypeDecode[T, A](implicit n: IsNewtype[T, A]): MappedEncoding[A, T] = MappedEncoding(n.wrap)

  /** 所有 [[muliao.contract.Labeled]] 枚举按 `label` 存取。库里出现程序不认识的写法是数据坏了（迁移保证库不比程序新），读出即报错。 */
  implicit def labeledEncode[E](implicit l: IsLabeled[E]): MappedEncoding[E, String] = MappedEncoding(l.label)
  implicit def labeledDecode[E](implicit l: IsLabeled[E]): MappedEncoding[String, E] =
    MappedEncoding(s => l.parse(s).fold(e => throw IllegalStateException(s"库里的值读不懂：$e"), identity))

/** 一次写事务里登记「提交成功后再做」的动作（如广播「数据变了」、投递通知）。回滚则不做。 */
final class TxHooks private (hooks: Ref[Chunk[UIO[Unit]]]):
  def afterCommit(effect: UIO[Unit]): UIO[Unit] = hooks.update(_ :+ effect)
  private[db] def drain: UIO[Chunk[UIO[Unit]]]  = hooks.getAndSet(Chunk.empty)

object TxHooks:
  private[db] def make: UIO[TxHooks] = Ref.make(Chunk.empty[UIO[Unit]]).map(TxHooks(_))

  def afterCommit(effect: UIO[Unit]): URIO[TxHooks, Unit] = ZIO.serviceWithZIO[TxHooks](_.afterCommit(effect))

/** 只读查询。 */
type Read[A] = ZIO[DataSource, AppError, A]

/** 写事务的内容：可读可写，可登记提交后动作；以 [[AppError]] 失败则整体回滚。 */
type Tx[A] = ZIO[DataSource & TxHooks, AppError, A]

/** 把 Quill 的 SQL 异常归为内部错误：`Sql.run(...).orInternal`。 */
extension [R, A](q: ZIO[R, SQLException, A])
  def orInternal: ZIO[R, AppError, A] = q.mapError(e => AppError.internal(s"数据库错误：${e.getMessage}", e))

/** SQLite 访问：单写者串行写（写连接只有一条，写事务排队），读走只读连接池，读写互不阻塞（WAL）。 */
trait Database:
  def read[A](q: Read[A]): IO[AppError, A]

  /** 在一个事务里执行；成功提交后依次执行登记的提交后动作（失败只记日志）。 */
  def write[A](tx: Tx[A]): IO[AppError, A]

object Database:
  final case class Settings(file: Path, readers: Int = 4)

  def read[A](q: Read[A]): ZIO[Database, AppError, A] = ZIO.serviceWithZIO[Database](_.read(q))
  def write[A](tx: Tx[A]): ZIO[Database, AppError, A] = ZIO.serviceWithZIO[Database](_.write(tx))

  /** 打开数据库并按顺序执行各领域的迁移。 */
  def layer(settings: Settings, migrations: Seq[Migrations]): ZLayer[Any, AppError, Database] =
    ZLayer.scoped {
      for
        writer <- pool(settings.file, "muliao-writer", 1, readOnly = false)
        _      <- Migrations.runAll(writer, migrations)
        reader <- pool(settings.file, "muliao-reader", settings.readers, readOnly = true)
        lock   <- Semaphore.make(1)
      yield Live(writer, reader, lock)
    }

  private final case class Live(writer: DataSource, reader: DataSource, lock: Semaphore) extends Database:
    def read[A](q: Read[A]): IO[AppError, A] = q.provideEnvironment(ZEnvironment(reader))

    def write[A](tx: Tx[A]): IO[AppError, A] =
      for
        hooks <- TxHooks.make
        a <- lock.withPermit(
               Sql
                 .transaction(tx.mapError(TxAbort(_)))
                 .provideEnvironment(ZEnvironment(writer).add(hooks))
                 .mapError {
                   case TxAbort(e) => e
                   case e          => AppError.internal(s"数据库事务失败：${e.getMessage}", e)
                 }
             )
        after <- hooks.drain
        _ <- ZIO.foreachDiscard(after)(
               _.catchAllCause(c => ZIO.logErrorCause("提交后动作失败", c))
             )
      yield a

  /** 事务体以领域错误中止时，借异常穿过 Quill 的 transaction，出来再还原。 */
  private final case class TxAbort(error: AppError) extends Exception(error.message, null, false, false)

  private def pool(file: Path, name: String, size: Int, readOnly: Boolean): ZIO[Scope, AppError, DataSource] =
    ZIO
      .fromAutoCloseable(ZIO.attemptBlocking {
        val c = HikariConfig()
        c.setPoolName(name)
        c.setJdbcUrl(s"jdbc:sqlite:${file.toAbsolutePath}")
        c.setMaximumPoolSize(size)
        c.setMinimumIdle(size)
        c.setReadOnly(readOnly)
        c.setAutoCommit(true)
        val props = java.util.Properties()
        props.setProperty("journal_mode", "WAL")
        props.setProperty("synchronous", "NORMAL")
        props.setProperty("foreign_keys", "true")
        props.setProperty("busy_timeout", "10000")
        if readOnly then props.setProperty("open_mode", "1"): Unit // SQLITE_OPEN_READONLY
        c.setDataSourceProperties(props)
        HikariDataSource(c)
      })
      .mapError(e => AppError.internal(s"打不开数据库 $file：${e.getMessage}", e))

/** 一个领域的表结构迁移：脚本放在 `resources/db/<domain>/V<n>__<说明>.sql`，版本记录在自己的表里，
  * 所以各领域的版本号互不干扰。一个领域待执行的迁移在同一事务里完成，失败整体回滚。
  */
final case class Migrations(domain: String)

object Migrations:
  private[db] def runAll(ds: DataSource, all: Seq[Migrations]): IO[AppError, Unit] =
    // 没有迁移目录的领域（没有自己的表）跳过，免得 Flyway 每次启动报「找不到迁移」
    ZIO.foreachDiscard(all.filter(m => getClass.getClassLoader.getResource(s"db/${m.domain}") != null)) { m =>
      ZIO
        .attemptBlocking(
          Flyway
            .configure(getClass.getClassLoader)
            .dataSource(ds)
            .locations(s"classpath:db/${m.domain}")
            .table(s"schema_history_${m.domain}")
            .group(true)
            // 各领域的版本记录表各自独立：库里已有别的领域的表时，从版本 0 起记，V1 起照常执行
            .baselineOnMigrate(true)
            .baselineVersion("0")
            .validateMigrationNaming(true)
            // 库里有程序不认识的迁移（库比程序新）时拒绝启动
            .ignoreMigrationPatterns("")
            .failOnMissingLocations(false)
            .load()
            .migrate()
        )
        .mapError(e => AppError.internal(s"数据库迁移失败（${m.domain}）：${e.getMessage}", e))
        .tap(r => ZIO.logInfo(s"迁移 ${m.domain}：执行 ${r.migrationsExecuted} 个，当前版本 ${Option(r.targetSchemaVersion).getOrElse("-")}"))
    }
