package muliao.platform

import muliao.contract.StringId
import muliao.platform.db.*
import muliao.platform.db.Sql.*
import io.getquill.*
import zio.*
import zio.test.*
import zio.test.TestAspect.*

import java.nio.file.{Files, Path}

object ItemName extends StringId
type ItemName = ItemName.Type

final case class DemoItem(id: Long, name: ItemName, owner: String)

object DatabaseSpec extends ZIOSpecDefault:

  val demo = Migrations("demo")

  def tempDb: UIO[Path] = ZIO.succeed(Files.createTempDirectory("muliao-db").resolve("t.db"))

  def dbLayer(file: Path, migrations: Seq[Migrations] = Seq(demo)) =
    Database.layer(Database.Settings(file), migrations)

  def insert(name: String, owner: String): Tx[Long] =
    val item = DemoItem(0, ItemName.wrap(name), owner)
    Sql.run(query[DemoItem].insertValue(lift(item)).returningGenerated(_.id)).orInternal

  val all: Read[List[DemoItem]] = Sql.run(query[DemoItem].sortBy(_.id)).orInternal

  def byName(n: ItemName): Read[List[DemoItem]] = Sql.run(query[DemoItem].filter(_.name == lift(n))).orInternal

  def spec = suite("Database")(
    test("迁移、写入、读出；编号类型按底层类型存取") {
      for
        id    <- Database.write(insert("a", "x"))
        items <- Database.read(all)
        found <- Database.read(byName(ItemName.wrap("a")))
      yield assertTrue(id > 0, items.map(_.name.value) == List("a"), found.map(_.id) == List(id))
    }.provideLayer(ZLayer.fromZIO(tempDb).flatMap(f => dbLayer(f.get))),
    test("事务以领域错误失败时整体回滚，错误原样返回，提交后动作不执行") {
      for
        fired <- Ref.make(0)
        err <- Database
                 .write(
                   insert("a", "x") *> TxHooks.afterCommit(fired.update(_ + 1)) *> insert("b", "x") *>
                     ZIO.fail(AppError.conflict("不行"))
                 )
                 .flip
        items <- Database.read(all)
        n     <- fired.get
      yield assertTrue(err == AppError.conflict("不行"), items.isEmpty, n == 0)
    }.provideLayer(ZLayer.fromZIO(tempDb).flatMap(f => dbLayer(f.get))),
    test("SQL 出错算内部错误并回滚") {
      for
        _     <- Database.write(insert("a", "x"))
        err   <- Database.write(insert("b", "x") *> insert("a", "y")).flip // name 唯一
        items <- Database.read(all)
      yield assertTrue(err.isInstanceOf[AppError.Internal], items.map(_.name.value) == List("a"))
    }.provideLayer(ZLayer.fromZIO(tempDb).flatMap(f => dbLayer(f.get))),
    test("提交成功后按登记顺序执行提交后动作，此时数据已可读") {
      for
        seen <- Ref.make(List.empty[Int])
        db   <- ZIO.service[Database]
        _ <- Database.write(
               insert("a", "x") *>
                 TxHooks.afterCommit(db.read(all).orDie.flatMap(xs => seen.update(_ :+ xs.size))) *>
                 TxHooks.afterCommit(seen.update(_ :+ 100))
             )
        s <- seen.get
      yield assertTrue(s == List(1, 100))
    }.provideLayer(ZLayer.fromZIO(tempDb).flatMap(f => dbLayer(f.get))),
    test("「数据变了」只在提交后广播，回滚不广播") {
      for
        bus  <- ZIO.service[muliao.platform.events.ChangeBus]
        sub  <- ZIO.scoped(bus.subscribe.flatMap(_.take(1).runCollect)).fork
        _    <- ZIO.sleep(100.millis)
        _    <- Database.write(insert("a", "x") *> bus.publishAfterCommit(muliao.contract.Topic.wrap("rolled")) *> ZIO.fail(AppError.conflict("x"))).ignore
        _    <- Database.write(insert("b", "x") *> bus.publishAfterCommit(muliao.contract.Topic.wrap("items")))
        got  <- sub.join
      yield assertTrue(got.map(_.topic.value) == Chunk("items"))
    }.provideLayer(ZLayer.fromZIO(tempDb).flatMap(f => dbLayer(f.get)) ++ muliao.platform.events.ChangeBus.layer) @@ withLiveClock,
    test("并发写串行执行，不丢、不报忙") {
      for
        _     <- ZIO.foreachParDiscard(1 to 50)(i => Database.write(insert(s"n$i", "x") *> ZIO.sleep(1.milli)))
        items <- Database.read(all)
      yield assertTrue(items.size == 50)
    }.provideLayer(ZLayer.fromZIO(tempDb).flatMap(f => dbLayer(f.get))) @@ withLiveClock,
    test("读连接是只读的") {
      for err <- Database.read(Sql.run(query[DemoItem].delete).orInternal).flip
      yield assertTrue(err.isInstanceOf[AppError.Internal])
    }.provideLayer(ZLayer.fromZIO(tempDb).flatMap(f => dbLayer(f.get))),
    test("再次打开不重复迁移；库比程序新（有程序不认识的迁移）时拒绝启动") {
      for
        file <- tempDb
        _    <- ZIO.scoped(dbLayer(file).build)
        _    <- ZIO.scoped(dbLayer(file).build) // 第二次打开正常
        _ <- ZIO.attemptBlocking {
               val c = java.sql.DriverManager.getConnection(s"jdbc:sqlite:$file")
               try
                 c.createStatement.executeUpdate(
                   """INSERT INTO schema_history_demo (installed_rank, version, description, type, script, checksum, installed_by, execution_time, success)
                     |VALUES (99, '99', 'future', 'SQL', 'V99__future.sql', 0, 'x', 0, 1)""".stripMargin
                 ): Unit
               finally c.close()
             }.orDie
        err <- ZIO.scoped(dbLayer(file).build).flip
      yield assertTrue(err.message.contains("demo"))
    },
    test("迁移脚本出错时这个领域整体回滚") {
      for
        file <- tempDb
        err  <- ZIO.scoped(dbLayer(file, Seq(demo, Migrations("broken"))).build).flip
        tables <- ZIO.attemptBlocking {
                    val c = java.sql.DriverManager.getConnection(s"jdbc:sqlite:$file")
                    try
                      val rs = c.createStatement.executeQuery("SELECT name FROM sqlite_master WHERE type='table'")
                      Iterator.continually(rs).takeWhile(_.next()).map(_.getString(1)).toList
                    finally c.close()
                  }.orDie
      yield assertTrue(err.message.contains("broken"), tables.contains("demo_item"), !tables.contains("broken_one"))
    }
  )
