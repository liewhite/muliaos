package muliao.notifications

import muliao.notifications.push.VapidKeys
import zio.*

import java.nio.file.{Files, Paths}

/** 验收：读现有私钥文件的副本，打出公钥（订阅用的 applicationServerKey，非机密），与 `openssl pkey -pubout` 的结果对照。
  *
  * {{{
  * sbt "notifications/Test/runMain muliao.notifications.VapidKeyCheck <私钥文件>"
  * }}}
  * 只读原文件：先复制到临时目录再读（读的时候会把权限收成 600）。
  */
object VapidKeyCheck extends ZIOAppDefault:
  def run =
    getArgs.flatMap { args =>
      for
        dir  <- ZIO.succeed(Files.createTempDirectory("vapid-check"))
        copy  = dir.resolve("k.pem")
        _    <- ZIO.attemptBlocking(Files.copy(Paths.get(args.head), copy))
        keys <- VapidKeys.loadOrCreate(copy).ensuring(ZIO.attemptBlocking { Files.deleteIfExists(copy); Files.delete(dir) }.ignore)
        _    <- Console.printLine(s"PUBLIC ${keys.publicKeyB64}")
      yield ()
    }
