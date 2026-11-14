package muliao.platform

import muliao.platform.Retry.*
import muliao.platform.http.{ProxyServer, ProxySettings}
import muliao.platform.log.RotatingFile
import zio.*
import zio.test.*

import java.nio.file.Files

object RetrySpec extends ZIOSpecDefault:
  def spec = suite("基础件")(
    suite("Retry")(
      test("暂时性失败按 2、4、8… 秒退避重试直到成功") {
        for
          n   <- Ref.make(0)
          eff  = n.updateAndGet(_ + 1).flatMap(i => if i < 4 then ZIO.fail(AppError.Unavailable("断网")) else ZIO.succeed(i))
          fib <- eff.retryTransient().fork
          _   <- TestClock.adjust(1.second)
          a   <- n.get
          _   <- TestClock.adjust(1.second)
          b   <- n.get
          _   <- TestClock.adjust(4.seconds + 8.seconds)
          r   <- fib.join
        yield assertTrue(a == 1, b == 2, r == 4)
      },
      test("非暂时性失败不重试") {
        for
          n <- Ref.make(0)
          e <- n.update(_ + 1).zipRight(ZIO.fail(AppError.invalid("参数不对"))).retryTransient().flip
          c <- n.get
        yield assertTrue(c == 1, e == AppError.invalid("参数不对"))
      },
      test("单次等待不超过上限，累计约 30 分钟后放弃") {
        for
          n   <- Ref.make(0)
          fib <- n.update(_ + 1).zipRight(ZIO.fail(AppError.Unavailable("断网"))).retryTransient().flip.fork
          _   <- TestClock.adjust(29.minutes)
          mid <- fib.poll
          _   <- TestClock.adjust(10.minutes)
          e   <- fib.join
          c   <- n.get
        // 2+4+…+256=510 秒（8 次）后每次 5 分钟：约 29 分钟时还在重试，30 分钟后放弃
        yield assertTrue(mid.isEmpty, e.isTransient, c >= 12, c <= 15)
      },
      test("外部给了 Retry-After 就至少等那么久") {
        for
          n   <- Ref.make(0)
          eff  = n.updateAndGet(_ + 1).flatMap(i => if i < 2 then ZIO.fail(AppError.Unavailable("限流", None, Some(60.seconds))) else ZIO.succeed(i))
          fib <- eff.retryTransient().fork
          _   <- TestClock.adjust(30.seconds)
          a   <- n.get
          _   <- TestClock.adjust(30.seconds)
          r   <- fib.join
        yield assertTrue(a == 1, r == 2)
      }
    ),
    suite("系统代理")(
      test("解析 scutil --proxy 输出") {
        val out =
          """<dictionary> {
            |  ExceptionsList : <array> {
            |    0 : 192.168.0.0/16
            |    1 : 169.254/16
            |    2 : *.local
            |    3 : localhost
            |    4 : example.org
            |  }
            |  ExcludeSimpleHostnames : 1
            |  HTTPEnable : 1
            |  HTTPPort : 6152
            |  HTTPProxy : 127.0.0.1
            |  HTTPSEnable : 1
            |  HTTPSPort : 6153
            |  HTTPSProxy : 10.0.0.2
            |  SOCKSEnable : 0
            |}""".stripMargin
        val p = ProxySettings.parse(out)
        assertTrue(
          p.http.contains(ProxyServer("127.0.0.1", 6152)),
          p.https.contains(ProxyServer("10.0.0.2", 6153)),
          p.exceptions.size == 5,
          p.forUrl("https", "api.anthropic.com").contains(ProxyServer("10.0.0.2", 6153)),
          p.forUrl("http", "api.anthropic.com").contains(ProxyServer("127.0.0.1", 6152)),
          p.forUrl("https", "192.168.1.10").isEmpty,
          p.forUrl("https", "169.254.3.4").isEmpty,
          p.forUrl("https", "printer.local").isEmpty,
          p.forUrl("https", "example.org").isEmpty,
          p.forUrl("https", "sub.example.org").nonEmpty,
          p.forUrl("https", "nas").isEmpty, // 简单主机名
          p.forUrl("https", "127.0.0.1").isEmpty,
          p.forUrl("https", "192.169.0.1").nonEmpty
        )
      },
      test("未启用的代理不用") {
        val p = ProxySettings.parse("<dictionary> {\n  HTTPEnable : 0\n  HTTPPort : 1\n  HTTPProxy : h\n}")
        assertTrue(p.forUrl("http", "a.com").isEmpty)
      }
    ),
    suite("日志文件")(
      test("超过上限按 .1 .2 轮转，只保留 keep 份") {
        for
          dir <- ZIO.succeed(Files.createTempDirectory("muliao-log"))
          f    = dir.resolve("m.log")
          _ <- ZIO.succeed {
                 val r = RotatingFile(f, 100, 2)
                 (1 to 20).foreach(i => r.writeLine(f"line $i%03d xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
                 r.close()
               }
          files <- ZIO.succeed(Files.list(dir).toArray.map(_.toString.split('/').last).toList.sorted)
          last  <- ZIO.succeed(Files.readString(f))
        yield assertTrue(files == List("m.log", "m.log.1", "m.log.2"), last.contains("line 020"), Files.size(f) <= 100)
      }
    )
  )
