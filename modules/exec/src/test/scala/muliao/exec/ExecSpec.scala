package muliao.exec

import zio.*
import zio.stream.*
import zio.test.*
import zio.test.TestAspect.*

import java.nio.file.{Files, Path}

object ExecSpec extends ZIOSpecDefault:

  def tempDir: UIO[Path] = ZIO.succeed(Files.createTempDirectory("muliao-exec"))

  def shell(script: String, cwd: Path, sandbox: Sandbox = Sandbox.Unrestricted, secrets: Chunk[SecretEnv] = Chunk.empty) =
    CommandSpec(Program.Shell(script), cwd, sandbox, secrets = secrets)

  def alive(pid: Long): UIO[Boolean] =
    ZIO.attemptBlocking(ProcessBuilder("/bin/kill", "-0", pid.toString).start().waitFor() == 0).orElseSucceed(false)

  def spec = suite("执行层")(
    test("在指定目录执行，合并标准输出与错误，返回退出码") {
      for
        dir <- tempDir
        r   <- Exec.run(shell("pwd; echo out; echo err >&2; exit 3", dir), 10.seconds)
      yield assertTrue(
        r.ending == Ending.Exited(3),
        r.output == s"${dir.toRealPath()}\nout\nerr"
      )
    },
    test("逐行读：标准错误单独成行，标准输入每项一行写完就关；先打码再分行（带换行的 secret 也换得掉）") {
      val secrets = Chunk(SecretEnv("PEM", "line1\nline2"))
      for
        dir  <- tempDir
        seen <- Ref.make(Chunk.empty[Line])
        code <- Exec.follow(shell("""while read l; do echo "got:$l"; done; echo "oops" >&2; printf 'k=%s' "$PEM"; exit 4""", dir, secrets = secrets), Stderr.Separate, Chunk("one", "两"))(l =>
                  seen.update(_ :+ l)
                )
        all  <- seen.get
      yield assertTrue(
        code == 4,
        all.collect { case Line.Out(t) => t } == Chunk("got:one", "got:两", "k=[secret:PEM]"),
        all.collect { case Line.Err(t) => t } == Chunk("oops")
      )
    },
    test("输出里有不合法的 UTF-8 字节：换成 �，照常读完（不断管道、不被 SIGPIPE 杀掉）") {
      for
        dir <- tempDir
        r   <- Exec.run(shell("""printf 'a\xe4\xb8\n'; head -c 300000 /dev/urandom; printf '\nok\n'""", dir), 20.seconds)
      yield assertTrue(r.ending == Ending.Exited(0), r.output.startsWith("a\uFFFD"), r.output.endsWith("\nok"))
    },
    test("命令不读标准输入就退出也不出错") {
      for
        dir  <- tempDir
        code <- Exec.follow(shell("exit 0", dir), Stderr.Separate, Chunk.fill(20000)("x" * 10))(_ => ZIO.unit)
      yield assertTrue(code == 0)
    },
    test("命令自成进程组") {
      for
        dir <- tempDir
        r   <- Exec.run(shell("ps -o pgid= -p $$; echo $$", dir), 10.seconds)
        nums = r.output.linesIterator.map(_.trim).toList
      yield assertTrue(nums.size == 2, nums(0) == nums(1))
    },
    test("注入 secret，输出里打码") {
      for
        dir <- tempDir
        r <- Exec.run(
               shell("""echo "token=$API_TOKEN"; printf '%s' "$API_TOKEN" | fold -w 3""", dir, secrets = Chunk(SecretEnv("API_TOKEN", "s3cr3t-value"))),
               10.seconds
             )
      yield assertTrue(r.output.contains("token=[secret:API_TOKEN]"), !r.output.contains("s3cr3t-value"))
    },
    test("打码跨块：secret 被拆在两块里也能换掉，代理字符不被拆开") {
      val m = Masker(Chunk(SecretEnv("K", "abcdef")))
      for
        out <- ZStream("xxab", "cd", "efyy😀", "ab").via(m.pipeline).runCollect
      yield assertTrue(out.mkString == "xx[secret:K]yy😀ab", SecretEnv("K", "abcdef").toString == "SecretEnv(K)")
    },
    test("超时结束整个进程组（含子进程）") {
      for
        dir   <- tempDir
        pidf   = dir.resolve("child.pid")
        start <- Clock.nanoTime
        r     <- Exec.run(shell(s"sleep 30 & echo $$! > $pidf; wait", dir), 1.second)
        took  <- Clock.nanoTime.map(n => (n - start) / 1000000)
        child <- ZIO.attemptBlocking(Files.readString(pidf).trim.toLong).orDie
        _     <- ZIO.sleep(200.millis)
        live  <- alive(child)
      yield assertTrue(r.ending == Ending.TimedOut, !live, took < 8000)
    } @@ withLiveClock,
    test("主进程退出后不等占着输出的后台子进程：限时读完就返回，子进程不被杀") {
      for
        dir   <- tempDir
        pidf   = dir.resolve("bg.pid")
        start <- Clock.nanoTime
        r     <- Exec.run(shell(s"echo before; (sleep 20; echo late) & echo $$! > $pidf; echo done", dir), 10.seconds)
        took  <- Clock.nanoTime.map(n => (n - start) / 1000000)
        bg    <- ZIO.attemptBlocking(Files.readString(pidf).trim.toLong).orDie
        live  <- alive(bg)
        _     <- ZIO.attemptBlocking(ProcessBuilder("/bin/kill", bg.toString).start().waitFor()).ignore
      yield assertTrue(r.ending == Ending.Exited(0), r.output.contains("before"), r.output.contains("done"), !r.output.contains("late"), took < 6000, live)
    } @@ withLiveClock,
    test("写沙箱：只能写给定目录（按真实路径），别处被系统拦下") {
      for
        root    <- tempDir
        allowed <- ZIO.succeed(Files.createDirectories(root.resolve("ok")))
        denied  <- ZIO.succeed(Files.createDirectories(root.resolve("no")))
        real    <- RealPath.resolve(allowed)
        r <- Exec.run(
               shell(s"echo a > '$allowed/f' && echo wrote-ok; echo b > '$denied/f' || echo blocked; echo c > /dev/null && echo devnull-ok", root, Sandbox.Confined(Chunk(real))),
               10.seconds
             )
      yield assertTrue(
        r.output.contains("wrote-ok"),
        r.output.contains("blocked"),
        r.output.contains("Operation not permitted"),
        r.output.contains("devnull-ok"),
        Files.exists(allowed.resolve("f")),
        !Files.exists(denied.resolve("f"))
      )
    },
    test("真实路径：展开符号链接、按磁盘大小写，不存在的部分接在后面") {
      for
        root <- tempDir
        dir  <- ZIO.succeed(Files.createDirectories(root.resolve("MixedCase")))
        link <- ZIO.succeed(Files.createSymbolicLink(root.resolve("link"), dir))
        a    <- RealPath.resolve(root.resolve("mixedcase"))
        b    <- RealPath.resolve(link.resolve("not/yet"))
        base <- RealPath.resolve(dir)
      yield assertTrue(
        a.path.getFileName.toString == "MixedCase",
        b.path == dir.toRealPath().resolve("not/yet"),
        RealPath.within(b, base),
        !RealPath.within(base, b)
      )
    },
    test("沙箱：私有路径读写都不行，即使在可写目录里") {
      for
        root    <- tempDir
        secret  <- ZIO.succeed(Files.createDirectories(root.resolve("private")))
        _       <- ZIO.succeed(Files.writeString(secret.resolve("key"), "k"))
        rootR   <- RealPath.resolve(root)
        secretR <- RealPath.resolve(secret)
        r <- Exec.run(
               shell(s"cat '$secret/key' || echo noread; echo x > '$secret/new' || echo nowrite; echo y > '$root/ok' && echo wrote", root, Sandbox.Confined(Chunk(rootR), Chunk(secretR))),
               10.seconds
             )
      yield assertTrue(r.output.contains("noread"), r.output.contains("nowrite"), r.output.contains("wrote"), !Files.exists(secret.resolve("new")))
    },
    test("沙箱：可写目录里收回写权限的部分能读不能写") {
      for
        root  <- tempDir
        ro    <- ZIO.succeed(Files.createDirectories(root.resolve("ro")))
        _     <- ZIO.succeed(Files.writeString(ro.resolve("f"), "keep"))
        rootR <- RealPath.resolve(root)
        roR   <- RealPath.resolve(ro)
        r <- Exec.run(
               shell(s"cat '$ro/f'; echo; echo x > '$ro/f' || echo nowrite; echo y > '$root/ok' && echo wrote", root, Sandbox.Confined(Chunk(rootR), readOnly = Chunk(roR))),
               10.seconds
             )
      yield assertTrue(r.output.contains("keep"), r.output.contains("nowrite"), r.output.contains("wrote"), Files.readString(ro.resolve("f")) == "keep")
    },
    test("沙箱：setuid 程序（ps、top）放到沙箱外执行，照常能跑") {
      for
        root  <- tempDir
        rootR <- RealPath.resolve(root)
        r <- Exec.run(
               shell("ps -o pid= -p $$ && echo ps-ok; top -l 1 -n 0 >/dev/null && echo top-ok", root, Sandbox.Confined(Chunk(rootR))),
               20.seconds
             )
      yield assertTrue(r.output.contains("ps-ok"), r.output.contains("top-ok"), !r.output.contains("Operation not permitted"))
    },
    test("沙箱规则：先禁止全部写，再放开设备文件") {
      val p = Sandbox.profile(Sandbox.Confined(Chunk.empty))
      assertTrue(p.contains("(deny file-write*)"), p.contains("/dev/null"))
    },
    test("写检查与沙箱规则同一优先级：私有 > 只读 > 可写，默认不可写") {
      for
        dir <- tempDir
        w   <- RealPath.resolve(dir)
        ro  <- RealPath.resolve(dir.resolve("ro"))
        hid <- RealPath.resolve(dir.resolve("ro/hidden"))
        out <- RealPath.resolve(dir.getParent)
        f    = (p: java.nio.file.Path) => RealPath.resolve(p)
        a   <- f(dir.resolve("a"))
        b   <- f(dir.resolve("ro/b"))
        c   <- f(dir.resolve("ro/hidden/c"))
        box  = new Sandbox.Confined(Chunk(w), hidden = Chunk(hid), readOnly = Chunk(ro))
      yield assertTrue(box.permits(a), !box.permits(b), !box.permits(c), box.hides(c), !box.hides(b), !box.permits(out))
    },
    test("被中断时还在跑的命令整组结束") {
      for
        dir   <- tempDir
        pid   <- Promise.make[Nothing, Long]
        fiber <- Exec.follow(shell("(sleep 30) & echo $!; sleep 30", dir), Stderr.Merged)(l => pid.succeed(l.text.trim.toLong)).fork
        child <- pid.await
        _     <- fiber.interrupt
        _     <- ZIO.sleep(200.millis)
        live  <- alive(child)
      yield assertTrue(!live)
    } @@ withLiveClock,
    test("对话式命令：按行收发，标准错误打码，scope 结束整组结束") {
      val secrets = Chunk(SecretEnv("TOKEN", "s3cr3t-value"))
      for
        dir <- tempDir
        out <- ZIO.scoped {
                 for
                   c     <- Exec.attach(shell("""echo "err $TOKEN" >&2; while read l; do echo "got:$l"; done""", dir, secrets = secrets))
                   _     <- c.send("one") *> c.send("两")
                   lines <- c.stdout.take(2).runCollect
                   err   <- c.stderr.take(1).runCollect
                 yield (c.pid, lines, err)
               }
        (pid, lines, err) = out
        _    <- ZIO.sleep(100.millis)
        live <- alive(pid)
      yield assertTrue(lines == Chunk("got:one", "got:两"), err == Chunk("err [secret:TOKEN]"), !live)
    } @@ withLiveClock
  ) @@ timeout(60.seconds)
