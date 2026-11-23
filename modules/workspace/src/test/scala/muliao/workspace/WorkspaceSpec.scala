package muliao.workspace

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.{AgentExec, ResourcePaths}
import muliao.resources.tree.Owner
import muliao.resources.vault.Vault
import muliao.sessions.{Answer, CallKey, Files, SessionConsole, Sessions}
import muliao.sessions.TestEnv as S
import muliao.sessions.TestEnv.{Script, call as modelCall, text as say}
import muliao.workspace.TestEnv.*
import muliao.workspace.jobs.Jobs
import muliao.workspace.pages.{PageGrant, Pages}
import muliao.workspace.scripts.ScriptGrants
import muliao.workspace.store.Repo
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files as JFiles, Path}
import java.nio.file.attribute.PosixFilePermissions

/** 工作区领域：文件工具、沙箱拦截说明、命令、后台任务的生命周期与重启、skill 脚本的批准、页面与浏览。 */
object WorkspaceSpec extends ZIOSpecDefault:

  /** 在 `d` 上起一套环境跑 `body`（同一个 `d` 再起一次就是重启）。 */
  private def withEnv[A](d: S.Dirs, m: S.FakeModels)(body: ZIO[Env & Scope, AppError, A]): IO[AppError, A] =
    ZIO.scoped(layer(d, m).build.flatMap(env => body.provideSomeEnvironment[Scope](_ ++ env)))

  private def args(fields: (String, Json)*): String = Json.Obj(Chunk.fromIterable(fields)).toJson
  private def str(s: String): Json                  = Json.Str(s)

  private def workspaceOf(id: AgentId): URIO[ResourcePaths, Path] = ZIO.serviceWith[ResourcePaths](_.workspace(id))

  private def jobRow(id: Long): ZIO[Database, AppError, store.JobRow] =
    ZIO.serviceWithZIO[Database](_.read(Repo.job(JobId.wrap(id)))).someOrFail(AppError.Internal("没有这个任务"))

  /** 模型：按最后一段用户方文字（含工具结果）决定怎么回。 */
  private def model(rules: PartialFunction[String, Script]): UIO[S.FakeModels] =
    S.fakeModels(req => rules.applyOrElse(S.lastUserText(req), (_: String) => Script.Say(List(say("好")))))

  private def sawText(m: S.FakeModels, what: String): IO[AppError, Unit] =
    S.eventually(s"模型看到「$what」")(m.requests)(_.exists(r => S.userTexts(r).exists(_.contains(what)))).unit

  def spec = suite("工作区")(
    test("读写文件：cat -n、多段读取、图片、二进制；write_file 自动建父目录") {
      for
        d <- S.dirs
        m <- model(PartialFunction.empty)
        out <- withEnv(d, m) {
                 for
                   alice <- S.agent("alice")
                   ctx   <- context(alice)
                   w     <- call("write_file", args("path" -> str("a/b/c.txt"), "content" -> str("x\ny\nz\n")), ctx)
                   ws    <- workspaceOf(alice)
                   _     <- ZIO.attemptBlocking(JFiles.write(ws.resolve("p.png"), Array(0x89, 0x50, 0x4e, 0x47, 1, 2).map(_.toByte))).orDie
                   _     <- ZIO.attemptBlocking(JFiles.write(ws.resolve("bin"), Array[Byte](1, 0, 2))).orDie
                   r1    <- call("read_file", args("path" -> str("a/b/c.txt"), "offset" -> Json.Num(2), "limit" -> Json.Num(1)), ctx)
                   r2 <- call(
                           "read_file",
                           args("reads" -> Json.Arr(Json.Obj("path" -> str("a/b/c.txt")), Json.Obj("path" -> str("nope.txt")), Json.Obj("path" -> str("p.png")))),
                           ctx
                         )
                   img   <- call("read_file", args("path" -> str("p.png")), ctx)
                   bin   <- call("read_file", args("path" -> str("bin")), ctx)
                   both  <- call("read_file", args("path" -> str("a/b/c.txt"), "reads" -> Json.Arr(Json.Obj("path" -> str("x")))), ctx)
                 yield (text(w), text(r1), text(r2), text(img), text(bin), text(both))
               }
        (w, r1, r2, img, bin, both) = out
      yield assertTrue(
        w.startsWith("已写入 a/b/c.txt（3 行"),
        r1 == "     2\ty\n[还有 1 行；从 offset=3 继续读]",
        r2.contains("==> a/b/c.txt lines 1-3 of 3 <==\n     1\tx"),
        r2.contains("==> nope.txt: "),
        r2.contains("==> p.png: 是图片"),
        img == "[image:image/png]",
        bin.startsWith("[rejected]") && bin.contains("二进制"),
        both.startsWith("[rejected]")
      )
    },
    test("edit_file：多文件一次改，任何一处失败都不写；成功回显改动处") {
      for
        d <- S.dirs
        m <- model(PartialFunction.empty)
        out <- withEnv(d, m) {
                 for
                   alice <- S.agent("alice")
                   ctx   <- context(alice)
                   ws    <- workspaceOf(alice)
                   _     <- write(ws.resolve("a.txt"), "alpha\nbeta\n")
                   _     <- write(ws.resolve("b.txt"), "gamma\n")
                   bad <- call(
                            "edit_file",
                            args("edits" -> Json.Arr(
                              Json.Obj("path" -> str("a.txt"), "old_text" -> str("beta"), "new_text" -> str("BETA")),
                              Json.Obj("path" -> str("b.txt"), "old_text" -> str("nothing"), "new_text" -> str("x"))
                            )),
                            ctx
                          )
                   a0 <- read(ws.resolve("a.txt"))
                   ok <- call(
                           "edit_file",
                           args("path" -> str("a.txt"), "edits" -> Json.Arr(
                             Json.Obj("old_text" -> str("beta"), "new_text" -> str("BETA")),
                             Json.Obj("start_line" -> Json.Num(1), "end_line" -> Json.Num(1), "anchor" -> str("alpha"), "new_text" -> str("ALPHA")),
                             Json.Obj("path" -> str("b.txt"), "old_text" -> str("gamma"), "new_text" -> str("GAMMA"))
                           )),
                           ctx
                         )
                   a1 <- read(ws.resolve("a.txt"))
                   b1 <- read(ws.resolve("b.txt"))
                   mixed <- call("edit_file", args("path" -> str("a.txt"), "old_text" -> str("x"), "new_text" -> str("y"), "edits" -> Json.Arr()), ctx)
                 yield (text(bad), a0, text(ok), a1, b1, text(mixed))
               }
        (bad, a0, ok, a1, b1, mixed) = out
      yield assertTrue(
        bad.startsWith("[rejected] b.txt：") && bad.contains("没有改任何文件"),
        a0 == "alpha\nbeta\n",
        ok.contains("edited a.txt (2 replacements; now 2 lines)"),
        ok.contains("     1\tALPHA"),
        ok.contains("edited b.txt (1 replacements; now 1 lines)"),
        a1 == "ALPHA\nBETA\n",
        b1 == "GAMMA\n",
        mixed.startsWith("[rejected]") // 给了 edits 又给了顶层 old_text：当作没给 edits，new_text 照常；这里 edits 为空所以按单处处理 → 找不到 x
      )
    },
    test("setuid 程序按文件权限位认出（它们在沙箱里报的 Operation not permitted 与写权限无关）") {
      assertTrue(AgentExec.setuid(Path.of("/bin/ps")), !AgentExec.setuid(Path.of("/bin/echo")), !AgentExec.setuid(Path.of("/no/such")))
    },
    test("写沙箱：文件工具写不了范围外、读不了平台私有文件，命令被拦时补说明；命令环境带工作目录路径") {
      for
        d    <- S.dirs
        m    <- model(PartialFunction.empty)
        mark  = s"muliao-ws-test-${java.lang.System.nanoTime}"
        out <- withEnv(d, m) {
                 for
                   alice <- S.agent("alice")
                   ctx   <- context(alice)
                   priv  <- write(d.data.resolve("private").resolve("key.pem"), "secret")
                   w     <- call("write_file", args("path" -> str(s"/tmp/$mark"), "content" -> str("x")), ctx)
                   r     <- call("read_file", args("path" -> str(priv.toString)), ctx)
                   sh    <- call("run_shell", args("command" -> str(s"touch /tmp/$mark; echo done"), "timeout" -> Json.Num(20)), ctx)
                   own   <- call("run_shell", args("command" -> str("echo hi > own.txt && cd / && cat \"$MULIAO_WORKSPACE/own.txt\""), "timeout" -> Json.Num(20)), ctx)
                   rd    <- call("run_shell", args("command" -> str(s"cat $priv"), "timeout" -> Json.Num(20)), ctx)
                 yield (text(w), text(r), text(sh), text(own), text(rd))
               }
        (w, r, sh, own, rd) = out
        priv = d.data.resolve("private").resolve("key.pem")
        leaked <- ZIO.attemptBlocking(JFiles.exists(Path.of(s"/tmp/$mark"))).orDie
      yield assertTrue(
        w.startsWith("[rejected] 写不了") && w.contains("不在你的可写范围"),
        r.startsWith("[rejected] 读不了") && r.contains("平台私有文件"),
        sh.startsWith("exit_code: 0"),
        sh.contains("Operation not permitted"),
        sh.contains(s"[沙箱拦下了 /tmp/$mark：不在你的可写范围"),
        !leaked,
        rd.contains(s"[沙箱拦下了 $priv：这是平台私有文件"), // 读被拒也按私有文件说明，不引向 write:path
        own == "exit_code: 0\nhi\n" || own == "exit_code: 0\nhi"
      )
    },
    test("资源文件经写入口：整体覆盖要先读过，记忆超长被拒，改 skill 脚本不丢可执行位") {
      for
        d <- S.dirs
        m <- model(PartialFunction.empty)
        out <- withEnv(d, m) {
                 for
                   alice  <- S.agent("alice")
                   ctx    <- context(alice, run = 7)
                   mem     = d.assets.resolve("alice/memory.md")
                   _      <- write(mem, "旧的")
                   blind  <- call("write_file", args("path" -> str(mem.toString), "content" -> str("新的")), ctx)
                   _      <- call("read_file", args("path" -> str(mem.toString)), ctx)
                   seen   <- call("write_file", args("path" -> str(mem.toString), "content" -> str("新的")), ctx)
                   again  <- call("write_file", args("path" -> str(mem.toString), "content" -> str("再新")), ctx) // 自己刚写过，不用重读
                   other  <- context(alice, run = 8)
                   stale  <- call("write_file", args("path" -> str(mem.toString), "content" -> str("别的运行")), other)
                   long   <- call("edit_file", args("path" -> str(mem.toString), "old_text" -> str("再新"), "new_text" -> str("长" * 5000)), ctx)
                   skill   = d.assets.resolve("alice/skills/demo")
                   _      <- write(skill.resolve("SKILL.md"), "---\ndescription: 演示\n---\n正文\n")
                   script <- write(skill.resolve("run.sh"), "#!/bin/bash\necho one\n")
                   _      <- ZIO.attemptBlocking(JFiles.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))).orDie
                   ed     <- call("edit_file", args("path" -> str(script.toString), "old_text" -> str("one"), "new_text" -> str("two")), ctx)
                   exe    <- ZIO.attemptBlocking(JFiles.isExecutable(script)).orDie
                   body   <- read(mem)
                 yield (text(blind), text(seen), text(again), text(stale), text(long), text(ed), exe, body)
               }
        (blind, seen, again, stale, long, ed, exe, body) = out
      yield assertTrue(
        blind.startsWith("[rejected]") && blind.contains("先在本次运行里 read_file"),
        seen.startsWith("已写入"),
        again.startsWith("已写入"),
        stale.startsWith("[rejected]"),
        long.startsWith("[rejected]") && long.contains("超过上限"),
        ed.startsWith("edited"),
        exe,
        body.trim == "再新" // 记忆经写入口规整（末尾补换行）
      )
    },
    test("run_shell：secret 注入并打码、前台必须给超时、超时结束整组；share_file 存成附件") {
      for
        d <- S.dirs
        m <- model(PartialFunction.empty)
        out <- withEnv(d, m) {
                 for
                   alice <- S.agent("alice")
                   _     <- ZIO.serviceWithZIO[Vault](_.set("TOKEN", muliao.resources.vault.SecretInput.ByUser(muliao.contract.SecretValue.Server("sekret-123", muliao.contract.SecretShape.Default), Owner.Agent(AgentName.wrap("alice"))), Some("测试")))
                   ctx   <- context(alice)
                   sec   <- call("run_shell", args("command" -> str("echo token=$TOKEN"), "timeout" -> Json.Num(20), "secrets" -> Json.Arr(str("TOKEN"))), ctx)
                   no    <- call("run_shell", args("command" -> str("echo hi")), ctx)
                   slow  <- call("run_shell", args("command" -> str("echo before; sleep 30"), "timeout" -> Json.Num(1)), ctx)
                   ws    <- workspaceOf(alice)
                   _     <- write(ws.resolve("report.md"), "# 报告\n")
                   sh    <- call("share_file", args("path" -> str("report.md")), ctx)
                   view   = text(sh).fromJson[FileView].toOption.get
                   found <- ZIO.serviceWithZIO[Files](_.find(List(view.id)))
                 yield (text(sec), text(no), text(slow), view, found)
               }
        (sec, no, slow, view, found) = out
      yield assertTrue(
        sec == "exit_code: 0\ntoken=[secret:TOKEN]\n" || sec == "exit_code: 0\ntoken=[secret:TOKEN]",
        no.startsWith("[rejected]") && no.contains("timeout"),
        slow.startsWith("killed after 1s timeout") && slow.contains("before"),
        view.name == "report.md",
        view.size == "# 报告\n".getBytes("UTF-8").length.toLong,
        found.head.mediaType.nonEmpty
      )
    },
    test("后台任务：会话等待中，心跳检查与结局作为事件回来，日志在工作目录") {
      for
        d <- S.dirs
        m <- model {
               case t if t.startsWith("跑后台") =>
                 Script.Say(List(modelCall("b1", "run_shell", args("command" -> str("echo start; sleep 2.5; echo end"), "background" -> Json.Bool(true), "heartbeat_seconds" -> Json.Num(1)))))
               case t if t.contains("[检查]") => Script.Say(List(say("…")))
             }
        out <- withEnv(d, m) {
                 for
                   _      <- start
                   topics <- Ref.make(Chunk.empty[Topic])
                   ready  <- Promise.make[Nothing, Unit]
                   bus    <- ZIO.service[ChangeBus]
                   sub    <- ZIO.scoped(bus.subscribe.flatMap(s => ready.succeed(()) *> s.runForeach(c => topics.update(_ :+ c.topic)))).fork
                   _      <- ready.await
                   alice <- S.agent("alice")
                   id    <- S.userSession(alice, "跑后台")
                   _     <- S.eventually("等后台任务")(S.stateOf(id))(_ == WorkState.Waiting)
                   _     <- sawText(m, "[检查] 后台任务 1")
                   _     <- sawText(m, "[后台任务 1 结束] 退出码 0")
                   _     <- S.idle(id)
                   row   <- jobRow(1)
                   ws    <- workspaceOf(alice)
                   log   <- read(ws.resolve("jobs/1.log"))
                   views <- ZIO.serviceWithZIO[Jobs](_.list(alice))
                   _     <- sub.interrupt
                   seen  <- topics.get
                 yield (row, log, views, seen)
               }
        (row, log, views, seen) = out
        reqs <- m.requests
      yield
        val first = S.userTexts(reqs(1)).last
        val end   = reqs.flatMap(S.userTexts).find(_.contains("[后台任务 1 结束]")).get
        assertTrue(
          first.startsWith("已在后台运行，任务号 1"),
          row.state == JobStatus.Exited,
          row.exitCode.contains(0),
          row.reported,
          log == "start\nend\n",
          end.contains("end"),
          views.map(_.log) == List("jobs/1.log"),
          seen.count(_ == Topic.Jobs) == 2 // 开始、结束各广播一次
        )
    },
    test("后台任务正常结束后不留下挂着的 fiber（否则服务关停时等所有 root fiber 会卡死，#897）") {
      // 前提：本 spec 顺序执行、测试按模块单独跑一个 JVM，所以 Jobs.scala 里 fork 的 root fiber 只有本用例的；去掉 sequential 可能偶发失败
      val jobFibers: UIO[Chunk[String]] =
        Fiber.roots.flatMap(fs => ZIO.filter(fs.filter(_.location.toString.contains("Jobs.scala")))(_.poll.map(_.isEmpty))).map(_.map(_.location.toString))
      for
        d <- S.dirs
        m <- model { case t if t.startsWith("跑个快的") => Script.Say(List(modelCall("b1", "run_shell", args("command" -> str("echo hi"), "background" -> Json.Bool(true))))) }
        left <- withEnv(d, m) {
                  for
                    _     <- start
                    alice <- S.agent("alice")
                    _     <- S.userSession(alice, "跑个快的")
                    _     <- sawText(m, "[后台任务 1 结束] 退出码 0")
                    left  <- S.eventually("后台任务的 fiber 都结束")(jobFibers)(_.isEmpty)
                  yield left
                }
      yield assertTrue(left.isEmpty)
    },
    test("后台任务：job_kill 与停止会话都结束整组进程") {
      for
        d <- S.dirs
        m <- model {
               case t if t.startsWith("起一个") =>
                 Script.Say(List(modelCall("b1", "run_shell", args("command" -> str("sleep 30; echo late > late.txt"), "background" -> Json.Bool(true)))))
               case t if t.startsWith("杀掉") => Script.Say(List(modelCall("k1", "job_kill", args("job_id" -> Json.Num(1)))))
             }
        out <- withEnv(d, m) {
                 for
                   _     <- start
                   alice <- S.agent("alice")
                   a     <- S.userSession(alice, "起一个")
                   _     <- S.eventually("等后台任务")(S.stateOf(a))(_ == WorkState.Waiting)
                   _     <- S.say(a, "杀掉")
                   _     <- sawText(m, "[后台任务 1 被结束]")
                   _     <- S.idle(a)
                   b     <- S.userSession(alice, "起一个")
                   _     <- S.eventually("等后台任务")(S.stateOf(b))(_ == WorkState.Waiting)
                   _     <- ZIO.serviceWithZIO[Sessions](_.stop(b))
                   r2    <- S.eventually("任务被结束")(jobRow(2))(_.state == JobStatus.Killed)
                   r1    <- jobRow(1)
                   n     <- ZIO.serviceWithZIO[Jobs](_.running(alice))
                 yield (r1, r2, n)
               }
        (r1, r2, n) = out
      yield assertTrue(r1.state == JobStatus.Killed, r2.state == JobStatus.Killed, n == 0)
    },
    test("重启：关停时结束后台任务并标为被中断，重启后告诉发起会话") {
      for
        d <- S.dirs
        m <- model {
               case t if t.startsWith("长任务") =>
                 Script.Say(List(modelCall("b1", "run_shell", args("command" -> str("sleep 2; echo late > late.txt"), "background" -> Json.Bool(true)))))
             }
        alice <- withEnv(d, m) {
                   for
                     _     <- start
                     alice <- S.agent("alice")
                     id    <- S.userSession(alice, "长任务")
                     _     <- S.eventually("等后台任务")(S.stateOf(id))(_ == WorkState.Waiting)
                   yield alice
                 }
        _ <- ZIO.sleep(3.seconds)
        out <- withEnv(d, m) {
                 for
                   before <- jobRow(1)
                   _      <- start
                   _      <- sawText(m, "[后台任务 1 被中断]")
                   after  <- jobRow(1)
                   ws     <- workspaceOf(alice)
                   late   <- ZIO.attemptBlocking(JFiles.exists(ws.resolve("late.txt"))).orDie
                 yield (before, after, late)
               }
        (before, after, late) = out
      yield assertTrue(before.state == JobStatus.Interrupted, !before.reported, after.reported, !late)
    },
    test("skill 脚本：要批准的等用户，始终允许后同样的调用直接跑，skill 文件改了又要批准；拒绝不执行") {
      for
        d <- S.dirs
        m <- model {
               case t if t.startsWith("部署") =>
                 Script.Say(List(modelCall("s1", "skill_run", args("skill" -> str("deploy"), "script" -> str("scripts/go.sh"), "args" -> Json.Arr(str("prod")), "timeout" -> Json.Num(20)))))
               case t if t.startsWith("看看") =>
                 Script.Say(List(modelCall("s2", "skill_run", args("skill" -> str("deploy"), "script" -> str("scripts/info.sh"), "timeout" -> Json.Num(20)))))
             }
        out <- withEnv(d, m) {
                 val skill = d.assets.resolve("alice/skills/deploy")
                 def approve(id: SessionId, ok: Boolean, always: Boolean) =
                   for
                     det <- S.eventually("等批准")(ZIO.serviceWithZIO[SessionConsole](_.detail(id)))(_.pending.exists(_.pending.isInstanceOf[PendingView.Approval]))
                     p    = det.pending.head
                     _   <- ZIO.serviceWithZIO[SessionConsole](_.answer(id, CallKey(p.entry, p.call), Answer.Decision(ok, always)))
                     _   <- S.idle(id)
                   yield p.pending match
                     case PendingView.Approval(s) => s
                     case _                       => ""
                 for
                   _     <- start
                   alice <- S.agent("alice")
                   _     <- write(skill.resolve("scripts/go.sh"), "#!/bin/bash\necho deployed $1\n")
                   _     <- write(skill.resolve("scripts/info.sh"), "#!/bin/bash\necho info\n")
                   _     <- ZIO.foreachDiscard(List("go.sh", "info.sh"))(f => ZIO.attemptBlocking(JFiles.setPosixFilePermissions(skill.resolve(s"scripts/$f"), PosixFilePermissions.fromString("rwxr-xr-x"))).orDie)
                   _     <- write(skill.resolve("SKILL.md"), "---\ndescription: 部署\napproval: [scripts/go.sh]\n---\n正文\n")
                   free  <- S.userSession(alice, "看看")
                   _     <- S.idle(free)
                   s1    <- S.userSession(alice, "部署 1")
                   sum   <- approve(s1, ok = true, always = true)
                   s2    <- S.userSession(alice, "部署 2")
                   _     <- S.idle(s2)
                   p2    <- ZIO.serviceWithZIO[SessionConsole](_.detail(s2)).map(_.pending)
                   gl    <- ZIO.serviceWithZIO[ScriptGrants](_.list)
                   _     <- write(skill.resolve("scripts/go.sh"), "#!/bin/bash\necho deployed v2 $1\n")
                   s3    <- S.userSession(alice, "部署 3")
                   _     <- approve(s3, ok = false, always = false)
                   gl2   <- ZIO.serviceWithZIO[ScriptGrants](_.list)
                 yield (sum, p2, gl, gl2)
               }
        (sum, p2, gl, gl2) = out
        reqs <- m.requests
        results = reqs.flatMap(S.userTexts)
      yield assertTrue(
        results.exists(_.contains("exit_code: 0\ninfo")),
        sum.contains("deploy") && sum.contains("scripts/go.sh") && sum.contains("prod"),
        results.count(_.contains("deployed prod")) == 2, // 批准后一次，始终允许后直接一次
        p2.isEmpty,
        gl.map(g => (g.skill, g.script, g.args, g.current)) == List(("deploy", "scripts/go.sh", List("prod"), true)),
        !results.exists(_.contains("deployed v2")),
        gl2.map(_.current) == List(false)
      )
    },
    test("页面分享：有期限的链接到期前能取（含附件）、到期后与改了到期时刻的都不行；永久同内嵌链接；没有的页面报不存在") {
      for
        d <- S.dirs
        m <- model(PartialFunction.empty)
        out <- withEnv(d, m) {
                 for
                   alice <- S.agent("alice")
                   ws    <- workspaceOf(alice)
                   _     <- write(ws.resolve("site/board/index.html"), "<h1>hi</h1>")
                   _     <- write(ws.resolve("site/board/data.json"), "{}")
                   pages <- ZIO.service[Pages]
                   list  <- pages.list
                   hour  <- pages.share(alice, "board", PageShareTtl.Hour)
                   month <- pages.share(alice, "board", PageShareTtl.Month)
                   ever  <- pages.share(alice, "board", PageShareTtl.Forever)
                   none  <- pages.share(alice, "nope", PageShareTtl.Day).either
                   now   <- Clock.instant
                   token  = hour.url.split('/')(4)
                   (exp, sig) = (token.takeWhile(_ != '.').toLong, token.dropWhile(_ != '.'))
                   index <- pages.file(alice, "board", token, Nil)
                   data  <- pages.file(alice, "board", token, List("data.json"))
                   other <- pages.file(alice, "board", s"${exp + 3600}$sig", Nil).either
                   past   = signer.token(alice, "board", PageGrant.Until(now.getEpochSecond - 1))
                   gone  <- pages.file(alice, "board", past, Nil).either
                   bad   <- pages.file(alice, "board", "x.y.z", Nil).either
                 yield (list, hour, month, ever, none, now, exp, index, data, other, gone, bad)
               }
        (list, hour, month, ever, none, now, exp, index, data, other, gone, bad) = out
      yield assertTrue(
        hour.expiresAt.contains(exp * 1000),
        exp - now.getEpochSecond > 3590 && exp - now.getEpochSecond <= 3600,
        month.expiresAt.exists(_ - now.toEpochMilli > 29L * 86400 * 1000),
        ever == PageShare(list.head.url, None),
        none.isLeft,
        String(index.bytes, "UTF-8") == "<h1>hi</h1>",
        data.contentType == "application/json",
        other.isLeft,
        gone.isLeft,
        bad.isLeft
      )
    },
    test("页面与浏览：签名链接取文件，签名不对或走出目录都不行；目录浏览不出工作目录") {
      for
        d <- S.dirs
        m <- model(PartialFunction.empty)
        out <- withEnv(d, m) {
                 for
                   alice <- S.agent("alice")
                   ws    <- workspaceOf(alice)
                   _     <- write(ws.resolve("site/board/index.html"), "<h1>hi</h1>")
                   _     <- write(ws.resolve("site/board/data.json"), "{}")
                   _     <- write(ws.resolve("notes/a.txt"), "a")
                   pages <- ZIO.service[Pages]
                   list  <- pages.list
                   sig    = list.head.url.split('/')(4)
                   index <- pages.file(alice, "board", sig, Nil)
                   data  <- pages.file(alice, "board", sig, List("data.json"))
                   bad   <- pages.file(alice, "board", "0" * 32, Nil).either
                   esc   <- pages.file(alice, "board", sig, List("..", "..", "notes", "a.txt")).either
                   br    <- ZIO.service[Browse]
                   root  <- br.listDir(alice, None)
                   sub   <- br.listDir(alice, Some("notes"))
                   up    <- br.listDir(alice, Some("../..")).either
                   f     <- br.workspaceFile(alice, "notes/a.txt")
                 yield (list, index, data, bad, esc, root, sub, up, f)
               }
        (list, index, data, bad, esc, root, sub, up, f) = out
      yield assertTrue(
        list.map(_.page) == List("board"),
        list.head.url.startsWith(s"/site/${list.head.agent.value}/board/"),
        String(index.bytes, "UTF-8") == "<h1>hi</h1>",
        index.contentType.startsWith("text/html"),
        data.contentType == "application/json",
        bad.isLeft,
        esc.isLeft,
        root.entries.map(e => (e.name, e.dir)).take(2) == List(("notes", true), ("site", true)),
        sub.entries.map(_.name) == List("a.txt"),
        up.isLeft,
        f.disposition.startsWith("attachment"),
        f.contentType == "application/octet-stream"
      )
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(120.seconds) @@ TestAspect.sequential
