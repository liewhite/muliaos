package muliao.resources

import muliao.platform.AppError
import muliao.resources.agents.{Agents, NewAgent}
import muliao.resources.guard.{AssetGuard, Expectation, FileFingerprint}
import muliao.resources.tree.{AssetStore, Vetted}
import muliao.resources.vault.{Actor, Vault}
import muliao.resources.tree.Owner
import zio.*
import zio.test.*

import java.nio.file.{Files, Path}

object GuardSpec extends ZIOSpecDefault:
  import TestEnv.write

  def vet(actor: Actor, p: Path, content: String, expect: Expectation = Expectation.Unconditional) =
    ZIO.serviceWithZIO[AssetGuard](_.check(actor, p, content, expect))

  /** 校验后要写的内容。 */
  def guard(actor: Actor, p: Path, content: String, expect: Expectation = Expectation.Unconditional) =
    vet(actor, p, content, expect).map(_.content)

  def reason(e: Either[AppError, ?]): String = e.left.toOption.map(_.message).getOrElse("（没被拒）")

  val alice = ZIO.serviceWithZIO[Agents](_.create(NewAgent("alice", "做事")))

  def spec = suite("资源写入口校验")(
    test("资源目录以外的路径原样放行；隐藏文件不校验") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- vet(Actor.User, dirs.base.resolve("elsewhere/x.txt"), "随便")
        b    <- vet(Actor.User, dirs.assets.resolve("platform/.tmp-x"), "随便")
      yield assertTrue(
        a match { case Vetted.Plain(_, "随便") => true; case _ => false },
        b match { case Vetted.Asset(_, "随便", false) => true; case _ => false }
      )
    },
    test("位置不对、agent 目录不存在都拒绝，并说明规定位置；按磁盘上的真实路径判断") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        _    <- alice
        a    <- guard(Actor.User, dirs.assets.resolve("stray.md"), "x").either
        b    <- guard(Actor.User, dirs.assets.resolve("alice/notes.md"), "x").either
        c    <- guard(Actor.User, dirs.assets.resolve("ghost/memory.md"), "x").either
        d    <- guard(Actor.User, dirs.assets.resolve("ALICE/memory.md"), "x").either
      yield assertTrue(
        reason(a).contains("规定位置"),
        reason(b).contains("规定位置"),
        reason(c).contains("create_agent"),
        d.isRight, // 不分大小写的文件系统上 ALICE 就是 alice 的目录（按真实路径判断）
        reason(a).contains("section 'Layout'")
      )
    },
    test("记忆：写入前规整，超上限且比原来长才拒绝") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        _    <- alice
        mem   = dirs.assets.resolve("alice/memory.md")
        a    <- guard(Actor.User, mem, "一行  \n\n\n二行\n\n")
        long  = "字" * 150
        b    <- guard(Actor.User, mem, long).either
        _     = write(mem, long) // 用 shell 写超了
        c    <- guard(Actor.User, mem, "字" * 120).either
        d    <- guard(Actor.User, mem, "字" * 160).either
        e    <- guard(Actor.User, dirs.assets.resolve("platform/memory/Bad_Name.md"), "x").either
      yield assertTrue(
        a == "一行\n\n二行\n",
        reason(b).contains("超过上限 100"),
        c == Right("字" * 120 + "\n"),
        reason(d).contains("不能变长"),
        reason(e).contains("小写")
      )
    },
    test("整体覆盖：没读过的已有文件拒绝；读过之后被改了拒绝；读到的指纹一致才放行") {
      for
        dirs  <- ZIO.service[TestEnv.Dirs]
        g     <- ZIO.service[AssetGuard]
        _     <- alice
        mem    = dirs.assets.resolve("alice/memory.md")
        _      = write(mem, "原来")
        fp    <- g.fingerprint(mem).someOrFailException.orDie
        a     <- guard(Actor.User, mem, "新", Expectation.NotRead).either
        b     <- guard(Actor.User, mem, "新", Expectation.ReadAs(fp))
        _      = write(mem, "别人改了")
        c     <- guard(Actor.User, mem, "新", Expectation.ReadAs(fp)).either
        d     <- guard(Actor.User, dirs.assets.resolve("platform/memory/fresh.md"), "新建", Expectation.NotRead)
      yield assertTrue(
        reason(a).contains("read_file"),
        b == "新\n",
        c.left.exists(_ == AppError.conflict(s"$mem 在你读过之后被改过了，重新 read_file 再写（或改用 edit_file）。没有写入")) || reason(c).contains("被改过"),
        d == "新建\n",
        fp == FileFingerprint.of("原来")
      )
    },
    test("skill：frontmatter 必须合格；附件要先有 SKILL.md；新建时名字全平台唯一") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        _    <- alice
        _     = write(dirs.assets.resolve("platform/skills/taken/SKILL.md"), "---\ndescription: x\n---\n")
        a    <- guard(Actor.User, dirs.assets.resolve("alice/skills/new-one/scripts/run.sh"), "#!/bin/sh").either
        b    <- guard(Actor.User, dirs.assets.resolve("alice/skills/new-one/SKILL.md"), "---\nname: new-one\n---\n").either
        c    <- guard(Actor.User, dirs.assets.resolve("alice/skills/new-one/SKILL.md"), "---\ndescription: 何时用\napproval: [scripts/run.sh]\n---\n").either
        d    <- guard(Actor.User, dirs.assets.resolve("alice/skills/new-one/SKILL.md"), "---\ndescription: 何时用\n---\n").either
        _     = write(dirs.assets.resolve("alice/skills/new-one/SKILL.md"), "---\ndescription: 何时用\n---\n")
        e    <- guard(Actor.User, dirs.assets.resolve("alice/skills/new-one/scripts/run.sh"), "#!/bin/sh").either
        f    <- guard(Actor.User, dirs.assets.resolve("alice/skills/taken/SKILL.md"), "---\ndescription: x\n---\n").either
      yield assertTrue(
        reason(a).contains("SKILL.md"),
        reason(b).contains("description"),
        reason(c).contains("scripts/run.sh"),
        d.isRight,
        e.isRight,
        reason(f).contains("platform 已有名为 taken")
      )
    },
    test("MCP：格式合格、引用的 secret 存在且自己能用") {
      for
        dirs  <- ZIO.service[TestEnv.Dirs]
        a     <- alice
        vault <- ZIO.service[Vault]
        _     <- vault.set(Actor.User, "OTHER_KEY", "v", None, None, Owner.Platform)
        _     <- vault.set(Actor.Agent(a.id), "MY_KEY", "v", None, None, Owner.Platform)
        f      = dirs.assets.resolve("alice/mcp/web.json")
        x     <- guard(Actor.Agent(a.id), f, """{"transport":"http","url":"ftp://x"}""").either
        y     <- guard(Actor.Agent(a.id), f, """{"transport":"http","url":"https://x","headers":{"A":"${vault:OTHER_KEY}"}}""").either
        z     <- guard(Actor.Agent(a.id), f, """{"transport":"http","url":"https://x","headers":{"A":"${vault:NOPE}"}}""").either
        ok    <- guard(Actor.Agent(a.id), f, """{"transport":"http","url":"https://x","headers":{"A":"${vault:MY_KEY}"}}""").either
      yield assertTrue(reason(x).contains("http"), reason(y).contains("OTHER_KEY"), reason(z).contains("不存在"), ok.isRight)
    },
    test("secret 文件：不能手写新建、不能改密文，只能改 description 与 allow_shell") {
      for
        dirs  <- ZIO.service[TestEnv.Dirs]
        a     <- alice
        vault <- ZIO.service[Vault]
        _     <- vault.set(Actor.Agent(a.id), "MY_KEY", "v", Some("说明"), None, Owner.Platform)
        f      = dirs.assets.resolve("alice/secrets/MY_KEY.toml")
        text  <- ZIO.succeed(Files.readString(f))
        ct     = text.linesIterator.find(_.startsWith("ciphertext")).get
        a1    <- guard(Actor.User, dirs.assets.resolve("alice/secrets/NEW_KEY.toml"), "ciphertext = \"x\"\n").either
        a2    <- guard(Actor.User, f, "description = \"说明\"\nciphertext = \"gAAAAforged\"\n").either
        a3    <- guard(Actor.User, f, s"description = \"新说明\"\n$ct\nallow_shell = false\n").either
      yield assertTrue(reason(a1).contains("vault_set"), reason(a2).contains("只能手改"), a3.isRight)
    },
    test("agent.toml：未知字段、空描述、模型不能用都拒绝") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        _    <- alice
        f     = dirs.assets.resolve("alice/agent.toml")
        a    <- guard(Actor.User, f, "description = \"x\"\nmodel = \"claude:y\"\nid = \"1\"\n").either
        b    <- guard(Actor.User, f, "description = \" \"\nmodel = \"claude:y\"\n").either
        c    <- guard(Actor.User, f, "description = \"x\"\nmodel = \"nope:y\"\n").either
        d    <- guard(Actor.User, f, "description = \"x\"\nmodel = \"claude:y\"\nthinking = \"off\"\n").either
      yield assertTrue(reason(a).contains("id"), reason(b).contains("description"), reason(c).contains("nope:y"), d.isRight)
    },
    test("access.toml：新规则至少匹配一项；只拦新出现的问题，原有的问题不挡路") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- alice
        _     = write(dirs.assets.resolve("platform/skills/relay-host/SKILL.md"), "---\ndescription: x\n---\n")
        f     = dirs.assets.resolve("platform/access.toml")
        base  = "[roles.r]\ndescription = \"\"\nallow = [\"use:skill:relay-host\"]\n"
        ok   <- guard(Actor.User, f, base).either
        bad  <- guard(Actor.User, f, base + "deny = [\"use:skill:relay-hots\"]\n").either
        toolOk <- guard(Actor.User, f, base + "deny = [\"use:tool:trigger_*\"]\n").either
        noRole <- guard(Actor.User, f, base + s"[agents.${a.id.value}]\nroles = [\"nope\"]\n").either
        noAgent <- guard(Actor.User, f, base + "[agents.ffffffffffff]\nroles = [\"r\"]\n").either
        // 原文件里已有一个 stale 段，再改别处不受它影响
        _       = write(f, base + "[agents.ffffffffffff]\nroles = [\"r\"]\n")
        _      <- ZIO.serviceWithZIO[AssetStore](_.snapshot)
        stale  <- guard(Actor.User, f, base.replace("description = \"\"", "description = \"改了说明\"") + "[agents.ffffffffffff]\nroles = [\"r\"]\n").either
        syntax <- guard(Actor.User, f, "[roles.r\n").either
      yield assertTrue(
        ok.isRight,
        reason(bad).contains("relay-hots"),
        toolOk.isRight,
        reason(noRole).contains("nope"),
        reason(noAgent).contains("不是现有的 agent"),
        stale.isRight,
        reason(syntax).contains("TOML")
      )
    }
  ).provideLayer(TestEnv.layer) @@ TestAspect.sequential
