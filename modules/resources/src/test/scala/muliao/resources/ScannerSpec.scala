package muliao.resources

import muliao.contract.{Thinking, AssetKind, SkillName}
import muliao.contract.AgentName
import muliao.resources.tree.*
import zio.*
import zio.test.*

import java.nio.file.{Files, Path}

object ScannerSpec extends ZIOSpecDefault:
  import TestEnv.write

  def tree(files: (String, String)*): Path =
    val root = Files.createTempDirectory("muliao-tree")
    files.foreach((p, c) => write(root.resolve(p), c))
    root

  val skill = "---\nname: %s\ndescription: 什么时候用\n---\n正文\n"

  def spec = suite("资源目录扫描")(
    test("各种资源正常加载：记忆、skill（多行 description、审批脚本）、MCP、secret、agent.toml") {
      val root = tree(
        "platform/memory/global.md" -> "全局",
        "platform/memory/trading.md" -> "交易",
        "platform/skills/code-std/SKILL.md" -> "---\ndescription: 写代码时\n  读它\napproval: [scripts/run.sh]\n---\n",
        "platform/skills/code-std/scripts/run.sh" -> "#!/bin/sh\n",
        "alice/agent.toml" -> "description = \"做事\"\nmodel = \"claude:x\"\nthinking = \"high\"\ngroup = \"g\"\n",
        "alice/memory.md" -> "私有",
        "alice/mcp/web.json" -> """{"transport":"http","url":"https://x/mcp","headers":{"A":"Bearer ${vault:TOKEN_A}"}}""",
        "alice/secrets/TOKEN_A.toml" -> "description = \"d\"\nciphertext = \"gAAA\"\nallow_shell = false\n",
        "alice/.hidden/whatever" -> "x"
      )
      val s = Scanner.scan(root)
      assertTrue(
        s.problems.isEmpty,
        s.skill(SkillName.wrap("code-std")).exists(d => d.description == "写代码时 读它" && d.approval == Set("scripts/run.sh") && d.owner == Owner.Platform),
        s.mcp("web").exists(_.config.secretRefs == Set("TOKEN_A")),
        s.secret("TOKEN_A").exists(d => !d.content.allowShell && d.owner == Owner.Agent(AgentName.wrap("alice"))),
        s.profiles.get(AgentName.wrap("alice")).contains(AgentProfile("做事", "claude:x", Thinking.High, Some("g"))),
        s.privateMemory(AgentName.wrap("alice")).exists(_.content == "私有"),
        s.sharedMemory("trading").isDefined,
        s.agentDirs == Set(AgentName.wrap("alice"))
      )
    },
    test("读坏的文件跳过并报出原因，不影响别的") {
      val root = tree(
        "alice/agent.toml" -> "description = \"\"\nmodel = \"claude:x\"\n",
        "bob/agent.toml" -> "description = \"x\"\nmodel = \"claude:x\"\nid = \"old\"\n",
        "alice/skills/no-desc/SKILL.md" -> "---\nname: no-desc\n---\n",
        "alice/skills/no-front/SKILL.md" -> "正文\n",
        "alice/skills/bad-name/SKILL.md" -> skill.format("other"),
        "alice/skills/missing-script/SKILL.md" -> "---\ndescription: x\napproval: [scripts/gone.sh]\n---\n",
        "alice/skills/orphan/notes.md" -> "附件",
        "alice/skills/ok/SKILL.md" -> skill.format("ok"),
        "alice/mcp/bad.json" -> """{"transport":"stdio","url":"http://x"}""",
        "alice/mcp/Upper.json" -> """{"transport":"stdio","command":"x"}""",
        "alice/secrets/BROKEN.toml" -> "ciphertext = ",
        "alice/stray.txt" -> "x",
        "platform/memory/bin.md" -> "",
        "platform/access.toml" -> "[roles.x]\nallow = [\"bogus\"]\n"
      )
      Files.write(root.resolve("platform/memory/bin.md"), Array[Byte](0xff.toByte, 0xfe.toByte))
      val s       = Scanner.scan(root)
      def why(p: String) = s.problems.find(_.path == root.resolve(p)).map(_.reason).getOrElse("")
      assertTrue(
        s.skills.map(_.name) == List("ok"),
        s.mcps.isEmpty,
        s.secrets.isEmpty,
        s.profiles.isEmpty,
        why("alice/agent.toml").contains("description"),
        why("bob/agent.toml").contains("id"),
        why("alice/skills/no-desc/SKILL.md").contains("description"),
        why("alice/skills/no-front/SKILL.md").contains("frontmatter"),
        why("alice/skills/bad-name/SKILL.md").contains("目录名"),
        why("alice/skills/missing-script/SKILL.md").contains("scripts/gone.sh"),
        why("alice/skills/orphan").contains("SKILL.md"),
        why("alice/mcp/bad.json").contains("url"),
        why("alice/mcp/Upper.json").contains("小写"),
        why("alice/secrets/BROKEN.toml").contains("TOML"),
        why("alice/stray.txt").contains("规定位置"),
        why("platform/memory/bin.md").contains("UTF-8"),
        s.access.exists(_.isLeft),
        s.problems.find(_.path == root.resolve("alice/skills/ok/SKILL.md")).isEmpty,
        s.problems.find(_.path == root.resolve("alice/stray.txt")).flatMap(_.owner).contains(Owner.Agent(AgentName.wrap("alice")))
      )
    },
    test("同名冲突：平台那份优先；两个 agent 同名的都不加载") {
      val root = tree(
        "platform/skills/shared/SKILL.md" -> skill.format("shared"),
        "alice/skills/shared/SKILL.md" -> skill.format("shared"),
        "alice/skills/dup/SKILL.md" -> skill.format("dup"),
        "bob/skills/dup/SKILL.md" -> skill.format("dup"),
        "alice/secrets/K.toml" -> "ciphertext = \"a\"\n",
        "bob/secrets/K.toml" -> "ciphertext = \"b\"\n"
      )
      val s = Scanner.scan(root)
      assertTrue(
        s.skill(SkillName.wrap("shared")).exists(_.owner == Owner.Platform),
        s.skill(SkillName.wrap("dup")).isEmpty,
        s.secret("K").isEmpty,
        s.problems.exists(p => p.path == root.resolve("alice/skills/shared/SKILL.md") && p.reason.contains("平台已有")),
        s.problems.count(_.reason.contains("都不加载")) == 4,
        s.occupied(AssetKind.Skill -> "dup").toSet == Set(Owner.Agent(AgentName.wrap("alice")), Owner.Agent(AgentName.wrap("bob")))
      )
    },
    test("指纹：内容、增删、隐藏文件之外的任何变化都会变；隐藏文件不影响") {
      val root = tree("platform/memory/global.md" -> "a")
      val f0   = Scanner.fingerprint(root)
      write(root.resolve("platform/.tmp"), "x")
      val f1 = Scanner.fingerprint(root)
      write(root.resolve("platform/memory/global.md"), "b")
      val f2 = Scanner.fingerprint(root)
      write(root.resolve("platform/memory/x.md"), "")
      val f3 = Scanner.fingerprint(root)
      assertTrue(f0 == f1, f1 != f2, f2 != f3)
    },
    test("记忆规整：去行尾空白、合并连续空行、去首尾空行；长度按字符") {
      assertTrue(
        Formats.normalizeMemory("\n\n第一行  \n\n\n\n第二行\t\n\n") == "第一行\n\n第二行\n",
        Formats.memoryLength("你好  \n\n\n世界\n") == 6,
        Formats.normalizeMemory("  \n") == ""
      )
    },
    test("平台写文件后立刻能读到新快照；access.toml 写坏时沿用上一份有效的，重启后也一样") {
      val ok = "[roles.r]\ndescription = \"x\"\nallow = [\"use:skill:a\"]\n"
      for
        dirs  <- ZIO.service[TestEnv.Dirs]
        store <- ZIO.service[AssetStore]
        file   = dirs.assets.resolve("platform/access.toml")
        _     <- store.write(file, ok)
        s1    <- store.access
        _     <- store.write(file, "[roles.r\n")
        s2    <- store.access
        snap  <- store.snapshot
        // 新开一个 store（模拟重启），上一份有效的副本还在
        s3 <- ZIO.scoped(
                AssetStore
                  .layer(AssetStore.Settings(dirs.assets, dirs.data.resolve("resources/access.last-good.toml")))
                  .build
                  .flatMap(_.get[AssetStore].access)
              ).provideLayer(muliao.platform.events.ChangeBus.layer)
      yield assertTrue(
        s1.broken.isEmpty,
        s1.config.roles.size == 1,
        s2.broken.isDefined,
        s2.config == s1.config,
        s3 == s2,
        snap.problems.exists(_.path.endsWith("access.toml")),
        Files.exists(dirs.assets.resolve("platform/memory/global.md"))
      )
    }.provideLayer(TestEnv.layer),
    test("目录变化被轮询发现并广播「数据变了」") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        bus  <- ZIO.service[muliao.platform.events.ChangeBus]
        got  <- ZIO.scoped(bus.subscribe.flatMap { s =>
                  ZIO.succeed(write(dirs.assets.resolve("platform/memory/new-one.md"), "x")) *> s.take(1).runCollect
                })
        snap <- ZIO.serviceWithZIO[AssetStore](_.snapshot)
      yield assertTrue(got.map(_.topic) == Chunk(muliao.contract.Topic.Assets), snap.sharedMemory("new-one").isDefined)
    }.provideLayer(TestEnv.layer) @@ TestAspect.withLiveClock @@ TestAspect.timeout(20.seconds)
  )
