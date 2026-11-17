package muliao.resources

import muliao.contract.{AgentId, CallId, SessionId}
import muliao.exec.{CommandSpec, Exec, Program, RealPath}
import muliao.resources.agents.{AgentPatch, Agents, NewAgent}
import muliao.resources.tools.AgentTools
import muliao.resources.tree.{AssetStore, Owner}
import muliao.resources.vault.*
import muliao.toolkit.{ToolContext, ToolFailure, ToolOutcome, ToolContent}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.Files

object VaultScopeToolsSpec extends ZIOSpecDefault:
  import TestEnv.write

  val agents = ZIO.service[Agents]

  def reason(e: Either[muliao.platform.AppError, ?]): String = e.left.toOption.map(_.message).getOrElse("（没被拒）")

  final case class Ctx(agent: AgentId) extends ToolContext:
    val session = SessionId.wrap("s")
    val call    = CallId.wrap("c")

  def text(o: ToolOutcome[Nothing]): String = o match
    case ToolOutcome.Completed(cs) => cs.collect { case ToolContent.Text(t) => t }.mkString

  val key  = "cw_0x689RpI-jtRR7oE8h_eQsKImvJapLeSbXpwF4e4="
  val spec = suite("vault、可写范围与工具")(
    test("Fernet：与规范的测试向量、Python cryptography 生成的密文互通；改一个字节就解不开") {
      val f     = Fernet.fromKey(key).toOption.get
      val ours  = f.encryptWith("hello".getBytes, 499162800L, (0 until 16).map(_.toByte).toArray)
      val py    = "gAAAAABq-L2IizhwxdDcZ6huDDof_GksU683ieBbKAKHLvUGJFiipyWlO5vCU1jEkP30aOHMQFS0z5lyeq7DxMn9TWA0Ub3chg3TpjkRUL2tmyVHzD79ZsU="
      val other = Fernet.fromKey(Fernet.generateKey()).toOption.get
      val tampered = py.updated(40, if py(40) == 'A' then 'B' else 'A')
      assertTrue(
        ours == "gAAAAAAdwJ6wAAECAwQFBgcICQoLDA0ODy021cpGVWKZ_eEwCGM4BLLF_5CV9dOPmrhuVUPgJobwOz7JcbmrR64jVmpU4IwqDA==",
        f.decrypt(py) == Right("你好 secret-值"),
        f.decrypt(f.encrypt("往返", 1L)) == Right("往返"),
        other.decrypt(py).isLeft,
        f.decrypt(tampered).isLeft,
        Fernet.fromKey("short").isLeft,
        !f.toString.contains(key)
      )
    },
    test("vault：存进自己目录；能用的才解得出；allow_shell=false 不能注入自写命令；改别人的要有 write") {
      for
        a     <- agents.flatMap(_.create(NewAgent("alice", "x")))
        b     <- agents.flatMap(_.create(NewAgent("bob", "x")))
        vault <- ZIO.service[Vault]
        dirs  <- ZIO.service[TestEnv.Dirs]
        n1    <- vault.set(Actor.Agent(a.id), "ALICE_TOKEN", muliao.contract.SecretValue.Plain("明文 v1"), Some("说明"), Some(false), Owner.Platform)
        file   = dirs.assets.resolve("alice/secrets/ALICE_TOKEN.toml")
        raw   <- ZIO.succeed(Files.readString(file))
        perm  <- ZIO.succeed(Files.getPosixFilePermissions(file).toString)
        s1    <- vault.reveal(a.id, List("ALICE_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试"))
        e1    <- vault.reveal(a.id, List("ALICE_TOKEN"), SecretUse.Shell, muliao.contract.AskPurpose(None, "测试")).either
        e2    <- vault.reveal(b.id, List("ALICE_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).either
        e3    <- vault.set(Actor.Agent(b.id), "ALICE_TOKEN", muliao.contract.SecretValue.Plain("hack"), None, None, Owner.Platform).either
        _     <- agents.flatMap(_.update(b.id, AgentPatch(allow = Some(List("use:secret:ALICE_*")))))
        s2    <- vault.reveal(b.id, List("ALICE_TOKEN"), SecretUse.Mcp, muliao.contract.AskPurpose(None, "测试"))
        n2    <- vault.set(Actor.Agent(a.id), "ALICE_TOKEN", muliao.contract.SecretValue.Plain("v2"), Some(""), None, Owner.Platform)
        s3    <- vault.reveal(a.id, List("ALICE_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试"))
        raw2  <- ZIO.succeed(Files.readString(file))
        e4    <- vault.set(Actor.Agent(a.id), "bad-key", muliao.contract.SecretValue.Plain("v"), None, None, Owner.Platform).either
        e5    <- vault.reveal(a.id, List("NOPE"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).either
      yield assertTrue(
        n1,
        !raw.contains("明文 v1"),
        raw.contains("allow_shell = false"),
        perm == "[OWNER_READ, OWNER_WRITE]",
        s1 == Map("ALICE_TOKEN" -> "明文 v1"),
        reason(e1).contains("skill 脚本"),
        reason(e2).contains("不能用"),
        reason(e3).contains("没有改它的权限"),
        s2 == Map("ALICE_TOKEN" -> "明文 v1"),
        !n2,
        s3 == Map("ALICE_TOKEN" -> "v2"),
        raw2.contains("说明"), // 空说明沿用原来的
        raw2.contains("allow_shell = false"),
        reason(e4).contains("不合法"),
        reason(e5).contains("NOPE"),
        !reason(e1).contains("明文 v1")
      )
    },
    test("主密钥不对拒绝启动（已有 secret 一个都解不开）") {
      for
        vault <- ZIO.service[Vault]
        _     <- vault.set(Actor.User, "PLATFORM_KEY", muliao.contract.SecretValue.Plain("v"), None, None, Owner.Platform)
        snap  <- ZIO.serviceWithZIO[AssetStore](_.snapshot)
        wrong  = Fernet.fromKey(Fernet.generateKey()).toOption.get
        right  = Fernet.fromKey(TestEnv.key).toOption.get
        e     <- Vault.verifyKey(wrong, snap).either
        ok    <- Vault.verifyKey(right, snap).either
      yield assertTrue(reason(e).contains("主密钥不对"), ok.isRight)
    },
    test("可写范围：普通 agent 只能写工作目录、自己的资源、授权的资源与目录；平台私有文件读写都不行") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- agents.flatMap(_.create(NewAgent("alice", "x")))
        _    <- agents.flatMap(_.create(NewAgent("bob", "x")))
        _     = write(dirs.assets.resolve("bob/skills/bobs/SKILL.md"), "---\ndescription: x\n---\n")
        _     = write(dirs.assets.resolve("platform/memory/trading.md"), "交易")
        extra = Files.createDirectories(dirs.base.resolve("granted"))
        _    <- agents.flatMap(_.update(a.id, AgentPatch(allow = Some(List("*:memory:trading", s"write:path:$extra")))))
        res  <- ZIO.service[Resources]
        sc   <- res.writableScope(a.id)
        ws    = dirs.workspaces.resolve(a.id.value)
        script = List(
                   s"echo 1 > '$ws/f' && echo WS_OK",
                   s"echo 1 > '${dirs.assets}/alice/memory.md' && echo OWN_OK",
                   s"echo 1 >> '${dirs.assets}/platform/memory/trading.md' && echo SHARED_OK",
                   s"echo 1 > '$extra/f' && echo PATH_OK",
                   s"echo 1 > '${dirs.assets}/bob/skills/bobs/x' || echo BOB_BLOCKED",
                   s"echo 1 > '${dirs.assets}/platform/memory/global.md' || echo GLOBAL_BLOCKED",
                   s"cat '${dirs.privateFile}' || echo PRIVATE_BLOCKED"
                 ).mkString("; ")
        r     <- Exec.run(CommandSpec(Program.Shell(script), ws, sc), 20.seconds)
        why   <- res.explainDenied(a.id, dirs.assets.resolve("bob/skills/bobs/x"))
        why2  <- res.explainDenied(a.id, dirs.base.resolve("elsewhere"))
        bobsR <- RealPath.resolve(dirs.assets.resolve("bob/skills/bobs/x"))
        wsR   <- RealPath.resolve(ws.resolve("f"))
      yield assertTrue(
        List("WS_OK", "OWN_OK", "SHARED_OK", "PATH_OK", "BOB_BLOCKED", "GLOBAL_BLOCKED", "PRIVATE_BLOCKED").forall(r.output.contains),
        !r.output.contains("secret = 1"),
        why.contains("bob 的资源"),
        why2.contains("write:path"),
        !sc.permits(bobsR),
        sc.permits(wsR)
      )
    } @@ TestAspect.withLiveClock,
    test("可写范围：admin 能写整棵资源树（被 deny 收回的除外），不受平台私有文件限制") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- agents.flatMap(_.create(NewAgent("boss", "x", roles = List(muliao.contract.RoleName.wrap("admin")))))
        _    <- agents.flatMap(_.create(NewAgent("bob", "x")))
        _     = write(dirs.assets.resolve("bob/skills/locked/SKILL.md"), "---\ndescription: x\n---\n")
        _    <- agents.flatMap(_.update(a.id, AgentPatch(deny = Some(List("write:skill:locked")))))
        sc   <- ZIO.serviceWithZIO[Resources](_.writableScope(a.id))
        script = List(
                   s"echo 1 > '${dirs.assets}/bob/memory.md' && echo BOB_OK",
                   s"echo 1 > '${dirs.assets}/platform/memory/global.md' && echo GLOBAL_OK",
                   s"echo 1 > '${dirs.assets}/bob/skills/locked/x' || echo LOCKED_BLOCKED",
                   s"cat '${dirs.privateFile}' && echo PRIVATE_READ"
                 ).mkString("; ")
        r <- Exec.run(CommandSpec(Program.Shell(script), dirs.base, sc), 20.seconds)
      yield assertTrue(List("BOB_OK", "GLOBAL_OK", "LOCKED_BLOCKED", "PRIVATE_READ").forall(r.output.contains))
    } @@ TestAspect.withLiveClock,
    test("一次运行的资源：记忆、skill 索引、MCP、secret、相关的问题；给模型的段落逐字节稳定") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- agents.flatMap(_.create(NewAgent("alice", "x")))
        _     = write(dirs.assets.resolve("platform/memory/global.md"), "全局规则")
        _     = write(dirs.assets.resolve("platform/memory/trading.md"), "交易")
        _     = write(dirs.assets.resolve("platform/skills/std/SKILL.md"), "---\ndescription: 写代码\n  时读\n---\n")
        _     = write(dirs.assets.resolve("platform/skills/broken/SKILL.md"), "---\nname: broken\n---\n")
        _     = write(dirs.assets.resolve("alice/skills/mine/SKILL.md"), "---\ndescription: 我的\n---\n")
        _     = write(dirs.assets.resolve("alice/memory.md"), "长" * 150)
        _     = write(dirs.assets.resolve("bob-ghost/skills/x/SKILL.md"), "坏")
        _    <- agents.flatMap(_.update(a.id, AgentPatch(allow = Some(List("use:memory:trading", "use:skill:std")))))
        _     = write(dirs.assets.resolve("platform/skills/std/SKILL.md"), "---\ndescription: 写代码\n  时读\n---\n") // 不变
        res  <- ZIO.service[Resources]
        r1   <- res.forRun(a.id)
        r2   <- res.forRun(a.id)
        mem   = ResourcePrompt.memories(r1)
        idx   = ResourcePrompt.skillIndex(r1)
        note  = ResourcePrompt.notices(r1, List("muliao"))
      yield assertTrue(
        r1.memories.map(m => (m.name, m.kind, m.editable)) == List(("global", MemoryKind.Global, false), ("trading", MemoryKind.Shared, false), ("alice", MemoryKind.Private, true)),
        r1.skills.map(s => (s.doc.name, s.owned)) == List(("mine", true), ("std", false)),
        r1.tools.contains("read_file"),
        !r1.tools.contains("create_agent"),
        mem == ResourcePrompt.memories(r2),
        idx == ResourcePrompt.skillIndex(r2),
        mem.contains("<memory name=\"global\" access=\"只读，全局\""),
        idx.contains("- `mine`（你的）：我的"),
        idx.contains("- `std`：写代码 时读"),
        !note.contains("broken"), // 它不能用这个 skill，与它无关
        !note.contains("bob-ghost"),
        note.contains("记忆 alice 现在 150 字")
      )
    },
    test("工具：vault_set 的值标为敏感；参数名 allow_shell；get_agent / list_agents 输出 JSON；不能删自己") {
      for
        a     <- agents.flatMap(_.create(NewAgent("alice", "管平台", roles = List(muliao.contract.RoleName.wrap("admin")))))
        b     <- agents.flatMap(_.create(NewAgent("bob", "做事", group = Some("组"))))
        ags   <- ZIO.service[Agents]
        res   <- ZIO.service[Resources]
        store <- ZIO.service[AssetStore]
        vault <- ZIO.service[Vault]
        paths <- ZIO.service[ResourcePaths]
        tools  = AgentTools.all(ags, res, store, vault, TestEnv.tools, paths).map(t => t.spec.name -> t).toMap
        vs     = tools("vault_set").spec
        args   = """{"key":"MY_TOKEN","value":"top-secret","allow_shell":false}""".fromJson[Json].toOption.get
        out   <- tools("vault_set").invoke(args, Ctx(b.id))
        got   <- vault.reveal(b.id, List("MY_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试"))
        sh    <- vault.reveal(b.id, List("MY_TOKEN"), SecretUse.Shell, muliao.contract.AskPurpose(None, "测试")).either
        ga    <- tools("get_agent").invoke(Json.Obj("agent" -> Json.Str("BOB")), Ctx(a.id))
        la    <- tools("list_agents").invoke(Json.Obj(), Ctx(a.id))
        self  <- tools("delete_agent").invoke(Json.Obj("agent" -> Json.Str("alice")), Ctx(a.id)).either
        mk    <- tools("create_agent").invoke(Json.Obj("name" -> Json.Str("carol"), "description" -> Json.Str("新"), "thinking" -> Json.Str("low")), Ctx(a.id))
        del   <- tools("delete_agent").invoke(Json.Obj("agent" -> Json.Str("carol")), Ctx(a.id))
        gone  <- ags.find("carol")
      yield
        val view = text(ga).fromJson[Json].toOption.get
        assertTrue(
          vs.sensitiveFields == Set("value"),
          vs.redact(args).toJson.contains("[redacted]"),
          !vs.redact(args).toJson.contains("top-secret"),
          vs.inputSchema.toJson.contains("\"allow_shell\""),
          text(out).contains("已新建"),
          got == Map("MY_TOKEN" -> "top-secret"),
          sh.isLeft,
          view.get(zio.json.ast.JsonCursor.field("name")).toOption.contains(Json.Str("bob")),
          view.get(zio.json.ast.JsonCursor.field("group")).toOption.contains(Json.Str("组")),
          text(ga).contains("\"available_roles\""),
          text(ga).contains("secret:MY_TOKEN"),
          text(la).contains("\"group\" : \"组\""),
          self.left.exists { case ToolFailure.Rejected(m) => m.contains("自己"); case _ => false },
          text(mk).contains("carol"),
          text(del).contains("已删除"),
          gone.isEmpty,
          tools("create_agent").spec.access == muliao.toolkit.Access.Admin
        )
    }
  ).provideLayer(TestEnv.layer) @@ TestAspect.sequential
