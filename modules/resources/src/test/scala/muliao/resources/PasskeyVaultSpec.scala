package muliao.resources

import muliao.contract.*
import muliao.resources.agents.{AgentPatch, Agents, NewAgent}
import muliao.resources.tree.{Formats, Owner, SecretFile, SecretLock}
import muliao.resources.vault.*
import zio.*
import zio.test.*

import java.nio.file.Files

/** passkey 加密的 secret（#915）：只存不解、等用户授权、一次与一段时间、按 agent 隔离、拒绝与中断。 */
object PasskeyVaultSpec extends ZIOSpecDefault:
  val box = PasskeyBox("muliao.example.com", "Y3JlZA", "aXY=", "Y3Q=")

  def reason(e: Either[muliao.platform.AppError, ?]): String = e.left.toOption.map(_.message).getOrElse("（没被拒）")

  /** 等到有 `n` 个请求在等。 */
  def asks(n: Int): URIO[Grants, List[SecretAsk]] =
    ZIO.serviceWithZIO[Grants](_.view.map(_._1)).repeatUntil(_.size == n)

  def answer(agent: AgentId, key: String, d: AskDecision) = ZIO.serviceWithZIO[Grants](_.answer(Slot(agent, key), d))

  /** alice 有一个 passkey secret（用户设的），bob 也被允许用它。 */
  val setup =
    for
      a     <- ZIO.serviceWithZIO[Agents](_.create(NewAgent("alice", "x")))
      b     <- ZIO.serviceWithZIO[Agents](_.create(NewAgent("bob", "x")))
      vault <- ZIO.service[Vault]
      _     <- vault.set(Actor.User, "PK_TOKEN", SecretValue.Passkey(box), Some("说明"), Some(false), Owner.Agent(a.name))
      _     <- ZIO.serviceWithZIO[Agents](_.update(b.id, AgentPatch(allow = Some(List("use:secret:PK_*")))))
    yield (a, b, vault)

  val spec = suite("passkey secret")(
    test("文件格式：[passkey] 表往返；ciphertext 与 [passkey] 恰好一个") {
      val f = SecretFile("d", SecretLock.Passkey(box), allowShell = false)
      val text = Formats.renderSecret(f)
      assertTrue(
        Formats.parseSecret(text) == Right(f),
        text.contains("[passkey]"),
        Formats.parseSecret("ciphertext = \"x\"\n[passkey]\nrp = \"a\"\ncredential = \"b\"\niv = \"c\"\nciphertext = \"d\"\n").isLeft,
        Formats.parseSecret("description = \"d\"\n").isLeft,
        Formats.parseSecret("[passkey]\nrp = \"a\"\n").isLeft
      )
    },
    test("只有用户能设、能改值；agent 的 vault_set 改不动；说明与 allow_shell 照常") {
      for
        (a, _, vault) <- setup
        dirs  <- ZIO.service[TestEnv.Dirs]
        raw   <- ZIO.succeed(Files.readString(dirs.assets.resolve("alice/secrets/PK_TOKEN.toml")))
        e1    <- vault.set(Actor.Agent(a.id), "PK_TOKEN", SecretValue.Plain("降级"), None, None, Owner.Platform).either
        e2    <- vault.set(Actor.Agent(a.id), "OTHER", SecretValue.Passkey(box), None, None, Owner.Platform).either
        n     <- vault.set(Actor.User, "PK_TOKEN", SecretValue.Plain("用户改回普通的"), None, None, Owner.Platform)
        v     <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试"))
      yield assertTrue(
        raw.contains("[passkey]"),
        raw.contains("allow_shell = false"),
        reason(e1).contains("只能由用户在界面上改值"),
        reason(e2).contains("只能由用户在界面上设值"),
        !n,
        v == Map("PK_TOKEN" -> "用户改回普通的")
      )
    },
    test("没授权时等用户：一次只给这次；一段时间内不再问、收回后再问；按 agent 隔离；调用被停请求留着；权限与 allow_shell 先查") {
      for
        (a, b, vault) <- setup
        sub   <- ZIO.serviceWithZIO[Grants](_.asked)
        shell <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Shell, muliao.contract.AskPurpose(None, "测试")).either
        f1    <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).fork
        w1    <- asks(1)
        note  <- sub.take
        _     <- answer(a.id, "PK_TOKEN", AskDecision.Once("v-once"))
        r1    <- f1.join
        f2    <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).fork
        _     <- asks(1)
        _     <- answer(a.id, "PK_TOKEN", AskDecision.For("v-hour", 60))
        r2    <- f2.join
        r3    <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Mcp, muliao.contract.AskPurpose(None, "测试"))
        g1    <- ZIO.serviceWithZIO[Grants](_.view.map(_._2))
        fb    <- vault.reveal(b.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).fork
        wb    <- asks(1)
        _     <- answer(b.id, "PK_TOKEN", AskDecision.Deny)
        rb    <- fb.join.either
        _     <- ZIO.serviceWithZIO[Grants](_.revoke(Slot(a.id, "PK_TOKEN")))
        g2    <- ZIO.serviceWithZIO[Grants](_.view.map(_._2))
        f4    <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).fork
        w4    <- asks(1)
        _     <- f4.interrupt
        w5    <- asks(1)
        _     <- answer(a.id, "PK_TOKEN", AskDecision.Once("没人取"))
        w6    <- asks(0)
        late  <- answer(a.id, "PK_TOKEN", AskDecision.Once("迟了")).either
      yield assertTrue(
        reason(shell).contains("不能注入自写命令"),
        w1.map(x => (x.agent, x.key, x.use, x.purpose.what, x.box)) == List((a.id, "PK_TOKEN", SecretUse.Script, "测试", box)),
        note == Asked(Slot(a.id, "PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")),
        r1 == Map("PK_TOKEN" -> "v-once"),
        r2 == Map("PK_TOKEN" -> "v-hour"),
        r3 == Map("PK_TOKEN" -> "v-hour"),
        g1.map(g => (g.agent, g.key, g.until)) == List((a.id, "PK_TOKEN", 60 * 60_000L)), // 测试时钟从 0 开始
        wb.map(_.agent) == List(b.id),
        reason(rb).contains("拒绝"),
        g2.isEmpty,
        w4.size == 1,
        w5.size == 1, // 等的调用被停不撤请求，答复后明文没人取就丢掉
        w6.isEmpty,
        reason(late).contains("已经没有了")
      )
    },
    test("几个调用同时等同一个：一次答复都拿到；到期后再问") {
      for
        (a, _, vault) <- setup
        f1 <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).fork
        _  <- asks(1)
        f2 <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试")).fork
        _  <- ZIO.yieldNow.repeatN(50)
        w  <- asks(1)
        _  <- answer(a.id, "PK_TOKEN", AskDecision.For("v", 30))
        r1 <- f1.join
        r2 <- f2.join
        // 授权按测试时钟（从 0 起）到 30 分钟；换成真实时钟就早过期了（拨测试时钟会让 AssetStore 的轮询空转太多次）
        f3 <- ZIO.withClock(Clock.ClockLive)(vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试"))).fork
        w3 <- ZIO.withClock(Clock.ClockLive)(asks(1))
        _  <- f3.interrupt
      yield assertTrue(w.size == 1, r1 == r2, r1 == Map("PK_TOKEN" -> "v"), w3.size == 1)
    }
  ).provideSomeLayer[Scope](TestEnv.layer) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
