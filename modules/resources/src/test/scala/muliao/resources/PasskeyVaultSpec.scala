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

  def answer(id: String, d: AskDecision) = ZIO.serviceWithZIO[Grants](_.answer(id, d))

  def in(session: String, what: String) = AskPurpose(Some(SessionId.wrap(session)), what)

  def use(vault: Vault, agent: AgentId, p: AskPurpose = in("s1", "echo $PK_TOKEN")) = vault.reveal(agent, List("PK_TOKEN"), SecretUse.Script, p)

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
    test("没授权时每次调用各自一个请求：带会话与命令；一次只结这一个；一段时间结掉这个 agent 这个 secret 的全部、之后不再问；按 agent 隔离；拒绝；中断撤请求") {
      for
        (a, b, vault) <- setup
        sub   <- ZIO.serviceWithZIO[Grants](_.asked)
        shell <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Shell, in("s1", "x")).either
        f1    <- use(vault, a.id, in("s1", "cmd1")).fork
        _     <- asks(1)
        f2    <- use(vault, a.id, in("s2", "cmd2")).fork
        w2    <- asks(2)
        note  <- sub.take
        _     <- answer(w2.find(_.purpose.what == "cmd2").get.id, AskDecision.Once("v-once"))
        r2    <- f2.join
        w1    <- asks(1)
        fb    <- use(vault, b.id, in("s3", "bob")).fork
        wb    <- asks(2)
        f3    <- use(vault, a.id, in("s1", "cmd3")).fork
        _     <- asks(3)
        _     <- answer(w1.head.id, AskDecision.For("v-hour", 60))
        r1    <- f1.join
        r3    <- f3.join
        left  <- asks(1)
        r4    <- use(vault, a.id)
        g1    <- ZIO.serviceWithZIO[Grants](_.view.map(_._2))
        _     <- answer(left.head.id, AskDecision.Deny)
        rb    <- fb.join.either
        f5    <- use(vault, b.id).fork
        _     <- asks(1)
        _     <- f5.interrupt
        gone  <- asks(0)
        late  <- answer(w1.head.id, AskDecision.Once("迟了")).either
      yield assertTrue(
        reason(shell).contains("不能注入自写命令"),
        w2.map(x => (x.agent, x.key, x.purpose, x.box)).toSet == Set((a.id, "PK_TOKEN", in("s1", "cmd1"), box), (a.id, "PK_TOKEN", in("s2", "cmd2"), box)),
        note == Asked(Slot(a.id, "PK_TOKEN"), in("s1", "cmd1")),
        r2 == Map("PK_TOKEN" -> "v-once"),
        w1.map(_.purpose.what) == List("cmd1"),
        wb.map(_.agent).contains(b.id),
        r1 == Map("PK_TOKEN" -> "v-hour"),
        r3 == Map("PK_TOKEN" -> "v-hour"),
        left.map(_.agent) == List(b.id),
        r4 == Map("PK_TOKEN" -> "v-hour"),
        g1.map(g => (g.agent, g.key, g.until)) == List((a.id, "PK_TOKEN", 60 * 60_000L)), // 测试时钟从 0 开始
        reason(rb).contains("拒绝"),
        gone.isEmpty,
        reason(late).contains("已经没有了")
      )
    },
    test("一段时间的授权到期后再问；收回后再问") {
      for
        (a, _, vault) <- setup
        f1 <- use(vault, a.id).fork
        w  <- asks(1)
        _  <- answer(w.head.id, AskDecision.For("v", 30))
        r1 <- f1.join
        // 授权按测试时钟（从 0 起）到 30 分钟；换成真实时钟就早过期了（拨测试时钟会让 AssetStore 的轮询空转太多次）
        f2 <- ZIO.withClock(Clock.ClockLive)(use(vault, a.id)).fork
        w2 <- ZIO.withClock(Clock.ClockLive)(asks(1))
        _  <- f2.interrupt
        f3 <- use(vault, a.id).fork
        w3 <- asks(1)
        _  <- answer(w3.head.id, AskDecision.For("v", 30))
        _  <- f3.join
        _  <- ZIO.serviceWithZIO[Grants](_.revoke(Slot(a.id, "PK_TOKEN")))
        f4 <- use(vault, a.id).fork
        w4 <- asks(1)
        _  <- f4.interrupt
      yield assertTrue(r1 == Map("PK_TOKEN" -> "v"), w2.size == 1, w4.size == 1)
    }
  ).provideSomeLayer[Scope](TestEnv.layer) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
