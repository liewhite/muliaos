package muliao.resources

import muliao.contract.*
import muliao.resources.agents.{AgentPatch, Agents, NewAgent}
import muliao.resources.tree.{Formats, Owner, SecretBody, SecretFile}
import muliao.resources.vault.*
import zio.*
import zio.test.*
import zio.json.ast.Json
import muliao.resources.tools.AgentTools
import muliao.toolkit.ToolContext

import java.nio.file.Files

/** passkey 加密的 secret（#915）：只存不解、等用户授权、一次与一段时间、按 agent 隔离、拒绝与中断。 */
object PasskeyVaultSpec extends ZIOSpecDefault:
  val box = PasskeyBox("muliao.example.com", "Y3JlZA", "aXY=", "Y3Q=")

  /** passkey 密钥对的公开身份由浏览器算好带来（#1098）：测试里随便一把 ed25519 公钥。 */
  val caId: KeyIdentity = KeyIdentity.Ed25519(SshCert.PublicKey.of(Array.fill[Byte](32)(1)).toOption.get)

  def reason(e: Either[muliao.platform.AppError, ?]): String = e.left.toOption.map(_.message).getOrElse("（没被拒）")

  /** 等到有 `n` 个请求在等。 */
  def asks(n: Int): URIO[Grants, List[SecretAsk]] =
    ZIO.serviceWithZIO[Grants](_.view.map(_._1)).repeatUntil(_.size == n)

  def answer(id: AskId, d: AskDecision) = ZIO.serviceWithZIO[Grants](_.answer(id, d))

  def in(session: String, what: String) = AskPurpose(Some(SessionId.wrap(session)), what)

  def use(vault: Vault, agent: AgentId, p: AskPurpose = in("s1", "echo $PK_TOKEN")) = vault.reveal(agent, List("PK_TOKEN"), SecretUse.Script, p)

  /** alice 有一个 passkey secret（用户设的），bob 也被允许用它。 */
  val setup =
    for
      a     <- ZIO.serviceWithZIO[Agents](_.create(NewAgent("alice", "x")))
      b     <- ZIO.serviceWithZIO[Agents](_.create(NewAgent("bob", "x")))
      vault <- ZIO.service[Vault]
      _     <- vault.set("PK_TOKEN", SecretInput.ByUser(SecretValue.Passkey(box, SecretContent.Opaque(RevealScope.Scripts)), Owner.Agent(a.name)), Some("说明"))
      _     <- ZIO.serviceWithZIO[Agents](_.update(b.id, AgentPatch(allow = Some(List("use:secret:PK_*")))))
    yield (a, b, vault)

  val spec = suite("passkey secret")(
    test("文件格式：[passkey] 表往返；ciphertext 与 [passkey] 恰好一个") {
      val f = SecretFile("d", SecretBody.Passkey(box, SecretContent.Opaque(RevealScope.Scripts)))
      val text = Formats.renderSecret(f)
      assertTrue(
        Formats.parseSecret(text) == Right(f),
        text.contains("[passkey]"),
        Formats.parseSecret("ciphertext = \"x\"\n[passkey]\nrp = \"a\"\ncredential = \"b\"\niv = \"c\"\nciphertext = \"d\"\n").isLeft,
        Formats.parseSecret("description = \"d\"\n").isLeft,
        Formats.parseSecret("[passkey]\nrp = \"a\"\n").isLeft
      )
    },
    test("只有用户能设、能改值（agent 交不出 passkey 值，类型上写不出）；agent 的 vault_set 改不动；说明与明文范围照常") {
      for
        (a, _, vault) <- setup
        dirs  <- ZIO.service[TestEnv.Dirs]
        raw   <- ZIO.succeed(Files.readString(dirs.assets.resolve("alice/secrets/PK_TOKEN.toml")))
        e1    <- vault.set("PK_TOKEN", SecretInput.ByAgent(a.id, "降级", None), None).either
        n     <- vault.set("PK_TOKEN", SecretInput.ByUser(SecretValue.Server("用户改回普通的", SecretShape.Opaque(RevealScope.Scripts)), Owner.Platform), None)
        v     <- vault.reveal(a.id, List("PK_TOKEN"), SecretUse.Script, muliao.contract.AskPurpose(None, "测试"))
      yield assertTrue(
        raw.contains("[passkey]"),
        raw.contains("usage = \"scripts\""),
        reason(e1).contains("只能由用户在界面上改值"),
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
        note == Asked(Slot(a.id, "PK_TOKEN"), AskWant.Reveal, in("s1", "cmd1")),
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
    ,
    test("passkey 的只能签名密钥对（#938、#1069）：文件写 keypair 与 usage = sign；只有用户能设；取明文被拒、不发请求；口令不能拿来签") {
      for
        (a, _, vault) <- setup
        _     <- vault.set("SSH_CA", SecretInput.ByUser(SecretValue.Passkey(box, SecretContent.KeyPair(caId, KeyExposure.SignOnly)), Owner.Agent(a.name)), Some("CA"))
        dirs  <- ZIO.service[TestEnv.Dirs]
        raw   <- ZIO.succeed(Files.readString(dirs.assets.resolve("alice/secrets/SSH_CA.toml")))
        e1    <- vault.set("SSH_CA", SecretInput.ByAgent(a.id, "降级", None), None).either
        rv    <- vault.reveal(a.id, List("SSH_CA"), SecretUse.Script, in("s1", "cat")).either
        none  <- ZIO.serviceWithZIO[Grants](_.view.map(_._1))
        req    = SshCertRequest("ssh-ed25519 AAAA", List("admin@relay"), 5, None, "id")
        gen   <- vault.sign(a.id, "PK_TOKEN", SignContent.SshCert(req), in("s1", "x")).either
      yield assertTrue(
        raw.contains("[passkey]"),
        raw.contains("keypair = \"ed25519\""),
        raw.contains(s"address = \"${caId.address}\""),
        Formats.parseSecret(raw.replace(s"address = \"${caId.address}\"\n", "")).left.exists(_.contains("缺少公开身份")),
        Formats.parseSecret(raw.replace(caId.address, "0x" + "1" * 40)).left.exists(_.contains("address")),
        Formats.parseSecret("description = \"d\"\naddress = \"x\"\nciphertext = \"x\"\n").left.exists(_.contains("只有密钥对")),
        raw.contains("usage = \"sign\""),
        Formats.parseSecret(raw) == Right(SecretFile("CA", SecretBody.Passkey(box, SecretContent.KeyPair(caId, KeyExposure.SignOnly)))),
        Formats.parseSecret("description = \"d\"\nusage = \"sign\"\nciphertext = \"x\"\n").left.exists(_.contains("只有密钥对")),
        Formats.parseSecret("description = \"d\"\nusage = \"any\"\nciphertext = \"x\"\n").isLeft,
        reason(e1).contains("只能由用户在界面上改值"),
        reason(rv).contains("取不出明文"),
        none.isEmpty,
        reason(gen).contains("不是 ed25519 密钥对")
      )
    },
    test("签 SSH 证书（#938）：工具发出请求（带能登哪些、多久、记录名）；每张都要批；答错种类被拒；交回证书后写好钥匙与证书；拒绝") {
      for
        (a, _, vault) <- setup
        _     <- vault.set("SSH_CA", SecretInput.ByUser(SecretValue.Passkey(box, SecretContent.KeyPair(caId, KeyExposure.SignOnly)), Owner.Agent(a.name)), Some("CA"))
        tool  <- toolOf("ssh_certificate")
        sub   <- ZIO.serviceWithZIO[Grants](_.asked)
        bad   <- tool.invoke(Json.Obj("logins" -> Json.Arr(Json.Str("relay"))), Ctx(a.id)).either
        f1    <- tool.invoke(Json.Obj("logins" -> Json.Arr(Json.Str("admin@relay"), Json.Str("root@jlp")), "minutes" -> Json.Num(10)), Ctx(a.id)).fork
        w1    <- asks(1)
        note  <- sub.take
        once  <- answer(w1.head.id, AskDecision.Once("pem")).either
        span  <- answer(w1.head.id, AskDecision.For("pem", 5)).either
        still <- asks(1)
        _     <- answer(w1.head.id, AskDecision.Signed("ssh-ed25519-cert-v01@openssh.com AAAA muliao"))
        out   <- f1.join
        path   = (out match { case muliao.toolkit.ToolOutcome.Completed(cs) => cs.collect { case muliao.toolkit.ToolContent.Text(t) => t }.mkString }).linesIterator.collectFirst { case l if l.startsWith("钥匙：") => l.stripPrefix("钥匙：").takeWhile(_ != '（') }.get
        cert  <- ZIO.attemptBlocking(Files.readString(java.nio.file.Path.of(path + "-cert.pub")))
        f2    <- tool.invoke(Json.Obj("logins" -> Json.Arr(Json.Str("admin@relay"))), Ctx(a.id)).fork
        w2    <- asks(1)
        _     <- answer(w2.head.id, AskDecision.Deny)
        denied <- f2.join.either
      yield
        val want = w1.head.want
        assertTrue(
          bad.isLeft,
          want match
            case AskWant.Sign(SignContent.SshCert(r), `caId`) =>
              r.principals == List("admin@relay", "root@jlp") && r.minutes == 10 && r.keyId == "muliao:alice:s" && SshCert.readPublic(r.publicKey).isRight
            case _ => false
          ,
          w1.head.purpose.session.contains(SessionId.wrap("s")),
          note.want == want,
          reason(once).contains("不符"),
          reason(span).contains("不符"),
          still.map(_.id) == w1.map(_.id),
          cert == "ssh-ed25519-cert-v01@openssh.com AAAA muliao\n",
          denied.left.toOption.exists(_.toString.contains("拒绝"))
        )
    },
    test("服务端保管的只能签名密钥对（#1069）：值要是认得的私钥；取明文被拒；ssh_certificate 在服务端直接签好（不发请求），证书由这把 CA 签、验得过；手改不能放开成可取明文") {
      for
        (a, _, vault) <- setup
        ca    <- SshKeys.generate
        pem    = SshCert.writePrivate(ca, 7, "ca")
        signOnly = SecretShape.KeyPair(SigningAlgorithm.Ed25519, KeyExposure.SignOnly)
        bad   <- vault.set("SSH_CA", SecretInput.ByUser(SecretValue.Server("不是私钥", signOnly), Owner.Agent(a.name)), Some("CA")).either
        _     <- vault.set("SSH_CA", SecretInput.ByUser(SecretValue.Server(pem, signOnly), Owner.Agent(a.name)), Some("CA"))
        rv    <- vault.reveal(a.id, List("SSH_CA"), SecretUse.Script, in("s1", "cat")).either
        tool  <- toolOf("ssh_certificate")
        out   <- tool.invoke(Json.Obj("logins" -> Json.Arr(Json.Str("admin@relay"))), Ctx(a.id))
        pending <- ZIO.serviceWithZIO[Grants](_.view.map(_._1))
        path   = (out match { case muliao.toolkit.ToolOutcome.Completed(cs) => cs.collect { case muliao.toolkit.ToolContent.Text(t) => t }.mkString }).linesIterator.collectFirst { case l if l.startsWith("钥匙：") => l.stripPrefix("钥匙：").takeWhile(_ != '（') }.get
        cert  <- ZIO.attemptBlocking(Files.readString(java.nio.file.Path.of(path + "-cert.pub")))
        dirs  <- ZIO.service[TestEnv.Dirs]
        file   = dirs.assets.resolve("alice/secrets/SSH_CA.toml")
        raw   <- ZIO.attemptBlocking(Files.readString(file))
        loosen <- ZIO.serviceWithZIO[muliao.resources.guard.AssetGuard](_.check(Actor.Agent(a.id), file, raw.replace("usage = \"sign\"\n", ""), muliao.resources.guard.Expectation.Unconditional)).either
        toScripts <- ZIO.serviceWithZIO[muliao.resources.guard.AssetGuard](_.check(Actor.Agent(a.id), file, raw.replace("usage = \"sign\"", "usage = \"scripts\""), muliao.resources.guard.Expectation.Unconditional)).either
        // 公开身份从私钥算出（#1098），手改不动
        caAddr     = chain.Base58.encode(ca.publicKey.bytes)
        reAddr    <- ZIO.serviceWithZIO[muliao.resources.guard.AssetGuard](_.check(Actor.Agent(a.id), file, raw.replace(caAddr, caId.address), muliao.resources.guard.Expectation.Unconditional)).either
        widen <- vault.set("SSH_CA", SecretInput.ByAgent(a.id, pem, Some(RevealScope.Shell)), None).either
      yield
        // 证书行：类型 base64 注释；签名在末尾（string 类型名 + string 64 字节签名），签的是之前的全部字节
        val blob = java.util.Base64.getDecoder.decode(cert.trim.split(" ")(1))
        val sig  = blob.takeRight(64)
        val tbs  = blob.dropRight(4 + 4 + SshCert.KeyType.length + 4 + 64)
        val pub  = java.security.KeyFactory.getInstance("Ed25519").generatePublic(
          java.security.spec.X509EncodedKeySpec(java.util.HexFormat.of().parseHex("302a300506032b6570032100") ++ ca.publicKey.bytes)
        )
        val v = java.security.Signature.getInstance("Ed25519")
        v.initVerify(pub)
        v.update(tbs)
        assertTrue(
          reason(bad).contains("不是认得的私钥"),
          reason(rv).contains("取不出明文"),
          pending.isEmpty,
          cert.startsWith(SshCert.CertType + " "),
          v.verify(sig),
          loosen.isLeft,
          toScripts.isLeft,
          raw.contains(s"address = \"$caAddr\""),
          reAddr.isLeft,
          reason(widen).contains("只能签名")
        )
    }
  ).provideSomeLayer[Scope](TestEnv.layer) @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)

  final case class Ctx(agent: AgentId) extends ToolContext:
    val session = SessionId.wrap("s")
    val call    = CallId.wrap("c")

  def toolOf(name: String) =
    for
      ags   <- ZIO.service[Agents]
      res   <- ZIO.service[Resources]
      store <- ZIO.service[muliao.resources.tree.AssetStore]
      vault <- ZIO.service[Vault]
      paths <- ZIO.service[ResourcePaths]
    yield AgentTools.all(ags, res, store, vault, TestEnv.tools, paths).find(_.spec.name == name).get
