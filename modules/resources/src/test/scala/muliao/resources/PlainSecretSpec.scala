package muliao.resources

import muliao.contract.*
import muliao.resources.agents.{AgentPatch, Agents, NewAgent}
import muliao.resources.guard.{AssetGuard, Expectation}
import muliao.resources.tree.{Formats, Owner, SecretBody, SecretFile}
import muliao.resources.vault.*
import zio.*
import zio.test.*

import java.nio.file.{Files, Path}

/** 明文保存的 secret（#1093）：文件写 value、只能是口令；agent 照常使用；vault_set 不能新建、改已有的仍是明文；手改能改值，不能换保存方式。 */
object PlainSecretSpec extends ZIOSpecDefault:

  def reason(e: Either[muliao.platform.AppError, ?]): String = e.left.toOption.map(_.message).getOrElse("（没被拒）")

  def guard(actor: Actor, p: Path, content: String) =
    ZIO.serviceWithZIO[AssetGuard](_.check(actor, p, content, Expectation.Unconditional))

  val why = AskPurpose(None, "测试")

  def spec = suite("明文保存的 secret")(
    test("文件格式：value 往返（含换行与引号）；只能是口令；与 ciphertext、[passkey] 只能有一个") {
      val f    = SecretFile("地址", SecretBody.Plain("https://x.example/a?b=\"c\"\n第二行", RevealScope.Scripts))
      val text = Formats.renderSecret(f)
      assertTrue(
        Formats.parseSecret(text) == Right(f),
        text.contains("value = "),
        !text.contains("ciphertext"),
        Formats.parseSecret("value = \"v\"\n") == Right(SecretFile("", SecretBody.Plain("v", RevealScope.Shell))),
        Formats.parseSecret("value = \"v\"\nkeypair = \"ed25519\"\naddress = \"39nhYfstc6AJ9wHfERt9BZfSiTa5ZpSE15cWVGkgEr7c\"\n").left.exists(_.contains("只能是口令")),
        Formats.parseSecret("value = \"v\"\nusage = \"sign\"\n").left.exists(_.contains("sign")),
        Formats.parseSecret("value = \"v\"\nciphertext = \"x\"\n").left.exists(_.contains("只能有一个")),
        SecretBody.Plain("v", RevealScope.Scripts).view == SecretView.Plain("v", RevealScope.Scripts),
        SecretBody.Server("t", SecretContent.Opaque(RevealScope.Shell)).view == SecretView.Server(SecretContent.Opaque(RevealScope.Shell))
      )
    },
    test("用户设成明文：文件里是原值；agent 照常取用（范围照旧管）；不能拿来签名；agent 改值仍是明文、可改范围；agent 新建的总是加密") {
      for
        a     <- ZIO.serviceWithZIO[Agents](_.create(NewAgent("alice", "x")))
        b     <- ZIO.serviceWithZIO[Agents](_.create(NewAgent("bob", "x")))
        vault <- ZIO.service[Vault]
        dirs  <- ZIO.service[TestEnv.Dirs]
        n     <- vault.set("PLAIN_URL", SecretInput.ByUser(SecretValue.Plain("https://a.example", RevealScope.Scripts), Owner.Agent(a.name)), Some("地址"))
        _     <- ZIO.serviceWithZIO[Agents](_.update(b.id, AgentPatch(allow = Some(List("*:secret:PLAIN_*")))))
        file   = dirs.assets.resolve("alice/secrets/PLAIN_URL.toml")
        raw   <- ZIO.attemptBlocking(Files.readString(file))
        s1    <- vault.reveal(b.id, List("PLAIN_URL"), SecretUse.Script, why)
        sh    <- vault.reveal(b.id, List("PLAIN_URL"), SecretUse.Shell, why).either
        sig   <- vault.sign(a.id, "PLAIN_URL", SignContent.SshCert(SshCertRequest("ssh-ed25519 AAAA", List("a@b"), 5, None, "id")), why).either
        _     <- vault.set("PLAIN_URL", SecretInput.ByAgent(b.id, "https://b.example", Some(RevealScope.Shell)), None)
        raw2  <- ZIO.attemptBlocking(Files.readString(file))
        s2    <- vault.reveal(b.id, List("PLAIN_URL"), SecretUse.Shell, why)
        _     <- vault.set("PLAIN_NEW", SecretInput.ByAgent(b.id, "v", None), None)
        raw3  <- ZIO.attemptBlocking(Files.readString(dirs.assets.resolve("bob/secrets/PLAIN_NEW.toml")))
        empty <- vault.set("PLAIN_URL", SecretInput.ByUser(SecretValue.Plain("", RevealScope.Shell), Owner.Platform), None).either
      yield assertTrue(
        n,
        raw.contains("value = \"https://a.example\""),
        raw.contains("usage = \"scripts\""),
        raw.contains("description = \"地址\""),
        s1 == Map("PLAIN_URL" -> "https://a.example"),
        reason(sh).contains("不能注入自写命令"),
        reason(sig).contains("不是 ed25519 密钥对"),
        raw2.contains("value = \"https://b.example\""),
        !raw2.contains("usage"),
        raw2.contains("description = \"地址\""),
        s2 == Map("PLAIN_URL" -> "https://b.example"),
        raw3.contains("ciphertext"),
        !raw3.contains("value"),
        reason(empty).contains("不能为空")
      )
    },
    test("手改：明文的能改 value、description、usage；不能在明文与加密之间换，也不能给明文加 keypair") {
      for
        a     <- ZIO.serviceWithZIO[Agents](_.create(NewAgent("alice", "x")))
        vault <- ZIO.service[Vault]
        dirs  <- ZIO.service[TestEnv.Dirs]
        _     <- vault.set("PLAIN_URL", SecretInput.ByUser(SecretValue.Plain("v1", RevealScope.Shell), Owner.Agent(a.name)), Some("地址"))
        _     <- vault.set("ENC_KEY", SecretInput.ByAgent(a.id, "secret", None), Some("加密的"))
        plain  = dirs.assets.resolve("alice/secrets/PLAIN_URL.toml")
        enc    = dirs.assets.resolve("alice/secrets/ENC_KEY.toml")
        ct    <- ZIO.attemptBlocking(Files.readString(enc)).map(_.linesIterator.find(_.startsWith("ciphertext")).get)
        ok    <- guard(Actor.Agent(a.id), plain, "description = \"新说明\"\nusage = \"scripts\"\nvalue = \"v2\"\n").either
        toEnc <- guard(Actor.Agent(a.id), plain, s"description = \"地址\"\n$ct\n").either
        toPln <- guard(Actor.Agent(a.id), enc, "description = \"加密的\"\nvalue = \"明文\"\n").either
        kp    <- guard(Actor.Agent(a.id), plain, "description = \"地址\"\nkeypair = \"ed25519\"\naddress = \"39nhYfstc6AJ9wHfERt9BZfSiTa5ZpSE15cWVGkgEr7c\"\nvalue = \"v2\"\n").either
        fresh <- guard(Actor.User, dirs.assets.resolve("alice/secrets/HAND_MADE.toml"), "value = \"v\"\n").either
      yield assertTrue(
        ok.isRight,
        reason(toEnc).contains("只能手改"),
        reason(toPln).contains("只能手改"),
        reason(kp).contains("只能是口令"),
        reason(fresh).contains("vault_set")
      )
    }
  ).provideLayer(TestEnv.layer) @@ TestAspect.sequential
