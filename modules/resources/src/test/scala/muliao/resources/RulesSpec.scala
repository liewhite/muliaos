package muliao.resources

import muliao.contract.{AgentId, AgentName, RoleName, AssetKind}
import muliao.resources.access.*
import muliao.resources.tree.Owner
import muliao.toolkit.Access
import zio.test.*

object RulesSpec extends ZIOSpecDefault:

  def rule(s: String): Rule = Rule.parse(s).toOption.get

  val me    = AgentName.wrap("me")
  val other = Owner.Agent(AgentName.wrap("other"))

  def perms(allow: List[String] = Nil, deny: List[String] = Nil, roles: List[String] = Nil, access: AccessConfig = AccessConfig.empty) =
    val id = AgentId.wrap("a1")
    Permissions.of(id, me, access.copy(agents = access.agents.updated(id, AgentGrants(roles.map(RoleName.wrap), allow.map(rule), deny.map(rule)))))

  def spec = suite("规则与权限判断")(
    test("规则解析：动作、种类、通配、path；不合法的说明原因") {
      val bad = List("use:skill", "read:skill:x", "use:thing:x", "use:skill:", "write:tool:x", "use:path:/tmp", "write:path:relative")
      assertTrue(
        rule("use:skill:relay-host").selects(Action.Use, AssetKind.Skill, "relay-host"),
        !rule("use:skill:relay-host").selects(Action.Write, AssetKind.Skill, "relay-host"),
        rule("*:memory:trading").selects(Action.Write, AssetKind.Memory, "trading"),
        rule("use:secret:RELAY_*").selects(Action.Use, AssetKind.Secret, "RELAY_KEY"),
        !rule("use:secret:RELAY_*").selects(Action.Use, AssetKind.Secret, "XRELAY_KEY"),
        rule("use:*:x?z").selects(Action.Use, AssetKind.Mcp, "xyz"),
        rule("use:tool:trigger_[cd]*").selects(Action.Use, AssetKind.Tool, "trigger_delete"),
        !rule("use:tool:trigger_[!cd]*").selects(Action.Use, AssetKind.Tool, "trigger_delete"),
        rule("use:skill:a.b").selects(Action.Use, AssetKind.Skill, "a.b"),
        !rule("use:skill:a.b").selects(Action.Use, AssetKind.Skill, "axb"),
        Rule.pathOf(rule("write:path:~/projects/x")).exists(_.toString.endsWith("/projects/x")),
        !Rule.pathOf(rule("write:path:~/projects/x")).exists(_.toString.startsWith("~")),
        bad.forall(b => Rule.parse(b).isLeft),
        Rule.parse("write:tool:x").left.exists(_.contains("工具只能 use"))
      )
    },
    test("自己的资源总能用能改，deny 也拦不住") {
      val p = perms(deny = List("*:*:*"))
      assertTrue(
        p.decide(Action.Write, AssetKind.Skill, "mine", Owner.Agent(me)) == Decision.Allowed(Grant.Ownership),
        !p.decide(Action.Use, AssetKind.Skill, "x", other).allowed
      )
    },
    test("base：全部非管理类工具与全局记忆；管理类工具要规则") {
      val p = perms()
      assertTrue(
        p.canUseTool("read_file", Access.Read),
        p.canUseTool("run_shell", Access.Write),
        !p.canUseTool("create_agent", Access.Admin),
        p.decide(Action.Use, AssetKind.Memory, "global", Owner.Platform) == Decision.Allowed(Grant.GlobalMemory),
        !p.decide(Action.Write, AssetKind.Memory, "global", Owner.Platform).allowed,
        !p.decide(Action.Use, AssetKind.Memory, "trading", Owner.Platform).allowed,
        perms(allow = List("use:tool:create_agent")).canUseTool("create_agent", Access.Admin)
      )
    },
    test("deny 优先于角色与自己的 allow、也收回 base 的工具；记录来自哪条规则") {
      val access = AccessConfig(
        Map(RoleName.wrap("trader") -> RoleDef("交易", List(rule("*:memory:trading"), rule("use:skill:*")), Nil)),
        Map.empty
      )
      val p = perms(roles = List("trader"), deny = List("use:skill:danger", "use:tool:run_shell"), access = access)
      assertTrue(
        p.decide(Action.Write, AssetKind.Memory, "trading", Owner.Platform) ==
          Decision.Allowed(Grant.ByRule(rule("*:memory:trading"), Source.Role(RoleName.wrap("trader")))),
        p.decide(Action.Use, AssetKind.Skill, "danger", other) == Decision.Denied(rule("use:skill:danger"), Source.Own),
        p.decide(Action.Use, AssetKind.Skill, "safe", other).allowed,
        !p.canUseTool("run_shell", Access.Write)
      )
    },
    test("admin：全部工具、能用能改全部资源，自己的 deny 仍能收回") {
      val p = perms(roles = List("admin"), deny = List("write:secret:PROD_*"))
      assertTrue(
        p.isAdmin,
        p.canUseTool("create_agent", Access.Admin),
        p.decide(Action.Write, AssetKind.Skill, "any", other).allowed,
        !p.decide(Action.Write, AssetKind.Secret, "PROD_KEY", other).allowed,
        p.decide(Action.Use, AssetKind.Secret, "PROD_KEY", other).allowed
      )
    },
    test("write:path 与 deny write:path") {
      val p = perms(allow = List("write:path:/opt/a", "write:path:/opt/b"), deny = List("write:path:/opt/a/keep"))
      assertTrue(p.writablePaths.map(_.toString) == List("/opt/a", "/opt/b"), p.deniedPaths.map(_.toString) == List("/opt/a/keep"))
    },
    test("access.toml：解析、按固定格式写回再解析得到同样内容；未知字段、坏规则整份不合法") {
      val text =
        """[roles.trader]
          |description = "交易类"
          |allow = ["*:memory:trading", "use:skill:relay-host"]
          |
          |[roles.base]
          |description = "人人都有"
          |allow = ["use:skill:code-dev-standards"]
          |
          |[agents.0bb6505d4aff] # aacoin-aws
          |roles = ["trader"]
          |allow = ["write:path:~/.ssh/x.pem"]
          |deny = ["use:tool:trigger_*"]
          |""".stripMargin
      val parsed   = AccessConfig.parse(text)
      val rendered = parsed.map(c => AccessConfig.render(c, id => Option.when(id.value == "0bb6505d4aff")(AgentName.wrap("aacoin-aws"))))
      val again    = rendered.flatMap(AccessConfig.parse)
      assertTrue(
        parsed.isRight,
        again == parsed,
        rendered.exists(_.contains("[agents.0bb6505d4aff] # aacoin-aws")),
        AccessConfig.parse("[roles.x]\ndescription = \"\"\nallowed = []\n").left.exists(_.contains("roles.x.allowed")),
        AccessConfig.parse("[roles.x]\nallow = [\"use:skil:a\"]\n").left.exists(_.contains("skil")),
        AccessConfig.parse("[agents.a]\nroles = \"x\"\n").isLeft,
        AccessConfig.parse("not toml [").isLeft
      )
    },
    test("access.toml 的引用问题：不存在的角色、不存在的 agent") {
      val c = AccessConfig.parse("[agents.a1]\nroles = [\"nope\", \"admin\"]\n[agents.gone]\nroles = []\n").toOption.get
      assertTrue(c.problems(Set(AgentId.wrap("a1"))) == List("[agents.a1] 引用了不存在的角色 nope", "[agents.gone] 不是现有的 agent"))
    }
  )
