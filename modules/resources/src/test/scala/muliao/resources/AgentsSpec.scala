package muliao.resources

import muliao.contract.{AgentState, Thinking}
import muliao.contract.{AgentId, RoleName}
import muliao.platform.AppError
import muliao.resources.agents.*
import muliao.resources.tree.{AssetStore, Owner}
import muliao.resources.vault.{Actor, Vault}
import zio.*
import zio.test.*

import java.nio.file.Files

object AgentsSpec extends ZIOSpecDefault:
  import TestEnv.write

  val agents = ZIO.service[Agents]

  def create(name: String, roles: List[String] = Nil, allow: List[String] = Nil) =
    agents.flatMap(_.create(NewAgent(name, s"$name 的职责", roles = roles.map(RoleName.wrap), allow = allow)))

  def reason(e: Either[AppError, ?]): String = e.left.toOption.map(_.message).getOrElse("（没被拒）")

  def busy(id: AgentId, why: String) = ZIO.serviceWithZIO[TestEnv.FakeHook](_.busyFor.update(_.updated(id, why)))
  def idle(id: AgentId)              = ZIO.serviceWithZIO[TestEnv.FakeHook](_.busyFor.update(_ - id))

  def spec = suite("agent 身份与生命周期")(
    test("新建：库里一行、资源目录（agent.toml、记忆）、工作目录、access.toml 里的角色与规则") {
      for
        dirs  <- ZIO.service[TestEnv.Dirs]
        a     <- agents.flatMap(_.create(NewAgent("小助手", "帮忙", Some("deepseek:v4"), Thinking.High, Some("日常"), List(RoleName.wrap("admin")), List("write:path:/opt/x"))))
        prof  <- agents.flatMap(_.profile(a.id))
        store <- ZIO.service[AssetStore]
        acc   <- store.access
        found <- agents.flatMap(_.find("小助手"))
      yield assertTrue(
        a.id.value.matches("[0-9a-f]{12}"),
        prof.model == "deepseek:v4",
        prof.thinking == Thinking.High,
        prof.group.contains("日常"),
        Files.isRegularFile(dirs.assets.resolve("小助手/memory.md")),
        Files.isDirectory(dirs.workspaces.resolve(a.id.value)),
        acc.config.grantsOf(a.id).roles == List(RoleName.wrap("admin")),
        acc.config.grantsOf(a.id).allow.map(_.text) == List("write:path:/opt/x"),
        Files.readString(dirs.assets.resolve("platform/access.toml")).contains(s"[agents.${a.id.value}] # 小助手"),
        found.map(_.id).contains(a.id)
      )
    },
    test("名字：唯一且不分大小写、不能叫 user 或 platform、不能含 /；模型要能跑；规则要合法") {
      for
        _ <- create("Alice")
        e <- ZIO.foreach(List("alice", "USER", "Platform", "a/b", ".x", " "))(n => create(n).either)
        m <- agents.flatMap(_.create(NewAgent("bob", "x", Some("nope:m")))).either
        r <- agents.flatMap(_.create(NewAgent("bob", "x", allow = List("use:skil:x")))).either
        n <- agents.flatMap(_.create(NewAgent("bob", "x", allow = List("use:skill:nonexistent")))).either
        b <- agents.flatMap(_.find("bob"))
      yield assertTrue(
        reason(e(0)).contains("已有叫 Alice"),
        e.drop(1).forall(_.isLeft),
        reason(m).contains("nope:m"),
        reason(r).contains("skil"),
        reason(n).contains("nonexistent"),
        b.isEmpty
      )
    },
    test("改名：资源目录跟着挪，编号、权限不变；只改大小写也行") {
      for
        dirs  <- ZIO.service[TestEnv.Dirs]
        a     <- create("alice", roles = List("admin"))
        _      = write(dirs.assets.resolve("alice/skills/mine/SKILL.md"), "---\ndescription: x\n---\n")
        b     <- agents.flatMap(_.update(a.id, AgentPatch(name = Some("bob"))))
        snap  <- ZIO.serviceWithZIO[AssetStore](_.snapshot)
        acc   <- ZIO.serviceWithZIO[AssetStore](_.access)
        c     <- agents.flatMap(_.update(a.id, AgentPatch(name = Some("Bob"))))
        names <- ZIO.succeed(Files.list(dirs.assets).toArray.map(_.toString.split('/').last).toSet)
        e     <- create("bob").either
      yield assertTrue(
        b.id == a.id,
        b.name.value == "bob",
        b.renamedFrom.isEmpty,
        snap.skill("mine").exists(_.owner.label == "bob"),
        acc.config.grantsOf(a.id).roles == List(RoleName.wrap("admin")),
        c.name.value == "Bob",
        names.contains("Bob"),
        !names.contains("bob"),
        !names.contains("alice"),
        reason(e).contains("已有叫 Bob")
      )
    },
    test("改名挪到一半服务重启：起动时接着挪完") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- create("alice")
        db   <- ZIO.service[muliao.platform.db.Database]
        // 模拟：库里已记下新名，目录还没挪
        _    <- db.write(AgentRepo.update(a.copy(name = muliao.contract.AgentName.wrap("carol"), nameKey = "carol", renamedFrom = Some(a.name))))
        _    <- agents.flatMap(_.resumePending)
        got  <- agents.flatMap(_.get(a.id))
      yield assertTrue(got.renamedFrom.isEmpty, Files.isDirectory(dirs.assets.resolve("carol")), !Files.exists(dirs.assets.resolve("alice")))
    },
    test("默认 agent 至多一个；归档要空闲，归档后不接新工作，也不再是默认") {
      for
        a  <- create("alice")
        b  <- create("bob")
        _  <- agents.flatMap(_.update(a.id, AgentPatch(isDefault = Some(true))))
        _  <- agents.flatMap(_.update(b.id, AgentPatch(isDefault = Some(true))))
        l1 <- agents.flatMap(_.list)
        _  <- busy(b.id, "有一个会话在运行")
        e1 <- agents.flatMap(_.update(b.id, AgentPatch(archived = Some(true)))).either
        _  <- idle(b.id)
        x  <- agents.flatMap(_.update(b.id, AgentPatch(archived = Some(true))))
        e2 <- agents.flatMap(_.requireActive(b.id)).either
        y  <- agents.flatMap(_.update(b.id, AgentPatch(archived = Some(false))))
      yield assertTrue(
        l1.filter(_.isDefault).map(_.id) == List(b.id),
        reason(e1).contains("有一个会话在运行"),
        x.state == AgentState.Archived,
        !x.isDefault,
        reason(e2).contains("已归档"),
        y.state == AgentState.Active
      )
    },
    test("删除：忙时拒绝；清理各领域、资源转给平台、去掉权限段、删目录；名字可再用，历史仍能查到名字") {
      for
        dirs  <- ZIO.service[TestEnv.Dirs]
        a     <- create("alice")
        b     <- create("bob", allow = Nil)
        _      = write(dirs.assets.resolve("alice/skills/shared-tool/SKILL.md"), "---\ndescription: x\n---\n")
        _      = write(dirs.assets.resolve("alice/skills/private-tool/SKILL.md"), "---\ndescription: x\n---\n")
        _     <- ZIO.serviceWithZIO[Vault](_.set(Actor.Agent(a.id), "ALICE_KEY", "v", None, None, Owner.Platform))
        _     <- agents.flatMap(_.update(b.id, AgentPatch(allow = Some(List("use:skill:shared-tool", "use:secret:ALICE_KEY")))))
        _     <- agents.flatMap(_.update(a.id, AgentPatch(allow = Some(List("use:skill:shared-tool")))))
        _     <- busy(a.id, "在等后台任务")
        e     <- agents.flatMap(_.delete(a.id)).either
        _     <- idle(a.id)
        _     <- agents.flatMap(_.delete(a.id))
        hook  <- ZIO.service[TestEnv.FakeHook]
        clean <- hook.cleaned.get
        snap  <- ZIO.serviceWithZIO[AssetStore](_.snapshot)
        acc   <- ZIO.serviceWithZIO[AssetStore](_.access)
        rec   <- agents.flatMap(_.get(a.id))
        again <- create("alice")
      yield assertTrue(
        reason(e).contains("在等后台任务"),
        clean == List(a.id),
        snap.skill("shared-tool").exists(_.owner == Owner.Platform),
        snap.skill("private-tool").isEmpty,
        snap.secret("ALICE_KEY").exists(_.owner == Owner.Platform),
        !acc.config.agents.contains(a.id),
        acc.config.agents.contains(b.id),
        !Files.exists(dirs.workspaces.resolve(a.id.value)),
        rec.state == AgentState.Deleted,
        rec.name.value == "alice",
        again.id != a.id,
        Files.isDirectory(dirs.assets.resolve("alice"))
      )
    },
    test("删到一半服务重启：起动时接着删完") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- create("alice")
        hook <- ZIO.service[TestEnv.FakeHook]
        _    <- hook.failOnce.set(true)
        e    <- agents.flatMap(_.delete(a.id)).either
        mid  <- agents.flatMap(_.get(a.id))
        list <- agents.flatMap(_.list)
        _    <- agents.flatMap(_.resumePending)
        done <- agents.flatMap(_.get(a.id))
      yield assertTrue(
        e.isLeft,
        mid.state == AgentState.Deleting,
        !list.exists(_.id == a.id),
        done.state == AgentState.Deleted,
        !Files.exists(dirs.assets.resolve("alice"))
      )
    },
    test("改配置：只改给出的字段，写回 agent.toml") {
      for
        a <- create("alice")
        _ <- agents.flatMap(_.update(a.id, AgentPatch(description = Some("新职责"), group = Some(Some("g")))))
        p <- agents.flatMap(_.profile(a.id))
        _ <- agents.flatMap(_.update(a.id, AgentPatch(group = Some(None), thinking = Some(Thinking.Off))))
        q <- agents.flatMap(_.profile(a.id))
        e <- agents.flatMap(_.update(a.id, AgentPatch(description = Some("  ")))).either
      yield assertTrue(
        p.description == "新职责",
        p.group.contains("g"),
        p.model == TestEnv.models.defaultModel,
        q.group.isEmpty,
        q.thinking == Thinking.Off,
        q.description == "新职责",
        e.isLeft
      )
    },
    test("access.toml 写坏时，平台拒绝在它上面改权限") {
      for
        dirs <- ZIO.service[TestEnv.Dirs]
        a    <- create("alice")
        _     = write(dirs.assets.resolve("platform/access.toml"), "[roles\n")
        e    <- agents.flatMap(_.update(a.id, AgentPatch(roles = Some(List(RoleName.wrap("admin")))))).either
      yield assertTrue(reason(e).contains("写坏了"))
    }
  ).provideLayer(TestEnv.layer) @@ TestAspect.sequential
