package muliao.resources

import muliao.contract.*
import muliao.platform.events.ChangeBus
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import zio.*
import zio.json.*
import zio.test.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

object HttpSpec extends ZIOSpecDefault:
  import TestEnv.write

  final case class Resp(status: Int, body: String)

  final class Client(base: String):
    private val http   = HttpClient.newHttpClient()
    @volatile private var cookie = ""
    def call(method: String, path: String, body: Option[String] = None): Task[Resp] = ZIO.attemptBlocking {
      val b = HttpRequest.newBuilder(URI.create(base + path)).header("Content-Type", "application/json")
      if cookie.nonEmpty then b.header("Cookie", cookie): Unit
      val r    = body.fold(b.method(method, HttpRequest.BodyPublishers.noBody()))(s => b.method(method, HttpRequest.BodyPublishers.ofString(s)))
      val resp = http.send(r.build(), HttpResponse.BodyHandlers.ofString())
      resp.headers.firstValue("set-cookie").ifPresent(c => cookie = c.takeWhile(_ != ';'))
      Resp(resp.statusCode, resp.body)
    }

  val server: ZIO[Scope & ResourcesHttp & ChangeBus, Throwable, Client] =
    for
      routes <- ZIO.service[ResourcesHttp]
      bus    <- ZIO.service[ChangeBus]
      settings = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
      auth   <- Auth.layer.build.map(_.get[Auth]).provideSomeLayer[Scope](ZLayer.succeed(settings))
      port   <- HttpServer.serve(settings, auth, bus, routes.routes, "test")
      client  = Client(s"http://127.0.0.1:$port")
      _      <- client.call("POST", "/api/auth/login", Some("""{"password":"pw"}"""))
    yield client

  val httpLayer = ZLayer.succeed(TestEnv.tools) ++ ZLayer.succeed(TestEnv.models)

  def spec = suite("HTTP 接口")(
    test("agent：建、列、详情、改名与默认、预览、删除；错误按统一错误体返回") {
      ZIO.scoped {
        for
          c   <- server
          mk  <- c.call("POST", "/api/agents", Some("""{"name":"alice","description":"做事","thinking":"low","roles":["admin"]}"""))
          d    = mk.body.fromJson[AgentDetail].toOption.get
          dup <- c.call("POST", "/api/agents", Some("""{"name":"ALICE","description":"x"}"""))
          ls  <- c.call("GET", "/api/agents")
          up  <- c.call("PATCH", s"/api/agents/${d.summary.id.value}", Some("""{"name":"alice2","isDefault":true,"group":"g"}"""))
          pv  <- c.call("POST", "/api/agents-preview", Some("""{"roles":[],"allow":["use:tool:create_agent"]}"""))
          df  <- c.call("GET", "/api/agent-defaults")
          del <- c.call("DELETE", s"/api/agents/${d.summary.id.value}")
          gone <- c.call("GET", "/api/agents")
        yield
          val updated = up.body.fromJson[AgentDetail].toOption.get
          val err     = dup.body.fromJson[ApiError].toOption.get
          assertTrue(
            mk.status == 201,
            d.thinking.contains(Thinking.Low),
            d.roles == List("admin"),
            d.tools.exists(_.name == "create_agent"),
            dup.status == 409,
            err.kind == ErrorKind.Conflict,
            ls.body.fromJson[List[AgentSummary]].toOption.exists(_.map(_.name.value) == List("alice")),
            updated.summary.name.value == "alice2",
            updated.summary.isDefault,
            updated.summary.group.contains("g"),
            pv.body.fromJson[PreviewResult].toOption.exists(_.tools.exists(t => t.name == "create_agent" && t.use.contains("use:tool:create_agent"))),
            df.body.fromJson[AgentDefaults].toOption.exists(_.model == TestEnv.models.defaultModel),
            del.status == 204,
            gone.body == "[]"
          )
      }
    },
    test("角色：建、改、规则检查、删（从 agent 身上也去掉）；内置角色不能删；新规则匹配不上拒绝") {
      ZIO.scoped {
        for
          dirs <- ZIO.service[TestEnv.Dirs]
          _     = write(dirs.assets.resolve("platform/skills/relay-host/SKILL.md"), "---\ndescription: x\n---\n")
          c    <- server
          put  <- c.call("PUT", "/api/roles/trader", Some("""{"description":"交易","allow":["use:skill:relay-host"]}"""))
          bad  <- c.call("PUT", "/api/roles/trader", Some("""{"description":"交易","allow":["use:skill:relay-hots"]}"""))
          mk   <- c.call("POST", "/api/agents", Some("""{"name":"bob","description":"x","roles":["trader"]}"""))
          ls   <- c.call("GET", "/api/roles")
          chk  <- c.call("POST", "/api/rules-check", Some("""{"rules":["use:skill:relay-*","bogus"]}"""))
          nob  <- c.call("DELETE", "/api/roles/base")
          del  <- c.call("DELETE", "/api/roles/trader")
          id    = mk.body.fromJson[AgentDetail].toOption.get.summary.id
          bob  <- c.call("GET", s"/api/agents/${id.value}")
        yield
          val roles  = ls.body.fromJson[List[RoleView]].toOption.get
          val checks = chk.body.fromJson[List[RuleCheck]].toOption.get
          assertTrue(
            put.status == 200,
            bad.status == 400,
            bad.body.contains("relay-hots"),
            roles.find(_.name.value == "trader").exists(_.members.map(_.value) == List("bob")),
            roles.exists(r => r.name.value == "base" && r.builtin),
            checks(0).matches == List("skill:relay-host"),
            checks(1).error.isDefined,
            nob.status == 400,
            del.status == 204,
            bob.body.fromJson[AgentDetail].toOption.exists(_.roles.isEmpty)
          )
      }
    },
    test("AI 资源：列表含归属与使用者；读写文件带指纹，被别人改过就冲突；secret 只能设不能看；转移与删除") {
      ZIO.scoped {
        for
          dirs <- ZIO.service[TestEnv.Dirs]
          c    <- server
          mk   <- c.call("POST", "/api/agents", Some("""{"name":"alice","description":"x"}"""))
          _     = write(dirs.assets.resolve("alice/skills/tool-a/SKILL.md"), "---\ndescription: 工具 A\n---\n")
          _     = write(dirs.assets.resolve("alice/skills/tool-a/scripts/x.sh"), "#!/bin/sh\n")
          rd   <- c.call("GET", "/api/assets/file?path=alice/memory.md")
          fp    = rd.body.fromJson[AssetFile].toOption.flatMap(_.fingerprint).get
          w1   <- c.call("PUT", "/api/assets/file", Some(AssetFileWrite("alice/memory.md", "记一条  \n\n\n", Some(fp)).toJson))
          w2   <- c.call("PUT", "/api/assets/file", Some(AssetFileWrite("alice/memory.md", "再记", Some(fp)).toJson))
          esc  <- c.call("GET", "/api/assets/file?path=../data/config.toml")
          sec  <- c.call("PUT", "/api/assets/secrets/API_TOKEN", Some("""{"value":{"kind":"Plain","text":"s3cret"},"description":"测试","owner":"alice"}"""))
          sec2 <- c.call("PUT", "/api/assets/secrets/API_TOKEN", Some("""{"allowShell":false}"""))
          files <- c.call("GET", "/api/assets/skills/tool-a/files")
          tr   <- c.call("POST", "/api/assets/skill/tool-a/transfer", Some("""{"to":"platform"}"""))
          idx  <- c.call("GET", "/api/assets")
          dl   <- c.call("DELETE", "/api/assets/memory/global")
          dl2  <- c.call("DELETE", "/api/assets/secret/API_TOKEN")
          idx2 <- c.call("GET", "/api/assets")
        yield
          val index = idx.body.fromJson[AssetIndex].toOption.get
          assertTrue(
            mk.status == 201,
            w1.status == 200,
            w1.body.fromJson[AssetFile].toOption.exists(_.content == "记一条\n"),
            w2.status == 409,
            esc.status == 400,
            sec.status == 204,
            sec2.status == 204,
            !idx.body.contains("s3cret"),
            index.items.exists(i => i.kind == AssetItemKind.Secret && i.name == "API_TOKEN" && i.owner == "alice" && i.allowShell.contains(false) && i.description.contains("测试")),
            files.body.fromJson[SkillFiles].toOption.exists(_.files == List("SKILL.md", "scripts/x.sh")),
            tr.status == 204,
            index.items.exists(i => i.kind == AssetItemKind.Skill && i.name == "tool-a" && i.owner == "platform"),
            index.items.exists(i => i.kind == AssetItemKind.Memory && i.name == "global" && i.usedBy.map(_.value) == List("alice")),
            dl.status == 400,
            dl2.status == 204,
            !idx2.body.contains("API_TOKEN")
          )
      }
    }
  ).provideLayer(TestEnv.layer >+> ((ZLayer.environment[TestEnv.Env] ++ httpLayer) >>> ZLayer.derive[ResourcesHttp])) @@ TestAspect.withLiveClock @@ TestAspect.sequential @@ TestAspect.timeout(60.seconds)
