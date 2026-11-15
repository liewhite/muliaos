package muliao.resources

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.server.{ApiEndpoint, Auth, Endpoints}
import muliao.resources.access.*
import muliao.resources.agents.*
import muliao.resources.guard.{AssetGuard, Expectation, FileFingerprint}
import muliao.resources.tree.*
import muliao.resources.vault.{Actor, Vault}
import zio.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** 本领域的 HTTP 接口：agent、角色、AI 资源（界面「agent」「角色」「AI 资源」三页要的）。 */
final case class ResourcesHttp(
    agents: Agents,
    resources: Resources,
    store: AssetStore,
    guard: AssetGuard,
    vault: Vault,
    editor: AccessEditor,
    models: ModelCheck,
    catalog: ToolCatalog,
    paths: ResourcePaths
):
  import Endpoints.*

  // ---- 转换 ----

  private def summary(a: AgentRecord, snap: Snapshot): AgentSummary =
    val p = snap.profiles.get(a.name)
    AgentSummary(a.id, a.name, p.map(_.description), p.flatMap(_.group), p.map(_.model), a.state, a.isDefault)

  private def grantView(item: String, kind: AssetKind, name: String, owner: Option[Owner], use: Grant, write: Option[Grant]) =
    GrantView(item, kind, name, owner.map(_.label), use.label, write.map(_.label))

  private def toolViews(e: Effective) = e.tools.map(t => grantView(t.name, AssetKind.Tool, t.name, None, t.by, None))

  private def assetViews(e: Effective) =
    e.assets.map(g => grantView(s"${g.kind.label}:${g.name}", g.kind, g.name, Some(g.owner), g.use, g.write))

  private def problemView(p: Problem) = ProblemView(rel(p.path), p.reason, p.owner.map(_.label))

  private def rel(p: Path): String = store.root.relativize(p).toString

  private def detail(id: AgentId): IO[AppError, AgentDetail] =
    for
      a      <- agents.get(id)
      snap   <- store.snapshot
      access <- store.access
      eff    <- resources.effective(id)
      grants  = access.config.grantsOf(id)
      profErr = Option.when(!snap.profiles.contains(a.name))(
                  snap.problems.find(_.path == paths.assets.resolve(a.name.value).resolve("agent.toml")).fold("agent.toml 不存在")(_.reason)
                )
    yield AgentDetail(
      summary = summary(a, snap),
      thinking = snap.profiles.get(a.name).map(_.thinking),
      roles = grants.roles.map(_.value),
      allow = grants.allow.map(_.text),
      deny = grants.deny.map(_.text),
      tools = toolViews(eff),
      assets = assetViews(eff),
      writablePaths = eff.paths.map(_.toString),
      workspace = paths.workspace(id).toString,
      assetsDir = paths.assets.resolve(a.name.value).toString,
      problems = snap.problems.filter(_.owner.contains(Owner.Agent(a.name))).map(problemView),
      profileError = profErr,
      accessBroken = access.broken
    )

  // ---- agent ----

  private def listAgents: IO[AppError, List[AgentSummary]] =
    for
      as   <- agents.list
      snap <- store.snapshot
    yield as.map(summary(_, snap))

  private def createAgent(req: AgentCreate): IO[AppError, AgentDetail] =
    for
      a <- agents.create(
             NewAgent(req.name, req.description, req.model, req.thinking.getOrElse(Thinking.Default), req.group, req.roles.map(RoleName.wrap), req.allow, req.deny)
           )
      d <- detail(a.id)
    yield d

  private def updateAgent(id: AgentId, req: AgentUpdate): IO[AppError, AgentDetail] =
    for
      _ <- agents.update(
             id,
             AgentPatch(
               name = req.name,
               description = req.description,
               model = req.model,
               thinking = req.thinking,
               group = req.group.map(g => Option(g.trim).filter(_.nonEmpty)),
               roles = req.roles.map(_.map(RoleName.wrap)),
               allow = req.allow,
               deny = req.deny,
               archived = req.archived,
               isDefault = req.isDefault
             )
           )
      d <- detail(id)
    yield d

  private def preview(req: PreviewRequest): IO[AppError, PreviewResult] =
    resources
      .preview(req.agent, req.roles.map(RoleName.wrap), req.allow, req.deny)
      .map(e => PreviewResult(toolViews(e), assetViews(e), e.paths.map(_.toString)))

  // ---- 角色 ----

  private def listRoles: IO[AppError, List[RoleView]] =
    for
      access <- store.access
      as     <- agents.list
    yield access.config.roleNames.flatMap { n =>
      access.config.role(n).map { r =>
        val members =
          if n == BuiltinRole.Base.roleName then as.map(_.name)
          else as.filter(a => access.config.grantsOf(a.id).roles.contains(n)).map(_.name)
        RoleView(n, BuiltinRole.of(n).isDefined, r.description, r.allow.map(_.text), r.deny.map(_.text), members)
      }
    }

  private def parseAll(texts: List[String]): Either[String, List[Rule]] =
    texts.foldLeft[Either[String, List[Rule]]](Right(Nil))((acc, t) => acc.flatMap(rs => Rule.parse(t).map(rs :+ _)))

  private def putRole(name: RoleName, req: RoleWrite): IO[AppError, RoleView] =
    for
      _ <- ZIO.unless(name.value.matches("[A-Za-z0-9_-]{1,64}"))(ZIO.fail(AppError.invalid("角色名只能用字母、数字、- 和 _")))
      _ <- editor.modify { c =>
             for
               al <- parseAll(req.allow)
               de <- parseAll(req.deny)
             yield c.copy(roles = c.roles.updated(name, RoleDef(req.description.trim, al, de)))
           }
      roles <- listRoles
      out   <- ZIO.fromOption(roles.find(_.name == name)).orElseFail(AppError.internal("角色写进去了却读不到", new IllegalStateException))
    yield out

  private def deleteRole(name: RoleName): IO[AppError, Unit] =
    for
      _ <- ZIO.when(BuiltinRole.of(name).isDefined)(ZIO.fail(AppError.invalid(s"${name.value} 是内置角色，不能删")))
      _ <- editor.modify { c =>
             if !c.roles.contains(name) then Left(s"没有角色 ${name.value}")
             else
               Right(
                 c.copy(
                   roles = c.roles - name,
                   agents = c.agents.view.mapValues(g => g.copy(roles = g.roles.filterNot(_ == name))).toMap
                 )
               )
           }
    yield ()

  private def checkRules(req: RulesRequest): IO[AppError, List[RuleCheck]] =
    store.snapshot.map { snap =>
      req.rules.map { t =>
        Rule.parse(t) match
          case Left(e) => RuleCheck(t, Some(e), Nil)
          case Right(r) =>
            val tools = catalog.tools.map(_.name).filter(n => r.selects(Action.Use, AssetKind.Tool, n))
            val named = snap.named.collect {
              case NamedAsset(k, n, _) if Action.values.exists(a => r.selects(a, k, n)) => s"${k.label}:$n"
            }
            val path = Rule.pathOf(r).map(_.toString).toList
            RuleCheck(t, None, tools ++ named.distinct ++ path)
      }
    }

  // ---- AI 资源 ----

  private def listAssets: IO[AppError, AssetIndex] =
    for
      snap   <- store.snapshot
      access <- store.access
      as     <- agents.list
      perms   = as.map(a => a -> Permissions.of(a.id, a.name, access.config))
    yield
      def usedBy(k: AssetKind, n: String, o: Owner): List[AgentName] =
        perms.collect {
          case (a, p) if (p.decide(Action.Use, k, n, o) match
                case Decision.Allowed(Grant.ByRule(_, _)) | Decision.Allowed(Grant.Ownership) | Decision.Allowed(Grant.GlobalMemory) => true
                case _                                                                                                       => false
              ) => a.name
        }
      val memories = snap.memories.map {
        case MemoryDoc(MemoryScope.Shared(n), f, _) =>
          AssetItem(AssetItemKind.Memory, n, Owner.PlatformDir, rel(f), usedBy(AssetKind.Memory, n, Owner.Platform), None, None, None)
        case MemoryDoc(MemoryScope.Private(a), f, _) =>
          AssetItem(AssetItemKind.Memory, a.value, a.value, rel(f), List(a), Some("私有记忆"), None, None)
      }
      val skills = snap.skills.map(s => AssetItem(AssetItemKind.Skill, s.name.value, s.owner.label, rel(s.main), usedBy(AssetKind.Skill, s.name.value, s.owner), Some(s.description), None, None))
      val mcps = snap.mcps.map(m => AssetItem(AssetItemKind.Mcp, m.name, m.owner.label, rel(m.file), usedBy(AssetKind.Mcp, m.name, m.owner), None, Some(m.config.enabled), None))
      val secrets = snap.secrets.map(s =>
        AssetItem(AssetItemKind.Secret, s.key, s.owner.label, rel(s.file), usedBy(AssetKind.Secret, s.key, s.owner), Some(s.content.description), None, Some(s.content.allowShell))
      )
      val agentFiles = as.filter(a => snap.profiles.contains(a.name)).map(a =>
        AssetItem(AssetItemKind.Agent, a.name.value, a.name.value, s"${a.name.value}/agent.toml", List(a.name), snap.profiles.get(a.name).map(_.description), None, None)
      )
      val accessFile = AssetItem(AssetItemKind.Access, "access.toml", Owner.PlatformDir, "platform/access.toml", Nil, Some("角色与各 agent 的权限"), None, None)
      AssetIndex(paths.assets.toString, accessFile :: memories ++ skills ++ mcps ++ secrets ++ agentFiles, snap.problems.map(problemView), access.broken)

  /** 界面传来的相对路径：不能走出资源目录，不能碰隐藏文件。 */
  private def resolveRel(relative: String): IO[AppError, Path] =
    val p = paths.assets.resolve(relative).normalize
    if relative.startsWith("/") || !p.startsWith(paths.assets.normalize) || paths.assets.relativize(p).iterator.asScala.exists(_.toString.startsWith("."))
    then ZIO.fail(AppError.invalid(s"路径 $relative 不在资源目录里"))
    else ZIO.succeed(p)

  private def readFile(relative: String): IO[AppError, AssetFile] =
    for
      p <- resolveRel(relative)
      text <- ZIO
                .attemptBlocking(if Files.isRegularFile(p) then Some(Files.readString(p)) else None)
                .mapError(e => AppError.invalid(s"读不了 $relative：${e.getMessage}"))
                .someOrFail(AppError.notFound(s"没有文件 $relative"))
    yield AssetFile(relative, text, Some(FileFingerprint.of(text).value))

  private def writeFile(req: AssetFileWrite): IO[AppError, AssetFile] =
    for
      p <- resolveRel(req.path)
      expect = req.fingerprint.fold(Expectation.NotRead)(f => Expectation.ReadAs(FileFingerprint(f)))
      content <- guard.check(Actor.User, p, req.content, expect)
      _       <- store.write(p, content, privateFile = AssetPath.classify(paths.assets.relativize(p)).isInstanceOf[AssetPath.Secret])
    yield AssetFile(req.path, content, Some(FileFingerprint.of(content).value))

  private def skillFiles(name: SkillName): IO[AppError, SkillFiles] =
    for
      snap <- store.snapshot
      s    <- ZIO.fromOption(snap.skill(name)).orElseFail(AppError.notFound(s"没有已加载的 skill ${name.value}"))
      files <- ZIO
                 .attemptBlocking(scala.util.Using.resource(Files.walk(s.dir))(_.iterator.asScala.filter(Files.isRegularFile(_)).map(f => s.dir.relativize(f).toString).filterNot(_.split('/').exists(_.startsWith("."))).toList.sorted))
                 .mapError(e => AppError.internal(s"列 skill ${name.value} 的文件失败", e))
    yield SkillFiles(name, s.owner.label, files)

  private def ownerOf(label: String): IO[AppError, Owner] =
    if label == Owner.PlatformDir then ZIO.succeed(Owner.Platform)
    else agents.resolve(label).map(a => Owner.Agent(a.name))

  private def putSecret(key: String, req: SecretWrite): IO[AppError, Unit] =
    for
      owner <- ZIO.foreach(req.owner)(ownerOf).map(_.getOrElse(Owner.Platform))
      _ <- req.value match
             case Some(v) => vault.set(Actor.User, key, v, req.description, req.allowShell, owner).unit
             case None =>
               for
                 snap <- store.snapshot
                 doc  <- ZIO.fromOption(snap.secret(key)).orElseFail(AppError.notFound(s"没有 secret $key（新建要给值）"))
                 next  = doc.content.copy(
                           description = req.description.map(_.trim).getOrElse(doc.content.description),
                           allowShell = req.allowShell.getOrElse(doc.content.allowShell)
                         )
                 _ <- store.write(doc.file, Formats.renderSecret(next), privateFile = true)
               yield ()
    yield ()

  /** 找到的一项资源：它的文件（skill 是目录）与归谁。 */
  private final case class Found(path: Path, owner: Owner)

  private def locate(kind: AssetKind, name: String): IO[AppError, Found] =
    store.snapshot.flatMap { snap =>
      def found[A](a: Option[A], what: String)(f: A => Found) = ZIO.fromOption(a.map(f)).orElseFail(AppError.notFound(s"没有已加载的 $what $name"))
      kind match
        case AssetKind.Skill  => found(snap.skill(SkillName.wrap(name)), "skill")(s => Found(s.dir, s.owner))
        case AssetKind.Mcp    => found(snap.mcp(name), "MCP")(m => Found(m.file, m.owner))
        case AssetKind.Secret => found(snap.secret(name), "secret")(s => Found(s.file, s.owner))
        case AssetKind.Memory =>
          if name == MemoryDoc.GlobalName then ZIO.fail(AppError.invalid("全局记忆总是存在，不能删或转移"))
          else found(snap.sharedMemory(name), "共享记忆")(m => Found(m.file, Owner.Platform))
        case AssetKind.Tool => ZIO.fail(AppError.invalid("工具不是资源文件，种类只能是 skill、mcp、secret、memory"))
    }

  private def transfer(kind: AssetKind, name: String, req: AssetTransfer): IO[AppError, Unit] =
    for
      at <- locate(kind, name)
      to <- ownerOf(req.to)
      dir = paths.assets.resolve(to.label)
      target <- kind match
                  case AssetKind.Skill                   => ZIO.succeed(dir.resolve("skills").resolve(name))
                  case AssetKind.Mcp                     => ZIO.succeed(dir.resolve("mcp").resolve(s"$name.json"))
                  case AssetKind.Secret                  => ZIO.succeed(dir.resolve("secrets").resolve(s"$name.toml"))
                  case AssetKind.Memory | AssetKind.Tool => ZIO.fail(AppError.invalid("共享记忆只能归平台；私有记忆跟着 agent"))
      _ <- store.move(at.path, target).unless(to == at.owner)
    yield ()

  private def deleteAsset(kind: AssetKind, name: String): IO[AppError, Unit] =
    locate(kind, name).flatMap(at => store.delete(at.path))

  def endpoints(auth: Auth): List[ApiEndpoint] =
    List(
      ResourcesApi.listAgents.secured(auth)(_ => _ => listAgents),
      ResourcesApi.createAgent.secured(auth)(_ => createAgent),
      ResourcesApi.getAgent.secured(auth)(_ => detail),
      ResourcesApi.updateAgent.secured(auth)(_ => (id, req) => updateAgent(id, req)),
      ResourcesApi.deleteAgent.secured(auth)(_ => agents.delete),
      ResourcesApi.previewAgent.secured(auth)(_ => preview),
      ResourcesApi.agentDefaults.secured(auth)(_ => _ => ZIO.succeed(AgentDefaults(models.defaultModel, Thinking.values.toList))),
      ResourcesApi.listRoles.secured(auth)(_ => _ => listRoles),
      ResourcesApi.putRole.secured(auth)(_ => (n, r) => putRole(n, r)),
      ResourcesApi.deleteRole.secured(auth)(_ => deleteRole),
      ResourcesApi.checkRules.secured(auth)(_ => checkRules),
      ResourcesApi.listAssets.secured(auth)(_ => _ => listAssets),
      ResourcesApi.readAssetFile.secured(auth)(_ => readFile),
      ResourcesApi.writeAssetFile.secured(auth)(_ => writeFile),
      ResourcesApi.skillFiles.secured(auth)(_ => skillFiles),
      ResourcesApi.putSecret.secured(auth)(_ => (k, r) => putSecret(k, r)),
      ResourcesApi.transferAsset.secured(auth)(_ => (k, n, r) => transfer(k, n, r)),
      ResourcesApi.deleteAsset.secured(auth)(_ => (k, n) => deleteAsset(k, n))
    )

object ResourcesHttp:
  val layer: ZLayer[Agents & Resources & AssetStore & AssetGuard & Vault & AccessEditor & ModelCheck & ToolCatalog & ResourcePaths, Nothing, ResourcesHttp] =
    ZLayer.derive[ResourcesHttp]
