package muliao.resources

import muliao.contract.{AgentId, AgentName, AssetKind, RoleName, SkillName}
import muliao.exec.{RealPath, Sandbox}
import muliao.platform.AppError
import muliao.resources.access.*
import muliao.resources.agents.{AgentRecord, Agents}
import muliao.resources.tree.*
import zio.*

import java.nio.file.Path

/** 工具为什么能用。 */
final case class ToolGrant(name: String, access: muliao.toolkit.Access, by: Grant)

/** 一项按名字分发的资源能不能用、能不能改，以及凭哪条规则。 */
final case class AssetGrant(kind: AssetKind, name: String, owner: Owner, use: Grant, write: Option[Grant])

/** 一个 agent 最终能用的工具与资源（界面详情、get_agent、新建前预览）。 */
final case class Effective(
    permissions: Permissions,
    tools: List[ToolGrant],
    assets: List[AssetGrant],
    /** `write:path` 给的目录 */
    paths: List[Path],
    /** 规则文件现在写坏了（沿用上一份）时的原因 */
    accessBroken: Option[String]
)

/** 记忆块在一次运行里的样子。 */
enum MemoryKind:
  case Global, Shared, Private

final case class MemoryBlock(name: String, kind: MemoryKind, file: Path, content: String, editable: Boolean)

final case class SkillEntry(doc: SkillDoc, owned: Boolean)

/** 一次运行要用的资源快照：运行开始时取一次，本次运行内不变。 */
final case class RunResources(
    agent: AgentRecord,
    profile: AgentProfile,
    permissions: Permissions,
    /** 能用的内置工具名 */
    tools: Set[String],
    memories: List[MemoryBlock],
    skills: List[SkillEntry],
    mcps: List[McpDoc],
    secrets: List[SecretDoc],
    /** 和它有关的没加载的文件 */
    problems: List[Problem],
    accessBroken: Option[String],
    memoryLimit: Int
)

/** 资源与权限的查询入口：最终权限、一次运行的资源快照、可写范围。 */
trait Resources:
  def permissions(id: AgentId): IO[AppError, Permissions]
  def effective(id: AgentId): IO[AppError, Effective]

  /** 新建或修改前预览：按给定的角色和规则算（`id` 给出时按这个 agent 的名字算「自己的资源」）。 */
  def preview(id: Option[AgentId], roles: List[RoleName], allow: List[String], deny: List[String]): IO[AppError, Effective]

  def forRun(id: AgentId): IO[AppError, RunResources]

  /** agent 的命令与文件写工具的可写范围（就是它的写沙箱）。解析真实路径限时，卡住（如等系统授权弹窗）时报错而不挂死。 */
  def writableScope(id: AgentId): IO[AppError, Sandbox.Confined]

  /** 写不了某路径时补的一句：被什么拦、找谁授权。 */
  def explainDenied(id: AgentId, path: Path): IO[AppError, String]

  /** 有 admin 角色的 agent 名（「找 admin」时点名）。 */
  def admins: IO[AppError, List[AgentName]]

object Resources:
  val layer: ZLayer[AssetStore & Agents & ToolCatalog & ResourcePaths & ResourcesSettings, Nothing, Resources] =
    ZLayer.derive[Live]

  /** 算可写范围的总时限。 */
  val ScopeTimeout: Duration = 30.seconds

  final case class Live(store: AssetStore, agents: Agents, catalog: ToolCatalog, paths: ResourcePaths, settings: ResourcesSettings)
      extends Resources:

    private def permsOf(a: AgentRecord, access: AccessConfig) = Permissions.of(a.id, a.name, access)

    def permissions(id: AgentId): IO[AppError, Permissions] =
      for
        a      <- agents.get(id)
        access <- store.access
      yield permsOf(a, access.config)

    def effective(id: AgentId): IO[AppError, Effective] =
      for
        a      <- agents.get(id)
        access <- store.access
        snap   <- store.snapshot
      yield compute(permsOf(a, access.config), snap, access.broken)

    def preview(id: Option[AgentId], roles: List[RoleName], allow: List[String], deny: List[String]): IO[AppError, Effective] =
      for
        a      <- ZIO.foreach(id)(agents.get)
        al     <- ZIO.foreach(allow)(t => ZIO.fromEither(Rule.parse(t)).mapError(AppError.invalid))
        de     <- ZIO.foreach(deny)(t => ZIO.fromEither(Rule.parse(t)).mapError(AppError.invalid))
        access <- store.access
        snap   <- store.snapshot
        fakeId  = a.fold(AgentId.wrap("preview"))(_.id)
        name    = a.fold(AgentName.wrap("\u0000preview"))(_.name)
        config  = access.config.copy(agents = access.config.agents.updated(fakeId, AgentGrants(roles, al, de)))
      yield compute(Permissions.of(fakeId, name, config), snap, access.broken)

    private def compute(p: Permissions, snap: Snapshot, broken: Option[String]): Effective =
      val tools = catalog.tools.flatMap { t =>
        p.decide(Action.Use, AssetKind.Tool, t.name, Owner.Platform, Some(t.access)) match
          case Decision.Allowed(g) => Some(ToolGrant(t.name, t.access, g))
          case _                   => None
      }
      val assets = snap.named.flatMap { case NamedAsset(k, n, o) =>
        p.decide(Action.Use, k, n, o) match
          case Decision.Allowed(g) =>
            val w = p.decide(Action.Write, k, n, o) match
              case Decision.Allowed(wg) => Some(wg)
              case _                    => None
            Some(AssetGrant(k, n, o, g, w))
          case _ => None
      }
      Effective(p, tools, assets.sortBy(a => (a.kind.ordinal, a.name)), p.writablePaths, broken)

    def forRun(id: AgentId): IO[AppError, RunResources] =
      for
        a       <- agents.requireActive(id)
        profile <- agents.profile(id)
        access  <- store.access
        snap    <- store.snapshot
        p        = permsOf(a, access.config)
        eff      = compute(p, snap, access.broken)
        usable   = eff.assets.map(g => (g.kind, g.name) -> g).toMap
      yield
        val shared = snap.memories.collect {
          case m @ MemoryDoc(MemoryScope.Shared(n), file, content) if usable.contains(AssetKind.Memory -> n) =>
            val kind = if n == MemoryDoc.GlobalName then MemoryKind.Global else MemoryKind.Shared
            MemoryBlock(n, kind, file, content, usable(AssetKind.Memory -> n).write.isDefined)
        }.sortBy(b => (b.kind != MemoryKind.Global, b.name))
        val own = snap.privateMemory(a.name).map(m => MemoryBlock(a.name.value, MemoryKind.Private, m.file, m.content, editable = true))
        RunResources(
          agent = a,
          profile = profile,
          permissions = p,
          tools = eff.tools.map(_.name).toSet,
          memories = shared ++ own,
          skills = snap.skills.filter(s => usable.contains(AssetKind.Skill -> s.name.value)).map(s => SkillEntry(s, s.owner == Owner.Agent(a.name))).sortBy(_.doc.name),
          mcps = snap.mcps.filter(m => usable.contains(AssetKind.Mcp -> m.name)).sortBy(_.name),
          secrets = snap.secrets.filter(s => usable.contains(AssetKind.Secret -> s.key)).sortBy(_.key),
          problems = snap.problems.filter(relevant(p, _)),
          accessBroken = access.broken.filter(_ => p.isAdmin),
          memoryLimit = settings.memoryLimit
        )

    /** 没加载的文件告诉谁：它的主人；按规则能用这个资源的；没有归属或规则文件的问题告诉 admin。 */
    private def relevant(p: Permissions, problem: Problem): Boolean =
      problem.owner.contains(Owner.Agent(p.name)) ||
        problem.subject.exists((k, n) => p.decide(Action.Use, k, n, problem.owner.getOrElse(Owner.Platform)).allowed) ||
        (p.isAdmin && (problem.subject.isEmpty || problem.owner.isEmpty))

    def writableScope(id: AgentId): IO[AppError, Sandbox.Confined] =
      val compute =
        for
          a      <- agents.get(id)
          access <- store.access
          snap   <- store.snapshot
          p       = permsOf(a, access.config)
          ownDir  = paths.assets.resolve(AssetPath.ownerDir(Owner.Agent(a.name)))
          resourcePaths = (a: NamedAsset) => AssetPath.home(a.kind, a.name, a.owner).map(paths.assets.resolve).toList
          canWrite      = (a: NamedAsset) => p.decide(Action.Write, a.kind, a.name, a.owner).allowed
          granted =
            if p.isAdmin then List(paths.assets)
            else
              snap.named.filter(x => x.owner != Owner.Agent(a.name) && canWrite(x)).flatMap(resourcePaths)
          revoked =
            if p.isAdmin then snap.named.filterNot(canWrite).flatMap(resourcePaths)
            else Nil
          writable <- resolveAll(List(paths.workspace(a.id), ownDir) ++ settings.writableDirs ++ p.writablePaths ++ granted)
          readOnly <- resolveAll(p.deniedPaths ++ revoked)
          hidden   <- if p.isAdmin then ZIO.succeed(Chunk.empty) else resolveAll(paths.privatePaths)
        yield new Sandbox.Confined(writable, hidden = hidden, readOnly = readOnly)
      compute.timeoutFail(AppError.Unavailable("算可写范围超时（可能在等系统授权弹窗），稍后再试"))(ScopeTimeout)

    private def resolveAll(ps: List[Path]): IO[AppError, Chunk[RealPath]] =
      ZIO.foreachPar(Chunk.fromIterable(ps.distinct))(RealPath.resolve(_)).map(_.distinct)

    def admins: IO[AppError, List[AgentName]] =
      for
        all    <- agents.list
        access <- store.access
      yield all.filter(a => permsOf(a, access.config).isAdmin).map(_.name)

    def explainDenied(id: AgentId, path: Path): IO[AppError, String] =
      for
        admin <- admins.map(_.map(_.value).mkString("、")).map(s => if s.isEmpty then "用户" else s)
        root  <- RealPath.resolve(paths.assets)
        real  <- RealPath.resolve(path)
        priv  <- resolveAll(paths.privatePaths)
      yield
        if priv.exists(RealPath.within(real, _)) then s"这是平台私有文件，非 admin 不能读写。"
        else if RealPath.within(real, root) then
          AssetPath.ownerOf(root.path.relativize(real.path)) match
            case Some(o) => s"这是 ${o.label} 的资源，你没有改它的权限：请它的主人改，或找 admin（$admin）在 access.toml 里授权。"
            case None    => s"资源目录根下不能放文件。${AssetPath.layoutHint}"
        else s"不在你的可写范围内（工作目录、常用临时与缓存目录、授权的目录）。要写这里，找 admin（$admin）加 write:path 规则。"
