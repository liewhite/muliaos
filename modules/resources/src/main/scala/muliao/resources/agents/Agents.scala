package muliao.resources.agents

import muliao.contract.{AgentState, AssetKind}
import muliao.contract.{AgentId, AgentName, RoleName, Topic}
import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.*
import muliao.resources.access.*
import muliao.resources.tree.*
import zio.*

import java.nio.file.Files

/** 建 agent 的请求。`model` 省略时用默认模型。规则是原文（`use:skill:x`）。 */
final case class NewAgent(
    name: String,
    description: String,
    model: Option[String] = None,
    group: Option[String] = None,
    roles: List[RoleName] = Nil,
    allow: List[String] = Nil,
    deny: List[String] = Nil
)

/** 改 agent：只改给出的字段。`group = Some(None)` 表示去掉分组。 */
final case class AgentPatch(
    name: Option[String] = None,
    description: Option[String] = None,
    model: Option[String] = None,
    group: Option[Option[String]] = None,
    roles: Option[List[RoleName]] = None,
    allow: Option[List[String]] = None,
    deny: Option[List[String]] = None,
    archived: Option[Boolean] = None,
    isDefault: Option[Boolean] = None
)

/** agent 的身份与生命周期：建、改（含改名）、默认、归档、删除（两阶段，重启续删）。 */
trait Agents:
  /** 现存的 agent（不含已删除、正在删除的），按创建时间。 */
  def list: IO[AppError, List[AgentRecord]]

  /** 按编号取（含已删除的，供历史显示名字）。 */
  def get(id: AgentId): IO[AppError, AgentRecord]

  /** 编号对应的显示名（含已删除的）；查不到的编号（含查询失败）显示编号本身。 */
  def names(ids: Iterable[AgentId]): UIO[AgentId => String]

  /** 按名字找现存的 agent（不分大小写）。 */
  def find(name: String): IO[AppError, Option[AgentRecord]]

  /** 按名字找，找不到报错（列出现有名字）。 */
  def resolve(name: String): IO[AppError, AgentRecord]

  /** 能接新工作的 agent：已归档、在删、已删都报错并说明原因。 */
  def requireActive(id: AgentId): IO[AppError, AgentRecord]

  /** agent.toml 的内容；文件坏了或缺了报错并说明。 */
  def profile(id: AgentId): IO[AppError, AgentProfile]

  def create(req: NewAgent): IO[AppError, AgentRecord]
  def update(id: AgentId, patch: AgentPatch): IO[AppError, AgentRecord]

  /** 删除：忙时拒绝；先标为「正在删」，再清理各领域数据和资源，最后标为已删除。 */
  def delete(id: AgentId): IO[AppError, Unit]

  /** 启动：定下各领域对 agent 生命周期的参与（忙碌判断、删除清理），再接着做没做完的改名与删除。只调一次；归档、删除在它之前会等它。 */
  def start(hooks: List[AgentHook]): IO[AppError, Unit]

object Agents:
  val layer: ZLayer[
    Database & AssetStore & AccessEditor & ModelCheck & ToolCatalog & ChangeBus & ResourcePaths,
    Nothing,
    Agents
  ] =
    ZLayer {
      for
        db     <- ZIO.service[Database]
        store  <- ZIO.service[AssetStore]
        editor <- ZIO.service[AccessEditor]
        hooks  <- Promise.make[Nothing, List[AgentHook]]
        models <- ZIO.service[ModelCheck]
        tools  <- ZIO.service[ToolCatalog]
        bus    <- ZIO.service[ChangeBus]
        paths  <- ZIO.service[ResourcePaths]
        lock   <- Semaphore.make(1)
      yield Live(db, store, editor, hooks, models, tools, bus, paths, lock)
    }

  /** 校验 agent 名：非空、不过长、能当目录名、不与保留名冲突。 */
  def validateName(raw: String): Either[String, AgentName] =
    val n = raw.trim
    if n.isEmpty then Left("名字不能为空")
    else if n.codePointCount(0, n.length) > 64 then Left("名字最长 64 个字符")
    else if n.exists(c => c == '/' || c == '\\' || c == ':' || Character.isISOControl(c)) then Left("名字不能含 / \\ : 和控制字符")
    else if n.startsWith(".") then Left("名字不能以 . 开头")
    else if Reserved(AgentRecord.keyOf(n)) then Left(s"$n 是保留名，不能当 agent 名")
    else Right(AgentName.wrap(n))

  private val Reserved = Set("user", Owner.PlatformDir)

  private final case class Live(
      db: Database,
      store: AssetStore,
      editor: AccessEditor,
      /** 各领域的参与，[[start]] 时定下 */
      hooks: Promise[Nothing, List[AgentHook]],
      models: ModelCheck,
      tools: ToolCatalog,
      bus: ChangeBus,
      paths: ResourcePaths,
      /** 生命周期改动（建、改名、删）串行，免得两个请求抢同一个名字或目录 */
      lock: Semaphore
  ) extends Agents:

    private def now = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
    private def dir(name: AgentName) = paths.assets.resolve(AssetPath.ownerDir(Owner.Agent(name)))
    private def publish = bus.publishAfterCommit(Topic.Agents)

    def list: IO[AppError, List[AgentRecord]] = db.read(AgentRepo.live)

    def get(id: AgentId): IO[AppError, AgentRecord] =
      db.read(AgentRepo.byId(id)).someOrFail(AppError.notFound(s"没有编号为 ${id.value} 的 agent"))

    def names(ids: Iterable[AgentId]): UIO[AgentId => String] =
      db.read(AgentRepo.byIds(ids.toSet))
        .map(_.map(a => a.id -> a.name.value).toMap)
        .catchAll(e => ZIO.logWarning(s"查 agent 名字失败，先显示编号：${e.message}").as(Map.empty))
        .map(ns => id => ns.getOrElse(id, id.value))

    def find(name: String): IO[AppError, Option[AgentRecord]] =
      db.read(AgentRepo.liveByKey(AgentRecord.keyOf(name.trim))).map(_.filter(_.live))

    def resolve(name: String): IO[AppError, AgentRecord] =
      find(name).someOrElseZIO(
        list.flatMap(all => ZIO.fail(AppError.notFound(s"没有叫 $name 的 agent。现有：${all.map(_.name.value).mkString("、")}")))
      )

    def requireActive(id: AgentId): IO[AppError, AgentRecord] =
      get(id).flatMap { a =>
        a.state match
          case AgentState.Active   => ZIO.succeed(a)
          case AgentState.Archived => ZIO.fail(AppError.conflict(s"agent ${a.name.value} 已归档，不接新工作（取消归档后才能用）"))
          case AgentState.Deleting | AgentState.Deleted => ZIO.fail(AppError.notFound(s"agent ${a.name.value} 已删除"))
      }

    def profile(id: AgentId): IO[AppError, AgentProfile] =
      for
        a    <- get(id)
        snap <- store.snapshot
        p <- ZIO
               .fromOption(snap.profiles.get(a.name))
               .orElseFail {
                 val why = snap.problems.find(_.path == paths.assets.resolve(AssetPath.render(AssetPath.AgentFile(a.name)))).fold("文件不存在")(_.reason)
                 AppError.conflict(s"agent ${a.name.value} 的 agent.toml 加载不了：$why")
               }
      yield p

    private def checkModel(model: String): IO[AppError, Unit] =
      ZIO.foreachDiscard(models.check(model))(r => ZIO.fail(AppError.invalid(s"模型 $model 不能用：$r")))

    /** 解析规则原文，并要求新加的（不在 `known` 里的）每条至少匹配一个现有的工具或资源（多半是名字写错了）。 */
    private def parseRules(texts: List[String], known: Set[String] = Set.empty): IO[AppError, List[Rule]] =
      for
        rules <- ZIO.foreach(texts)(t => ZIO.fromEither(Rule.parse(t)).mapError(AppError.invalid))
        snap  <- store.snapshot
        names  = tools.tools.map(_.name)
        fresh  = rules.filterNot(r => known(r.text))
        _ <- ZIO.foreachDiscard(fresh.filterNot(AccessValidation.matchesSomething(_, snap, names)))(r =>
               ZIO.fail(AppError.invalid(s"规则 `${r.text}` 匹配不上任何现有的工具或资源（名字写错了？）"))
             )
      yield rules

    private def nameFree(name: AgentName, except: Option[AgentId]): IO[AppError, Unit] =
      db.read(AgentRepo.liveByKey(AgentRecord.keyOf(name.value))).flatMap {
        case Some(other) if !except.contains(other.id) => ZIO.fail(AppError.conflict(s"已有叫 ${other.name.value} 的 agent"))
        case _                                         => ZIO.unit
      }

    private def dirFree(name: AgentName): IO[AppError, Unit] =
      ZIO.whenZIO(ZIO.attemptBlocking(Files.exists(dir(name))).orDie)(
        ZIO.fail(AppError.conflict(s"资源目录里已有目录 ${dir(name)}（不属于任何 agent），先移走或删掉"))
      ).unit

    private def newId: IO[AppError, AgentId] =
      val candidate = Random.nextBytes(6).map(b => AgentId.wrap(b.map(x => f"${x & 0xff}%02x").mkString))
      def loop: IO[AppError, AgentId] =
        candidate.flatMap(id => db.read(AgentRepo.byId(id)).flatMap(found => if found.isEmpty then ZIO.succeed(id) else loop))
      loop

    private def setGrants(id: AgentId, f: AgentGrants => AgentGrants): IO[AppError, Unit] =
      editor.modify(c => Right(c.copy(agents = c.agents.updated(id, f(c.grantsOf(id))).filterNot(_._2.isEmpty)))).unit

    def create(req: NewAgent): IO[AppError, AgentRecord] =
      lock.withPermit {
        for
          name  <- ZIO.fromEither(validateName(req.name)).mapError(AppError.invalid)
          _     <- ZIO.when(req.description.trim.isEmpty)(ZIO.fail(AppError.invalid("描述不能为空：别的 agent 按它决定找谁")))
          model  = req.model.map(_.trim).filter(_.nonEmpty).getOrElse(models.defaultModel)
          _     <- checkModel(model)
          allow <- parseRules(req.allow)
          deny  <- parseRules(req.deny)
          access <- store.access
          _ <- ZIO.foreachDiscard(req.roles)(r =>
                 ZIO.when(access.config.role(r).isEmpty)(ZIO.fail(AppError.invalid(s"没有角色 ${r.value}")))
               )
          _  <- nameFree(name, None)
          _  <- dirFree(name)
          id <- newId
          t  <- now
          profile = AgentProfile(req.description.trim, model, req.group.map(_.trim).filter(_.nonEmpty))
          _ <- store.write(AssetPath.AgentFile(name), Formats.renderAgent(profile))
          _ <- store.write(AssetPath.PrivateMemory(name), "")
          _ <- paths.ensureWorkspace(id)
          record = AgentRecord(id, name, AgentRecord.keyOf(name.value), AgentState.Active, false, None, t, t)
          _ <- db.write(AgentRepo.insert(record) *> publish).tapError(_ => store.delete(dir(name)).ignoreLogged)
          grants = AgentGrants(req.roles.distinct, allow, deny)
          _ <- ZIO.unless(grants.isEmpty)(
                 setGrants(id, _ => grants).mapError(e =>
                   AppError.Internal(s"agent ${name.value} 已建好，但角色与规则没写进 access.toml：${e.message}；请在 agent 设置里重新配", Some(e))
                 )
               )
          _ <- ZIO.logInfo(s"建了 agent ${name.value}（${id.value}）")
        yield record
      }

    def update(id: AgentId, patch: AgentPatch): IO[AppError, AgentRecord] =
      lock.withPermit {
        for
          a <- get(id)
          _ <- ZIO.unless(a.live)(ZIO.fail(AppError.notFound(s"agent ${a.name.value} 已删除")))
          _ <- updateProfile(a, patch)
          _ <- updateGrants(a, patch)
          a <- patch.name.fold(ZIO.succeed(a))(rename(a, _))
          a <- patch.archived.fold(ZIO.succeed(a))(setArchived(a, _))
          a <- patch.isDefault.fold(ZIO.succeed(a))(setDefault(a, _))
        yield a
      }

    private def updateProfile(a: AgentRecord, p: AgentPatch): IO[AppError, Unit] =
      val touched = p.description.isDefined || p.model.isDefined || p.group.isDefined
      ZIO.when(touched) {
        for
          old <- profile(a.id)
          desc = p.description.map(_.trim).getOrElse(old.description)
          _   <- ZIO.when(desc.isEmpty)(ZIO.fail(AppError.invalid("描述不能为空")))
          model = p.model.map(_.trim).getOrElse(old.model)
          _   <- ZIO.when(p.model.isDefined)(checkModel(model))
          next = AgentProfile(desc, model, p.group.fold(old.group)(_.map(_.trim).filter(_.nonEmpty)))
          _   <- store.write(AssetPath.AgentFile(a.name), Formats.renderAgent(next))
        yield ()
      }.unit

    private def updateGrants(a: AgentRecord, p: AgentPatch): IO[AppError, Unit] =
      ZIO.when(p.roles.isDefined || p.allow.isDefined || p.deny.isDefined) {
        for
          access <- store.access
          old     = access.config.grantsOf(a.id)
          allow  <- ZIO.foreach(p.allow)(parseRules(_, old.allow.map(_.text).toSet))
          deny   <- ZIO.foreach(p.deny)(parseRules(_, old.deny.map(_.text).toSet))
          _ <- ZIO.foreachDiscard(p.roles.toList.flatten)(r =>
                 ZIO.when(access.config.role(r).isEmpty)(ZIO.fail(AppError.invalid(s"没有角色 ${r.value}")))
               )
          _ <- setGrants(a.id, g => AgentGrants(p.roles.fold(g.roles)(_.distinct), allow.getOrElse(g.allow), deny.getOrElse(g.deny)))
        yield ()
      }.unit

    /** 改名：先在库里记下新名和旧名，再挪资源目录，最后清掉旧名。挪到一半重启由 [[start]] 接着挪。 */
    private def rename(a: AgentRecord, raw: String): IO[AppError, AgentRecord] =
      for
        name <- ZIO.fromEither(validateName(raw)).mapError(AppError.invalid)
        out <-
          if name == a.name then ZIO.succeed(a)
          else
            val caseOnly = AgentRecord.keyOf(name.value) == a.nameKey
            for
              _ <- nameFree(name, Some(a.id))
              _ <- ZIO.unless(caseOnly)(dirFree(name))
              t <- now
              renamed = a.copy(name = name, nameKey = AgentRecord.keyOf(name.value), renamedFrom = Some(a.name), updatedAt = t)
              _ <- db.write(AgentRepo.update(renamed) *> publish)
              done <- finishRename(renamed)
              _    <- ZIO.logInfo(s"agent ${a.name.value} 改名为 ${name.value}（${a.id.value}）")
            yield done
      yield out

    private def finishRename(a: AgentRecord): IO[AppError, AgentRecord] =
      a.renamedFrom match
        case None => ZIO.succeed(a)
        case Some(old) =>
          val from = dir(old)
          val to   = dir(a.name)
          for
            fromExists <- ZIO.attemptBlocking(Files.exists(from)).orDie
            sameFile   <- ZIO.attemptBlocking(Files.exists(to) && Files.isSameFile(from, to)).orDie
            _ <-
              if !fromExists then ZIO.unit // 已经挪过了
              else if sameFile then
                // 只改大小写：不分大小写的文件系统上先挪到隐藏的中转名
                val tmp = paths.assets.resolve(s".rename-${a.id.value}")
                store.move(from, tmp) *> store.move(tmp, to)
              else store.move(from, to)
            done = a.copy(renamedFrom = None)
            _ <- db.write(AgentRepo.update(done) *> publish)
          yield done

    private def busyReasons(id: AgentId): IO[AppError, List[String]] =
      hooks.await.flatMap(hs => ZIO.foreach(hs)(h => h.busy(id))).map(_.flatten)

    private def setArchived(a: AgentRecord, archived: Boolean): IO[AppError, AgentRecord] =
      (a.state, archived) match
        case (AgentState.Active, true) =>
          for
            busy <- busyReasons(a.id)
            _ <- ZIO.when(busy.nonEmpty)(
                   ZIO.fail(AppError.conflict(s"agent ${a.name.value} 正忙（${busy.mkString("；")}），空闲时才能归档"))
                 )
            t <- now
            next = a.copy(state = AgentState.Archived, isDefault = false, updatedAt = t)
            _ <- db.write(AgentRepo.update(next) *> publish)
          yield next
        case (AgentState.Archived, false) =>
          now.flatMap { t =>
            val next = a.copy(state = AgentState.Active, updatedAt = t)
            db.write(AgentRepo.update(next) *> publish).as(next)
          }
        case _ => ZIO.succeed(a)

    private def setDefault(a: AgentRecord, isDefault: Boolean): IO[AppError, AgentRecord] =
      if isDefault == a.isDefault then ZIO.succeed(a)
      else if isDefault && a.state != AgentState.Active then ZIO.fail(AppError.conflict("已归档的 agent 不能设为默认"))
      else
        now.flatMap { t =>
          val next = a.copy(isDefault = isDefault, updatedAt = t)
          db.write(AgentRepo.clearDefault.when(isDefault) *> AgentRepo.update(next) *> publish).as(next)
        }

    def delete(id: AgentId): IO[AppError, Unit] =
      lock.withPermit {
        for
          a <- get(id)
          _ <- ZIO.unless(a.live)(ZIO.fail(AppError.notFound(s"agent ${a.name.value} 已删除或正在删除")))
          busy <- busyReasons(id)
          _ <- ZIO.when(busy.nonEmpty)(
                 ZIO.fail(AppError.conflict(s"agent ${a.name.value} 正忙（${busy.mkString("；")}），空闲时才能删除"))
               )
          t <- now
          deleting = a.copy(state = AgentState.Deleting, isDefault = false, updatedAt = t)
          _ <- db.write(AgentRepo.update(deleting) *> publish)
          _ <- ZIO.logInfo(s"开始删除 agent ${a.name.value}（${id.value}）")
          _ <- finishDelete(deleting)
        yield ()
      }

    /** 删除的第二阶段，每一步都可重复执行。 */
    private def finishDelete(a: AgentRecord): IO[AppError, Unit] =
      for
        a  <- finishRename(a)
        hs <- hooks.await
        _ <- ZIO.foreachDiscard(hs)(h =>
               h.cleanup(a.id).mapError(e => AppError.Internal(s"删除 agent ${a.name.value} 时 ${h.name} 清理失败：${e.message}", Some(e)))
             )
        _ <- handOverResources(a)
        _ <- removeGrants(a.id)
        _ <- store.delete(dir(a.name))
        _ <- ZIO.attemptBlocking(deleteTree(paths.workspace(a.id))).mapError(e => AppError.internal("删工作目录失败", e))
        t <- now
        _ <- db.write(AgentRepo.update(a.copy(state = AgentState.Deleted, updatedAt = t)) *> publish)
        _ <- ZIO.logInfo(s"agent ${a.name.value}（${a.id.value}）已删除")
      yield ()

    /** 它名下仍被别的 agent 按规则用着的资源转给平台（挪进 platform 目录）；平台已有同名的就不转（它本来也没加载）。 */
    private def handOverResources(a: AgentRecord): IO[AppError, Unit] =
      for
        snap   <- store.snapshot
        access <- store.access
        others <- list.map(_.filterNot(_.id == a.id))
        perms   = others.map(o => Permissions.of(o.id, o.name, access.config))
        owner   = Owner.Agent(a.name)
        usedByOthers = (kind: AssetKind, name: String) =>
                         perms.exists(p =>
                           p.decide(Action.Use, kind, name, owner) match
                             case Decision.Allowed(Grant.ByRule(_, _)) => true
                             case _                                    => false
                         )
        moves = snap.named.filter(n => n.owner == owner && usedByOthers(n.kind, n.name)).flatMap { n =>
                  AssetPath.home(n.kind, n.name, owner).zip(AssetPath.home(n.kind, n.name, Owner.Platform)).map((f, t) => paths.assets.resolve(f) -> paths.assets.resolve(t))
                }
        _ <- ZIO.foreachDiscard(moves) { (from, to) =>
               ZIO.whenZIO(ZIO.attemptBlocking(Files.exists(to)).orDie.negate)(
                 store.move(from, to) *> ZIO.logInfo(s"删除 agent ${a.name.value}：$from 仍有人用，转给平台")
               )
             }
      yield ()

    private def removeGrants(id: AgentId): IO[AppError, Unit] =
      store.access.flatMap { s =>
        if !s.config.agents.contains(id) then ZIO.unit
        else if s.broken.isDefined then ZIO.logWarning(s"access.toml 写坏了，删除的 agent ${id.value} 的段没能去掉")
        else editor.modify(c => Right(c.copy(agents = c.agents - id))).unit
      }

    private def deleteTree(p: java.nio.file.Path): Unit =
      if Files.exists(p, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
        scala.util.Using.resource(Files.walk(p)) { s =>
          s.sorted(java.util.Comparator.reverseOrder()).forEach(x => Files.delete(x))
        }

    def start(hs: List[AgentHook]): IO[AppError, Unit] =
      // 先定下参与方再拿锁：拿着锁的归档、删除会等它们
      hooks.succeed(hs) *> lock.withPermit {
        for
          all <- db.read(AgentRepo.all)
          _   <- ZIO.foreachDiscard(all.filter(a => a.renamedFrom.isDefined && a.live))(finishRename)
          _ <- ZIO.foreachDiscard(all.filter(_.state == AgentState.Deleting)) { a =>
                 ZIO.logInfo(s"接着删除 agent ${a.name.value}（${a.id.value}）") *> finishDelete(a)
               }
        yield ()
      }
