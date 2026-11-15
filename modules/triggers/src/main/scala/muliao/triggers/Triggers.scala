package muliao.triggers

import muliao.contract.*
import muliao.notifications.Notifications
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.{AgentHook, ResourcePaths}
import muliao.resources.agents.Agents
import muliao.sessions.{Contribution, Sessions}
import muliao.toolkit.ToolSpec
import muliao.triggers.store.Repo
import muliao.workspace.AgentExec
import zio.*

import java.time.OffsetDateTime

/** 触发器领域给界面、工具与组装入口的入口。 */
trait Triggers:
  /** 起到期扫描与常驻流托管。在 [[Sessions.start]] 之后调用；只起一次。 */
  def start: UIO[Unit]

  def kinds: List[TriggerKindView]

  /** 解释 cron 与不带时区的时刻用的时区（配置 `[general] timezone`）。 */
  def zone: java.time.ZoneId

  /** `finished`：列已结束的一次性触发器（否则列没结束的）。 */
  def list(finished: Boolean): IO[AppError, List[TriggerView]]
  def view(id: TriggerId): IO[AppError, TriggerView]

  /** 按名字找：没结束的那个；`orFinished` 时找不到再取最近结束的同名的。 */
  def named(name: String, orFinished: Boolean = false): IO[AppError, TriggerId]

  def create(req: NewTrigger): IO[AppError, TriggerView]

  /** 只改给出的字段。改配置、改前置检查、重新启用会清掉脚本故障与跳过计数；改配置或重新启用从此刻重算下次到期（停用期间错过的不补）。 */
  def update(id: TriggerId, patch: TriggerPatch): IO[AppError, TriggerView]

  def delete(id: TriggerId): IO[AppError, Unit]

  /** 立即运行：走同一个触发出口（先跑前置检查、防重叠），不消耗一次性的那次；前置检查失败只报给调用方。 */
  def run(id: TriggerId): IO[AppError, FireResult]

  /** 删除 agent 时删掉它的全部触发器（可重复执行）。 */
  def forgetAgent(agent: AgentId): IO[AppError, Unit]

object Triggers:
  /** 界面的触发器页（通知点开去这里）。 */
  val PageUrl = "/triggers"

  /** 本领域工具的静态定义（组装时汇进全部内置工具目录）。 */
  val specs: List[ToolSpec] = TriggerTools.specs

  /** 给会话领域的工具，以及删除 agent 时删掉它的触发器。 */
  val contribution: URIO[Triggers & Agents, Contribution] =
    for
      t      <- ZIO.service[Triggers]
      agents <- ZIO.service[Agents]
    yield Contribution(tools = TriggerTools.all(t, agents), agentHook = Some(AgentHook("触发器", cleanup = t.forgetAgent)))

  type Env = Database & ChangeBus & Sessions & Agents & Notifications & AgentExec & ResourcePaths & GeneralSettings & TriggerSettings

  val layer: ZLayer[Env, Nothing, Triggers] = layerWith(Tuning())

  /** `tuning`：运行节奏（测试里调小）。 */
  def layerWith(tuning: Tuning): ZLayer[Env, Nothing, Triggers] =
    ZLayer.scoped(
      for
        db       <- ZIO.service[Database]
        bus      <- ZIO.service[ChangeBus]
        sessions <- ZIO.service[Sessions]
        agents   <- ZIO.service[Agents]
        notes    <- ZIO.service[Notifications]
        exec     <- ZIO.service[AgentExec]
        paths    <- ZIO.service[ResourcePaths]
        general  <- ZIO.service[GeneralSettings]
        settings <- ZIO.service[TriggerSettings]
        signal   <- Signal.make
        inFlight <- Ref.make(Set.empty[TriggerId])
        health   <- Ref.make(Map.empty[TriggerId, Health])
        core      = Core(db, bus, sessions, agents, notes, exec, paths, general.timezone, settings, tuning, signal, inFlight, health)
        scope    <- ZIO.scope
        started  <- Ref.make(false)
      yield Live(core, scope, started)
    )

  private final case class Revision(next: Trigger, restart: Boolean)

  private final case class Live(core: Core, scope: Scope, started: Ref[Boolean]) extends Triggers:
    import core.*

    override val zone: java.time.ZoneId = core.zone

    def start: UIO[Unit] =
      started.getAndSet(true).flatMap {
        case true => ZIO.unit
        case false =>
          for
            streams <- Streams.make(core, scope)
            _       <- DueRunner(core, scope).run.forkIn(scope)
            _       <- streams.run.forkIn(scope)
            _       <- ZIO.logInfo("触发器已启动")
          yield ()
      }

    val kinds: List[TriggerKindView] = TriggerKind.values.toList.map(k => TriggerKindView(k, Kinds.description(k)))

    // ---- 查看 ----

    def list(finished: Boolean): IO[AppError, List[TriggerView]] =
      db.read(Repo.list(finished)).flatMap(rows => ZIO.foreach(rows)(decode)).flatMap(views)

    def view(id: TriggerId): IO[AppError, TriggerView] =
      load(id).someOrFail(missing(id)).flatMap(t => views(List(t))).map(_.head)

    private def missing(id: TriggerId) = AppError.notFound(s"触发器 ${id.value} 不存在（可能已删除）")

    private def views(ts: List[Trigger]): IO[AppError, List[TriggerView]] =
      for
        faults <- db.read(Repo.faultsOf(ts.map(_.id)))
        names  <- agents.names(ts.map(_.agent))
        hs     <- health.get
      yield
        val byTrigger = faults.groupBy(_.triggerId)
        ts.map { t =>
          TriggerView(
            t.id, t.name, t.agent, names(t.agent), t.mode.config(zone), t.prompt, t.mode.enabled,
            t.precheck.map(_.spec), t.mode.nextAt, t.skipped, hs.get(t.id).flatMap(_.runningSince), t.lastSession,
            t.lastFiredAt, t.mode.finishedAt,
            byTrigger.getOrElse(t.id, Nil).map(f => Fault(f.source, f.error, f.at).view).sortBy(_.source.ordinal),
            t.createdAt, t.updatedAt
          )
        }

    def named(name: String, orFinished: Boolean): IO[AppError, TriggerId] =
      for
        n    <- ZIO.fromEither(TriggerName.make(name.trim)).mapError(AppError.invalid)
        live <- db.read(Repo.live(n))
        done <- if live.isEmpty && orFinished then db.read(Repo.finishedNamed(n)).map(_.headOption) else ZIO.none
        row  <- ZIO.fromOption(live.orElse(done)).orElseFail(AppError.notFound(s"没有触发器 $name（trigger_list 看现有的）"))
      yield row.id

    // ---- 改动 ----

    private def validPrompt(p: String): IO[AppError, String] =
      if p.trim.isEmpty then ZIO.fail(AppError.invalid("prompt 不能为空：它是每次触发交给 agent 的处理说明")) else ZIO.succeed(p)

    /** 解读新给的配置（得到停用的）；一次性的时刻要在将来。 */
    private def mode(c: TriggerConfig, at: Long): IO[AppError, Mode] =
      ZIO.fromEither(Mode.parse(c, zone)).mapError(AppError.invalid).tap {
        case Mode.Once(t, _) if t.toEpochMilli <= at =>
          ZIO.fail(AppError.invalid(s"once 的时刻 ${OffsetDateTime.ofInstant(t, zone)} 已经过去，要在将来；要现在跑就用立即运行"))
        case _ => ZIO.unit
      }

    private def precheck(p: PrecheckSpec): IO[AppError, Precheck] = ZIO.fromEither(Precheck.parse(p)).mapError(AppError.invalid)

    def create(req: NewTrigger): IO[AppError, TriggerView] =
      for
        name   <- ZIO.fromEither(TriggerName.make(req.name.trim)).mapError(AppError.invalid)
        _      <- agents.requireActive(req.agent)
        t0     <- now
        m      <- mode(req.config, t0)
        pre    <- ZIO.foreach(req.precheck)(precheck)
        prompt <- validPrompt(req.prompt)
        draft = Trigger(TriggerId.wrap(1L), name, req.agent, prompt, m.switched(req.enabled, instant(t0), zone), pre, 0, None, None, None, None, t0, t0)
        id <- db.write(
                for
                  taken <- Repo.live(name)
                  _     <- ZIO.when(taken.isDefined)(ZIO.fail(AppError.conflict(s"已有触发器叫 ${name.value}")))
                  id    <- Repo.insert(encode(draft).copy(id = TriggerId.wrap(0L)))
                  _     <- changed
                yield id
              )
        _ <- signal.ping
        v <- view(id)
      yield v

    def update(id: TriggerId, patch: TriggerPatch): IO[AppError, TriggerView] =
      for
        t0 <- now
        _ <- db.write(
               for
                 cur <- loadTx(id)
                 _   <- ZIO.when(cur.mode.finished)(ZIO.fail(AppError.conflict(s"触发器 ${cur.name.value} 已结束，不能再改")))
                 m <- ZIO.foreach(patch.config)(c =>
                        if c.kind == cur.mode.kind then mode(c, t0)
                        else ZIO.fail(AppError.invalid(s"不能把 ${cur.mode.kind.label} 改成 ${c.kind.label}：种类不变，要换种类就删掉重建"))
                      )
                 pre <- if patch.removePrecheck then ZIO.none else ZIO.foreach(patch.precheck)(precheck).map(_.orElse(cur.precheck))
                 prompt <- ZIO.foreach(patch.prompt)(validPrompt).map(_.getOrElse(cur.prompt))
                 rev     = revise(cur, m, pre, prompt, patch.enabled.getOrElse(cur.mode.enabled), t0)
                 _      <- ZIO.when(rev.restart)(Repo.clearFaults(id, None))
                 _ <- Repo.save(encode(rev.next))
                 _ <- changed
               yield ()
             )
        _ <- signal.ping
        v <- view(id)
      yield v

    /** 改动后的触发器（纯函数）：人工改了怎么触发（配置、前置检查）或重新启用，就从头来（`restart`）——清跳过计数与脚本故障，
      * 前置检查换了基线作废，配置变了或重新启用从此刻重算下次到期。
      */
    private def revise(cur: Trigger, patched: Option[Mode], pre: Option[Precheck], prompt: String, enabled: Boolean, at: Long): Revision =
      val modeChanged = patched.exists(m => !m.sameFiring(cur.mode))
      val preChanged  = pre != cur.precheck
      val reEnabled   = enabled && !cur.mode.enabled
      val restart     = modeChanged || preChanged || reEnabled
      val firing      = patched.filter(_ => modeChanged).getOrElse(cur.mode)
      Revision(
        cur.copy(
          prompt = prompt,
          mode = if modeChanged || enabled != cur.mode.enabled then firing.switched(enabled, instant(at), zone) else cur.mode,
          precheck = pre,
          skipped = if restart then 0 else cur.skipped,
          baseline = if preChanged then None else cur.baseline,
          updatedAt = at
        ),
        restart
      )

    def delete(id: TriggerId): IO[AppError, Unit] =
      db.write(Repo.get(id).someOrFail(missing(id)) *> Repo.delete(id) *> changed) *> signal.ping

    def run(id: TriggerId): IO[AppError, FireResult] =
      for
        t <- load(id).someOrFail(missing(id))
        _ <- ZIO.when(t.mode.finished)(ZIO.fail(AppError.conflict(s"触发器 ${t.name.value} 已结束，不能再运行")))
        r <- fire(t, Occasion.Manual)
      yield r.result

    /** 删 agent 时：删掉它的全部触发器（含已结束的），常驻命令随之结束。 */
    def forgetAgent(agent: AgentId): IO[AppError, Unit] =
      db.write(
        Repo.ofAgent(agent).flatMap(rows => ZIO.foreachDiscard(rows)(r => Repo.delete(r.id))) *> changed
      ) *> signal.ping
