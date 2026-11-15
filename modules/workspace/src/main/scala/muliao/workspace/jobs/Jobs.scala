package muliao.workspace.jobs

import muliao.contract.{AgentId, EventKind, JobId, JobStatus, JobView, SessionId, Topic}
import muliao.exec.{Exec, Sandbox, Stderr}
import muliao.platform.AppError
import muliao.platform.db.{Database, Tx}
import muliao.platform.events.ChangeBus
import muliao.resources.{AgentCommand, AgentExec, ResourcePaths}
import muliao.sessions.{Letter, Sessions, Source}
import muliao.workspace.Durations
import muliao.workspace.store.{JobRow, Repo}
import zio.*

import java.io.{FileOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** 后台任务怎么结束的（还在跑的没有这个值）。`verb` 用在结局与 job_status 的文字里。 */
enum Ended(val status: JobStatus, val verb: String):
  case Exited(code: Int) extends Ended(JobStatus.Exited, "结束")
  /** 被结束（job_kill、界面、会话被停止） */
  case Killed extends Ended(JobStatus.Killed, "被结束")
  /** 服务关停或崩溃时它还在跑：平台没有接管它 */
  case Interrupted extends Ended(JobStatus.Interrupted, "被中断")

  def exitCode: Option[Int] = this match
    case Exited(c)            => Some(c)
    case Killed | Interrupted => None

  /** 「退出码 0，运行了 3 分钟」这样的一句；`ran` 是「运行了 …」。 */
  def detail(ran: String): String = this match
    case Exited(c)   => s"退出码 $c，$ran"
    case Killed      => ran
    case Interrupted => "服务重启时它还在跑"

object Ended:
  /** 从库里的状态与退出码还原：还在跑的是 None（结束了却没有退出码的按被中断算）。 */
  def of(row: JobRow): Option[Ended] = (row.state, row.exitCode) match
    case (JobStatus.Running, _)      => None
    case (JobStatus.Exited, Some(c)) => Some(Exited(c))
    case (JobStatus.Killed, _)       => Some(Killed)
    case _                           => Some(Interrupted)

/** 谁起的后台任务：结局与心跳回到这个会话。 */
final case class JobOwner(agent: AgentId, session: SessionId)

/** 后台任务：登记为发起会话的异步工作（会话因此「等待中」），输出写进工作目录的 `jobs/<编号>.log`，结局与心跳作为事件回到会话。
  * 服务关停时结束全部任务并标为被中断；重启后把上次没收尾的标为被中断（不盲杀：进程号可能已被别的进程用了）并告诉 agent。
  */
trait Jobs:
  def start(owner: JobOwner, cmd: AgentCommand, label: String, heartbeat: Option[Duration]): IO[AppError, JobId]

  /** 这个 agent 的某个任务（别人的当作不存在）。 */
  def get(agent: AgentId, id: JobId): IO[AppError, JobRow]

  /** `job_status` 的文字：状态、运行时长、日志最后几行。 */
  def describe(row: JobRow, tailLines: Int): IO[AppError, String]

  /** 心跳检查里的一句现状：运行时长与日志最后几行。 */
  def brief(id: JobId): IO[AppError, String]

  /** 结束一个任务；已经结束的返回 false。结局照常作为事件回到发起会话。 */
  def kill(id: JobId): UIO[Boolean]

  def list(agent: AgentId): IO[AppError, List[JobView]]

  /** 日志最后 `lines` 行。 */
  def tail(id: JobId, lines: Int): IO[AppError, String]

  /** 启动时收尾：上次没收尾的标为被中断，没交回的结局补交。返回处理了几个。要在 [[Sessions.start]] 之后调用。 */
  def recover: IO[AppError, Int]

  /** 这个 agent 在跑的任务数。 */
  def running(agent: AgentId): UIO[Int]

  /** 删除 agent 时：结束它的任务，删记录。 */
  def cleanup(agent: AgentId): IO[AppError, Unit]

object Jobs:
  val layer: ZLayer[Database & ChangeBus & Sessions & AgentExec & ResourcePaths, Nothing, Jobs] =
    ZLayer.scoped(
      for
        db       <- ZIO.service[Database]
        bus      <- ZIO.service[ChangeBus]
        sessions <- ZIO.service[Sessions]
        exec     <- ZIO.service[AgentExec]
        paths    <- ZIO.service[ResourcePaths]
        registry <- Ref.make(Map.empty[JobId, Live])
        // 关停时中断全部任务，并行收尾（每个都可能要等 TERM 之后的宽限）。不用并行子 scope 做 forkIn：任务自己结束时
        // 关它的子 scope 会另起 fiber 去中断它自己而互等，留下永远挂着的 root fiber，关停时卡死（#897）
        _ <- ZIO.addFinalizer(registry.get.flatMap { all =>
               ZIO.when(all.nonEmpty)(ZIO.logInfo(s"服务关停，结束 ${all.size} 个后台任务")) *> ZIO.foreachParDiscard(all.values)(_.fiber.interrupt)
             })
      yield Impl(db, bus, sessions, exec, paths, registry)
    )

  /** 日志文件（相对工作目录）。 */
  def logName(id: JobId): String = s"jobs/${id.value}.log"

  /** 一个在跑的任务：`stop` 完成就结束它（被结束）；打断 `fiber` 是服务关停（被中断）。 */
  private final case class Live(agent: AgentId, writable: Sandbox.Confined, stop: Promise[Nothing, Unit], fiber: Fiber[Nothing, Unit])

  private final case class Impl(
      db: Database,
      bus: ChangeBus,
      sessions: Sessions,
      exec: AgentExec,
      paths: ResourcePaths,
      registry: Ref[Map[JobId, Live]]
  ) extends Jobs:

    private val now = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    /** 界面能看到的任务数据（状态、退出码、时间）都经它写：提交后广播主题 jobs。 */
    private def write[A](tx: Tx[A]): IO[AppError, A] = db.write(tx <* bus.publishAfterCommit(Topic.Jobs))

    private def logFile(agent: AgentId, id: JobId): Path = paths.workspace(agent).resolve(logName(id))

    def start(owner: JobOwner, cmd: AgentCommand, label: String, heartbeat: Option[Duration]): IO[AppError, JobId] =
      for
        t  <- now
        id <- write(Repo.insertJob(JobRow(JobId.wrap(0L), owner.agent, owner.session, label, JobStatus.Running, None, true, t, None)))
        _ <- sessions
               .openJobWait(owner.session, id, s"后台任务 ${id.value}：$label", heartbeat)
               .tapError(_ => end(id, Ended.Exited(-1), reported = true))
        log = logFile(owner.agent, id)
        _ <- launch(owner, cmd, id, log).tapError(_ => end(id, Ended.Exited(-1), reported = true) *> sessions.closeJobWait(id, None).ignore)
      yield id

    private def launch(owner: JobOwner, cmd: AgentCommand, id: JobId, log: Path): IO[AppError, Unit] =
      for
        out  <- ZIO.attemptBlocking { Files.createDirectories(log.getParent); FileOutputStream(log.toFile, true) }.mapError(e => AppError.internal(s"建日志 $log 失败", e))
        stop <- Promise.make[Nothing, Unit]
        // 先登记再放行：任务结束时从登记里删掉自己，放行前结束会留下删不掉的登记
        gate <- Promise.make[Nothing, Unit]
        _ <- ZIO.uninterruptibleMask { restore =>
               restore(gate.await *> watch(id, cmd, out, stop)).forkDaemon
                 .flatMap(f => registry.update(_.updated(id, Live(owner.agent, cmd.scope, stop, f))) *> gate.succeed(()))
             }
      yield ()

    /** 把输出写进日志，等它自己结束或被结束，记下结局并交回会话。服务关停时它被中断：进程由执行层结束，标为被中断，
      * 结局留到下次启动时交回（那时会话领域已经起来了）。
      */
    private def watch(id: JobId, cmd: AgentCommand, out: OutputStream, stop: Promise[Nothing, Unit]): UIO[Unit] =
      def write(text: String): UIO[Unit] =
        ZIO
          .attemptBlocking { out.write((text + "\n").getBytes(StandardCharsets.UTF_8)); out.flush() }
          .catchAll(e => ZIO.logWarning(s"写后台任务 ${id.value} 的日志出错：${e.getMessage}"))
      val ran: UIO[Ended] = Exec
        .follow(cmd.spec, Stderr.Merged)(l => write(l.text))
        .map(Ended.Exited(_))
        .catchAll(e => write(s"[muliao] ${e.message}").as(Ended.Exited(-1)))
        .race(stop.await.as(Ended.Killed))
      ZIO
        .acquireReleaseWith(ZIO.succeed(out))(o => ZIO.attemptBlocking(o.close()).ignore)(_ => ran)
        .onInterrupt(registry.update(_ - id) *> end(id, Ended.Interrupted, reported = false))
        .flatMap { ended =>
          registry.update(_ - id) *>
            settle(id, ended).catchAll(e => ZIO.logError(s"后台任务 ${id.value} 收尾失败（重启时补交）：${e.message}"))
        }

    private def end(id: JobId, ended: Ended, reported: Boolean): UIO[Unit] =
      now.flatMap(t => write(Repo.setOutcome(id, ended.status, ended.exitCode, t) *> ZIO.when(reported)(Repo.setReported(id)))).unit.catchAll(e => ZIO.logError(s"记后台任务 ${id.value} 的结局失败：${e.message}"))

    private def settle(id: JobId, ended: Ended): IO[AppError, Unit] =
      now.flatMap(t => write(Repo.setOutcome(id, ended.status, ended.exitCode, t))) *> db.read(Repo.job(id)).flatMap(ZIO.foreachDiscard(_)(report(_, ended)))

    /** 把结局作为事件交回发起会话（会话已经不等了就不投），记为已交回。 */
    private def report(row: JobRow, ended: Ended): IO[AppError, Unit] =
      for
        t    <- now
        text <- endText(row, ended, t)
        _    <- sessions.closeJobWait(row.id, Some(Letter(Source.Event(EventKind.Job), text)))
        _    <- db.write(Repo.setReported(row.id))
      yield ()

    /** 「运行了 …」：已结束的算到结束时间，还在跑的算到 `now`。 */
    private def ranSoFar(row: JobRow, now: Long): String = s"运行了 ${Durations.human(row.endedAt.getOrElse(now) - row.startedAt)}"

    private def endText(row: JobRow, ended: Ended, now: Long): IO[AppError, String] =
      val advice = ended match
        case Ended.Interrupted =>
          "：服务正常关停时会结束它，意外退出时进程可能还在（平台没有接管，也没有杀它）。看日志和相关进程确认结果，需要的话重新起"
        case _ => ""
      val head = s"[后台任务 ${row.id.value} ${ended.verb}] ${ended.detail(ranSoFar(row, now))}$advice。"
      tail(row.id, 20).map { last =>
        val shown = if last.isBlank then "（没有输出）" else last
        s"$head\n命令：${row.label}\n日志：${logName(row.id)}（在工作目录下），最后几行：\n$shown"
      }

    def get(agent: AgentId, id: JobId): IO[AppError, JobRow] =
      db.read(Repo.job(id)).someOrFail(AppError.notFound(s"没有后台任务 ${id.value}")).filterOrFail(_.agentId == agent)(
        AppError.notFound(s"没有后台任务 ${id.value}（只能看自己起的）")
      )

    def describe(row: JobRow, tailLines: Int): IO[AppError, String] =
      for
        t    <- now
        last <- tail(row.id, tailLines)
      yield
        val state = Ended.of(row).fold(s"运行中，${ranSoFar(row, t)}")(e => s"${e.verb}，${e.detail(ranSoFar(row, t))}")
        s"后台任务 ${row.id.value}：$state\n命令：${row.label}\n日志：${logName(row.id)}，最后 $tailLines 行：\n${if last.isBlank then "（没有输出）" else last}"

    def brief(id: JobId): IO[AppError, String] =
      for
        row  <- db.read(Repo.job(id)).someOrFail(AppError.notFound(s"没有后台任务 ${id.value}"))
        t    <- now
        last <- tail(id, 5)
      yield s"${ranSoFar(row, t)}；日志最后几行：\n${if last.isBlank then "（还没有输出）" else last}"

    def kill(id: JobId): UIO[Boolean] =
      registry.get.map(_.get(id)).flatMap {
        case Some(j) => j.stop.succeed(())
        case None    => ZIO.succeed(false)
      }

    def list(agent: AgentId): IO[AppError, List[JobView]] =
      db.read(Repo.jobsOf(agent, 200)).map(_.map(r => JobView(r.id, r.agentId, r.sessionId, r.label, r.state, r.exitCode, r.startedAt, r.endedAt, logName(r.id))))

    def tail(id: JobId, lines: Int): IO[AppError, String] =
      db.read(Repo.job(id)).someOrFail(AppError.notFound(s"没有后台任务 ${id.value}")).flatMap { row =>
        val f = logFile(row.agentId, id)
        ZIO.attemptBlocking(LogTail.read(f, lines)).mapError(e => AppError.internal(s"读日志 $f 失败", e))
      }.flatMap { text =>
        registry.get.map(_.get(id)).flatMap {
          case Some(j) => exec.annotate(j.agent, j.writable, text)
          case None    => ZIO.succeed(text)
        }
      }

    def recover: IO[AppError, Int] =
      for
        rows <- db.read(Repo.unsettled)
        live <- registry.get
        todo  = rows.filterNot(r => live.contains(r.id))
        t    <- now
        _ <- write(ZIO.foreachDiscard(todo.filter(_.state == JobStatus.Running))(r => Repo.setOutcome(r.id, Ended.Interrupted.status, None, t)))
        fresh <- ZIO.foreach(todo)(r => db.read(Repo.job(r.id))).map(_.flatten.flatMap(r => Ended.of(r).map(r -> _)))
        _     <- ZIO.foreachDiscard(fresh)((r, e) => report(r, e))
        _     <- ZIO.when(todo.nonEmpty)(ZIO.logInfo(s"收尾了 ${todo.size} 个上次没收尾的后台任务"))
      yield todo.size

    def running(agent: AgentId): UIO[Int] = registry.get.map(_.values.count(_.agent == agent))

    def cleanup(agent: AgentId): IO[AppError, Unit] =
      for
        mine <- registry.get.map(_.filter(_._2.agent == agent).keys.toList)
        _    <- ZIO.foreachDiscard(mine)(kill)
        _    <- write(Repo.deleteJobsOf(agent))
      yield ()

/** 读日志文件的最后几行（只读文件末尾一段，日志再大也不慢）。 */
object LogTail:
  private val Window = 256 * 1024

  def read(file: Path, lines: Int): String =
    if !Files.exists(file) then ""
    else
      scala.util.Using.resource(java.io.RandomAccessFile(file.toFile, "r")) { raf =>
        val size  = raf.length()
        val start = (size - Window).max(0L)
        val buf   = new Array[Byte]((size - start).toInt)
        raf.seek(start)
        raf.readFully(buf)
        val text = String(buf, StandardCharsets.UTF_8)
        text.split("\n", -1).toList.reverse.dropWhile(_.isEmpty).take(lines).reverse.mkString("\n")
      }
