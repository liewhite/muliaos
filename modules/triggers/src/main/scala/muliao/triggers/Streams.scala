package muliao.triggers

import muliao.contract.{AgentId, TriggerId, TriggerName}
import muliao.exec.{Exec, Masker, Program}
import muliao.platform.AppError
import muliao.resources.vault.SecretUse
import zio.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}

/** 常驻流：平台托管一条常驻命令，标准输出每行一个事件。
  *
  *   - 配置（agent、命令、secret、防抖）变了才重起；停用、删除、关停时结束整个进程组。
  *   - 退出后按退避重启（`restartFirst` 起翻倍，最长 `restartMax`）；偶尔重连是常态，连续 `failuresToFault` 次没跑到 `stableAfter`
  *     就退出才算脚本故障，跑稳了算恢复。运行状况只在内存里。
  *   - 事件先防抖：第一行到达后 `debounce` 内的合在一起；上一次开的会话没做完就继续排队，做完后合并成一次触发。队列有上限，满了就
  *     不再读命令的输出、让它自己阻塞：不重叠、不丢、内存有界。排队的事件不持久化（重连、去重、游标由命令负责）。
  *   - 标准错误（已打码）写进工作目录 `trigger-logs/<名字>.stderr.log`，超过上限轮换成 `.1`。
  */
private[triggers] final case class Streams(core: Core, scope: Scope, hosted: Ref.Synchronized[Map[TriggerId, Streams.Hosted]]):
  import core.*

  def run: UIO[Nothing] =
    (reconcile.catchAllCause(c => ZIO.logErrorCause("常驻流对账出错", c)) *> signal.streamWake).forever

  /** 按最新的行起、停常驻命令。 */
  private def reconcile: IO[AppError, Unit] =
    active.flatMap { all =>
      val wanted = all.flatMap(t =>
        t.mode match
          case m: Mode.Stream => Some(t.id -> Streams.Key(t.agent, t.name, m))
          case _              => None
      ).toMap
      hosted.updateZIO { current =>
        val stale = current.filter((id, h) => !wanted.get(id).contains(h.key))
        val fresh = wanted.filter((id, k) => !current.get(id).exists(_.key == k))
        for
          _       <- ZIO.foreachDiscard(stale)((id, h) => h.fiber.interrupt *> health.update(_ - id))
          started <- ZIO.foreach(fresh.toList)((id, k) => host(id, k).forkIn(scope).map(f => id -> Streams.Hosted(k, f)))
        yield current -- stale.keys ++ started
      }
    }

  /** 托管一条：命令一直重起，事件交给消费方。 */
  private def host(id: TriggerId, key: Streams.Key): UIO[Nothing] =
    for
      queue <- Queue.bounded[String](tuning.queueLines)
      fails <- Ref.make(0)
      _     <- health.update(_.updated(id, Health(None, 0)))
      _     <- consume(id, key, queue).fork
      n     <- (runOnce(id, key, queue, fails) *> backoff(fails)).forever
    yield n

  private def backoff(fails: Ref[Int]): UIO[Unit] =
    fails.get.flatMap { n =>
      val wait = if n <= 1 then tuning.restartFirst else (tuning.restartFirst * math.pow(2, (n - 1).min(30).toDouble)).min(tuning.restartMax)
      Clock.sleep(wait.min(tuning.restartMax))
    }

  /** 起一次命令直到它退出；没跑到稳定就退出算一次失败，连续失败够数记脚本故障。 */
  private def runOnce(id: TriggerId, key: Streams.Key, queue: Queue[String], fails: Ref[Int]): UIO[Unit] =
    val log = paths.workspace(key.agent).resolve(Streams.logName(key.name))
    val attempt: IO[AppError, Int] = ZIO.scoped(
      for
        cmd    <- exec.command(key.agent, Program.Shell(key.mode.command), key.mode.secrets, SecretUse.Shell)
        c      <- Exec.attach(cmd.spec)
        _      <- c.closeInput
        at     <- now
        _      <- fails.get.flatMap(n => health.update(_.updated(id, Health(Some(at), n))))
        _      <- (Clock.sleep(tuning.stableAfter) *> stable(id, key, fails)).forkScoped
        _      <- Streams.background(c.stderr.runForeach(line => Streams.append(log, line, tuning.stderrMaxBytes)))
        mask    = Masker(cmd.spec.secrets)
        read   <- Promise.make[Nothing, Unit]
        _      <- Streams.background(c.stdout.map(mask.mask).filter(_.trim.nonEmpty).runForeach(queue.offer) *> read.succeed(()))
        code   <- c.exitCode
        // 主进程退出后限时读完余下的输出（留在后台的子进程占着输出不放时不等它）
        _      <- read.await.timeout(Exec.DrainGrace)
      yield code
    )
    attempt.either.flatMap { r =>
      val why = r.fold(e => s"起不来：${e.message}", code => s"退出码 $code")
      for
        _ <- health.update(_.updatedWith(id)(_.map(_.copy(runningSince = None))))
        n <- fails.updateAndGet(_ + 1)
        _ <- health.update(_.updatedWith(id)(_.map(_.copy(failures = n))))
        _ <- Streams.append(log, s"[muliao] 命令结束（$why），第 $n 次连续没跑稳", tuning.stderrMaxBytes)
        _ <- ZIO.when(n >= tuning.failuresToFault)(
               load(id).someOrFail(AppError.notFound("触发器已删除")).flatMap(t =>
                 recordFault(t, FaultSource.Stream, s"常驻命令连续 $n 次没跑满 ${tuning.stableAfter.toSeconds} 秒就退出（最后一次：$why）。标准错误见工作目录 ${Streams.logName(key.name)}")
               ).ignore
             )
      yield ()
    }

  /** 跑稳了：连续失败清零，故障清除。 */
  private def stable(id: TriggerId, key: Streams.Key, fails: Ref[Int]): UIO[Unit] =
    fails.set(0) *> health.update(_.updatedWith(id)(_.map(_.copy(failures = 0)))) *>
      load(id).flatMap(ZIO.foreachDiscard(_)(t => clearFault(t, FaultSource.Stream))).ignore

  /** 事件消费：防抖 → 等上一次的会话做完（期间继续排队）→ 合并成一次触发。 */
  private def consume(id: TriggerId, key: Streams.Key, queue: Queue[String]): UIO[Nothing] =
    def deliver(lines: Chunk[String]): UIO[Unit] =
      load(id).flatMap {
        case None => ZIO.unit
        case Some(t) =>
          blocker(t).flatMap {
            case Some((s, state)) => noteBlocked(t, s, state) *> Clock.sleep(tuning.busyPoll) *> deliver(lines)
            case None =>
              queue.takeAll.flatMap { more =>
                fire(t, Occasion.Events(lines ++ more)).flatMap {
                  case Fired.Blocked(_) => Clock.sleep(tuning.busyPoll) *> deliver(lines ++ more)
                  case Fired.NotDone(why) => ZIO.logWarning(s"常驻流 ${t.name.value} 的事件没交出去：$why")
                  case _                  => ZIO.unit
                }
              }
          }
      }.catchAll(e => ZIO.logError(s"常驻流 ${key.name.value} 的事件没交出去：${e.message}"))
    (for
      first <- queue.take
      _     <- Clock.sleep(key.mode.debounce)
      rest  <- queue.takeAll
      _     <- deliver(first +: rest)
    yield ()).forever

private[triggers] object Streams:
  /** 决定要不要重起的东西：这些变了才重起（提示词、前置检查变了不用）。 */
  final case class Key(agent: AgentId, name: TriggerName, mode: Mode.Stream)

  final case class Hosted(key: Key, fiber: Fiber.Runtime[Nothing, Nothing])

  def make(core: Core, scope: Scope): UIO[Streams] =
    Ref.Synchronized.make(Map.empty[TriggerId, Hosted]).map(Streams(core, scope, _))

  /** 标准错误日志（相对工作目录）。 */
  def logName(name: TriggerName): String = s"trigger-logs/${name.value}.stderr.log"

  /** 读命令输出的一路：读管道是阻塞调用、中断要等它返回，所以不跟着 scope 等它结束——scope 结束时只发中断，进程组随后被结束、管道关闭，
    * 它自然收尾。
    */
  def background(read: UIO[Any]): URIO[Scope, Unit] =
    read.forkDaemon.flatMap(f => ZIO.addFinalizer(f.interruptFork)).unit

  /** 追加一行；超过上限先把现有的轮换成 `.1`。 */
  def append(file: Path, line: String, maxBytes: Long): UIO[Unit] =
    ZIO
      .attemptBlocking {
        Files.createDirectories(file.getParent)
        if Files.exists(file) && Files.size(file) > maxBytes then
          Files.move(file, file.resolveSibling(file.getFileName.toString + ".1"), StandardCopyOption.REPLACE_EXISTING): Unit
        Files.write(file, (line + "\n").getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND): Unit
      }
      .catchAll(e => ZIO.logWarning(s"写 $file 失败：${e.getMessage}"))
