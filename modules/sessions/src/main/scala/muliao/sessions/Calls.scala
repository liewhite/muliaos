package muliao.sessions

import muliao.contract.*
import muliao.platform.{AppError, Retry}
import muliao.platform.db.Tx
import muliao.sessions.store.*
import muliao.toolkit.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/** 工具调用状态的唯一写入口。 */
private[sessions] object CallTx:
  def save(key: CallKey, s: CallStatus): Tx[Unit] =
    val pause: Option[Pause] = s match
      case CallStatus.Suspended(p, _)   => Some(p)
      case CallStatus.Approved(a, _, _) => Some(a)
      case _                            => None
    Repo.setCall(key.entry, key.call, s.state, pause.map(_.toJson), s.outcome.map(_.toJson), s.started, s.finished)

  /** 标成做完并写结果（开始时间沿用原来的）。 */
  def finish(c: Call, r: CallResult, t: Long): Tx[Unit] = save(c.key, CallStatus.Done(r, c.status.started, t))

/** 执行一条回复里的工具调用：`Ordered` 的工具与其他调用保持先后顺序，相邻的 `Concurrent` 调用并发执行；每个调用开始与结束都落库。 */
private[sessions] final case class CallRunner(kernel: Kernel, files: Files, delegations: Delegations):
  import kernel.*

  /** 一个调用的结局。 */
  private enum Fate:
    case Result(r: CallResult)
    case Wait(p: Pause)
    /** 系统性错误或暂时不可用到放弃：结果交给模型，整次运行最终失败 */
    case Abort(r: CallResult, why: String)

  /** 同一工具连续被拒多少次后提示换做法。 */
  private val RejectHint = 3

  def execute(kit: Kit, run: RunId, calls: List[Call], rejects: Ref[Map[String, Int]], slot: Slot): IO[AppError, Unit] =
    val segments = calls.sortBy(c => (c.entryId, c.seq)).foldLeft(List.empty[List[Call]]) { (acc, c) =>
      val ordered = kit.specs.get(c.name).forall(_.scheduling == Scheduling.Ordered) // 认不出的工具按顺序处理
      acc match
        case last :: rest if !ordered && last.forall(x => kit.specs.get(x.name).exists(_.scheduling == Scheduling.Concurrent)) => (last :+ c) :: rest
        case _                                                                                                              => List(c) :: acc
    }.reverse
    def go(segs: List[List[Call]]): IO[AppError, Option[String]] = segs match
      case Nil => ZIO.none
      case seg :: rest =>
        ZIO.foreachPar(seg)(c => one(kit, run, c, rejects, slot)).flatMap { aborted =>
          aborted.flatten.headOption match
            case Some(why) => skipRest(kit.info.id, rest.flatten).as(Some(why))
            case None      => go(rest)
        }
    go(segments).flatMap(ZIO.foreachDiscard(_)(why => ZIO.fail(AppError.Internal(why))))

  /** 前面的调用出了系统错误：后面的不执行。 */
  private def skipRest(id: SessionId, cs: List[Call]): IO[AppError, Unit] =
    now.flatMap(t => db.write(ZIO.foreachDiscard(cs)(c => CallTx.finish(c, CallResult.error("[没有执行：同一回复里前面的调用出了系统错误。]"), t)) *> touched(id, t))).unit

  /** 执行一个调用；系统性失败时返回原因。 */
  private def one(kit: Kit, run: RunId, c: Call, rejects: Ref[Map[String, Int]], slot: Slot): IO[AppError, Option[String]] =
    val id = kit.info.id
    for
      t0 <- now
      _  <- db.write(CallTx.save(c.key, CallStatus.Running(t0)) *> touched(id, t0))
      waiting <- info(id).map(_.userWaiting) // 运行中用户可能插话，按调用时的算
      ctx = RunContext(kit.info.agent, kit.res.agent.name, id, run, c.callId, kit.info.origin, kit.workspace, kit.res, waiting)
      args    <- slot.secrets.get.map(_.getOrElse(c.key, c.args))
      outcome <- invoke(kit, c.copy(args = args), ctx)
      fate    <- toFate(c, outcome, rejects)
      t1 <- now
      _  <- save(kit, c, fate, t0, t1)
    yield fate match
      case Fate.Abort(_, why) => Some(why)
      case _                  => None

  private def invoke(kit: Kit, c: Call, ctx: RunContext): UIO[Either[ToolFailure, ToolOutcome[Pause]]] =
    val tool = ZIO.fromOption(kit.tools.get(c.name)).orElseFail(ToolFailure.Rejected(s"没有工具 ${c.name}（或你没有权限用它）。只能用工具列表里的。"))
    val attempt: IO[ToolFailure, ToolOutcome[Pause]] = c.status match
      case CallStatus.Approved(a, always, _) =>
        tool.flatMap {
          case t: Approvable => t.approved(a.payload, always, ctx)
          case _             => ZIO.fail(ToolFailure.Aborted(s"工具 ${c.name} 不接受批准后的执行"))
        }
      case _ =>
        tool.flatMap(t =>
          ZIO
            .fromEither(c.args.fromJson[Json])
            .mapError(_ => ToolFailure.Rejected("参数不是合法的 JSON 对象，改好了再调用。"))
            .flatMap(args => t.invoke(args, ctx))
        )
    // 暂时不可用：这个调用退避重试（约 30 分钟放弃），期间运行仍算在跑；其余结局原样交出
    attempt
      .catchAllDefect(d => ZIO.fail(ToolFailure.Aborted(s"工具 ${c.name} 出错：${d.getMessage}", Some(d))))
      .foldZIO(
        {
          case ToolFailure.Unavailable(m, after) => ZIO.fail(AppError.Unavailable(m, None, after))
          case other                             => ZIO.succeed(Left(other))
        },
        o => ZIO.succeed(Right(o))
      )
      .retry(Retry.schedule(Retry.transient))
      .catchAll(e => ZIO.succeed(Left(ToolFailure.Unavailable(e.message))))

  private def toFate(c: Call, outcome: Either[ToolFailure, ToolOutcome[Pause]], rejects: Ref[Map[String, Int]]): IO[AppError, Fate] =
    outcome match
      case Right(ToolOutcome.Completed(content)) =>
        rejects.update(_ - c.name) *> stored(content).map(Fate.Result(_))
      case Right(ToolOutcome.Suspended(p)) => ZIO.succeed(Fate.Wait(p))
      case Left(ToolFailure.Rejected(m)) =>
        rejects.modify { m0 =>
          val n = m0.getOrElse(c.name, 0) + 1
          (n, m0.updated(c.name, n))
        }.map { n =>
          val hint = if n >= RejectHint then s"\n[${c.name} 已连续 $n 次出错，换个做法。]" else ""
          Fate.Result(CallResult.error(m + hint))
        }
      case Left(ToolFailure.Unavailable(m, _)) =>
        ZIO.succeed(Fate.Abort(CallResult.error(s"[暂时不可用，重试约 30 分钟仍失败：$m]"), s"工具 ${c.name} 一直不可用：$m"))
      case Left(ToolFailure.Aborted(m, cause)) =>
        ZIO.foreachDiscard(cause)(e => ZIO.logErrorCause(s"工具 ${c.name} 系统错误", Cause.die(e))) *>
          ZIO.succeed(Fate.Abort(CallResult.error(s"[系统错误：$m]"), s"工具 ${c.name} 出了系统错误：$m"))

  /** 工具结果落库的样子：图片存附件，文字超长截断。 */
  private def stored(content: Chunk[ToolContent]): IO[AppError, CallResult] =
    val limit = settings.toolOutputLimit
    ZIO
      .foreach(content) {
        case ToolContent.Text(t) if t.length > limit =>
          ZIO.succeed(StoredPart.Text(t.take(limit) + s"\n[输出超过 $limit 个字符，后面 ${t.length - limit} 个截掉了；需要的话分段取。]"))
        case ToolContent.Text(t)     => ZIO.succeed(StoredPart.Text(t))
        case ToolContent.Image(m, b) => files.put(b).map(StoredPart.Image(m, _))
      }
      .map(CallResult(_, isError = false))

  private def save(kit: Kit, c: Call, fate: Fate, t0: Long, t1: Long): IO[AppError, Unit] =
    val i = kit.info
    fate match
      case Fate.Result(r)   => db.write(CallTx.save(c.key, CallStatus.Done(r, Some(t0), t1)) *> touched(i.id, t1))
      case Fate.Abort(r, _) => db.write(CallTx.save(c.key, CallStatus.Done(r, Some(t0), t1)) *> touched(i.id, t1))
      case Fate.Wait(p) =>
        db.write(
          for
            _ <- CallTx.save(c.key, CallStatus.Suspended(p, t0))
            _ <- p match
                   case Pause.AwaitThread(d, _, _) => delegations.linkTx(d, c.key, t1)
                   case _                          => ZIO.unit
            waiting <- infoTx(i.id).map(_.userWaiting)
            _ <- p match
                   case Pause.AskUser(qs)          => ports.emit(SessionEvent.NeedsUser(i.id, i.agent, UserNeed.Answers(qs.size), waiting))
                   case Pause.Approval(summary, _) => ports.emit(SessionEvent.NeedsUser(i.id, i.agent, UserNeed.Approval(summary), waiting))
                   case _: Pause.AwaitThread       => ZIO.unit
            _ <- touched(i.id, t1)
          yield ()
        )

private[sessions] object CallRunner:
  val layer: URLayer[Kernel & Files & Delegations, CallRunner] = ZLayer.derive[CallRunner]
