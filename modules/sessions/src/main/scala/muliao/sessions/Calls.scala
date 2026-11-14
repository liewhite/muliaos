package muliao.sessions

import muliao.contract.*
import muliao.platform.{AppError, Retry}
import muliao.sessions.store.*
import muliao.toolkit.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/** 执行一条回复里的工具调用：`Ordered` 的工具与其他调用保持先后顺序，相邻的 `Concurrent` 调用并发执行；每个调用开始与结束都落库。 */
private[sessions] trait CallsPart:
  this: Engine =>
  import deps.*

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
    now.flatMap(t => db.write(ZIO.foreachDiscard(cs)(c => setResult(c, CallResult.error("[没有执行：同一回复里前面的调用出了系统错误。]"), t)) *> changed)).unit

  /** 执行一个调用；系统性失败时返回原因。 */
  private def one(kit: Kit, run: RunId, c: Call, rejects: Ref[Map[String, Int]], slot: Slot): IO[AppError, Option[String]] =
    val id = kit.info.id
    for
      t0 <- now
      _  <- db.write(Repo.setCall(c.entryId, c.callId, CallState.Running.label, c.pause.map(_.toJson), None, Some(t0), None) *> changed)
      _  <- live.publish(id, LiveEvent.Changed)
      waiting <- info(id).map(_.userWaiting) // 运行中用户可能插话，按调用时的算
      ctx = RunContext(kit.info.agent, kit.res.agent.name, id, run, CallId.wrap(c.callId), kit.info.origin, kit.workspace, kit.res, waiting)
      args    <- slot.secrets.get.map(_.getOrElse((c.entryId, c.callId), c.args))
      outcome <- invoke(kit, c.copy(args = args), ctx)
      fate    <- toFate(kit, c, outcome, rejects)
      t1 <- now
      _  <- save(kit, c, fate, t0, t1)
      _  <- live.publish(id, LiveEvent.Changed)
    yield fate match
      case Fate.Abort(_, why) => Some(why)
      case _                  => None

  private def invoke(kit: Kit, c: Call, ctx: RunContext): UIO[Either[ToolFailure, ToolOutcome]] =
    val attempt: IO[ToolFailure, ToolOutcome] = c.state match
      case CallState.Approved | CallState.ApprovedAlways =>
        c.pause match
          case Some(Pause.Approval(handler, _, payload)) =>
            hooks.approval(handler).flatMap {
              case Some(h) => h.approved(payload, c.state == CallState.ApprovedAlways, ctx)
              case None    => ZIO.fail(ToolFailure.Aborted(s"批准后的执行者 $handler 没有登记"))
            }
          case _ => ZIO.fail(ToolFailure.Aborted("批准了的调用没有记下要执行什么"))
      case _ =>
        kit.tools.get(c.name) match
          case None => ZIO.fail(ToolFailure.Rejected(s"没有工具 ${c.name}（或你没有权限用它）。只能用工具列表里的。"))
          case Some(tool) =>
            ZIO
              .fromEither(c.args.fromJson[Json])
              .mapError(_ => ToolFailure.Rejected("参数不是合法的 JSON 对象，改好了再调用。"))
              .flatMap(args => tool.invoke(args, ctx))
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

  private def toFate(kit: Kit, c: Call, outcome: Either[ToolFailure, ToolOutcome], rejects: Ref[Map[String, Int]]): IO[AppError, Fate] =
    outcome match
      case Right(ToolOutcome.Completed(content)) =>
        rejects.update(_ - c.name) *> stored(content).map(Fate.Result(_))
      case Right(ToolOutcome.Suspended(p: Pause)) => ZIO.succeed(Fate.Wait(p))
      case Right(ToolOutcome.Suspended(other)) =>
        ZIO.succeed(Fate.Abort(CallResult.error("[系统错误：工具挂起的方式平台不认识]"), s"工具 ${c.name} 挂起的方式不认识：$other"))
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
      case Fate.Result(r)   => db.write(Repo.setCall(c.entryId, c.callId, CallState.Done.label, c.pause.map(_.toJson), Some(r.toJson), Some(t0), Some(t1)) *> changed)
      case Fate.Abort(r, _) => db.write(Repo.setCall(c.entryId, c.callId, CallState.Done.label, c.pause.map(_.toJson), Some(r.toJson), Some(t0), Some(t1)) *> changed)
      case Fate.Wait(p) =>
        db.write(
          for
            _ <- Repo.setCall(c.entryId, c.callId, CallState.Suspended.label, Some((p: Pause).toJson), None, Some(t0), None)
            _ <- p match
                   case Pause.AwaitThread(d, _, _) => linkDelegation(d, c.entryId, c.callId, t1)
                   case _                          => ZIO.unit
            _ <- changed
          yield ()
        ) *> info(i.id).map(_.userWaiting).flatMap { waiting =>
          p match
            case Pause.AskUser(qs) =>
              hooks.emit(SessionEvent.NeedsUser(i.id, i.agent, s"有 ${qs.size} 个问题等你回答", waiting))
            case Pause.Approval(_, summary, _) =>
              hooks.emit(SessionEvent.NeedsUser(i.id, i.agent, s"等你批准：$summary", waiting))
            case _: Pause.AwaitThread => ZIO.unit
        }
