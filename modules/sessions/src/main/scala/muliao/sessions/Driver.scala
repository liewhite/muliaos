package muliao.sessions

import muliao.contract.*
import muliao.models.llm.*
import muliao.platform.AppError
import muliao.resources.{ResourcePrompt, Resources}
import muliao.sessions.store.*
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.time.Instant

/** 运行驱动：应门铃踢会话，按 [[Plan.next]] 一步步做——开运行、请求模型、执行工具、挂起、收尾。每步落库，重启后从库里接着算。 */
private[sessions] final class Driver(
    kernel: Kernel,
    slots: Slots,
    doorbell: Doorbell,
    mailbox: Mailbox,
    delegations: Delegations,
    requests: Requests,
    compaction: Compaction,
    calls: CallRunner,
    resources: Resources,
    scope: Scope
):
  import kernel.*

  /** 启动：收尾上次被打断的调用，续跑没跑完的运行（含挂起的：等的结果可能已到、门铃没来得及按）、排队的来信与批准了的调用，
    * 以及待收尾的会话（运行做完了、收尾前被重启的，由 [[settleIfDone]] 补收尾），然后开始应门铃。
    */
  def start: IO[AppError, Unit] =
    for
      t       <- now
      running <- db.read(Repo.callsIn(List(CallState.Running)))
      pending <- db.read(Repo.callsIn(List(CallState.Pending)))
      _ <- db.write(
             ZIO.foreachDiscard(running)(c =>
               CallTx.save(CallKey(c.entryId, c.callId), CallStatus.Done(CallResult.error(Interrupted.restartedWhileRunning), c.startedAt, t))
             ) *> ZIO.foreachDiscard(pending)(c =>
               CallTx.save(CallKey(c.entryId, c.callId), CallStatus.Done(CallResult.error(Interrupted.restartedBeforeStart), None, t))
             )
           )
      _ <- ZIO.when(running.nonEmpty || pending.nonEmpty)(ZIO.logWarning(s"启动：${running.size} 个执行中、${pending.size} 个没开始的工具调用标成被打断"))
      runs     <- db.read(Repo.runsIn(List(RunState.Running, RunState.Suspended)))
      mail     <- db.read(Repo.sessionsWithMail)
      approved <- db.read(Repo.callsIn(List(CallState.Approved, CallState.ApprovedAlways)))
      unsettled <- db.read(Repo.sessionsToSettle)
      toKick    = (runs.map(_.sessionId) ++ mail ++ approved.map(_.sessionId) ++ unsettled).distinct
      _ <- ZIO.logInfo(s"启动：续跑 ${toKick.size} 个会话")
      _ <- ZIO.foreachDiscard(toKick)(doorbell.ring)
      _ <- doorbell.next.flatMap(kick).forever.forkIn(scope)
    yield ()

  /** 让会话动起来：没有驱动就起一个；有就让它结束前再看一遍。 */
  private def kick(id: SessionId): UIO[Unit] = slots.kick(id)(s => worker(id, s).forkIn(scope))

  private def worker(id: SessionId, s: Slot): UIO[Unit] =
    val once = ZIO.scoped(drive(id, s)).catchAllCause(c => unexpected(id, c))
    def loop: UIO[Unit] = once *> slots.again(id, s).flatMap(again => loop.when(again).unit)
    loop

  /** 驱动里没料到的错误：记日志，把这次运行标成最终失败（让人看见），不让它静默。 */
  private def unexpected(id: SessionId, c: Cause[AppError]): UIO[Unit] =
    val msg = c.failureOption.map(_.message).getOrElse("内部错误，详情见服务日志")
    ZIO.logErrorCause(s"会话 ${id.value} 的运行出错", c) *> failLastRun(id, msg).catchAllCause(c2 => ZIO.logErrorCause("记录运行失败时出错", c2))

  // ---- 一轮驱动 ----

  /** 驱动一个会话直到没事可做或挂起。 */
  private def drive(id: SessionId, slot: Slot): ZIO[Scope, AppError, Unit] =
    for
      kit     <- requests.prepareKit(id).memoize
      rejects <- Ref.make(Map.empty[String, Int])
      _       <- Round(id, slot, kit, rejects).loop
    yield ()

  /** 一轮驱动的现场：本轮的运行准备（只准备一次）、各工具连续被拒的次数。 */
  private final case class Round(id: SessionId, slot: Slot, kit: ZIO[Scope, AppError, Kit], rejects: Ref[Map[String, Int]]):
    def loop: ZIO[Scope, AppError, Unit] =
      for
        last   <- db.read(Repo.lastRun(id))
        run     = last.collect { case r if r.state.active => ActiveRun(r.id, r.state == RunState.Suspended) }
        open   <- db.read(Repo.openCalls(id)).flatMap(Decode.calls)
        queued <- db.read(Repo.queued(id)).map(_.nonEmpty)
        _ <- Plan.next(run, open, queued) match
               case Next.Rest     => settleIfDone(id)
               case Next.Pause(r) => suspend(id, r)
               case Next.Start    => begin(id, kit) *> loop
               case Next.Request(r) =>
                 (resume(id, r) *> kit.flatMap(k => request(k, r.id, slot))).catchAll(e => failRun(id, r.id, e.message)) *> loop
               case Next.Execute(r, cs) =>
                 (resume(id, r) *> kit.flatMap(k => calls.execute(k, r.id, cs, rejects, slot))).catchAll(e => failRun(id, r.id, e.message)) *> loop
               case Next.Decline(_, cs) => decline(id, cs) *> loop
      yield ()

  // ---- 开运行 ----

  /** 开一次新运行：建运行、写本次运行上下文、取进排队的信。准备失败（如 agent 归档了）就把这次运行记成最终失败。 */
  private def begin(id: SessionId, kit: ZIO[Scope, AppError, Kit]): ZIO[Scope, AppError, Unit] =
    kit.either.flatMap { k =>
      for
        admins <- resources.admins.map(_.map(_.value)).orElseSucceed(Nil)
        t      <- now
        run <- db.write(
                 for
                   run     <- Repo.insertRun(id, t)
                   ctxId   <- Repo.insertEntry(EntryRow(0L, id, Some(run), EntryKind.Context, "", "", t, t))
                   letters <- mailbox.takeMail(id, run, t)
                   pending <- infoTx(id).map(_.pendingReply)
                   names    = letters.collect { case Letter(Source.Delegation(from, agent), _, _, Some(ReplyTo.Delegation(d)), _) => d -> s"${agent.value}，会话 ${from.value}" }.toMap
                   text     = k.fold(
                                e => s"[本次运行没能开始：${e.message}]",
                                kit => Prompt.runContext(Instant.ofEpochMilli(t), zone, Prompt.replyNote(pending, names), ResourcePrompt.notices(kit.res, admins), kit.mcpProblems)
                              )
                   _ <- Repo.updateEntryBody(ctxId, EntryBody.Context(text).encode, "", t)
                   _ <- touched(id, t)
                 yield run
               )
        _ <- k.fold(e => failRun(id, run, e.message), _ => ZIO.unit)
      yield ()
    }

  /** 挂起后接着跑：运行状态改回运行中。 */
  private def resume(id: SessionId, run: ActiveRun): IO[AppError, Unit] =
    now.flatMap(t => db.write(Repo.setRunState(run.id, RunState.Running, None, None) *> touched(id, t))).when(run.suspended).unit

  private def suspend(id: SessionId, run: ActiveRun): IO[AppError, Unit] =
    now.flatMap(t => db.write(Repo.setRunState(run.id, RunState.Suspended, None, None) *> touched(id, t))).unless(run.suspended).unit

  // ---- 请求模型 ----

  private def request(kit: Kit, run: RunId, slot: Slot): IO[AppError, Unit] =
    val id = kit.info.id
    for
      t       <- now
      _       <- db.write(mailbox.takeMail(id, run, t))
      _       <- compaction.maybeCompact(kit, run)
      req     <- requests.buildRequest(kit)
      started <- now
      reply   <- requests.call(kit, Some(run), req, Some(Shown(run, slot)))
      _       <- saveReply(kit, run, reply, started, slot)
    yield ()

  /** 敏感参数打码后再存（值不进会话记录）。没有敏感参数的工具原样保存，保证回传时逐字节不变。 */
  private def redact(kit: Kit, p: ReplyPart): ReplyPart = p match
    case c @ ReplyPart.ToolCall(_, name, args, _) =>
      kit.specs.get(name).filter(_.sensitiveFields.nonEmpty).fold(c) { spec =>
        args.fromJson[Json].fold(_ => c, j => c.copy(args = spec.redact(j).toJson))
      }
    case other => other

  /** 存一条回复及其中的调用与用量；没有调用就是这次运行的结尾。 */
  private def saveReply(kit: Kit, run: RunId, reply: Reply, started: Long, slot: Slot): IO[AppError, Unit] =
    val id    = kit.info.id
    val parts = reply.parts.map(redact(kit, _))
    val calls = parts.collect { case c: ReplyPart.ToolCall => c }.toList
    val raw   = reply.toolCalls.map(c => c.id -> c.args).toMap
    for
      t <- now
      entry <- db.write(
                 for
                   entry <- Repo.insertEntry(
                              EntryRow(0L, id, Some(run), EntryKind.Reply, EntryBody.Reply(parts, kit.model.toString, reply.usage, reply.stopReason, false).encode,
                                ModelView.searchText(parts), started, t)
                            )
                   _ <- Repo.insertCalls(calls.zipWithIndex.map((c, i) =>
                          ToolCallRow(entry, CallId.wrap(c.id), id, i, c.name, c.args, CallStatus.Pending.state, None, None, None, None)
                        ))
                   _ <- requests.recordUsage(kit, reply.usage, t)
                   _ <- (Repo.setRunState(run, RunState.Finished, None, Some(t)) *> Repo.setSettleDue(id, true)).when(calls.isEmpty)
                   _ <- touched(id, t)
                 yield entry
               )
      _ <- slot.secrets.set(calls.collect { case c if raw.get(c.id).exists(_ != c.args) => CallKey(entry, CallId.wrap(c.id)) -> raw(c.id) }.toMap)
      _ <- slot.partial.set(None)
      _ <- live.done(id)
    yield ()

  // ---- 收尾 ----

  /** 驱动没事可做时：待收尾（运行做完了还没收尾）、且没有进行中的异步工作，就是一段工作做完了：收尾。 */
  private def settleIfDone(id: SessionId): IO[AppError, Unit] =
    for
      i     <- info(id)
      run   <- db.read(Repo.lastRun(id))
      waits <- db.read(Repo.waitsOf(id))
      _     <- settle(i).when(i.settleDue && waits.isEmpty && run.exists(_.state == RunState.Finished))
    yield ()

  private def settle(i: SessionInfo): IO[AppError, Unit] =
    for
      t      <- now
      answer <- lastAnswer(i.id)
      _      <- db.write(delegations.settleTx(i, Outcome.Answered(answer), t) *> touched(i.id, t))
    yield ()

  /** 运行最终失败：没结果的调用补上说明，记下错误，交回失败。 */
  private def failRun(id: SessionId, run: RunId, message: String): IO[AppError, Unit] =
    for
      t <- now
      _ <- ZIO.logWarning(s"会话 ${id.value} 的运行 ${run.value} 最终失败：$message")
      i <- info(id)
      _ <- live.done(id)
      _ <- db.write(
             for
               open <- Repo.openCalls(id).flatMap(Decode.calls)
               _    <- ZIO.foreachDiscard(open)(c => CallTx.finish(c, CallResult.error(failedNote(c.status)), t))
               _    <- Repo.setRunState(run, RunState.Failed, Some(message), Some(t))
               _    <- noticeTx(id, Some(run), NoticeKind.Failed, message, t)
               _    <- delegations.settleTx(i, Outcome.Failed(message), t)
               _    <- touched(id, t)
             yield ()
           )
    yield ()

  private def failedNote(s: CallStatus): String = s match
    case CallStatus.Running(_)      => "[运行失败时这个调用还在执行，被打断；它的副作用可能已经发生，先核实。]"
    case CallStatus.Suspended(_, _) => "[运行失败，没等到答复。]"
    case _                          => "[运行失败，这个调用没有执行。]"

  private def failLastRun(id: SessionId, message: String): IO[AppError, Unit] =
    db.read(Repo.lastRun(id)).flatMap {
      case Some(r) if r.state.active => failRun(id, r.id, message)
      case _                         => ZIO.logWarning(s"会话 ${id.value}：$message")
    }

  /** 来了新信，没答复的提问与批准当作没答复。 */
  private def decline(id: SessionId, cs: List[Call]): IO[AppError, Unit] =
    for
      t <- now
      _ <- db.write(ZIO.foreachDiscard(cs) { c =>
             val note = c.status match
               case CallStatus.Suspended(_: Pause.Approval, _) => "[用户没有批准（改发了新消息，见下），没有执行。]"
               case _                                          => "[用户没有回答这个问题，而是发来了新消息（见下）。]"
             CallTx.finish(c, CallResult.error(note), t)
           } *> touched(id, t))
    yield ()

private[sessions] object Driver:
  val layer: ZLayer[Kernel & Slots & Doorbell & Mailbox & Delegations & Requests & Compaction & CallRunner & Resources, Nothing, Driver] =
    ZLayer.scoped(
      for
        k     <- ZIO.service[Kernel]
        sl    <- ZIO.service[Slots]
        d     <- ZIO.service[Doorbell]
        m     <- ZIO.service[Mailbox]
        dl    <- ZIO.service[Delegations]
        rq    <- ZIO.service[Requests]
        cp    <- ZIO.service[Compaction]
        cr    <- ZIO.service[CallRunner]
        res   <- ZIO.service[Resources]
        scope <- ZIO.scope
      yield Driver(k, sl, d, m, dl, rq, cp, cr, res, scope)
    )
