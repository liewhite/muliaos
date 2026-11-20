package muliao.sessions

import muliao.contract.*
import muliao.models.llm.{ReplyPart, ShownPart, Usage}
import muliao.platform.AppError
import muliao.platform.db.Tx
import muliao.sessions.store.*
import zio.*

/** 用户与别的领域对会话的控制：停止、重试、答复挂起的调用、手动压缩、删除，以及 agent 删除时的忙碌判断与清理。 */
private[sessions] final case class Control(
    kernel: Kernel,
    slots: Slots,
    states: States,
    doorbell: Doorbell,
    mailbox: Mailbox,
    waits: Waits,
    delegations: Delegations,
    requests: Requests,
    compaction: Compaction
):
  import kernel.*

  /** 有排队的信就按门铃（停止、独占结束后）。 */
  private def ringIfMail(id: SessionId): IO[AppError, Unit] =
    doorbell.ring(id).whenZIO(db.read(Repo.queued(id)).map(_.nonEmpty)).unit

  // ---- 停止 ----

  /** 停止一个会话连同它开出的、没空闲的 thread（逐层）：先挡住它们的驱动，再在一个事务里记下全部停止的现场（保存已生成的部分，
    * 没结果的调用补上说明，丢弃排队的信，放弃等待中的工作，交回「被停止」），提交后结束等待中的后台任务。
    * 一个事务记下整棵，服务在中间重启也不会留下「上面停了、thread 还在跑」。
    * `byUser`：用户在界面上点的（会话空闲时报错）；空闲时别的调用方什么也不停。
    * `also`：调用方要和停止一起提交的改动（看板叫停 issue：改状态、投叫停留言），写在停止之后，所以投给这个会话的信不会被丢弃；会话空闲时单独提交。
    */
  def stop(id: SessionId, byUser: Boolean, also: Tx[Unit] = ZIO.unit): IO[AppError, Unit] =
    states.state(id).flatMap {
      case WorkState.Idle if byUser => ZIO.fail(AppError.conflict("会话空闲，没有可停的"))
      case WorkState.Idle           => db.write(also)
      case _ =>
        for
          held <- Ref.make(List.empty[(SessionId, Slot)])
          _    <- (haltTree(id, held) *> held.get.flatMap(h => stopped(h.reverse, also))).ensuring(held.get.flatMap(ZIO.foreachDiscard(_)((s, _) => slots.release(s))))
          _    <- held.get.flatMap(ZIO.foreachDiscard(_)((s, _) => ringIfMail(s)))
        yield ()
    }

  /** 挡住会话的驱动，再逐层挡住它开出的、没空闲的 thread；挡住的按先上后下记进 `held`（出错时调用方据此放开）。 */
  private def haltTree(id: SessionId, held: Ref[List[(SessionId, Slot)]]): IO[AppError, Unit] =
    for
      slot <- slots.halt(id)
      _    <- held.update((id, slot) :: _)
      kids <- db.read(Repo.threadsOf(id)).map(_.map(_.id))
      st   <- states.states(kids)
      _    <- ZIO.foreachDiscard(kids.filter(k => st.get(k).exists(_ != WorkState.Idle)))(haltTree(_, held))
    yield ()

  /** 一个要停的会话：挡住时的槽，停止前读好的基本信息和交回的文字。 */
  private final case class Halting(info: SessionInfo, slot: Slot, partial: Option[Partial], text: String)

  private def stopped(tree: List[(SessionId, Slot)], also: Tx[Unit]): IO[AppError, Unit] =
    for
      hs <- ZIO.foreach(tree) { (id, slot) =>
              for
                partial <- slot.partial.get
                i       <- info(id)
                text    <- ZIO.succeed(partial.map(p => ModelView.answerText(p.builder.parts)).filter(_.nonEmpty)).someOrElseZIO(lastAnswer(id))
              yield Halting(i, slot, partial, text)
            }
      t <- now
      // 上层先停：它放弃的委派先记成结束，下层 thread 的「被停止」就不再交回给它
      dropped <- db.write(ZIO.foreach(hs)(stoppedTx(_, t)) <* also)
      _ <- ZIO.foreachDiscard(hs)(h => h.slot.partial.set(None) *> live.done(h.info.id))
      // 放弃等待中的工作：后台任务结束；委派的 thread 已一并停下
      _ <- ZIO.foreachDiscard(dropped.flatten.map(_.ref)) {
             case WaitRef.Job(job)                       => ports.jobs.flatMap(_.cancel(job))
             case WaitRef.Wake(_) | WaitRef.Delegation(_) => ZIO.unit
           }
      _ <- ZIO.logInfo(s"已停止 ${hs.map(_.info.id.value).mkString("、")}（放弃 ${dropped.flatten.size} 项等待）")
    yield ()

  /** 在停止的事务里记下一个会话的停止现场，返回放弃的等待。 */
  private def stoppedTx(h: Halting, t: Long): Tx[List[Wait]] =
    val id = h.info.id
    for
      _    <- ZIO.foreachDiscard(h.partial.filter(_.builder.parts.exists(visible)))(p => savePartial(id, p, t))
      open <- Repo.openCalls(id).flatMap(Decode.calls)
      _    <- ZIO.foreachDiscard(open)(c => CallTx.finish(c, CallResult.error(stoppedNote(c.status)), t))
      run  <- Repo.lastRun(id)
      _    <- ZIO.foreachDiscard(run.filter(_.state.active))(r => Repo.setRunState(r.id, RunState.Stopped, None, Some(t)))
      _    <- Repo.dropQueued(id)
      ws   <- waits.dropTx(id)
      _    <- delegations.abandonTx(id)
      _    <- noticeTx(id, run.map(_.id), NoticeKind.Stopped, "已停止", t)
      _    <- delegations.settleTx(h.info, Outcome.Stopped(h.text), t)
      _    <- touched(id, t)
    yield ws

  private def visible(p: ShownPart): Boolean = p match
    case ReplyPart.Text(t, _)  => t.nonEmpty
    case _: ReplyPart.ToolCall => false

  /** 被打断的回复：只存文字（半截的工具调用不存，也就不会发回模型）。 */
  private def savePartial(id: SessionId, p: Partial, t: Long) =
    val parts = p.builder.parts.filter(visible)
    Repo.insertEntry(
      EntryRow(0L, id, Some(p.run), EntryKind.Reply, EntryBody.Reply(parts, p.model, Usage.zero, None, interrupted = true).encode, ModelView.searchText(parts), p.startedAt, t)
    )

  private def stoppedNote(s: CallStatus): String = s match
    case CallStatus.Running(_)      => Interrupted.stoppedWhileRunning
    case CallStatus.Suspended(_, _) => Interrupted.stoppedWhileWaiting
    case _                          => Interrupted.stoppedBeforeStart

  // ---- 重试 ----

  /** 最终失败的运行从失败处接着跑。 */
  def retry(id: SessionId): IO[AppError, Unit] =
    for
      _ <- states.requireIdle(id, "不用重试")
      run <- db.read(Repo.lastRun(id)).flatMap {
               case Some(r) if r.state == RunState.Failed => ZIO.succeed(r)
               case _                                     => ZIO.fail(AppError.conflict("最近一次运行没有失败，没什么可重试的"))
             }
      t <- now
      _ <- db.write(
             Repo.setRunState(run.id, RunState.Running, None, None) *> noticeTx(id, Some(run.id), NoticeKind.Info, "重试", t) *>
               mailbox.addPendingReply(id, List(ReplyTo.User)) *> touched(id, t)
           )
      _ <- doorbell.ring(id)
    yield ()

  // ---- 答复挂起的调用 ----

  def answer(id: SessionId, key: CallKey, a: Answer): IO[AppError, Unit] =
    for
      t <- now
      _ <- db.write(
             for
               row <- Repo.call(key.entry, key.call).someOrFail(AppError.notFound("没有这个调用"))
               c   <- Decode.call(row)
               _   <- ZIO.when(row.sessionId != id)(ZIO.fail(AppError.notFound("没有这个调用")))
               _ <- (c.status, a) match
                      case (CallStatus.Suspended(Pause.AskUser(qs), _), Answer.Answers(as)) =>
                        if as.size != qs.size then ZIO.fail(AppError.invalid(s"要回答 ${qs.size} 个问题，收到 ${as.size} 个"))
                        else CallTx.finish(c, CallResult.ok(answersText(qs, as)), t)
                      case (CallStatus.Suspended(p: Pause.Approval, t0), Answer.Decision(true, always)) =>
                        CallTx.save(c.key, CallStatus.Approved(p, always, t0))
                      case (CallStatus.Suspended(_: Pause.Approval, _), Answer.Decision(false, _)) =>
                        CallTx.finish(c, CallResult.error("[用户没有批准，没有执行。]"), t)
                      case (CallStatus.Suspended(_, _), _) => ZIO.fail(AppError.invalid("答复的种类和这个调用在等的不一致"))
                      case _                               => ZIO.fail(AppError.conflict("这个调用已经不在等答复了"))
               _ <- touched(id, t)
             yield ()
           )
      _ <- doorbell.ring(id)
    yield ()

  private def answersText(qs: List[Question], as: List[String]): String =
    ("用户的回答：" :: qs.zip(as).map((q, a) => s"- ${q.header}（${q.question}）：$a")).mkString("\n")

  // ---- 手动压缩 ----

  /** 独占会话（必须空闲且没有驱动）压缩一次，做完照常看一遍有没有新信。 */
  def compactNow(id: SessionId): IO[AppError, Boolean] =
    for
      _    <- states.requireIdle(id, "等它空闲再做")
      _    <- slots.reserve(id)
      done <- ZIO.scoped(requests.prepareKit(id).flatMap(k => compaction.compact(k, None))).ensuring(slots.release(id) *> ringIfMail(id).ignore)
    yield done

  // ---- 删除 ----

  /** 删除会话：忙时拒绝；它开出的 thread 保留；交给它的未完成委派以「被删除」回给发起方。 */
  def delete(id: SessionId): IO[AppError, Unit] = states.requireIdle(id, "先停止再删除") *> remove(id)

  /** 删掉（不查忙不忙）。 */
  private def remove(id: SessionId): IO[AppError, Unit] =
    for
      i <- info(id)
      // touch 对删掉的行无效，这里只借它发通知（列表去掉它，开着的会话页重拉得到「不存在」）
      t <- now
      _ <- db.write(
             delegations.threadGoneTx(i, t) *> Repo.orphanThreads(id) *> touched(id, t) *> Repo.deleteSession(id) *>
               ports.emit(SessionEvent.Deleted(id, i.agent, i.origin))
           )
      _ <- live.done(id)
    yield ()

  // ---- agent 的忙碌与清理 ----

  def agentBusy(agent: AgentId): IO[AppError, Option[String]] =
    for
      ss <- db.read(Repo.sessionsOfAgent(agent))
      st <- states.states(ss.map(_.id))
    yield
      val busy = ss.filter(s => st.get(s.id).exists(_ != WorkState.Idle))
      Option.when(busy.nonEmpty)(s"有 ${busy.size} 个会话在运行或等待（${busy.take(3).map(_.title).mkString("、")}）")

  /** 删 agent 时删掉它的全部会话（可重复执行）。 */
  def agentCleanup(agent: AgentId): IO[AppError, Unit] =
    db.read(Repo.sessionsOfAgent(agent)).flatMap(ZIO.foreachDiscard(_)(s => stop(s.id, byUser = false) *> remove(s.id)))

private[sessions] object Control:
  val layer: URLayer[Kernel & Slots & States & Doorbell & Mailbox & Waits & Delegations & Requests & Compaction, Control] = ZLayer.derive[Control]
