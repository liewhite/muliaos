package muliao.sessions

import muliao.contract.*
import muliao.models.llm.{ReplyPart, ShownPart, Usage}
import muliao.platform.AppError
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

  /** 停止一个会话：保存已生成的部分，没结果的调用补上说明，丢弃排队的信，放弃等待中的工作，逐层停下它开出的 thread。
    * `byUser`：用户在界面上点的（会话空闲时报错）；内部逐层停止时空闲的直接跳过。
    */
  def stop(id: SessionId, byUser: Boolean): IO[AppError, Unit] =
    states.state(id).flatMap {
      case WorkState.Idle if byUser => ZIO.fail(AppError.conflict("会话空闲，没有可停的"))
      case WorkState.Idle           => ZIO.unit
      case _ =>
        for
          slot <- slots.halt(id)
          _    <- stopped(id, slot).ensuring(slots.release(id))
          _    <- ringIfMail(id)
        yield ()
    }

  /** 停止的收尾：一个事务里记下停止的现场，再放弃等待中的工作、停下 thread、交回「被停止」。 */
  private final case class Halted(waits: List[Wait], pending: List[ReplyTo], asked: Int)

  private def stopped(id: SessionId, slot: Slot): IO[AppError, Unit] =
    for
      partial <- slot.partial.get
      t       <- now
      i       <- info(id)
      h <- db.write(
             for
               _     <- ZIO.foreachDiscard(partial.filter(_.builder.parts.exists(visible)))(p => savePartial(id, p, t))
               open  <- Repo.openCalls(id).flatMap(Decode.calls)
               _     <- ZIO.foreachDiscard(open)(c => CallTx.finish(c, CallResult.error(stoppedNote(c.status)), t))
               run   <- Repo.lastRun(id)
               _     <- ZIO.foreachDiscard(run.filter(_.state.active))(r => Repo.setRunState(r.id, RunState.Stopped, None, Some(t)))
               _     <- Repo.dropQueued(id)
               ws    <- waits.dropTx(id)
               asked <- delegations.abandonTx(id)
               _     <- noticeTx(id, run.map(_.id), NoticeKind.Stopped, "已停止", t)
               p     <- mailbox.takePendingReply(id)
               _     <- touched(id, t)
             yield Halted(ws, p, asked.size)
           )
      _ <- slot.partial.set(None)
      _ <- live.done(id)
      // 放弃等待中的工作：后台任务结束；委派的 thread 由下面逐层停下
      _ <- ZIO.foreachDiscard(h.waits.map(_.ref)) {
             case WaitRef.Job(job)                       => ports.jobs.flatMap(_.cancel(job))
             case WaitRef.Wake(_) | WaitRef.Delegation(_) => ZIO.unit
           }
      threads <- db.read(Repo.threadsOf(id))
      _ <- ZIO.foreachDiscard(threads)(th => stop(th.id, byUser = false).catchAll(e => ZIO.logWarning(s"停 thread ${th.id.value} 失败：${e.message}")))
      _ <- ZIO.logInfo(s"会话 ${i.id.value} 已停止（放弃 ${h.waits.size} 项等待、${h.asked} 个在等的委派）")
      text <- ZIO.succeed(partial.map(p => ModelView.answerText(p.builder.parts)).filter(_.nonEmpty)).someOrElseZIO(lastAnswer(id))
      _    <- delegations.handBack(i, h.pending, Outcome.Stopped(text))
    yield ()

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
      _ <- delegations.threadGone(i)
      // touch 对删掉的行无效，这里只借它发通知（列表去掉它，开着的会话页重拉得到「不存在」）
      t <- now
      _ <- db.write(Repo.orphanThreads(id) *> touched(id, t) *> Repo.deleteSession(id))
      _ <- live.done(id)
      _ <- ports.emit(SessionEvent.Deleted(id, i.agent, i.origin))
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
