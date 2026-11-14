package muliao.sessions

import muliao.contract.*
import muliao.models.llm.{ReplyPart, Usage}
import muliao.platform.AppError
import muliao.sessions.store.*
import zio.*
import zio.json.*

/** 用户与别的领域对会话的控制：停止、重试、答复挂起的调用、手动压缩、删除，以及 agent 删除时的清理。 */
private[sessions] trait ControlPart:
  this: Engine =>
  import deps.*

  // ---- 停止 ----

  /** 停止一个会话：保存已生成的部分，没结果的调用补上说明，丢弃排队的信，放弃等待中的工作，逐层停下它开出的 thread。
    * `byUser`：用户在界面上点的（会话空闲时报错）；内部逐层停止时空闲的直接跳过。
    */
  def stop(id: SessionId, byUser: Boolean): IO[AppError, Unit] =
    state(id).flatMap {
      case WorkState.Idle if byUser => ZIO.fail(AppError.conflict("会话空闲，没有可停的"))
      case WorkState.Idle           => ZIO.unit
      case _ =>
        for
          slot <- slots.modifyZIO { m =>
                    m.get(id) match
                      case Some(s) => s.stopping.set(true).as((s, m))
                      case None    => Slot.make.tap(_.stopping.set(true)).map(s => (s, m.updated(id, s)))
                  }
          _ <- slot.fiber.get.flatMap(ZIO.foreachDiscard(_)(_.interrupt))
          _ <- stopped(id, slot).ensuring(slots.update(_ - id))
          _ <- kick(id).whenZIO(db.read(Repo.queued(id)).map(_.nonEmpty))
        yield ()
    }

  private def stopped(id: SessionId, slot: Slot): IO[AppError, Unit] =
    for
      partial <- slot.partial.get
      t       <- now
      i       <- info(id)
      (waits, pending, asked) <- db.write(
                                   for
                                     _ <- ZIO.foreachDiscard(partial.filter(_.builder.parts.exists(visible)))(p => savePartial(id, p, t))
                                     open <- Repo.openCalls(id).flatMap(Decode.calls)
                                     _ <- ZIO.foreachDiscard(open)(c => setResult(c, CallResult.error(stoppedNote(c.state)), t))
                                     run <- Repo.lastRun(id)
                                     _ <- ZIO.foreachDiscard(run.filter(r => r.state == RunState.Running.label || r.state == RunState.Suspended.label))(r =>
                                            Repo.setRunState(r.id, RunState.Stopped.label, None, Some(t))
                                          )
                                     _     <- Repo.dropQueued(id)
                                     waits <- Repo.waitsOf(id)
                                     _     <- ZIO.foreachDiscard(waits)(w => Repo.deleteWait(w.id))
                                     asked <- Repo.openDelegationsFrom(id)
                                     _     <- ZIO.foreachDiscard(asked)(d => Repo.finishDelegation(d.id, CallResult.error("[发起方被停止了]").toJson))
                                     _ <- Repo.insertEntry(EntryRow(0L, id, run.map(_.id), "notice", EntryBody.Notice(NoticeKind.Stopped, "已停止").encode, "", t, t))
                                     p <- takePendingReply(id)
                                     _ <- Repo.touch(id, t) *> changed
                                   yield (waits, p, asked)
                                 )
      _ <- waitCache.update(_ -- waits.map(_.id))
      _ <- slot.partial.set(None)
      _ <- live.done(id)
      // 放弃等待中的工作：后台任务结束、委派的 thread 逐层停下
      _ <- ZIO.foreachDiscard(waits)(w => hooks.workKind(w.kind).flatMap(ZIO.foreachDiscard(_)(_.cancel(w.ref))))
      threads <- db.read(Repo.threadsOf(id))
      _ <- ZIO.foreachDiscard(threads)(th => stop(th.id, byUser = false).catchAll(e => ZIO.logWarning(s"停 thread ${th.id.value} 失败：${e.message}")))
      _ <- ZIO.logInfo(s"会话 ${i.id.value} 已停止（放弃 ${waits.size} 项等待、${asked.size} 个在等的委派）")
      text <- ZIO.succeed(partial.map(p => ModelView.answerText(p.builder.parts)).filter(_.nonEmpty)).someOrElseZIO(lastAnswer(id))
      _ <- handBack(i, pending, Outcome.Stopped(text))
    yield ()

  private def visible(p: ReplyPart): Boolean = p match
    case ReplyPart.Text(t, _)     => t.nonEmpty
    case ReplyPart.Thinking(t, _) => t.nonEmpty
    case _: ReplyPart.ToolCall    => false

  /** 被打断的回复：只存文字与思考（半截的工具调用不存，也就不会发回模型）。 */
  private def savePartial(id: SessionId, p: Partial, t: Long) =
    val parts = p.builder.parts.filter(visible)
    Repo.insertEntry(
      EntryRow(0L, id, Some(p.run), "reply", EntryBody.Reply(parts, p.model, Usage.zero, None, interrupted = true).encode, ModelView.searchText(parts), p.startedAt, t)
    )

  private def stoppedNote(s: CallState): String = s match
    case CallState.Running   => Interrupted.stoppedWhileRunning
    case CallState.Suspended => Interrupted.stoppedWhileWaiting
    case _                   => Interrupted.stoppedBeforeStart

  // ---- 重试 ----

  /** 最终失败的运行从失败处接着跑。 */
  def retry(id: SessionId): IO[AppError, Unit] =
    for
      st  <- state(id)
      _   <- ZIO.when(st != WorkState.Idle)(ZIO.fail(AppError.conflict("会话不空闲，不用重试")))
      run <- db.read(Repo.lastRun(id)).flatMap {
               case Some(r) if r.state == RunState.Failed.label => ZIO.succeed(r)
               case _                                           => ZIO.fail(AppError.conflict("最近一次运行没有失败，没什么可重试的"))
             }
      t <- now
      _ <- db.write(
             Repo.setRunState(run.id, RunState.Running.label, None, None) *>
               Repo.insertEntry(EntryRow(0L, id, Some(run.id), "notice", EntryBody.Notice(NoticeKind.Info, "重试").encode, "", t, t)) *>
               addPendingReply(id, List(ReplyTo.User)) *> Repo.touch(id, t) *> changed
           )
      _ <- kick(id)
    yield ()

  // ---- 答复挂起的调用 ----

  def answer(id: SessionId, entry: Long, callId: String, a: Answer): IO[AppError, Unit] =
    for
      t <- now
      _ <- db.write(
             for
               row <- Repo.call(entry, callId).someOrFail(AppError.notFound("没有这个调用"))
               c   <- Decode.call(row)
               _   <- ZIO.when(row.sessionId != id)(ZIO.fail(AppError.notFound("没有这个调用")))
               _   <- ZIO.when(c.state != CallState.Suspended)(ZIO.fail(AppError.conflict("这个调用已经不在等答复了")))
               _ <- (c.pause, a) match
                      case (Some(Pause.AskUser(qs)), Answer.Answers(as)) =>
                        if as.size != qs.size then ZIO.fail(AppError.invalid(s"要回答 ${qs.size} 个问题，收到 ${as.size} 个"))
                        else setResult(c, CallResult.ok(answersText(qs, as)), t)
                      case (Some(_: Pause.Approval), Answer.Decision(true, always)) =>
                        val next = if always then CallState.ApprovedAlways else CallState.Approved
                        Repo.setCall(c.entryId, c.callId, next.label, c.pause.map(_.toJson), None, c.startedAt, None)
                      case (Some(_: Pause.Approval), Answer.Decision(false, _)) =>
                        setResult(c, CallResult.error("[用户没有批准，没有执行。]"), t)
                      case _ => ZIO.fail(AppError.invalid("答复的种类和这个调用在等的不一致"))
               _ <- Repo.touch(id, t) *> changed
             yield ()
           )
      _ <- kick(id)
    yield ()

  private def answersText(qs: List[Question], as: List[String]): String =
    ("用户的回答：" :: qs.zip(as).map((q, a) => s"- ${q.header}（${q.question}）：$a")).mkString("\n")

  // ---- 手动压缩 ----

  def compactNow(id: SessionId): IO[AppError, Boolean] =
    exclusive(id)(ZIO.scoped(prepareKit(id).flatMap(k => compact(k, None))))

  // ---- 删除 ----

  /** 删除会话：忙时拒绝；它开出的 thread 保留；交给它的未完成委派以「被删除」回给发起方。 */
  def delete(id: SessionId): IO[AppError, Unit] =
    for
      st <- state(id)
      _  <- ZIO.when(st != WorkState.Idle)(ZIO.fail(AppError.conflict(s"会话正在${if st == WorkState.Running then "运行" else "等待"}，先停止再删除")))
      _  <- remove(id)
    yield ()

  /** 删掉（不查忙不忙）。 */
  def remove(id: SessionId): IO[AppError, Unit] =
    for
      i    <- info(id)
      open <- db.read(Repo.openDelegationsTo(id))
      _    <- ZIO.foreachDiscard(open)(d => finishDelegation(d.id, i, Outcome.Failed("这个 thread 被删除了")))
      _    <- db.write(Repo.orphanThreads(id) *> Repo.deleteSession(id) *> changed)
      _    <- waitCache.update(_.filterNot(_._2.sessionId == id))
      _    <- live.done(id)
      _    <- hooks.emit(SessionEvent.Deleted(id, i.agent, i.origin))
    yield ()

  // ---- agent 的忙碌与清理 ----

  def agentBusy(agent: AgentId): IO[AppError, Option[String]] =
    for
      ss <- db.read(Repo.sessionsOfAgent(agent))
      st <- states(ss.map(_.id))
    yield
      val busy = ss.filter(s => st.get(s.id).exists(_ != WorkState.Idle))
      Option.when(busy.nonEmpty)(s"有 ${busy.size} 个会话在运行或等待（${busy.take(3).map(_.title).mkString("、")}）")

  /** 删 agent 时删掉它的全部会话（可重复执行）。 */
  def agentCleanup(agent: AgentId): IO[AppError, Unit] =
    db.read(Repo.sessionsOfAgent(agent)).flatMap(ZIO.foreachDiscard(_)(s => stop(s.id, byUser = false) *> remove(s.id)))
