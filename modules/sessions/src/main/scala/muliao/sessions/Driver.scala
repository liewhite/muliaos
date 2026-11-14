package muliao.sessions

import muliao.contract.*
import muliao.models.llm.*
import muliao.models.mcp.{Launch, McpServerSpec, McpTransport, SecretLookup}
import muliao.platform.{AppError, Retry}
import muliao.platform.db.Tx
import muliao.resources.{ResourcePrompt, RunResources}
import muliao.resources.tree.{McpTransport as AssetTransport}
import muliao.resources.vault.SecretUse
import muliao.sessions.store.*
import muliao.toolkit.{Tool, ToolSpec}
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.{Files as JFiles, Path}
import java.time.Instant

/** 一次运行要的全部东西：运行开始（驱动 fiber 起来）时准备一次。 */
private[sessions] final case class Kit(
    info: SessionInfo,
    res: RunResources,
    model: ModelRef,
    thinking: Thinking,
    system: String,
    tools: Map[String, Tool[RunContext]],
    defs: Chunk[ToolDef],
    specs: Map[String, ToolSpec],
    mcpProblems: List[String],
    workspace: Path
)

/** 运行驱动：按 [[Plan.next]] 一步步做——开运行、请求模型、执行工具、挂起、收尾。每步落库，重启后从库里接着算。 */
private[sessions] trait DriverPart:
  this: Engine =>
  import deps.*

  /** 驱动一个会话直到没事可做或挂起。 */
  def drive(id: SessionId, slot: Slot): ZIO[Scope, AppError, Unit] =
    for
      kit     <- prepareKit(id).memoize
      ran     <- Ref.make(false)
      rejects <- Ref.make(Map.empty[String, Int])
      _       <- loop(id, slot, kit, ran, rejects)
    yield ()

  private def loop(id: SessionId, slot: Slot, kit: ZIO[Scope, AppError, Kit], ran: Ref[Boolean], rejects: Ref[Map[String, Int]]): ZIO[Scope, AppError, Unit] =
    for
      last   <- db.read(Repo.lastRun(id))
      run     = last.collect { case r if r.state == RunState.Running.label || r.state == RunState.Suspended.label => ActiveRun(r.id, r.state == RunState.Suspended.label) }
      open   <- db.read(Repo.openCalls(id)).flatMap(Decode.calls)
      queued <- db.read(Repo.queued(id)).map(_.nonEmpty)
      next    = Plan.next(run, open, queued)
      _ <- next match
             case Next.Rest => settleIfDone(id, ran)
             case Next.Pause(r) => suspend(id, r)
             case Next.Start => ran.set(true) *> start(id, kit) *> loop(id, slot, kit, ran, rejects)
             case Next.Request(r) =>
               ran.set(true) *> (resume(id, r) *> kit.flatMap(k => request(k, r.id, slot))).catchAll(e => failRun(id, r.id, e.message)) *>
                 loop(id, slot, kit, ran, rejects)
             case Next.Execute(r, cs) =>
               ran.set(true) *> (resume(id, r) *> kit.flatMap(k => execute(k, r.id, cs, rejects, slot))).catchAll(e => failRun(id, r.id, e.message)) *>
                 loop(id, slot, kit, ran, rejects)
             case Next.Decline(_, cs) => decline(id, cs) *> loop(id, slot, kit, ran, rejects)
    yield ()

  // ---- 准备 ----

  def prepareKit(id: SessionId): ZIO[Scope, AppError, Kit] =
    for
      i     <- info(id)
      r     <- resources.forRun(i.agent)
      model <- ZIO.fromEither(ModelRef.parse(r.profile.model)).mapError(AppError.invalid)
      _     <- ZIO.foreachDiscard(models.problem(model.toString))(p => ZIO.fail(AppError.invalid(s"agent 的模型 $model 用不了：$p")))
      ws     = paths.workspace(i.agent)
      _     <- ZIO.attemptBlocking(JFiles.createDirectories(ws)).mapError(e => AppError.internal(s"建不了工作目录 $ws：${e.getMessage}", e))
      builtin <- hooks.tools.map(_.filter(t => r.tools(t.spec.name)))
      conn    <- connectMcp(i, r, ws)
      names    = builtin.map(_.spec.name).toSet
      mcpTools = conn._1.filterNot(t => names(t.spec.name)).sortBy(_.spec.name)
      all      = builtin ++ mcpTools
    yield Kit(
      info = i,
      res = r,
      model = model,
      thinking = Thinking.parse(r.profile.thinking.label).getOrElse(Thinking.Default),
      system = Prompt.system(r, i.origin, ws),
      tools = all.map(t => t.spec.name -> t).toMap,
      defs = Chunk.fromIterable(all.map(t => ToolDef(t.spec.name, t.spec.description, t.spec.inputSchema))),
      specs = all.map(t => t.spec.name -> t.spec).toMap,
      mcpProblems = conn._2,
      workspace = ws
    )

  /** 连上它能用、启用了的 MCP server（连接随驱动 fiber 结束而断开）。 */
  private def connectMcp(i: SessionInfo, r: RunResources, ws: Path): ZIO[Scope, AppError, (List[Tool[RunContext]], List[String])] =
    val specs = Chunk.fromIterable(r.mcps.filter(_.config.enabled).map { m =>
      val t = m.config.transport match
        case AssetTransport.Stdio(c, args, env) => McpTransport.Stdio(c, Chunk.fromIterable(args), env)
        case AssetTransport.Http(url, headers)  => McpTransport.Http(url, headers)
      McpServerSpec(m.name, t)
    })
    if specs.isEmpty then ZIO.succeed((Nil, Nil))
    else
      val lookup = new SecretLookup:
        def reveal(key: String): IO[AppError, String] =
          vault.reveal(i.agent, List(key), SecretUse.Mcp).flatMap(m => ZIO.fromOption(m.get(key)).orElseFail(AppError.invalid(s"取不到 secret $key")))
      resources.writableScope(i.agent).either.flatMap {
        case Left(e) => ZIO.succeed((Nil, List(s"MCP 这次没有连：算写沙箱失败（${e.message}）")))
        case Right(scope) =>
          mcp.connectAll(specs, lookup, Launch(ws, scope.sandbox)).map(c => (c.tools.toList, c.problems.toList))
      }

  // ---- 开运行 ----

  /** 开一次新运行：建运行、写本次运行上下文、取进排队的信。准备失败（如 agent 归档了）就把这次运行记成最终失败。 */
  private def start(id: SessionId, kit: ZIO[Scope, AppError, Kit]): ZIO[Scope, AppError, Unit] =
    kit.either.flatMap { k =>
      for
        admins <- resources.admins.map(_.map(_.value)).orElseSucceed(Nil)
        t      <- now
        run <- db.write(
                 for
                   run     <- Repo.insertRun(id, t)
                   ctxId   <- Repo.insertEntry(EntryRow(0L, id, Some(run), "context", "", "", t, t))
                   letters <- takeMail(id, run, t)
                   pending <- infoTx(id).map(_.pendingReply)
                   names    = letters.collect { case Letter(Source.Delegation(from, agent), _, _, Some(ReplyTo.Delegation(d)), _) => d -> s"$agent，会话 $from" }.toMap
                   text     = k.fold(
                                e => s"[本次运行没能开始：${e.message}]",
                                kit => Prompt.runContext(Instant.ofEpochMilli(t), zone, Prompt.replyNote(pending, names), ResourcePrompt.notices(kit.res, admins), kit.mcpProblems)
                              )
                   _ <- Repo.updateEntryBody(ctxId, EntryBody.Context(text).encode, "", t)
                   _ <- Repo.touch(id, t) *> changed
                 yield run
               )
        _ <- live.publish(id, LiveEvent.Changed)
        _ <- k.fold(e => failRun(id, run, e.message), _ => ZIO.unit)
      yield ()
    }

  /** 挂起后接着跑：运行状态改回运行中。 */
  private def resume(id: SessionId, run: ActiveRun): IO[AppError, Unit] =
    (db.write(Repo.setRunState(run.id, RunState.Running.label, None, None) *> changed) *> live.publish(id, LiveEvent.Changed)).when(run.suspended).unit

  private def suspend(id: SessionId, run: ActiveRun): IO[AppError, Unit] =
    (db.write(Repo.setRunState(run.id, RunState.Suspended.label, None, None) *> changed) *> live.publish(id, LiveEvent.Changed)).unless(run.suspended).unit

  // ---- 请求模型 ----

  private def request(kit: Kit, run: RunId, slot: Slot): IO[AppError, Unit] =
    val id = kit.info.id
    for
      t       <- now
      _       <- db.write(takeMail(id, run, t))
      _       <- maybeCompact(kit, run)
      req     <- buildRequest(kit)
      started <- now
      reply   <- callModel(kit, Some(run), req, Some(slot))
      _       <- saveReply(kit, run, reply, started, slot)
    yield ()

  /** 组装这一次请求：固定说明 + 工具 + 最近压缩点之后的时间线。 */
  def buildRequest(kit: Kit): IO[AppError, ModelRequest] =
    window(kit.info.id).map((summary, entries, calls, att) =>
      ModelRequest(kit.system, kit.defs, ModelView.messages(summary, entries, calls, att), kit.thinking, cacheKey = Some(kit.info.id.value))
    )

  /** 最近压缩点之后的时间线、其中的调用与用到的附件。 */
  def window(id: SessionId): IO[AppError, (Option[String], List[Entry], Map[Long, List[Call]], ModelView.Attachments)] =
    for
      last    <- db.read(Repo.lastEntry(id, "compaction")).flatMap(ZIO.foreach(_)(Decode.entry))
      from     = last.collect { case Entry(_, _, EntryBody.Compaction(_, f), _, _) => f }.getOrElse(0L)
      rows    <- db.read(Repo.entriesFrom(id, from))
      entries <- Decode.entries(rows)
      all      = last.toList ++ entries.filter(e => last.forall(_.id != e.id))
      (summary, win) = ModelView.window(all.sortBy(_.id))
      calls   <- db.read(Repo.callsOfSession(id)).flatMap(Decode.calls).map(_.groupBy(_.entryId))
      att     <- attachments(win, calls)
    yield (summary, win, calls, att)

  private def attachments(entries: List[Entry], calls: Map[Long, List[Call]]): IO[AppError, ModelView.Attachments] =
    val fileIds = entries.flatMap {
      case Entry(_, _, EntryBody.Mail(l), _, _) => l.files.toList
      case _                                    => Nil
    }.distinct
    val windowIds = entries.map(_.id).toSet
    val resultShas = calls.filter((e, _) => windowIds(e)).values.flatten.flatMap(_.result.toList.flatMap(_.content.collect { case StoredPart.Image(_, s) => s }))
    for
      uploads <- db.read(Repo.uploadsOf(fileIds.map(FileId.wrap)))
      shas     = (uploads.map(_.sha256) ++ resultShas).distinct
      bytes   <- ZIO.foreach(shas)(s => files.read(s).option.map(s -> _))
    yield ModelView.Attachments(uploads.map(u => u.id.value -> u).toMap, bytes.collect { case (s, Some(b)) => s -> b }.toMap, files.path)

  /** 请求一次模型，暂时性失败退避重试（约 30 分钟放弃）。`slot` 给出时实时展示并记下正在生成的部分。 */
  def callModel(kit: Kit, run: Option[RunId], req: ModelRequest, slot: Option[Slot]): IO[AppError, Reply] =
    val id = kit.info.id
    def show(e: UIO[Unit]): UIO[Unit] = e.when(slot.isDefined).unit
    val once =
      for
        t <- now
        _ <- ZIO.foreachDiscard(slot zip run)((s, r) => s.partial.set(Some(Partial(r, kit.model.toString, t, ReplyBuilder.empty))))
        _ <- show(live.publish(id, LiveEvent.Begin))
        fin <- models
                 .request(kit.model, req)
                 .runFoldZIO((ReplyBuilder.empty, Option.empty[Reply])) { case ((b, done), ev) =>
                   for
                     nb <- ZIO.fromEither(b(ev)).mapError(e => AppError.External(s"模型回复的流对不上：$e"))
                     _  <- ZIO.foreachDiscard(slot)(_.partial.update(_.map(_.copy(builder = nb))))
                     _  <- show(live.track(id, nb.parts) *> liveOf(ev).fold(ZIO.unit)(live.publish(id, _)))
                   yield (nb, ev match
                     case ReplyEvent.Finished(r) => Some(r)
                     case _                      => done
                   )
                 }
        reply <- ZIO.fromOption(fin._2).orElseFail(AppError.Unavailable("模型的回复没有正常结束"))
      yield reply
    for
      retried <- Ref.make(false)
      reply <- once
                 .tapError(e =>
                   ZIO.when(e.isTransient)(
                     retried.set(true) *> ZIO.logWarning(s"会话 ${id.value} 请求模型暂时失败，稍后重试：${e.message}") *>
                       ZIO.foreachDiscard(run)(r => db.write(Repo.setRetry(r, None, Some(e.message)) *> changed)).ignore *> live.publish(id, LiveEvent.Changed)
                   )
                 )
                 .retry(Retry.schedule(Retry.transient))
      _ <- ZIO.foreachDiscard(run)(r => db.write(Repo.setRetry(r, None, None) *> changed)).whenZIO(retried.get)
    yield reply

  private def liveOf(e: ReplyEvent): Option[LiveEvent] = e match
    case ReplyEvent.Started(i, p)  => Some(LiveEvent.Part(i, p))
    case ReplyEvent.Replaced(i, p) => Some(LiveEvent.Part(i, p))
    case ReplyEvent.Grew(i, g, t)  => Some(LiveEvent.Grew(i, g, t))
    case ReplyEvent.Finished(_)    => None

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
                          EntryRow(0L, id, Some(run), "reply", EntryBody.Reply(parts, kit.model.toString, reply.usage, reply.stopReason, false).encode,
                            ModelView.searchText(parts), started, t)
                        )
               _ <- Repo.insertCalls(calls.zipWithIndex.map((c, i) =>
                      ToolCallRow(entry, c.id, id, i, c.name, c.args, CallState.Pending.label, None, None, None, None)
                    ))
               _ <- recordUsage(kit, reply.usage, t)
               _ <- Repo.setRunState(run, RunState.Finished.label, None, Some(t)).when(calls.isEmpty)
               _ <- Repo.touch(id, t) *> changed
             yield entry
           )
      _ <- slot.secrets.set(calls.collect { case c if raw.get(c.id).exists(_ != c.args) => (entry, c.id) -> raw(c.id) }.toMap)
      _ <- slot.partial.set(None)
      _ <- live.done(id)
    yield ()

  def recordUsage(kit: Kit, u: Usage, t: Long): Tx[Unit] =
    Repo.insertUsage(UsageRow(0L, t, kit.info.agent, kit.info.id, kit.model.toString, u.input, u.output, u.cacheRead, u.cacheWrite))

  // ---- 收尾 ----

  /** 驱动没事可做时：这次跑过运行、或有等着交回的回答，且没有进行中的异步工作，就是一段工作做完了：交回回答。 */
  private def settleIfDone(id: SessionId, ran: Ref[Boolean]): IO[AppError, Unit] =
    for
      r     <- ran.get
      i     <- info(id)
      run   <- db.read(Repo.lastRun(id))
      waits <- db.read(Repo.waitsOf(id))
      _ <- settle(i).when((r || i.pendingReply.nonEmpty) && waits.isEmpty && run.exists(_.state == RunState.Finished.label))
    yield ()

  private def settle(i: SessionInfo): IO[AppError, Unit] =
    for
      pending <- db.write(takePendingReply(i.id) <* changed)
      answer  <- lastAnswer(i.id)
      _       <- handBack(i, pending, Outcome.Answered(answer))
    yield ()

  /** 最后一条回复的正文。 */
  def lastAnswer(id: SessionId): IO[AppError, String] =
    db.read(Repo.lastEntry(id, "reply")).flatMap(ZIO.foreach(_)(Decode.entry)).map {
      case Some(Entry(_, _, EntryBody.Reply(parts, _, _, _, _), _, _)) => ModelView.answerText(parts)
      case _                                                           => ""
    }

  /** 把结局交给等着的各方：用户（发事件，通知领域推送）、委派方（交回）。 */
  def handBack(i: SessionInfo, pending: List[ReplyTo], outcome: Outcome): IO[AppError, Unit] =
    for
      _ <- ZIO.foreachDiscard(pending.collect { case ReplyTo.Delegation(d) => d })(d => finishDelegation(d, i, outcome))
      _ <- hooks.emit(SessionEvent.Settled(i.id, i.agent, i.origin, outcome, pending.contains(ReplyTo.User)))
    yield ()

  /** 运行最终失败：没结果的调用补上说明，记下错误，交回失败。 */
  def failRun(id: SessionId, run: RunId, message: String): IO[AppError, Unit] =
    for
      t <- now
      _ <- ZIO.logWarning(s"会话 ${id.value} 的运行 ${run.value} 最终失败：$message")
      pending <- db.write(
                   for
                     open <- Repo.openCalls(id).flatMap(Decode.calls)
                     _ <- ZIO.foreachDiscard(open)(c => setResult(c, CallResult.error(failedNote(c.state)), t))
                     _ <- Repo.setRunState(run, RunState.Failed.label, Some(message), Some(t))
                     _ <- Repo.insertEntry(EntryRow(0L, id, Some(run), "notice", EntryBody.Notice(NoticeKind.Failed, message).encode, "", t, t))
                     p <- takePendingReply(id)
                     _ <- Repo.touch(id, t) *> changed
                   yield p
                 )
      _ <- live.done(id)
      i <- info(id)
      _ <- handBack(i, pending, Outcome.Failed(message))
    yield ()

  private def failedNote(s: CallState): String = s match
    case CallState.Running   => "[运行失败时这个调用还在执行，被打断；它的副作用可能已经发生，先核实。]"
    case CallState.Suspended => "[运行失败，没等到答复。]"
    case _                   => "[运行失败，这个调用没有执行。]"

  def failLastRun(id: SessionId, message: String): IO[AppError, Unit] =
    db.read(Repo.lastRun(id)).flatMap {
      case Some(r) if r.state == RunState.Running.label || r.state == RunState.Suspended.label => failRun(id, r.id, message)
      case _                                                                                   => ZIO.logWarning(s"会话 ${id.value}：$message")
    }

  /** 把一个调用标成做完并写结果。 */
  def setResult(c: Call, r: CallResult, t: Long): Tx[Unit] =
    Repo.setCall(c.entryId, c.callId, CallState.Done.label, c.pause.map(_.toJson), Some(r.toJson), c.startedAt, Some(t))

  /** 来了新信，没答复的提问与批准当作没答复。 */
  private def decline(id: SessionId, cs: List[Call]): IO[AppError, Unit] =
    for
      t <- now
      _ <- db.write(ZIO.foreachDiscard(cs) { c =>
             val note = c.pause match
               case Some(_: Pause.Approval) => "[用户没有批准（改发了新消息，见下），没有执行。]"
               case _                       => "[用户没有回答这个问题，而是发来了新消息（见下）。]"
             setResult(c, CallResult.error(note), t)
           } *> changed)
      _ <- live.publish(id, LiveEvent.Changed)
    yield ()
