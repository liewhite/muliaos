package muliao.sessions

import muliao.contract.*
import muliao.models.llm.*
import muliao.models.mcp.{Launch, McpConnector, McpServerSpec, SecretLookup}
import muliao.platform.{AppError, Retry}
import muliao.platform.db.Tx
import muliao.resources.{ResourcePaths, Resources, RunResources}
import muliao.resources.vault.{SecretUse, Vault}
import muliao.sessions.store.*
import muliao.toolkit.{Tool, ToolSpec}
import zio.*

import java.nio.file.Path

/** 一次运行要的全部东西：运行开始（驱动 fiber 起来）时准备一次。 */
private[sessions] final case class Kit(
    info: SessionInfo,
    res: RunResources,
    model: ModelRef,
    system: String,
    tools: Map[String, Tool[RunContext, Pause]],
    defs: Chunk[ToolDef],
    specs: Map[String, ToolSpec],
    mcpProblems: List[String],
    workspace: Path
)

/** 连上的 MCP 工具与没连上的原因。 */
private[sessions] final case class McpTools(tools: List[Tool[RunContext, Nothing]], problems: List[String])

/** 给模型看的窗口：最近压缩点的摘要、它之后的各条、其中的调用、用到的附件。 */
private[sessions] final case class Window(summary: Option[String], entries: List[Entry], calls: Map[Long, List[Call]], attachments: ModelView.Attachments):
  def messages: Chunk[Message] = ModelView.messages(summary, entries, calls, attachments)

/** 请求模型：准备一次运行的 [[Kit]]（资源、模型、工具、MCP）、组装请求窗口、带退避重试地请求并实时展示、记用量。 */
private[sessions] final case class Requests(
    kernel: Kernel,
    files: Files,
    resources: Resources,
    models: Models,
    mcp: McpConnector,
    vault: Vault,
    paths: ResourcePaths
):
  import kernel.*

  // ---- 准备 ----

  def prepareKit(id: SessionId): ZIO[Scope, AppError, Kit] =
    for
      i     <- info(id)
      r     <- resources.forRun(i.agent)
      model <- ZIO.fromEither(ModelRef.parse(r.profile.model)).mapError(AppError.invalid)
      _     <- ZIO.foreachDiscard(models.problem(model.toString))(p => ZIO.fail(AppError.invalid(s"agent 的模型 $model 用不了：$p")))
      ws    <- paths.ensureWorkspace(i.agent)
      builtin <- ports.tools.map(_.filter(t => r.tools(t.spec.name)))
      conn    <- connectMcp(i, r, ws)
      names    = builtin.map(_.spec.name).toSet
      all      = builtin ++ conn.tools.filterNot(t => names(t.spec.name)).sortBy(_.spec.name)
    yield Kit(
      info = i,
      res = r,
      model = model,
      system = Prompt.system(r, i.origin, ws),
      tools = all.map(t => t.spec.name -> t).toMap,
      defs = Chunk.fromIterable(all.map(t => ToolDef(t.spec.name, t.spec.description, t.spec.inputSchema))),
      specs = all.map(t => t.spec.name -> t.spec).toMap,
      mcpProblems = conn.problems,
      workspace = ws
    )

  /** 连上它能用、启用了的 MCP server（连接随驱动 fiber 结束而断开）。 */
  private def connectMcp(i: SessionInfo, r: RunResources, ws: Path): ZIO[Scope, AppError, McpTools] =
    val specs = Chunk.fromIterable(r.mcps.filter(_.config.enabled).map(m => McpServerSpec(m.name, m.config.transport)))
    if specs.isEmpty then ZIO.succeed(McpTools(Nil, Nil))
    else
      val purpose = AskPurpose(Some(i.id), s"连接 MCP：${specs.map(_.name).mkString("、")}（本次运行开头）")
      val lookup = new SecretLookup:
        def reveal(key: String): IO[AppError, String] =
          vault.reveal(i.agent, List(key), SecretUse.Mcp, purpose).flatMap(m => ZIO.fromOption(m.get(key)).orElseFail(AppError.invalid(s"取不到 secret $key")))
      resources.writableScope(i.agent).either.flatMap {
        case Left(e) => ZIO.succeed(McpTools(Nil, List(s"MCP 这次没有连：算写沙箱失败（${e.message}）")))
        case Right(scope) =>
          mcp.connectAll(specs, lookup, Launch(ws, scope)).map(c => McpTools(c.tools.toList, c.problems.toList))
      }

  // ---- 窗口 ----

  /** 组装这一次请求：固定说明 + 工具 + 最近压缩点之后的时间线。 */
  def buildRequest(kit: Kit): IO[AppError, ModelRequest] =
    window(kit.info.id).map(w => ModelRequest(kit.system, kit.defs, w.messages, cacheKey = Some(kit.info.id.value)))

  /** 最近压缩点之后的时间线、其中的调用与用到的附件。 */
  def window(id: SessionId): IO[AppError, Window] =
    for
      last    <- db.read(Repo.lastEntry(id, EntryKind.Compaction)).flatMap(ZIO.foreach(_)(Decode.entry))
      from     = last.collect { case Entry(_, _, EntryBody.Compaction(_, f), _, _) => f }.getOrElse(0L)
      rows    <- db.read(Repo.entriesFrom(id, from))
      entries <- Decode.entries(rows)
      all      = last.toList ++ entries.filter(e => last.forall(_.id != e.id))
      cut      = ModelView.window(all.sortBy(_.id))
      calls   <- db.read(Repo.callsOfSession(id)).flatMap(Decode.calls).map(_.groupBy(_.entryId))
      att     <- attachments(cut.entries, calls)
    yield Window(cut.summary, cut.entries, calls, att)

  private def attachments(entries: List[Entry], calls: Map[Long, List[Call]]): IO[AppError, ModelView.Attachments] =
    val fileIds = entries.flatMap {
      case Entry(_, _, EntryBody.Mail(l), _, _) => l.files.toList
      case _                                    => Nil
    }.distinct
    val windowIds  = entries.map(_.id).toSet
    val resultShas = calls.filter((e, _) => windowIds(e)).values.flatten.flatMap(_.status.outcome.toList.flatMap(_.content.collect { case StoredPart.Image(_, s) => s }))
    for
      uploads <- db.read(Repo.uploadsOf(fileIds))
      shas     = (uploads.map(_.sha256) ++ resultShas).distinct
      bytes   <- ZIO.foreach(shas)(s => files.read(s).option.map(s -> _))
    yield ModelView.Attachments(uploads.map(u => u.id -> u).toMap, bytes.collect { case (s, Some(b)) => s -> b }.toMap, files.path)

  /** 当前上下文大小：最近一次请求的输入 token（含缓存）；最近一次压缩之后还没有回复时不知道（0），压缩前的回复报的是压缩前的大小。 */
  def contextSize(id: SessionId): IO[AppError, Long] =
    for
      reply <- db.read(Repo.lastEntry(id, EntryKind.Reply)).flatMap(ZIO.foreach(_)(Decode.entry))
      comp  <- db.read(Repo.lastEntry(id, EntryKind.Compaction))
    yield reply match
      case Some(Entry(rid, _, EntryBody.Reply(_, _, u, _, false), _, _)) if comp.forall(_.id < rid) => u.input
      case _                                                                                          => 0L

  // ---- 请求 ----

  /** 请求一次模型，暂时性失败退避重试（约 30 分钟放弃）。`shown` 给出时实时展示并记下正在生成的部分。 */
  def call(kit: Kit, run: Option[RunId], req: ModelRequest, shown: Option[Shown]): IO[AppError, Reply] =
    val id = kit.info.id
    def show(e: UIO[Unit]): UIO[Unit] = e.when(shown.isDefined).unit
    val once =
      for
        t <- now
        _ <- ZIO.foreachDiscard(shown)(s => s.slot.partial.set(Some(Partial(s.run, kit.model.toString, t, ReplyBuilder.empty))))
        _ <- show(live.publish(id, LiveEvent.Begin))
        fin <- models
                 .request(kit.model, req)
                 .runFoldZIO(Folded(ReplyBuilder.empty, None)) { (acc, ev) =>
                   for
                     nb <- ZIO.fromEither(acc.builder(ev)).mapError(e => AppError.External(s"模型回复的流对不上：$e"))
                     _  <- ZIO.foreachDiscard(shown)(_.slot.partial.update(_.map(_.copy(builder = nb))))
                     _  <- show(live.track(id, nb.parts) *> liveOf(ev).fold(ZIO.unit)(live.publish(id, _)))
                   yield Folded(nb, ev match
                     case ReplyEvent.Finished(r) => Some(r)
                     case _                      => acc.reply
                   )
                 }
        reply <- ZIO.fromOption(fin.reply).orElseFail(AppError.Unavailable("模型的回复没有正常结束"))
      yield reply
    for
      retried <- Ref.make(false)
      reply <- once
                 .tapError(e =>
                   ZIO.when(e.isTransient)(
                     retried.set(true) *> ZIO.logWarning(s"会话 ${id.value} 请求模型暂时失败，稍后重试：${e.message}") *>
                       ZIO.foreachDiscard(run)(r => now.flatMap(t => db.write(Repo.setRetry(r, None, Some(e.message)) *> touched(id, t)))).ignore
                   )
                 )
                 .retry(Retry.schedule(Retry.transient))
      _ <- ZIO.foreachDiscard(run)(r => now.flatMap(t => db.write(Repo.setRetry(r, None, None) *> touched(id, t)))).whenZIO(retried.get)
    yield reply

  private final case class Folded(builder: ReplyBuilder, reply: Option[Reply])

  private def liveOf(e: ReplyEvent): Option[LiveEvent] = e match
    case ReplyEvent.Started(i, p)  => Some(LiveEvent.Part(i, p))
    case ReplyEvent.Replaced(i, p) => Some(LiveEvent.Part(i, p))
    case ReplyEvent.Grew(i, g, t)  => Some(LiveEvent.Grew(i, g, t))
    case ReplyEvent.Finished(_)    => None

  def recordUsage(kit: Kit, u: Usage, t: Long): Tx[Unit] =
    Repo.insertUsage(UsageRow(0L, t, kit.info.agent, kit.info.id, kit.model.toString, u.input, u.output, u.cacheRead, u.cacheWrite))

/** 实时展示正在生成的回复：属于哪次运行、记在哪个驱动槽。 */
private[sessions] final case class Shown(run: RunId, slot: Slot)

private[sessions] object Requests:
  val layer: URLayer[Kernel & Files & Resources & Models & McpConnector & Vault & ResourcePaths, Requests] = ZLayer.derive[Requests]
