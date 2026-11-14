package muliao.migrate

import muliao.contract.{AgentId, RunId, SessionId}
import muliao.migrate.OldPayload.{Content, Mark, Part}
import muliao.models.llm.{Protocol, ReplyPart, Seal, Usage}
import muliao.sessions.*
import muliao.sessions.store.{EntryRow, RunRow, SessionRow, ToolCallRow}
import zio.Chunk
import zio.json.*
import zio.json.ast.Json

/** 一个会话在新版里的来源与来信来源怎么认（由会话行与触发器、agent 名推出）。 */
final case class SessionOrigin(row: SessionRow, sources: Senders)

/** 旧版标记的发送方种类 → 新版来信来源。没标记的信按会话的发起方算。 */
final case class Senders(user: Source, trigger: Source, issue: Source, delegation: Source, default: Source):
  def of(sender: String): Source = sender match
    case "user"       => user
    case "trigger"    => trigger
    case "issue"      => issue
    case "delegation" => delegation
    case _            => default

/** 旧版附件：文件编号 → 内容摘要与类型。 */
final case class FileInfo(sha256: String, mediaType: String, name: String)

/** 一个会话转换出来的东西。`callEntry`：调用编号 → 所在回复的条目编号（委派记录要引用发起方的调用）。 */
final case class Converted(
    runs: List[RunRow],
    entries: List[EntryRow],
    calls: List[ToolCallRow],
    callEntry: Map[String, Long],
    stats: Stats
)

/** 转换中的计数（报告与核对用）。 */
final case class Stats(
    messages: Long = 0,
    replies: Long = 0,
    letters: Long = 0,
    fileOnlyLetters: Long = 0,
    attachments: Long = 0,
    contexts: Long = 0,
    compactions: Long = 0,
    notices: Long = 0,
    calls: Long = 0,
    results: Long = 0,
    missingResults: Long = 0,
    orphanResults: Long = 0,
    badPayloads: Long = 0,
    unfinishedRuns: Long = 0
):
  def +(o: Stats): Stats =
    Stats(messages + o.messages, replies + o.replies, letters + o.letters, fileOnlyLetters + o.fileOnlyLetters, attachments + o.attachments, contexts + o.contexts, compactions + o.compactions,
      notices + o.notices, calls + o.calls, results + o.results, missingResults + o.missingResults, orphanResults + o.orphanResults,
      badPayloads + o.badPayloads, unfinishedRuns + o.unfinishedRuns)

/** 旧版一个会话的运行、消息、压缩 → 新版的运行与时间线（条目、工具调用）。纯函数：条目编号从 `firstId` 起连续分配。
  *
  * 对应关系：
  *   - 助手消息 → 一条回复；其中的工具调用各成一行调用，结果取自后面用户消息里同编号的工具结果；
  *   - 用户消息按段拆：运行上下文那段 → 上下文条目；每段信件文字（连同紧跟的附件段）→ 一封来信，来源按旧版标记；
  *   - 压缩 → 写在它记下的「当时最后一条消息」之后，`from` 指向被压缩部分之后的第一条；
  *   - 失败、被停止的运行在它最后一条之后记一条提示；旧版还没结束的运行记成被停止（新版不续跑）。
  *
  * 迁移后每个调用都有结果（旧版没有的给一个说明），运行都已结束，所以新版启动时不会续跑或执行任何东西。
  */
object Transcript:
  val missingResult = "[迁移：旧版没有记下这个调用的结果]"
  val unfinishedRun = "迁移时这次运行还没结束；新版不续跑"

  def convert(
      origin: SessionOrigin,
      runs: List[Old.Run],
      messages: List[Old.Message],
      compactions: List[Old.Compaction],
      files: Map[String, FileInfo],
      firstId: Long
  ): Converted = Builder(origin, files, firstId).run(runs, messages, compactions)

  /** 新版回复里可搜的文字（与会话领域写回复时一致：正文与工具调用的名字和参数）。 */
  def searchText(parts: Chunk[ReplyPart]): String =
    parts.collect {
      case ReplyPart.Text(t, _)           => t
      case ReplyPart.ToolCall(_, n, a, _) => s"$n $a"
    }.mkString("\n")

  def runState(outcome: Option[String]): RunState = outcome match
    case Some("done")    => RunState.Finished
    case Some("failed")  => RunState.Failed
    case Some("stopped") => RunState.Stopped
    case _               => RunState.Stopped // 没结束的、认不出的：记成被停止，新版不续跑

  def eventKind(event: String): String = event.takeWhile(_ != ':') match
    case "job"     => "job"
    case "thread"  => "delegation"
    case "wake_me" => "wake"
    case other     => other

  private final class Builder(origin: SessionOrigin, files: Map[String, FileInfo], firstId: Long):
    private val sid: SessionId = origin.row.id
    private var nextId         = firstId
    private val entries        = List.newBuilder[EntryRow]
    private var stats          = Stats()
    // 调用编号 → 调用行（等结果）；同一编号以最近的为准
    private val calls     = scala.collection.mutable.LinkedHashMap.empty[(Long, String), ToolCallRow]
    private val openCalls = scala.collection.mutable.Map.empty[String, (Long, String)]
    private val callEntry = scala.collection.mutable.Map.empty[String, Long]
    // 旧消息编号 → 它转出的第一条条目编号（压缩的 from 用）
    private val firstEntryOf = scala.collection.mutable.TreeMap.empty[Long, Long]

    private def add(run: Option[Long], body: EntryBody, search: String, at: Long): Long =
      val id = nextId
      nextId += 1
      entries += EntryRow(id, sid, run.map(RunId.wrap), body.tag, body.encode, search, at, at)
      id

    def run(runs: List[Old.Run], messages: List[Old.Message], compactions: List[Old.Compaction]): Converted =
      val byRun   = messages.groupBy(_.runId)
      val ordered = runs.sortBy(_.id)
      // 运行记录里没有的消息（不应出现）按编号排在最前，不丢
      val orphan  = messages.filterNot(m => ordered.exists(_.id == m.runId))
      var pending = compactions.sortBy(_.id)

      def emitCompactionsBefore(messageId: Long): Unit =
        val (due, rest) = pending.partition(_.at < messageId)
        due.foreach(compaction)
        pending = rest

      def emitMessage(m: Old.Message): Unit =
        emitCompactionsBefore(m.id)
        message(m)
        val (due, rest) = pending.partition(_.at <= m.id)
        due.foreach(compaction)
        pending = rest

      orphan.sortBy(_.id).foreach(emitMessage)
      val runRows = ordered.map { r =>
        val ms = byRun.getOrElse(r.id, Nil).sortBy(_.id)
        ms.foreach(emitMessage)
        val last  = ms.lastOption.map(_.createdAt).getOrElse(r.startedAt)
        val state = runState(r.outcome)
        val end   = r.endedAt.getOrElse(last)
        val note: Option[EntryBody] = (r.outcome, state) match
          case (None, _)                    => Some(EntryBody.Notice(NoticeKind.Stopped, unfinishedRun))
          case (_, RunState.Failed)         => Some(EntryBody.Notice(NoticeKind.Failed, r.error.getOrElse("失败")))
          case (_, RunState.Stopped)        => Some(EntryBody.Notice(NoticeKind.Stopped, "已停止"))
          case _                            => None
        note.foreach { n =>
          add(Some(r.id), n, "", end): Unit
          stats = stats.copy(notices = stats.notices + 1)
        }
        if r.outcome.isEmpty then stats = stats.copy(unfinishedRuns = stats.unfinishedRuns + 1)
        RunRow(RunId.wrap(r.id), sid, state.label, r.error.filter(_ => state == RunState.Failed), None, None, r.startedAt, Some(end))
      }
      pending.foreach(compaction)

      val callRows = calls.values.toList.map { c =>
        c.result match
          case Some(_) => c
          case None =>
            stats = stats.copy(missingResults = stats.missingResults + 1)
            c.copy(result = Some(CallResult.error(missingResult).toJson), finishedAt = c.startedAt)
      }
      Converted(runRows, entries.result(), callRows, callEntry.toMap, stats)

    private def compaction(c: Old.Compaction): Unit =
      val from = firstEntryOf.rangeFrom(c.through + 1).headOption.map(_._2).getOrElse(nextId)
      add(None, EntryBody.Compaction(c.summary, from), "", c.createdAt): Unit
      stats = stats.copy(compactions = stats.compactions + 1)

    private def message(m: Old.Message): Unit =
      stats = stats.copy(messages = stats.messages + 1)
      firstEntryOf.update(m.id, nextId)
      OldPayload.decode(m.payload) match
        case Left(err) =>
          // 解不开的消息记成一条提示，不丢、不中断
          stats = stats.copy(badPayloads = stats.badPayloads + 1, notices = stats.notices + 1)
          add(Some(m.runId), EntryBody.Notice(NoticeKind.Info, s"[迁移：旧版消息 ${m.id} 解不开：$err]"), "", m.createdAt): Unit
        case Right(p) if p.role == "assistant" => reply(m, p)
        case Right(p)                          => request(m, p)

    private def seal(protocol: Option[Protocol], meta: Option[Json]): Option[Seal] =
      for
        pr <- protocol
        d  <- meta
      yield Seal(pr, d)

    private def reply(m: Old.Message, p: OldPayload.Message): Unit =
      val protocol = p.protocol.flatMap(Protocol.parse(_).toOption)
      val parts: Chunk[ReplyPart] = Chunk.fromIterable(p.parts.collect {
        case Part.Text(t, meta)            => ReplyPart.Text(t, seal(protocol, meta))
        case Part.Thinking(t, meta)        => ReplyPart.Thinking(t, seal(protocol, meta))
        case Part.ToolCall(id, n, a, meta) => ReplyPart.ToolCall(id, n, a, seal(protocol, meta))
      })
      val u     = p.usage.getOrElse(OldPayload.Usage())
      val usage = Usage(u.input_tokens, u.output_tokens, u.cache_read_tokens, u.cache_write_tokens)
      val body  = EntryBody.Reply(parts, p.model.getOrElse(""), usage, p.finish_reason, p.metadata.flatMap(_.muliao_interrupted).contains(true))
      val id    = add(Some(m.runId), body, searchText(parts), m.createdAt)
      stats = stats.copy(replies = stats.replies + 1)
      parts.collect { case c: ReplyPart.ToolCall => c }.zipWithIndex.foreach { (c, seq) =>
        calls.update((id, c.id), ToolCallRow(id, c.id, sid, seq, c.name, c.args, CallState.Done.label, None, None, Some(m.createdAt), None))
        openCalls.update(c.id, (id, c.id))
        callEntry.update(c.id, id)
        stats = stats.copy(calls = stats.calls + 1)
      }

    /** 用户一方的消息：按段拆成上下文、来信与工具结果。 */
    private def request(m: Old.Message, p: OldPayload.Message): Unit =
      val meta  = p.metadata
      val marks = meta.flatMap(_.muliao_parts).getOrElse(Nil).map(k => k.part -> k).toMap
      val ctx   = meta.flatMap(_.muliao_run_context)
      // 正在拼的一封信：来源、正文、附件
      var letter: Option[Letter] = None
      def flush(): Unit =
        letter.foreach { l =>
          add(Some(m.runId), EntryBody.Mail(l), l.text, m.createdAt): Unit
          stats = stats.copy(letters = stats.letters + 1)
        }
        letter = None

      p.parts.zipWithIndex.foreach {
        case (Part.ToolResult(callId, content, isError, rmeta), _) =>
          result(callId, content, isError.contains(true), rmeta.flatMap(_.muliao_finished_at).map(Old.millis).getOrElse(m.createdAt))
        case (Part.Text(t, _), i) if ctx.contains(i) =>
          flush()
          add(Some(m.runId), EntryBody.Context(t), "", m.createdAt): Unit
          stats = stats.copy(contexts = stats.contexts + 1)
        case (Part.Text(t, _), i) =>
          flush()
          letter = Some(newLetter(marks.get(i), t))
        case (Part.File(_, url), i) =>
          val id = OldPayload.fileId(url).getOrElse(url)
          stats = stats.copy(attachments = stats.attachments + 1)
          letter match
            case Some(l) => letter = Some(l.copy(files = l.files :+ id))
            case None =>
              letter = Some(newLetter(marks.get(i), "").copy(files = Chunk(id)))
              stats = stats.copy(fileOnlyLetters = stats.fileOnlyLetters + 1)
        case (_: Part.Thinking | _: Part.ToolCall, _) => () // 用户一方不会有
      }
      flush()

    private def newLetter(mark: Option[Mark], text: String): Letter =
      mark match
        case Some(Mark(_, _, Some(event), checkIn)) => Letter(Source.Event(eventKind(event)), text, checkIn = checkIn.contains(true))
        case Some(Mark(_, Some(sender), _, _))      => Letter(origin.sources.of(sender), text)
        case _                                      => Letter(origin.sources.default, text)

    private def result(callId: String, content: List[Content], isError: Boolean, at: Long): Unit =
      openCalls.remove(callId) match
        case None => stats = stats.copy(orphanResults = stats.orphanResults + 1)
        case Some(key) =>
          val parts = Chunk.fromIterable(content.map {
            case Content.Text(t) => StoredPart.Text(t)
            case Content.File(mt, url) =>
              OldPayload.fileId(url).flatMap(files.get) match
                case Some(f) if mt.startsWith("image/") => StoredPart.Image(mt, f.sha256)
                case Some(f)                             => StoredPart.Text(s"[文件 ${f.name}（$mt）]")
                case None                                => StoredPart.Text(s"[文件 $url（$mt），迁移时没找到]")
          })
          calls.updateWith(key)(_.map(_.copy(result = Some(CallResult(parts, isError).toJson), finishedAt = Some(at)))): Unit
          stats = stats.copy(results = stats.results + 1)

  /** 新版会话行与来信来源。`triggerName`：触发器编号 → 名字；`sessionAgent`：会话编号 → 它的 agent 名（委派的发起方）。 */
  def origin(s: Old.Session, triggerName: Map[Long, String], sessionAgent: Map[String, String]): SessionOrigin =
    val trigger = s.originRef.flatMap(_.toLongOption).flatMap(triggerName.get)
      // 已删的触发器：会话标题是「名字 · 时间」
      .orElse(Option.when(s.title.contains(" · "))(s.title.takeWhile(_ != ' ')).filter(_.nonEmpty))
      .getOrElse(s"trigger-${s.originRef.getOrElse("?")}")
    val parentAgent = s.parent.flatMap(sessionAgent.get).getOrElse("?")
    val delegation  = Source.Delegation(s.parent.getOrElse(""), parentAgent)
    val issue       = s.originRef.flatMap(_.toLongOption).map(Source.Issue(_)).getOrElse(Source.User)
    val (o, ref, parent, default) = s.origin match
      case "trigger"    => ("trigger", Some(trigger), None, Source.Trigger(trigger))
      case "issue"      => ("issue", s.originRef, None, issue)
      case "delegation" => ("thread", Some(s.thread.getOrElse("thread")), s.parent, delegation)
      case _            => ("user", None, None, Source.User)
    val row = SessionRow(SessionId.wrap(s.id), AgentId.wrap(s.agentId), o, ref, parent.map(SessionId.wrap), s.title, "[]", s.createdAt, s.updatedAt)
    SessionOrigin(row, Senders(Source.User, Source.Trigger(trigger), issue, delegation, default))
