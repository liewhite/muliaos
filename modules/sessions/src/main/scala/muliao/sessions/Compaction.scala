package muliao.sessions

import muliao.contract.*
import muliao.models.llm.{ModelRequest, ReplyPart}
import muliao.platform.AppError
import muliao.sessions.store.*
import zio.*

/** 上下文压缩：把较早的时间线连同上一份摘要交给模型总结，之后用「摘要 + 最近若干步原文」继续。原文不删。
  *
  * 一次只交给模型总长不超过 [[Compaction.batchChars]] 的最早若干步，剩下的接着压，直到保留窗口之外没有可压的（每批各自落库）。
  * 同一次运行里压缩后的第一次回复仍超阈值，说明保留的最近几步本身就超了，再压也压不下去：这次运行不再自动压缩。
  */
private[sessions] final case class Compaction(kernel: Kernel, requests: Requests):
  import kernel.*

  /** 上一次请求的输入超过阈值就压缩（长运行中途也会）；这次运行压缩过却没压下去时不再压。 */
  def maybeCompact(kit: Kit, run: RunId): IO[AppError, Unit] =
    val id = kit.info.id
    requests.contextSize(id).flatMap { size =>
      ZIO.when(size > settings.compactionThresholdTokens)(
        exhausted(id, run).flatMap(done => if done then ZIO.logDebug(s"会话 ${id.value} 压缩后仍超阈值，这次运行不再压缩") else compact(kit, Some(run)).unit)
      ).unit
    }

  /** 这次运行压缩过，且压缩后的第一次回复仍超阈值。 */
  private def exhausted(id: SessionId, run: RunId): IO[AppError, Boolean] =
    db.read(Repo.lastEntry(id, EntryKind.Compaction)).flatMap {
      case Some(c) if c.runId.contains(run) =>
        db.read(Repo.firstEntryAfter(id, EntryKind.Reply, c.id)).flatMap(ZIO.foreach(_)(Decode.entry)).map {
          case Some(Entry(_, _, EntryBody.Reply(_, _, u, _, false), _, _)) => u.input > settings.compactionThresholdTokens
          case _                                                            => false
        }
      case _ => ZIO.succeed(false)
    }

  /** 压缩到保留窗口之外没有可压的为止；没什么可压（最近压缩点之后不到两步）时返回 false。 */
  def compact(kit: Kit, run: Option[RunId]): IO[AppError, Boolean] =
    compactBatch(kit, run).flatMap {
      case None        => ZIO.succeed(false)
      case Some(true)  => compactRest(kit, run).as(true)
      case Some(false) => ZIO.succeed(true)
    }

  private def compactRest(kit: Kit, run: Option[RunId]): IO[AppError, Unit] =
    compactBatch(kit, run).flatMap {
      case Some(true) => compactRest(kit, run)
      case _          => ZIO.unit
    }

  /** 压一批。None：没什么可压；Some(more)：压了，more 表示保留窗口之外还有没压的。 */
  private def compactBatch(kit: Kit, run: Option[RunId]): IO[AppError, Option[Boolean]] =
    val id = kit.info.id
    requests.window(id).flatMap { w =>
      Compaction.plan(w, settings.keepRecentSteps, Compaction.batchChars) match
        case None => ZIO.none
        case Some(Compaction.Batch(cut, more)) =>
          val ask     = Entry(Long.MaxValue, None, EntryBody.Context(Prompt.compactionRequest), 0L, 0L)
          val earlier = w.copy(entries = w.entries.filter(_.id < cut) :+ ask)
          val req     = ModelRequest(kit.system, kit.defs, earlier.messages, kit.thinking, cacheKey = Some(id.value))
          for
            _     <- ZIO.logInfo(s"会话 ${id.value} 压缩上下文：总结第 $cut 条之前的部分${if more then "（分批，后面还有）" else ""}")
            reply <- requests.call(kit, run, req, None)
            text   = ModelView.answerText(reply.parts)
            _     <- ZIO.when(text.isEmpty)(ZIO.fail(AppError.External("压缩时模型没有给出摘要")))
            t     <- now
            _ <- db.write(
                   Repo.insertEntry(EntryRow(0L, id, run, EntryKind.Compaction, EntryBody.Compaction(text, cut).encode, "", t, t)) *>
                     requests.recordUsage(kit, reply.usage, t) *> touched(id, t)
                 )
          yield Some(more)
    }

private[sessions] object Compaction:
  val layer: URLayer[Kernel & Requests, Compaction] = ZLayer.derive[Compaction]

  /** 一批交给模型总结的内容的字符上限（超出的步留给下一批）。 */
  val batchChars: Long = 400_000L

  /** 一批压到哪：`cut` 之前的由摘要代替；`more`：`cut` 到保留窗口之间还有没压的。 */
  final case class Batch(cut: Long, more: Boolean)

  /** 这一批的切点：保留最近 `keep` 步（模型回复）原文，之前的从头取总长不超过 `limit` 的若干步（至少一步）。不到两步时 None。 */
  def plan(w: Window, keep: Int, limit: Long): Option[Batch] =
    val replies = w.entries.collect { case e @ Entry(_, _, _: EntryBody.Reply, _, _) => e.id }.toVector
    Option.when(replies.size >= 2) {
      val last       = replies.size - keep.max(1).min(replies.size - 1) // 最终切点在 replies 里的下标（≥ 1）
      val base       = w.summary.fold(0L)(_.length.toLong)
      val sizes      = w.entries.map(e => e.id -> chars(e, w.calls.getOrElse(e.id, Nil)))
      def before(cut: Long): Long = base + sizes.collect { case (i, n) if i < cut => n }.sum
      val fitting    = (1 to last).takeWhile(k => before(replies(k)) <= limit).lastOption.getOrElse(1)
      Batch(replies(fitting), fitting < last)
    }

  /** 一条时间线交给模型时的大致字符数（图片、附件不计）。 */
  def chars(e: Entry, calls: List[Call]): Long = e.body match
    case EntryBody.Context(t) => t.length.toLong
    case EntryBody.Mail(l)    => l.text.length.toLong
    case EntryBody.Reply(parts, _, _, _, _) =>
      val own = parts.map {
        case ReplyPart.Text(t, _)          => t.length.toLong
        case ReplyPart.Thinking(t, _)      => t.length.toLong
        case ReplyPart.ToolCall(_, n, a, _) => (n.length + a.length).toLong
      }.sum
      own + calls.flatMap(_.result.toList).flatMap(_.content).collect { case StoredPart.Text(t) => t.length.toLong }.sum
    case _: EntryBody.Compaction | _: EntryBody.Notice => 0L
