package muliao.sessions

import muliao.contract.*
import muliao.models.llm.ModelRequest
import muliao.platform.AppError
import muliao.sessions.store.*
import zio.*

/** 上下文压缩：把较早的时间线连同上一份摘要交给模型总结，之后用「摘要 + 最近若干步原文」继续。原文不删。 */
private[sessions] final case class Compaction(kernel: Kernel, requests: Requests):
  import kernel.*

  /** 上一次请求的输入超过阈值就压缩（长运行中途也会）。 */
  def maybeCompact(kit: Kit, run: RunId): IO[AppError, Unit] =
    requests.contextSize(kit.info.id).flatMap(size => compact(kit, Some(run)).when(size > settings.compactionThresholdTokens).unit)

  /** 压缩一次；没什么可压（最近压缩点之后不到两步）时返回 false。 */
  def compact(kit: Kit, run: Option[RunId]): IO[AppError, Boolean] =
    val id = kit.info.id
    requests.window(id).flatMap { w =>
      val replies = w.entries.collect { case e @ Entry(_, _, _: EntryBody.Reply, _, _) => e.id }
      if replies.size < 2 then ZIO.succeed(false)
      else
        val keep    = settings.keepRecentSteps.max(1).min(replies.size - 1)
        val cut     = replies(replies.size - keep)
        val ask     = Entry(Long.MaxValue, None, EntryBody.Context(Prompt.compactionRequest), 0L, 0L)
        val earlier = w.copy(entries = w.entries.filter(_.id < cut) :+ ask)
        val req     = ModelRequest(kit.system, kit.defs, earlier.messages, kit.thinking, cacheKey = Some(id.value))
        for
          _     <- ZIO.logInfo(s"会话 ${id.value} 压缩上下文：总结第 $cut 条之前的部分")
          reply <- requests.call(kit, run, req, None)
          text   = ModelView.answerText(reply.parts)
          _     <- ZIO.when(text.isEmpty)(ZIO.fail(AppError.External("压缩时模型没有给出摘要")))
          t     <- now
          _ <- db.write(
                 Repo.insertEntry(EntryRow(0L, id, run, EntryKind.Compaction, EntryBody.Compaction(text, cut).encode, "", t, t)) *>
                   requests.recordUsage(kit, reply.usage, t) *> Repo.touch(id, t) *> changed
               )
          _ <- live.publish(id, LiveEvent.Changed)
        yield true
    }

private[sessions] object Compaction:
  val layer: URLayer[Kernel & Requests, Compaction] = ZLayer.derive[Compaction]
