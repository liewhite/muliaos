package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.Tx
import muliao.sessions.store.*
import zio.*
import zio.json.*

/** 统一来信入口：各来源都经 [[deliver]] 投信，投不进时以领域错误说明原因。 */
private[sessions] trait InboxPart:
  this: Engine =>
  import deps.*

  /** 投一封信。`dedupe`：同一会话里已有同样键的信在排队就不再投（返回 None）。 */
  def deliver(to: Address, letter: Letter, dedupe: Option[String] = None): IO[AppError, Option[Delivered]] =
    for
      _      <- files.find(letter.files.toList.map(FileId.wrap))
      agent  <- agentOf(to)
      _      <- ZIO.foreachDiscard(agent)(agents.requireActive)
      t      <- now
      result <- db.write(deliverTx(to, letter, dedupe, t))
      _      <- ZIO.foreachDiscard(result)(d => kick(d.session))
    yield result

  private def agentOf(to: Address): IO[AppError, Option[AgentId]] = to match
    case Address.To(id)              => info(id).map(i => Some(i.agent))
    case Address.Fresh(a, _, _)      => ZIO.some(a)
    case Address.IssueOf(a, _, _)    => ZIO.some(a)
    case Address.ThreadOf(_, a, _)   => ZIO.some(a)

  /** 在写事务里投信（委派等需要和别的写一起提交时用）。不踢驱动：调用方提交后自己踢。 */
  def deliverTx(to: Address, letter: Letter, dedupe: Option[String], t: Long): Tx[Option[Delivered]] =
    for
      (row, created) <- locateTx(to, letter.source, t)
      queued         <- enqueueTx(row, letter, dedupe, t)
    yield Option.when(queued)(Delivered(row.id, created))

  /** 把信放进会话的队列；`dedupe` 键已有信在排队就不放（返回 false）。 */
  def enqueueTx(row: SessionRow, letter: Letter, dedupe: Option[String], t: Long): Tx[Boolean] =
    ZIO.foreach(dedupe)(k => Repo.hasQueuedKey(row.id, k)).map(_.contains(true)).flatMap {
      case true => ZIO.succeed(false)
      case false =>
        Repo.enqueue(row.id, letter.toJson, dedupe, t) *> Repo.touch(row.id, t) *> changed *>
          afterCommit(live.publish(row.id, LiveEvent.Changed)).as(true)
    }

  /** 找到或新开目标会话，并检查这个来源能不能往里写。 */
  def locateTx(to: Address, source: Source, t: Long): Tx[(SessionRow, Boolean)] =
    def fresh(agent: AgentId, origin: Origin, title: String): Tx[(SessionRow, Boolean)] =
      val row = SessionInfo.row(Ids.session, agent, origin, title, t)
      Repo.insertSession(row).as((row, true))
    def check(row: SessionRow): Tx[(SessionRow, Boolean)] =
      SessionInfo.decode(row).flatMap(i => ZIO.fromEither(Source.mayWrite(source, i.origin)).mapError(AppError.forbidden)).as((row, false))
    to match
      case Address.To(id) =>
        Repo.session(id).someOrFail(AppError.notFound(s"会话 ${id.value} 不存在（可能已删除）")).flatMap(check)
      case Address.Fresh(agent, origin, title) => fresh(agent, origin, title)
      case Address.IssueOf(agent, issue, title) =>
        Repo.issueSession(agent, issue.value.toString).flatMap {
          case Some(row) => check(row)
          case None      => fresh(agent, Origin.Issue(issue), title)
        }
      case Address.ThreadOf(requester, agent, name) =>
        Repo.session(requester).someOrFail(AppError.notFound(s"发起会话 ${requester.value} 不存在")) *>
          Repo.thread(requester, agent, name.value).flatMap {
            case Some(row) => check(row)
            case None      => fresh(agent, Origin.Thread(Some(requester), name), name.value)
          }

  /** 把排队的信取进时间线（下一次请求模型前），并记下回答去向。返回取进来的信。 */
  def takeMail(id: SessionId, run: RunId, t: Long): Tx[List[Letter]] =
    for
      rows    <- Repo.queued(id)
      letters <- ZIO.foreach(rows)(r => Decode.letter(r.letter).map(r -> _))
      _ <- ZIO.foreachDiscard(letters) { (r, l) =>
             Repo.insertEntry(EntryRow(0L, id, Some(run), "letter", EntryBody.Mail(l).encode, l.text, r.createdAt, t))
           }
      _ <- ZIO.when(rows.nonEmpty)(Repo.dequeue(rows.map(_.id)))
      _ <- addPendingReply(id, letters.flatMap(_._2.replyTo))
    yield letters.map(_._2)

  def addPendingReply(id: SessionId, more: List[ReplyTo]): Tx[List[ReplyTo]] =
    for
      i <- infoTx(id)
      all = (i.pendingReply ++ more).distinct
      _ <- ZIO.when(all != i.pendingReply)(Repo.setPendingReply(id, all.toJson))
    yield all

  /** 取走回答去向（交回时用）。 */
  def takePendingReply(id: SessionId): Tx[List[ReplyTo]] =
    infoTx(id).flatMap(i => Repo.setPendingReply(id, "[]").when(i.pendingReply.nonEmpty).as(i.pendingReply))
