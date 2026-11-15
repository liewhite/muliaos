package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.Tx
import muliao.resources.agents.Agents
import muliao.sessions.store.*
import zio.*
import zio.json.*

/** 找到（或新开）的目标会话。 */
private[sessions] final case class Located(row: SessionRow, created: Boolean)

/** 信箱：各来源统一投信（投不进时以领域错误说明原因）、按地址找或开会话、把排队的信取进时间线、记下与取走回答去向。 */
private[sessions] final case class Mailbox(kernel: Kernel, files: Files, agents: Agents, doorbell: Doorbell):
  import kernel.*

  /** 投一封信。`dedupe`：同一会话里已有同样键的信在排队就不再投（返回 None）。 */
  def deliver(to: Address, letter: Letter, dedupe: Option[String] = None): IO[AppError, Option[Delivered]] =
    for
      _      <- files.find(letter.files.toList)
      _      <- requireActive(to)
      t      <- now
      result <- db.write(deliverTx(to, letter, dedupe, t))
      _      <- ZIO.foreachDiscard(result)(d => doorbell.ring(d.session))
    yield result

  /** 在调用方的写事务里投一封只有文字的信（看板：与 issue 的改动同一事务提交）；提交后才开始跑。
    * 只收来源与文字：附件要先核对存在（[[deliver]] 在事务外做），这里写不进来。
    */
  def deliverIn(to: Address, source: Source, text: String, dedupe: Option[String]): Tx[Option[Delivered]] =
    for
      _      <- requireActive(to)
      t      <- now
      result <- deliverTx(to, Letter(source, text), dedupe, t)
      _      <- ZIO.foreachDiscard(result)(d => afterCommit(doorbell.ring(d.session)))
    yield result

  private def requireActive(to: Address): IO[AppError, Unit] =
    val agent = to match
      case Address.To(id)            => info(id).map(_.agent)
      case Address.Fresh(a, _, _)    => ZIO.succeed(a)
      case Address.IssueOf(a, _, _)  => ZIO.succeed(a)
      case Address.ThreadOf(_, a, _) => ZIO.succeed(a)
    agent.flatMap(agents.requireActive).unit

  /** 在写事务里投信（和别的写一起提交时用）。不按门铃：调用方提交后自己按。 */
  def deliverTx(to: Address, letter: Letter, dedupe: Option[String], t: Long): Tx[Option[Delivered]] =
    for
      at     <- locateTx(to, letter.source, t)
      queued <- enqueueTx(at.row, letter, dedupe, t)
    yield Option.when(queued)(Delivered(at.row.id, at.created))

  /** 把信放进会话的队列；`dedupe` 键已有信在排队就不放（返回 false）。 */
  def enqueueTx(row: SessionRow, letter: Letter, dedupe: Option[String], t: Long): Tx[Boolean] =
    ZIO.foreach(dedupe)(k => Repo.hasQueuedKey(row.id, k)).map(_.contains(true)).flatMap {
      case true => ZIO.succeed(false)
      case false =>
        (Repo.enqueue(row.id, letter.toJson, dedupe, t) *> touched(row.id, t)).as(true)
    }

  /** 找到或新开目标会话，并检查这个来源能不能往里写。 */
  def locateTx(to: Address, source: Source, t: Long): Tx[Located] =
    def fresh(agent: AgentId, origin: Origin, title: String): Tx[Located] =
      val row = SessionInfo.row(Ids.session, agent, origin, title, t)
      Repo.insertSession(row).as(Located(row, created = true))
    def existing(row: SessionRow): Tx[Located] =
      SessionInfo.decode(row).flatMap(i => ZIO.fromEither(Source.mayWrite(source, i.origin)).mapError(AppError.forbidden)).as(Located(row, created = false))
    to match
      case Address.To(id) =>
        Repo.session(id).someOrFail(AppError.notFound(s"会话 ${id.value} 不存在（可能已删除）")).flatMap(existing)
      case Address.Fresh(agent, origin, title) => fresh(agent, origin, title)
      case Address.IssueOf(agent, issue, title) =>
        Repo.issueSession(agent, issue).flatMap {
          case Some(row) => existing(row)
          case None      => fresh(agent, Origin.Issue(issue), title)
        }
      case Address.ThreadOf(requester, agent, name) =>
        Repo.session(requester).someOrFail(AppError.notFound(s"发起会话 ${requester.value} 不存在")) *>
          Repo.thread(requester, agent, name).flatMap {
            case Some(row) => existing(row)
            case None      => fresh(agent, Origin.Thread(Some(requester), name), name.value)
          }

  /** 把排队的信取进时间线（下一次请求模型前），并记下回答去向。返回取进来的信。 */
  def takeMail(id: SessionId, run: RunId, t: Long): Tx[List[Letter]] =
    for
      rows    <- Repo.queued(id)
      letters <- ZIO.foreach(rows)(r => Decode.letter(r.letter).map(r -> _))
      _ <- ZIO.foreachDiscard(letters) { (r, l) =>
             Repo.insertEntry(EntryRow(0L, id, Some(run), EntryKind.Letter, EntryBody.Mail(l).encode, l.text, r.createdAt, t))
           }
      _ <- ZIO.when(rows.nonEmpty)(Repo.dequeue(rows.map(_.id)) *> touched(id, t))
      _ <- addPendingReply(id, letters.flatMap(_._2.replyTo))
    yield letters.map(_._2)

  def addPendingReply(id: SessionId, more: List[ReplyTo]): Tx[List[ReplyTo]] =
    for
      i <- infoTx(id)
      all = (i.pendingReply ++ more).distinct
      _ <- ZIO.when(all != i.pendingReply)(setPendingTx(id, all))
    yield all

  /** 取走回答去向（交回时用）。 */
  def takePendingReply(id: SessionId): Tx[List[ReplyTo]] =
    infoTx(id).flatMap(i => setPendingTx(id, Nil).when(i.pendingReply.nonEmpty).as(i.pendingReply))

private[sessions] object Mailbox:
  val layer: URLayer[Kernel & Files & Agents & Doorbell, Mailbox] = ZLayer.derive[Mailbox]
