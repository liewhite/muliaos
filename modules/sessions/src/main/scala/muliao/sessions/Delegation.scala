package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.Tx
import muliao.resources.agents.{AgentRecord, Agents}
import muliao.sessions.store.*
import zio.*
import zio.json.*

/** 要交回结局的委派怎么交回：作为事件交回发起方（`report_back`），或作为发起方等着的调用的结果（`ask_agent`）。
  * 回答推给用户的委派（`delegate_task` 默认）不用交回，也就不记委派。
  */
enum TrackedMode(val label: String) extends Labeled:
  case Report extends TrackedMode("report")
  case Ask    extends TrackedMode("ask")

object TrackedMode extends Labels[TrackedMode]("委派方式", TrackedMode.values)

/** 开出的要交回结局的委派：做它的 thread 会话与委派编号。 */
final case class Tracked(thread: SessionId, delegation: DelegationId)

/** `list_threads` 的一行。 */
final case class ThreadView(thread: ThreadName, agent: String, session: SessionId, state: WorkState, failure: Option[String], answer: Option[String])
    derives JsonEncoder

/** 委派与 thread：交给某 agent 的 thread（同名续聊）；thread 回到空闲、失败或被停止时把结局交回（[[handBack]]）。 */
private[sessions] final case class Delegations(kernel: Kernel, mailbox: Mailbox, waits: Waits, states: States, doorbell: Doorbell, agents: Agents):
  import kernel.*

  private def address(requester: RunContext, target: AgentRecord, name: ThreadName): Address = Address.ThreadOf(requester.session, target.id, name)

  private def source(requester: RunContext): Source = Source.Delegation(requester.session, requester.agentName)

  /** 交给 `target` 的 thread `name`，回答推送给用户、不回给发起方。返回 thread 会话。 */
  def notifyUser(requester: RunContext, target: AgentRecord, name: ThreadName, task: String): IO[AppError, SessionId] =
    for
      _ <- agents.requireActive(target.id)
      t <- now
      d <- db.write(
             mailbox
               .deliverTx(address(requester, target, name), Letter(source(requester), task, replyTo = Some(ReplyTo.User)), None, t)
               .someOrFail(AppError.Internal("投递委派失败"))
           )
      _ <- doorbell.ring(d.session)
    yield d.session

  /** 交给 `target` 的 thread `name`，结局按 `mode` 交回发起方；`Report` 时发起会话因此等待中（`heartbeat` 给出时定期投检查）。 */
  def track(requester: RunContext, target: AgentRecord, name: ThreadName, task: String, mode: TrackedMode, heartbeat: Option[Duration]): IO[AppError, Tracked] =
    for
      _ <- agents.requireActive(target.id)
      t <- now
      tracked <- db.write(
                   for
                     at <- mailbox.locateTx(address(requester, target, name), source(requester), t)
                     d  <- Repo.insertDelegation(DelegationRow(DelegationId.wrap(0L), requester.session, at.row.id, mode, None, None, false, None, t))
                     _  <- mailbox.enqueueTx(at.row, Letter(source(requester), task, replyTo = Some(ReplyTo.Delegation(d))), None, t)
                     _ <- mode match
                            case TrackedMode.Report =>
                              waits.openTx(requester.session, WaitRef.Delegation(d), s"委派给 ${target.name.value} 的 thread「${name.value}」", heartbeat, t).unit
                            case TrackedMode.Ask => ZIO.unit
                   yield Tracked(at.row.id, d)
                 )
      _ <- doorbell.ring(tracked.thread)
    yield tracked

  /** 把一段工作的结局交给等着的各方：委派方（交回）、用户（发事件，通知领域推送）。 */
  def handBack(i: SessionInfo, pending: List[ReplyTo], outcome: Outcome): IO[AppError, Unit] =
    for
      _ <- ZIO.foreachDiscard(pending.collect { case ReplyTo.Delegation(d) => d })(d => finish(d, i, outcome))
      _ <- hooks.emit(SessionEvent.Settled(i.id, i.agent, i.origin, outcome, pending.contains(ReplyTo.User)))
    yield ()

  /** thread 的一段工作有了结局：交回发起方（作为事件，或作为它等着的调用的结果）。已交回过的不再交。 */
  def finish(d: DelegationId, thread: SessionInfo, outcome: Outcome): IO[AppError, Unit] =
    for
      row   <- db.read(Repo.delegation(d))
      agent <- agents.get(thread.agent).map(_.name.value).orElseSucceed("?")
      name   = threadName(thread)
      result = outcome match
                 case Outcome.Answered(a) => CallResult.ok(if a.isEmpty then "（没有文字回答）" else a)
                 case Outcome.Failed(e)   => CallResult.error(s"[agent $agent 的 thread「$name」最终失败：$e]")
                 case Outcome.Stopped(p)  => CallResult.error(s"[agent $agent 的 thread「$name」被停止了。停止前的输出：\n$p]")
      t <- now
      requester <- db.write(
                     row.fold(ZIO.none) { r =>
                       Repo.finishDelegation(d, result.toJson).flatMap {
                         case 0 => ZIO.none
                         case _ =>
                           r.mode match
                             case TrackedMode.Report =>
                               waits.closeTx(WaitKey.delegation(d), Some(Letter(Source.Event(EventKind.Delegation), reportText(agent, name, outcome))), t)
                             case TrackedMode.Ask => resolveAsk(r, result, t)
                       }
                     }
                   )
      _ <- ZIO.foreachDiscard(requester)(doorbell.ring)
    yield ()

  private def reportText(agent: String, name: String, o: Outcome): String = o match
    case Outcome.Answered(a) => s"[委派回报] agent $agent 的 thread「$name」做完了，回答：\n$a"
    case Outcome.Failed(e)   => s"[委派回报] agent $agent 的 thread「$name」最终失败：$e"
    case Outcome.Stopped(p)  => s"[委派回报] agent $agent 的 thread「$name」被停止了。停止前的输出：\n$p"

  private def threadName(i: SessionInfo): String = i.origin match
    case Origin.Thread(_, n) => n.value
    case _                   => i.title

  /** `ask_agent`：发起方挂起的调用拿到结果。调用还没挂上（thread 先做完了）就留着，挂上时取。 */
  private def resolveAsk(r: DelegationRow, result: CallResult, t: Long): Tx[Option[SessionId]] =
    (r.callEntry, r.callId) match
      case (Some(e), Some(c)) =>
        Repo.call(e, c).flatMap {
          case Some(row) if row.state == CallState.Suspended =>
            Repo.setCall(e, c, CallState.Done, row.pause, Some(result.toJson), row.startedAt, Some(t)) *> Repo.touch(r.requesterId, t) *> changed.as(Some(r.requesterId))
          case _ => ZIO.none
        }
      case _ => ZIO.none

  /** 发起方的 `ask_agent` 调用挂起时挂上委派；thread 已经做完了就直接拿结果。 */
  def linkTx(d: DelegationId, call: CallKey, t: Long): Tx[Unit] =
    Repo.delegation(d).flatMap {
      case Some(r) if r.done =>
        for
          row <- Repo.call(call.entry, call.call)
          res  = r.result.getOrElse(CallResult.error("[没有结果]").toJson)
          _   <- ZIO.foreachDiscard(row)(x => Repo.setCall(call.entry, call.call, CallState.Done, x.pause, Some(res), x.startedAt, Some(t)))
        yield ()
      case Some(_) => Repo.setDelegationCall(d, call.entry, call.call)
      case None    => ZIO.unit
    }

  /** 发起会话被停止：它在等的委派都标成已交回（不再交回给它），返回这些委派。它开出的 thread 由停止方逐层停下。 */
  def abandonTx(requester: SessionId): Tx[List[DelegationRow]] =
    for
      asked <- Repo.openDelegationsFrom(requester)
      _     <- ZIO.foreachDiscard(asked)(d => Repo.finishDelegation(d.id, CallResult.error("[发起方被停止了]").toJson))
    yield asked

  /** thread 被删除：交给它的未完成委派以失败交回发起方。 */
  def threadGone(thread: SessionInfo): IO[AppError, Unit] =
    db.read(Repo.openDelegationsTo(thread.id)).flatMap(ZIO.foreachDiscard(_)(d => finish(d.id, thread, Outcome.Failed("这个 thread 被删除了"))))

  /** 心跳检查里写的 thread 现状。 */
  def status(d: DelegationId): IO[AppError, String] =
    db.read(Repo.delegation(d)).someOrFail(AppError.notFound(s"委派 ${d.value} 不存在")).flatMap { r =>
      for
        st     <- states.state(r.threadId)
        answer <- lastAnswer(r.threadId)
      yield
        val tail = if answer.isEmpty then "" else s"；最新回复：${answer.take(300)}"
        s"thread 现在${st.text}$tail"
    }

  /** 这些委派做它们的 thread 会话。 */
  def threadsOf(ds: List[DelegationId]): IO[AppError, Map[DelegationId, SessionId]] =
    db.read(Repo.delegationsOf(ds)).map(_.map(d => d.id -> d.threadId).toMap)

  /** 发起会话开出的 thread。 */
  def threads(requester: SessionId): IO[AppError, List[ThreadView]] =
    for
      rows <- db.read(Repo.threadsOf(requester))
      st   <- states.states(rows.map(_.id))
      out <- ZIO.foreach(rows) { r =>
               for
                 i      <- SessionInfo.decode(r)
                 agent  <- agents.get(r.agentId).map(_.name.value).orElseSucceed(r.agentId.value)
                 run    <- db.read(Repo.lastRun(r.id))
                 answer <- lastAnswer(r.id)
               yield ThreadView(
                 thread = ThreadName.wrap(threadName(i)),
                 agent = agent,
                 session = r.id,
                 state = st.getOrElse(r.id, WorkState.Idle),
                 failure = run.filter(_.state == RunState.Failed).flatMap(_.error),
                 answer = Option(answer).filter(_.nonEmpty)
               )
             }
    yield out

  /** 发起会话下的某个 thread（不给 agent 时按名字找，重名要指明 agent）。 */
  def findThread(requester: SessionId, name: ThreadName, agent: Option[AgentRecord]): IO[AppError, SessionRow] =
    db.read(Repo.threadsOf(requester)).flatMap { rows =>
      rows.filter(r => r.originRef.contains(name.value) && agent.forall(_.id == r.agentId)) match
        case List(one) => ZIO.succeed(one)
        case Nil       => ZIO.fail(AppError.notFound(s"这个会话没有开过叫「${name.value}」的 thread（list_threads 看有哪些）"))
        case _         => ZIO.fail(AppError.invalid(s"有好几个 agent 的 thread 都叫「${name.value}」，用 agent 参数指明是哪个"))
    }

private[sessions] object Delegations:
  val layer: URLayer[Kernel & Mailbox & Waits & States & Doorbell & Agents, Delegations] = ZLayer.derive[Delegations]
