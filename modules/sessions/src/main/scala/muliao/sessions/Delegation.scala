package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.db.Tx
import muliao.resources.agents.AgentRecord
import muliao.sessions.store.*
import zio.*
import zio.json.*

/** 委派方式：回答推给用户、作为事件交回发起方、发起方的调用等着回答。 */
enum DelegationMode(val label: String):
  case Notify extends DelegationMode("notify")
  case Report extends DelegationMode("report")
  case Ask    extends DelegationMode("ask")

/** `list_threads` 的一行。 */
final case class ThreadView(thread: String, agent: String, session: String, state: String, failure: Option[String], answer: Option[String])
    derives JsonEncoder

/** 委派与 thread：交给某 agent 的 thread（同名续聊），thread 回到空闲、失败或被停止时把结局交回。 */
private[sessions] trait DelegationPart:
  this: Engine =>
  import deps.*

  val DelegationKind = "delegation"

  /** 交给 `target` 在发起会话下的 thread `name`。返回 thread 会话与委派编号（Notify 没有编号）。 */
  def delegate(
      requester: RunContext,
      target: AgentRecord,
      name: ThreadName,
      task: String,
      mode: DelegationMode,
      heartbeat: Option[Duration]
  ): IO[AppError, (SessionId, Option[Long])] =
    for
      _ <- agents.requireActive(target.id)
      t <- now
      source = Source.Delegation(requester.session.value, requester.agentName.value)
      out <- db.write(
               mode match
                 case DelegationMode.Notify =>
                   deliverTx(Address.ThreadOf(requester.session, target.id, name), Letter(source, task, replyTo = Some(ReplyTo.User)), None, t)
                     .someOrFail(AppError.Internal("投递委派失败"))
                     .map(d => (d.session, Option.empty[Long]))
                 case _ =>
                   for
                     (thread, _) <- locateTx(Address.ThreadOf(requester.session, target.id, name), source, t)
                     d <- Repo.insertDelegation(DelegationRow(0L, requester.session, thread.id, mode.label, None, None, false, None, t))
                     _ <- enqueueTx(thread, Letter(source, task, replyTo = Some(ReplyTo.Delegation(d))), None, t)
                     _ <- openWaitTx(requester.session, DelegationKind, d.toString, s"委派给 ${target.name.value} 的 thread「${name.value}」", heartbeat, None, t)
                            .when(mode == DelegationMode.Report)
                   yield (thread.id, Option(d))
             )
      _ <- kick(out._1)
    yield out

  /** thread 的一段工作有了结局：交回发起方（作为事件，或作为它等着的调用的结果）。已交回过的不再交。 */
  def finishDelegation(d: Long, thread: SessionInfo, outcome: Outcome): IO[AppError, Unit] =
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
                             case "report" => closeWaitTx(DelegationKind, d.toString, Some(Letter(Source.Event("delegation"), reportText(agent, name, outcome))), t)
                             case "ask"    => resolveAsk(r, result, t)
                             case _        => ZIO.none
                       }
                     }
                   )
      _ <- ZIO.foreachDiscard(requester)(kick)
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
          case Some(row) if row.state == CallState.Suspended.label =>
            Repo.setCall(e, c, CallState.Done.label, row.pause, Some(result.toJson), row.startedAt, Some(t)) *> Repo.touch(r.requesterId, t) *> changed.as(Some(r.requesterId))
          case _ => ZIO.none
        }
      case _ => ZIO.none

  /** 发起方的 `ask_agent` 调用挂起时挂上委派；thread 已经做完了就直接拿结果。 */
  def linkDelegation(d: Long, entry: Long, call: String, t: Long): Tx[Unit] =
    Repo.delegation(d).flatMap {
      case Some(r) if r.done =>
        for
          row <- Repo.call(entry, call)
          res  = r.result.getOrElse(CallResult.error("[没有结果]").toJson)
          _   <- ZIO.foreachDiscard(row)(x => Repo.setCall(entry, call, CallState.Done.label, x.pause, Some(res), x.startedAt, Some(t)))
        yield ()
      case Some(_) => Repo.setDelegationCall(d, entry, call)
      case None    => ZIO.unit
    }

  /** 心跳检查里写的 thread 现状。 */
  def delegationStatus(d: Long): IO[AppError, String] =
    db.read(Repo.delegation(d)).someOrFail(AppError.notFound(s"委派 $d 不存在")).flatMap { r =>
      for
        st     <- state(r.threadId)
        answer <- lastAnswer(r.threadId)
      yield
        val s = st match
          case WorkState.Running => "运行中"
          case WorkState.Waiting => "等待中"
          case WorkState.Idle    => "空闲"
        val tail = if answer.isEmpty then "" else s"；最新回复：${answer.take(300)}"
        s"thread 现在$s$tail"
    }

  /** 发起方放弃这件委派（被停止）：标成已交回，停下 thread。 */
  def cancelDelegation(d: Long): IO[AppError, Unit] =
    db.read(Repo.delegation(d)).flatMap {
      case None => ZIO.unit
      case Some(r) =>
        db.write(Repo.finishDelegation(d, CallResult.error("[发起方放弃了]").toJson)) *> stop(r.threadId, byUser = false)
    }

  /** 发起会话开出的 thread。 */
  def threads(requester: SessionId): IO[AppError, List[ThreadView]] =
    for
      rows <- db.read(Repo.threadsOf(requester))
      st   <- states(rows.map(_.id))
      out <- ZIO.foreach(rows) { r =>
               for
                 i      <- SessionInfo.decode(r)
                 agent  <- agents.get(r.agentId).map(_.name.value).orElseSucceed(r.agentId.value)
                 run    <- db.read(Repo.lastRun(r.id))
                 answer <- lastAnswer(r.id)
               yield ThreadView(
                 thread = threadName(i),
                 agent = agent,
                 session = r.id.value,
                 state = st.getOrElse(r.id, WorkState.Idle).label,
                 failure = run.filter(_.state == RunState.Failed.label).flatMap(_.error),
                 answer = Option(answer).filter(_.nonEmpty)
               )
             }
    yield out

  /** 发起会话下的某个 thread（不给 agent 时按名字找，重名要指明 agent）。 */
  def findThread(requester: SessionId, name: String, agent: Option[AgentRecord]): IO[AppError, SessionRow] =
    db.read(Repo.threadsOf(requester)).flatMap { rows =>
      rows.filter(r => r.originRef.contains(name) && agent.forall(_.id == r.agentId)) match
        case List(one) => ZIO.succeed(one)
        case Nil       => ZIO.fail(AppError.notFound(s"这个会话没有开过叫「$name」的 thread（list_threads 看有哪些）"))
        case _         => ZIO.fail(AppError.invalid(s"有好几个 agent 的 thread 都叫「$name」，用 agent 参数指明是哪个"))
    }
