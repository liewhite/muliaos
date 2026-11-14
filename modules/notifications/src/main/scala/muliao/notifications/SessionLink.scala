package muliao.notifications

import muliao.contract.SessionId
import muliao.platform.AppError
import muliao.resources.agents.Agents
import muliao.sessions.*
import muliao.toolkit.*
import zio.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.description

/** 把本领域接到会话领域：订阅会话事件发通知、会话被删时删掉指向它的通知、会话的未读、`notify_user` 工具。组装时在
  * `Sessions.start` 之前调用 [[attach]]。
  */
object SessionLink:

  final case class NotifyIn(
      @description("短标题") title: String,
      @description("通知正文，至多几句话") body: String
  )
  given Schema[NotifyIn] = DeriveSchema.gen[NotifyIn]

  /** 本领域给 agent 的工具名（组装时汇进全部内置工具目录）。 */
  val toolNames: List[String] = List("notify_user")

  def notifyUser(n: Notifications): Tool[RunContext] =
    Tool[RunContext, NotifyIn]("notify_user", "给用户的设备发一条推送通知，点开打开这个会话。", Access.Write) { (in, ctx) =>
      n.notify(Notice(in.title, in.body, Target.Session(ctx.session)))
        .mapBoth(ToolFailure.fromAppError, _ => ToolOutcome.text("已通知用户"))
    }

  def attach: URIO[Notifications & SessionHooks & Sessions & Agents, Unit] =
    for
      n        <- ZIO.service[Notifications]
      sessions <- ZIO.service[Sessions]
      agents   <- ZIO.service[Agents]
      hooks    <- ZIO.service[SessionHooks]
      _        <- hooks.addTools(List(notifyUser(n)))
      _        <- hooks.addListener(listener(n, sessions, agents))
      _        <- hooks.setUnread(unread(n))
    yield ()

  private def listener(n: Notifications, sessions: Sessions, agents: Agents): SessionListener = e =>
    val work: IO[AppError, Unit] = e match
      case SessionEvent.Deleted(id, _, _) => n.forgetSession(id)
      case _ =>
        ZIO.foreachDiscard(SessionRules.wanted(e)) { w =>
          for
            info <- sessions.info(w.session)
            name <- agents.get(info.agent).map(_.name.value).orElseSucceed(info.agent.value)
            _    <- n.notify(SessionRules.notice(w, name, info.title))
          yield ()
        }
    work.catchAll(err => ZIO.logError(s"会话事件没能转成通知：${err.message}"))

  private def unread(n: Notifications): UnreadSource = new UnreadSource:
    def unread(ids: List[SessionId]): UIO[Set[SessionId]] =
      n.unreadSessions(ids).catchAll(e => ZIO.logError(s"查会话未读失败：${e.message}").as(Set.empty))
    def markRead(id: SessionId): UIO[Unit] =
      n.readPage(Target.sessionUrl(id)).catchAll(e => ZIO.logError(s"会话标已读失败：${e.message}"))
