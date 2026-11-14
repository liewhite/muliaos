package muliao.web.pages.notifications

import com.raquo.laminar.api.L.*
import muliao.contract.{NotificationId, NotificationView, NotificationsApi, ReadThrough}
import muliao.web.core.*
import muliao.web.ui.*
import org.scalajs.dom

import Http.ec

object NotificationsPage:
  def view(): HtmlElement =
    val inbox = Remote("notifications")(Http.call(NotificationsApi.list)(None))
    // 这次进页面时见过的未读：自己标了已读、列表重拉后它们仍显示未读点，让用户看出哪些是新的（下次进页面才消失）
    val seenUnread = Var(Set.empty[NotificationId])
    Ui.page("通知")(
      Ui.loading(inbox) { data =>
        val items = data.map(_.items)
        Seq(
          items --> (l => seenUnread.update(_ ++ l.filter(_.unread).map(_.id))),
          // 看到了就是已读：只标到这次列出的最新一条未读，之后才到的保持未读
          items.map(_.find(_.unread).map(_.id)).distinct --> (_.foreach(markRead)),
          child.maybe <-- items.map(l => Option.when(l.isEmpty)(Ui.emptyHint("没有通知"))),
          div(
            cls := "flex flex-col gap-2",
            children <-- items.splitSeq(_.id)(n => item(n.now(), seenUnread.signal.map(_.contains(n.now().id))))
          )
        )
      }
    )

  private def markRead(id: NotificationId): Unit =
    Http.call(NotificationsApi.readThrough)(ReadThrough(id)).failed.foreach(e => dom.console.error("marking notifications read failed", e.getMessage))

  /** 一条通知：有 url 的点开去那里（返回回到通知页）；系统通知没有 url，留在本页。 */
  private def item(n: NotificationView, seen: Signal[Boolean]): HtmlElement =
    val unread = seen.map(_ || n.unread)
    def body(hover: Boolean) =
      div(
        cls := "card card--default" + (if hover then " transition-colors hover:bg-surface-hover" else ""),
        dataAttr("slot") := "card",
        div(
          cls := "card__content flex flex-row items-start gap-3",
          div(
            cls := "min-w-0 flex-1",
            div(
              cls := "flex items-center gap-2 font-medium",
              child.maybe <-- unread.map(u =>
                Option.when(u)(span(cls := "contents", span(cls := "size-2 shrink-0 rounded-full bg-accent", aria.hidden := true), span(cls := "sr-only", "未读")))
              ),
              span(cls := "truncate", n.title)
            ),
            div(cls := "mt-1 line-clamp-6 whitespace-pre-wrap break-words text-sm text-muted", n.body),
            div(cls := "mt-1 text-xs text-muted", Ui.formatTime(n.createdAt))
          ),
          Option.when(hover)(Icon("ChevronRight", "mt-0.5 size-4 shrink-0 text-muted"))
        )
      )
    n.url match
      case Some(url) => Ui.appLink(url)(cls := "block", body(hover = true))
      case None      => body(hover = false)
