package muliao.board

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.server.{ApiEndpoint, Auth, Endpoints}
import zio.*

/** 本领域的 HTTP 接口（用户在界面上的操作，都以用户的身份记）。 */
final case class BoardHttp(board: Board):
  import Endpoints.*

  private def patch(p: IssuePatch): IO[AppError, Patch] =
    if p.unassign && p.assignee.isDefined then ZIO.fail(AppError.invalid("assignee 与 unassign 只能给一个"))
    else ZIO.succeed(Patch(p.title, p.description, p.status, p.priority, p.owner, if p.unassign then Some(None) else p.assignee.map(Some(_)), p.dependsOn))

  def endpoints(auth: Auth): List[ApiEndpoint] =
    List(
      BoardApi.list.secured(auth)(_ => (statuses, assignee, owner) => board.list(Filter(statuses.toSet, assignee.map(Assignee.decode), owner))),
      BoardApi.get.secured(auth)(_ => board.show),
      BoardApi.create.secured(auth)(_ => n =>
        board
          .create(By.user, List(Draft(None, n.title, n.owner, n.description, n.status, n.priority, n.assignee, n.dependsOn.map(Upstream.Existing(_)))))
          .flatMap(ids => board.show(ids.head))
      ),
      BoardApi.update.secured(auth)(_ => (id, p) => patch(p).flatMap(board.update(By.user, id, _)) *> board.show(id)),
      BoardApi.comment.secured(auth)(_ => (id, c) => board.comment(By.user, id, c.body)),
      BoardApi.approve.secured(auth)(_ => board.approve),
      BoardApi.sendBack.secured(auth)(_ => (id, r) => board.sendBack(id, r.reason)),
      BoardApi.stop.secured(auth)(_ => (id, s) => ZIO.fromEither(Halt.make(s.status, s.message)).mapError(AppError.invalid).flatMap(board.stop(By.user, id, _))),
      BoardApi.start.secured(auth)(_ => board.start),
      BoardApi.delete.secured(auth)(_ => board.delete)
    )

object BoardHttp:
  val layer: URLayer[Board, BoardHttp] = ZLayer.derive[BoardHttp]
