package muliao.board

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.server.{Endpoints, Route}
import zio.*

/** 本领域的 HTTP 接口（用户在界面上的操作，都以用户的身份记）。 */
final case class BoardHttp(board: Board):
  import Endpoints.*

  private def patch(p: IssuePatch): IO[AppError, Patch] =
    if p.unassign && p.assignee.isDefined then ZIO.fail(AppError.invalid("assignee 与 unassign 只能给一个"))
    else ZIO.succeed(Patch(p.title, p.description, p.status, p.priority, p.owner, if p.unassign then Some(None) else p.assignee.map(Some(_)), p.dependsOn))

  def routes: List[Route] =
    List(
      BoardApi.list.secured(q => board.list(Filter(q.status.toSet, q.assignee, q.owner))),
      BoardApi.get.secured(board.show),
      BoardApi.create.secured(n =>
        board
          .create(By.user, List(Draft(None, n.title, n.owner, n.description, n.status, n.priority, n.assignee, n.dependsOn.map(Upstream.Existing(_)))))
          .flatMap(ids => board.show(ids.head))
      ),
      BoardApi.update.secured((id, p) => patch(p).flatMap(board.update(By.user, id, _)) *> board.show(id)),
      BoardApi.comment.secured((id, c) => board.comment(By.user, id, c.body)),
      BoardApi.approve.secured(board.approve),
      BoardApi.sendBack.secured((id, r) => board.sendBack(id, r.reason)),
      BoardApi.stop.secured((id, s) => ZIO.fromEither(Halt.make(s.status, s.message)).mapError(AppError.invalid).flatMap(board.stop(By.user, id, _))),
      BoardApi.start.secured(board.start),
      BoardApi.delete.secured(board.delete)
    )
