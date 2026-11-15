package muliao.triggers

import muliao.contract.*
import muliao.platform.server.{Endpoints, Route}
import zio.*

/** 本领域的 HTTP 接口：列表、种类、建、改、立即运行、删。 */
final case class TriggersHttp(triggers: Triggers):
  import Endpoints.*

  def routes: List[Route] =
    List(
      TriggersApi.list.secured(finished => triggers.list(finished.getOrElse(false))),
      TriggersApi.kinds.secured(_ => ZIO.succeed(triggers.kinds)),
      TriggersApi.create.secured(triggers.create),
      TriggersApi.update.secured((id, patch) => triggers.update(id, patch)),
      TriggersApi.run.secured(triggers.run),
      TriggersApi.delete.secured(triggers.delete)
    )
