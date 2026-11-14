package muliao.triggers

import muliao.contract.*
import muliao.platform.server.{ApiEndpoint, Auth, Endpoints}
import zio.*

/** 本领域的 HTTP 接口：列表、种类、建、改、立即运行、删。 */
final case class TriggersHttp(triggers: Triggers):
  import Endpoints.*

  def endpoints(auth: Auth): List[ApiEndpoint] =
    List(
      TriggersApi.list.secured(auth)(_ => finished => triggers.list(finished.getOrElse(false))),
      TriggersApi.kinds.secured(auth)(_ => _ => ZIO.succeed(triggers.kinds)),
      TriggersApi.create.secured(auth)(_ => triggers.create),
      TriggersApi.update.secured(auth)(_ => (id, patch) => triggers.update(id, patch)),
      TriggersApi.run.secured(auth)(_ => triggers.run),
      TriggersApi.delete.secured(auth)(_ => triggers.delete)
    )

object TriggersHttp:
  val layer: URLayer[Triggers, TriggersHttp] = ZLayer.derive[TriggersHttp]
