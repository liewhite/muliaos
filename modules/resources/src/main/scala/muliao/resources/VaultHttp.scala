package muliao.resources

import muliao.contract.*
import muliao.platform.server.{Endpoints, Route}
import muliao.resources.tree.{AssetStore, SecretLock}
import muliao.resources.vault.{Grants, Slot}
import zio.*

/** passkey secret 授权的接口（#915）：界面列出等授权的请求，交回浏览器里解开的明文或拒绝，收回授权。 */
final case class VaultHttp(grants: Grants, store: AssetStore):
  import Endpoints.*

  private def state: IO[muliao.platform.AppError, VaultState] =
    for
      (asks, live) <- grants.view
      snap         <- store.snapshot
    yield
      val creds = snap.secrets.map(_.content.lock).collect { case SecretLock.Passkey(b) => PasskeyCredential(b.rp, b.credential) }.distinct
      VaultState(asks, live, creds)

  def routes: List[Route] =
    List(
      VaultApi.vaultState.secured(_ => state),
      VaultApi.answerAsk.secured(a => grants.answer(Slot(a.agent, a.key), a.decision)),
      VaultApi.revokeGrant.secured(r => grants.revoke(Slot(r.agent, r.key)))
    )
