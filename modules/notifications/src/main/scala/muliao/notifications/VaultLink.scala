package muliao.notifications

import muliao.resources.agents.Agents
import muliao.resources.vault.{Asked, Grants}
import zio.*

/** 有 agent 开始等 passkey secret 的授权时通知用户（#915），点开是 Secret 页上的待授权列表。 */
object VaultLink:
  /** 授权页的前端路由（AI 资产的 Secret 标签）。 */
  val Page = "/ai-assets?kind=secret"

  def notice(agent: String, a: Asked): Notice =
    Notice(s"$agent 要用 secret ${a.slot.key}", s"用在${a.use.shown}，等你用 passkey 授权（一次或一段时间）或拒绝。", Target.Page(Page))

  /** 在调用方的 scope 里订阅并转发：返回时已订上。 */
  def start(grants: Grants, agents: Agents, n: Notifications): URIO[Scope, Unit] =
    grants.asked.flatMap { q =>
      q.take
        .flatMap(a => agents.names(List(a.slot.agent)).flatMap(name => n.notify(notice(name(a.slot.agent), a))))
        .catchAll(e => ZIO.logWarning(s"passkey 授权请求的通知没发出去：${e.getMessage}"))
        .forever
        .forkScoped
        .unit
    }
