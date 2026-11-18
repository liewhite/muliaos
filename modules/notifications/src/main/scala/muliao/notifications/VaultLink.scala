package muliao.notifications

import muliao.contract.AskWant
import muliao.resources.agents.Agents
import muliao.resources.vault.{Asked, Grants}
import zio.*

/** 有 agent 开始等 passkey secret 的授权时通知用户（#915）：点开是那个会话（在那里看到要跑什么并批准）；没有会话的（触发器）去 Secret 页。 */
object VaultLink:
  /** 没有会话时的授权页（AI 资产的 Secret 标签）。 */
  val Page = "/ai-assets?kind=secret"

  /** 要拿去做什么只取第一行、截短（全文在会话或 Secret 页上）。 */
  def notice(agent: String, a: Asked): Notice =
    val first = a.purpose.what.linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
    val what  = if first.length > 120 then first.take(120) + "…" else first
    val where = a.purpose.session.fold("在 Secret 页")(_ => "点开在会话里")
    val (title, body) = a.want match
      case AskWant.Reveal     => (s"$agent 要用 secret ${a.slot.key}", s"要拿它跑：$what。${where}用 passkey 批准（一次或一段时间）或拒绝。")
      case AskWant.SshCert(r) => (s"$agent 要签 SSH 证书", s"可登 ${r.principals.mkString("、")}，${r.minutes} 分钟。${where}用 passkey 签发或拒绝。")
    Notice(title, body, a.purpose.session.fold(Target.Page(Page))(Target.Session(_)))

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
