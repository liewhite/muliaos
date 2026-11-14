package muliao.notifications

import muliao.contract.SessionId
import muliao.platform.config.ConfigCodecs.given
import muliao.platform.config.ConfigSection
import muliao.sessions.{Outcome, SessionEvent, UserNeed}
import zio.json.*

import java.nio.file.Path

/** 点开通知去哪。 */
enum Target:
  /** 关于一个会话：点开去会话页；打开会话即已读，会话删了通知随之删除 */
  case Session(id: SessionId)
  /** 发送方给的页面（前端路由，如请用户审核的 issue 页）：打开这个页面即已读 */
  case Page(path: String)
  /** 系统通知（如测试推送）：点开看通知页 */
  case Inbox

  def url: Option[String] = this match
    case Session(id) => Some(Target.sessionUrl(id))
    case Page(p)     => Some(p)
    case Inbox       => None

  def session: Option[SessionId] = this match
    case Session(id) => Some(id)
    case _           => None

object Target:
  /** 会话页的前端路由。 */
  def sessionUrl(id: SessionId): String = s"/sessions/${id.value}"

/** 要发给用户的一条通知。 */
final case class Notice(title: String, body: String, target: Target)

/** 会话事件里哪些要通知用户（规则只在这里）。 */
object SessionRules:
  /** 运行失败的通知正文（错误原文）最多这么多字，全文在会话页。 */
  val FailureBodyMax = 500

  /** 要通知的事：哪个会话、正文、标题后缀。标题前面的「agent · 会话标题」由调用方查了再补。 */
  final case class Wanted(session: SessionId, body: String, suffix: String)

  /** 用户在等的回答完成、在等的会话停下来提问；等批准总要通知（没人在看的触发器、issue 会话也只有用户能批）；最终失败无论谁在等都通知。
    * 被停止是用户自己点的，不通知。
    */
  def wanted(e: SessionEvent): Option[Wanted] = e match
    case SessionEvent.Settled(s, _, _, Outcome.Answered(a), true) => Some(Wanted(s, a, ""))
    case SessionEvent.Settled(_, _, _, Outcome.Answered(_), false) => None
    case SessionEvent.Settled(s, _, _, Outcome.Failed(err), _)     => Some(Wanted(s, clip(err.trim, FailureBodyMax), " 运行失败"))
    case SessionEvent.Settled(_, _, _, Outcome.Stopped(_), _)      => None
    case SessionEvent.NeedsUser(s, _, n: UserNeed.Approval, _)     => Some(Wanted(s, n.text, ""))
    case SessionEvent.NeedsUser(s, _, n: UserNeed.Answers, true)   => Some(Wanted(s, n.text, ""))
    case SessionEvent.NeedsUser(_, _, _: UserNeed.Answers, false)  => None
    case SessionEvent.Deleted(_, _, _)                             => None

  def notice(w: Wanted, agentName: String, title: String): Notice =
    Notice(s"$agentName · $title${w.suffix}", w.body, Target.Session(w.session))

/** 按字（码点）截断，截了补「…」。 */
def clip(s: String, max: Int): String =
  if s.codePointCount(0, s.length) <= max then s else s.substring(0, s.offsetByCodePoints(0, max)) + "…"

/** VAPID 声明里的联系方式：Web Push 协议要求是 `mailto:` 或 `https:` 地址。 */
opaque type VapidSubject = String

object VapidSubject:
  def make(s: String): Either[String, VapidSubject] =
    Either.cond(s.startsWith("mailto:") || s.startsWith("https://"), s, s"vapid_subject 应以 mailto: 或 https:// 开头：$s")
  extension (s: VapidSubject) def value: String = s
  given JsonCodec[VapidSubject] = JsonCodec.string.transformOrFail(make, identity)

/** 配置文件 `[push]` 段。
  * @param vapidSubject  VAPID 声明里的联系方式
  * @param vapidKeyFile  VAPID 私钥（PKCS#8 PEM）；默认数据目录下的 `vapid_private.pem`，没有就生成
  */
@jsonMemberNames(SnakeCase)
final case class PushSettings(vapidSubject: VapidSubject, vapidKeyFile: Option[Path] = None) derives JsonCodec

object PushSettings:
  val section: ConfigSection[PushSettings] = ConfigSection("push")
