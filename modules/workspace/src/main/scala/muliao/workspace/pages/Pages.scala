package muliao.workspace.pages

import muliao.contract.{AgentId, PageShare, PageShareTtl, PageView}
import muliao.exec.RealPath
import muliao.platform.AppError
import muliao.resources.ResourcePaths
import muliao.resources.agents.Agents
import zio.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** 一条页面链接的有效期：永久（界面内嵌打开用的链接、选「永久」的分享链接），或到某一刻（秒）为止。 */
enum PageGrant:
  case Permanent
  case Until(epochSecond: Long)

  def validAt(nowSecond: Long): Boolean = this match
    case Permanent => true
    case Until(t)  => nowSecond < t

/** 页面链接的签名：页面在与界面隔离的环境里运行、拿不到登录 cookie，靠链接里的签名段访问。签名只绑定 agent、页面名与有效期，
  * 所以页面里的相对链接（脚本、样式、数据文件）沿用同一个前缀即可。签名段的写法：永久是 `<签名>`（与加有效期前发出的链接相同），
  * 有期限的是 `<到期秒>.<签名>`；到期时刻在签名里，改了就对不上。
  */
final class PageSigner private (key: Array[Byte]):
  private def mac(text: String): String =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(text.getBytes(StandardCharsets.UTF_8)).take(16).map(b => f"${b & 0xff}%02x").mkString

  // 页面名、agent 编号里没有 `/`，永久与有期限的签名内容不会重合
  private def sig(agent: AgentId, page: String, grant: PageGrant): String = grant match
    case PageGrant.Permanent => mac(s"${agent.value}/$page")
    case PageGrant.Until(t)  => mac(s"${agent.value}/$page/$t")

  /** 链接里的签名段。 */
  def token(agent: AgentId, page: String, grant: PageGrant): String = grant match
    case PageGrant.Permanent => sig(agent, page, grant)
    case PageGrant.Until(t)  => s"$t.${sig(agent, page, grant)}"

  /** 解析签名段：签名对得上才给出它的有效期（是否已过期由调用方按当前时刻判断）。 */
  def verify(agent: AgentId, page: String, token: String): Option[PageGrant] =
    val parsed = token.split('.') match
      case Array(s)    => Some(PageGrant.Permanent -> s)
      case Array(t, s) => t.toLongOption.map(PageGrant.Until(_) -> s)
      case _           => None
    parsed.collect {
      case (grant, s) if MessageDigest.isEqual(sig(agent, page, grant).getBytes(StandardCharsets.US_ASCII), s.getBytes(StandardCharsets.US_ASCII)) => grant
    }

object PageSigner:
  /** 从服务的签名密钥派生（与登录 cookie 的签名分开用途）。 */
  def fromSecret(secret: String): PageSigner =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"))
    PageSigner(m.doFinal("muliao-pages".getBytes(StandardCharsets.UTF_8)))

/** 页面的一个文件。 */
final case class PageFile(contentType: String, bytes: Array[Byte])

/** agent 页面：工作目录的 `site/<page>/index.html` 及其附件。列出全部页面（带永久签名链接），生成有期限的分享链接，按签名链接取文件。 */
trait Pages:
  def list: IO[AppError, List[PageView]]

  /** 给一个页面生成分享链接（站内路径，前端补上自己的源）；页面不存在时报不存在。 */
  def share(agent: AgentId, page: String, ttl: PageShareTtl): IO[AppError, PageShare]

  /** 取页面里的一个文件：签名不对、已过期、路径走出页面目录、文件不存在都当作不存在。`file` 为空时取 index.html。 */
  def file(agent: AgentId, page: String, token: String, file: List[String]): IO[AppError, PageFile]

object Pages:
  /** 页面地址的前缀（不在 `/api` 下：页面不是平台接口）。 */
  val Prefix = "site"

  /** 页面响应的隔离头：在沙箱里以不透明来源运行（拿不到界面的 cookie 与存储，调用平台接口会被当作跨站），能跑脚本；
    * 允许跨源读取，页面才能用相对地址轮询自己的数据文件。
    */
  val IsolationHeaders: List[(String, String)] = List(
    "Content-Security-Policy"      -> "sandbox allow-scripts allow-forms allow-popups allow-modals allow-downloads",
    "X-Content-Type-Options"       -> "nosniff",
    "Referrer-Policy"              -> "no-referrer",
    "Access-Control-Allow-Origin"  -> "*",
    "Cache-Control"                -> "no-cache"
  )

  val layer: URLayer[Agents & ResourcePaths & PageSigner, Pages] = ZLayer.derive[Live]

  def siteDir(paths: ResourcePaths, agent: AgentId): Path = paths.workspace(agent).resolve("site")

  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8).replace("+", "%20")

  def url(signer: PageSigner, agent: AgentId, page: String, grant: PageGrant = PageGrant.Permanent): String =
    s"/$Prefix/${enc(agent.value)}/${enc(page)}/${signer.token(agent, page, grant)}/index.html"

  /** 分享有效期折成秒；None 是永久。一个月按 30 天。 */
  def lifetime(ttl: PageShareTtl): Option[Long] = ttl match
    case PageShareTtl.Hour    => Some(3600L)
    case PageShareTtl.Day     => Some(86400L)
    case PageShareTtl.Month   => Some(30 * 86400L)
    case PageShareTtl.Forever => None

  private def validName(s: String): Boolean = !(s.isEmpty || s == "." || s == ".." || s.contains('/'))

  def contentType(name: String): String =
    val ext = name.lastIndexOf('.') match
      case -1 => ""
      case i  => name.substring(i + 1).toLowerCase
    ext match
      case "html" | "htm" => "text/html; charset=utf-8"
      case "js" | "mjs"   => "text/javascript; charset=utf-8"
      case "css"          => "text/css; charset=utf-8"
      case "json"         => "application/json"
      case "csv"          => "text/csv; charset=utf-8"
      case "txt" | "log" | "md" => "text/plain; charset=utf-8"
      case "svg"          => "image/svg+xml"
      case "png"          => "image/png"
      case "jpg" | "jpeg" => "image/jpeg"
      case "gif"          => "image/gif"
      case "webp"         => "image/webp"
      case "wasm"         => "application/wasm"
      case _              => "application/octet-stream"

  final case class Live(agents: Agents, paths: ResourcePaths, signer: PageSigner) extends Pages:

    def list: IO[AppError, List[PageView]] =
      agents.list.flatMap { all =>
        ZIO.foreach(all) { a =>
          val site = siteDir(paths, a.id)
          ZIO
            .attemptBlocking {
              if !Files.isDirectory(site) then Nil
              else
                scala.util.Using.resource(Files.list(site)) { s =>
                  s.toArray.toList.map(_.asInstanceOf[Path]).filter(p => Files.isRegularFile(p.resolve("index.html"))).map { p =>
                    val page = p.getFileName.toString
                    PageView(a.id, a.name.value, page, url(signer, a.id, page), Files.getLastModifiedTime(p.resolve("index.html")).toMillis)
                  }
                }
            }
            .mapError(e => AppError.internal(s"列 $site 失败", e))
        }.map(_.flatten.sortBy(v => (-v.modifiedAt, v.page)))
      }

    def share(agent: AgentId, page: String, ttl: PageShareTtl): IO[AppError, PageShare] =
      val missing = AppError.notFound(s"没有这个页面：site/$page")
      for
        _ <- ZIO.fail(missing).unless(validName(page))
        _ <- ZIO
               .attemptBlocking(Files.isRegularFile(siteDir(paths, agent).resolve(page).resolve("index.html")))
               .mapError(e => AppError.internal("查页面失败", e))
               .filterOrFail(identity)(missing)
        now <- Clock.instant
        grant = lifetime(ttl).fold(PageGrant.Permanent)(s => PageGrant.Until(now.getEpochSecond + s))
      yield PageShare(
        url(signer, agent, page, grant),
        grant match
          case PageGrant.Permanent => None
          case PageGrant.Until(t)  => Some(t * 1000)
      )

    def file(agent: AgentId, page: String, token: String, file: List[String]): IO[AppError, PageFile] =
      val missing = AppError.notFound("页面文件不存在")
      val segs    = if file.isEmpty then List("index.html") else file
      if !(page :: segs).forall(validName) then ZIO.fail(missing)
      else
        val root = siteDir(paths, agent).resolve(page)
        for
          now   <- Clock.instant
          _     <- ZIO.fail(missing).unless(signer.verify(agent, page, token).exists(_.validAt(now.getEpochSecond)))
          real  <- RealPath.under(root, segs.foldLeft(root)(_.resolve(_))).someOrFail(missing)
          bytes <- ZIO
                     .attemptBlocking(Option.when(Files.isRegularFile(real.path))(Files.readAllBytes(real.path)))
                     .mapError(e => AppError.internal("读页面文件失败", e))
                     .someOrFail(missing)
        yield PageFile(contentType(segs.last), bytes)
