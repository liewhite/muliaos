package muliao.workspace.pages

import muliao.contract.{AgentId, PageView}
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

/** 页面链接的签名：页面在与界面隔离的环境里运行、拿不到登录 cookie，靠链接里的签名访问。签名只绑定 agent 与页面名，
  * 所以页面里的相对链接（脚本、样式、数据文件）沿用同一个前缀即可。
  */
final class PageSigner private (key: Array[Byte]):
  private def mac(text: String): Array[Byte] =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(text.getBytes(StandardCharsets.UTF_8)).take(16)

  def sign(agent: AgentId, page: String): String =
    mac(s"${agent.value}/$page").map(b => f"${b & 0xff}%02x").mkString

  def verify(agent: AgentId, page: String, sig: String): Boolean =
    MessageDigest.isEqual(sign(agent, page).getBytes(StandardCharsets.US_ASCII), sig.getBytes(StandardCharsets.US_ASCII))

object PageSigner:
  /** 从服务的签名密钥派生（与登录 cookie 的签名分开用途）。 */
  def fromSecret(secret: String): PageSigner =
    val m = Mac.getInstance("HmacSHA256")
    m.init(SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"))
    PageSigner(m.doFinal("muliao-pages".getBytes(StandardCharsets.UTF_8)))

/** 页面的一个文件。 */
final case class PageFile(contentType: String, bytes: Array[Byte])

/** agent 页面：工作目录的 `site/<page>/index.html` 及其附件。列出全部页面（带签名链接），按签名链接取文件。 */
trait Pages:
  def list: IO[AppError, List[PageView]]

  /** 取页面里的一个文件：签名不对、路径走出页面目录、文件不存在都当作不存在。`file` 为空时取 index.html。 */
  def file(agent: AgentId, page: String, sig: String, file: List[String]): IO[AppError, PageFile]

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

  def url(signer: PageSigner, agent: AgentId, page: String): String =
    s"/$Prefix/${enc(agent.value)}/${enc(page)}/${signer.sign(agent, page)}/index.html"

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

    def file(agent: AgentId, page: String, sig: String, file: List[String]): IO[AppError, PageFile] =
      val missing = AppError.notFound("页面文件不存在")
      val segs    = if file.isEmpty then List("index.html") else file
      if !signer.verify(agent, page, sig) || (page :: segs).exists(s => s.isEmpty || s == "." || s == ".." || s.contains('/')) then ZIO.fail(missing)
      else
        val root = siteDir(paths, agent).resolve(page)
        for
          real  <- RealPath.under(root, segs.foldLeft(root)(_.resolve(_))).someOrFail(missing)
          bytes <- ZIO
                     .attemptBlocking(Option.when(Files.isRegularFile(real.path))(Files.readAllBytes(real.path)))
                     .mapError(e => AppError.internal("读页面文件失败", e))
                     .someOrFail(missing)
        yield PageFile(contentType(segs.last), bytes)
