package muliao.platform.server

import sttp.model.{Header, HeaderNames, MediaType, StatusCode}
import sttp.tapir.DecodeResult
import sttp.tapir.ztapir.*
import zio.*

import java.nio.file.{Files, Path}

/** 前端静态文件：开发时来自目录，打包后来自程序里的 `web/` 资源。`/api` 下的路径在它之前由统一的 404 接住；
  * 其余找不到的路径回 `index.html`（前端路由）。
  */
object StaticFiles:

  private val nonApiPaths =
    paths.mapDecode(segs =>
      if segs.exists(s => s == ".." || s.startsWith(".")) then DecodeResult.Mismatch("普通路径", segs.mkString("/"))
      else DecodeResult.Value(segs)
    )(identity)

  private val endpointDef =
    endpoint.get
      .in(nonApiPaths)
      .out(statusCode)
      .out(headers)
      .out(byteArrayBody)

  /** @param dir 前端文件目录；`None` 时读程序里的 `web/` 资源 */
  def serverEndpoint(dir: Option[Path]): ApiEndpoint =
    endpointDef.zServerLogic[Any] { segs =>
      val wanted = if segs.isEmpty then List("index.html") else segs
      (read(dir, wanted) <> read(dir, List("index.html")).map(_.copy(_2 = "index.html")))
        .map { (bytes, name) =>
          (StatusCode.Ok, List(Header(HeaderNames.ContentType, contentType(name)), cacheControl(name)), bytes)
        }
        .orElseSucceed((StatusCode.NotFound, List(Header(HeaderNames.ContentType, "text/plain; charset=utf-8")), "前端文件不存在".getBytes("UTF-8")))
    }

  private def read(dir: Option[Path], segs: List[String]): IO[Unit, (Array[Byte], String)] =
    val name = segs.last
    dir match
      case Some(d) =>
        val f = segs.foldLeft(d)(_.resolve(_))
        ZIO.attemptBlocking(Option.when(Files.isRegularFile(f))(Files.readAllBytes(f))).orElseFail(()).someOrFail(()).map(_ -> name)
      case None =>
        ZIO
          .attemptBlocking(Option(getClass.getClassLoader.getResourceAsStream(("web" :: segs).mkString("/"))).map(in =>
            try in.readAllBytes() finally in.close()
          ))
          .orElseFail(())
          .someOrFail(())
          .map(_ -> name)

  private def contentType(name: String): String =
    val ext = name.lastIndexOf('.') match
      case -1 => ""
      case i  => name.substring(i + 1).toLowerCase
    ext match
      case "html"        => MediaType.TextHtml.charset("utf-8").toString
      case "js" | "mjs"  => "text/javascript; charset=utf-8"
      case "css"         => MediaType.TextCss.charset("utf-8").toString
      case "json"        => MediaType.ApplicationJson.toString
      case "webmanifest" => "application/manifest+json"
      case "svg"         => "image/svg+xml"
      case "png"         => "image/png"
      case "ico"         => "image/x-icon"
      case "wasm"        => "application/wasm"
      case "map"         => MediaType.ApplicationJson.toString
      case _             => Option(java.net.URLConnection.guessContentTypeFromName(name)).getOrElse("application/octet-stream")

  /** index.html 不缓存（换版本后立即生效），其余资源短缓存。 */
  private def cacheControl(name: String): Header =
    Header(HeaderNames.CacheControl, if name == "index.html" then "no-cache" else "public, max-age=300")
