package muliao.platform.http

import muliao.platform.AppError
import zio.*

/** 代理服务器地址。 */
final case class ProxyServer(host: String, port: Int)

/** macOS 系统代理设置（系统设置 › 网络 › 代理），来自 `scutil --proxy`。
  *
  * @param exceptions 「忽略这些主机与域的代理设置」：主机名通配（`*.local`）、IP、CIDR（`10.0.0.0/8`、`169.254/16`）
  */
final case class ProxySettings(
    http: Option[ProxyServer],
    https: Option[ProxyServer],
    exceptions: List[String],
    excludeSimpleHostnames: Boolean
):
  /** 访问这个地址该走哪个代理；`None` 为直连。 */
  def forUrl(scheme: String, host: String): Option[ProxyServer] =
    val server = if scheme.equalsIgnoreCase("https") then https else http
    server.filterNot(_ => bypass(host))

  private def bypass(host: String): Boolean =
    val h = host.toLowerCase.stripPrefix("[").stripSuffix("]")
    h == "localhost" || ProxySettings.isLoopback(h) || (excludeSimpleHostnames && !h.contains('.') && !h.contains(':')) ||
    exceptions.exists(ProxySettings.matches(_, h))

object ProxySettings:
  val direct: ProxySettings = ProxySettings(None, None, Nil, false)

  /** 解析 `scutil --proxy` 的输出。 */
  def parse(out: String): ProxySettings =
    val lines  = out.linesIterator.map(_.trim).toList
    val scalar = lines.flatMap { l =>
      l.split(" : ", 2) match
        case Array(k, v) if !v.startsWith("<") => Some(k -> v)
        case _                                 => None
    }.toMap
    val exceptions = lines
      .dropWhile(!_.startsWith("ExceptionsList"))
      .drop(1)
      .takeWhile(_ != "}")
      .flatMap(_.split(" : ", 2).lift(1))
    def server(prefix: String) =
      for
        _    <- scalar.get(s"${prefix}Enable").filter(_ == "1")
        host <- scalar.get(s"${prefix}Proxy")
        port <- scalar.get(s"${prefix}Port").flatMap(_.toIntOption)
      yield ProxyServer(host, port)
    ProxySettings(server("HTTP"), server("HTTPS"), exceptions, scalar.get("ExcludeSimpleHostnames").contains("1"))

  private def isLoopback(h: String): Boolean =
    h.startsWith("127.") || h == "::1"

  /** 一条例外是否命中主机 `h`（已小写）。 */
  private[http] def matches(pattern: String, h: String): Boolean =
    val p = pattern.trim.toLowerCase
    p.split('/') match
      case Array(net, bits) => bits.toIntOption.exists(b => inCidr(net, b, h))
      case _ if p.contains('*') =>
        val re = ("\\Q" + p.replace("*", "\\E.*\\Q") + "\\E").r
        re.matches(h)
      case _ => p == h || (p.startsWith(".") && h.endsWith(p))

  /** `169.254/16` 这类省略写法补足四段再比。只比 IPv4 字面量，不解析域名。 */
  private def inCidr(net: String, bits: Int, h: String): Boolean =
    def ipv4(s: String): Option[Long] =
      val parts = s.split('.').toList
      Option.when(parts.nonEmpty && parts.size <= 4 && parts.forall(_.forall(_.isDigit)) && parts.forall(_.nonEmpty)) {
        (parts ++ List.fill(4 - parts.size)("0")).map(_.toLong).foldLeft(0L)((acc, x) => acc * 256 + x)
      }
    val hostIsIp = h.split('.').length == 4 && h.forall(c => c.isDigit || c == '.')
    (for
      n    <- ipv4(net)
      addr <- if hostIsIp then ipv4(h) else None
      mask  = if bits == 0 then 0L else (0xffffffffL << (32 - bits)) & 0xffffffffL
    yield (n & mask) == (addr & mask)).getOrElse(false)

/** 当前的系统代理设置；定期从系统重读（用户改了代理不用重启）。 */
trait SystemProxy:
  def current: UIO[ProxySettings]

object SystemProxy:
  /** 固定设置（测试、或不用系统代理时）。 */
  def fixed(s: ProxySettings): ULayer[SystemProxy] = ZLayer.succeed(new SystemProxy { def current = ZIO.succeed(s) })

  /** 每隔 `every` 重读一次 `scutil --proxy`；读失败沿用上一次的设置。 */
  def macOS(every: Duration = 1.minute): ZLayer[Any, Nothing, SystemProxy] =
    ZLayer.scoped {
      for
        first <- read.catchAll(e => ZIO.logWarning(s"读系统代理失败，先直连：${e.message}").as(ProxySettings.direct))
        ref   <- Ref.make(first)
        _ <- read
               .flatMap(ref.set)
               .catchAll(e => ZIO.logWarning(s"重读系统代理失败，沿用上次的：${e.message}"))
               .repeat(Schedule.spaced(every))
               .delay(every)
               .forkScoped
      yield new SystemProxy { def current = ref.get }
    }

  private val read: IO[AppError, ProxySettings] =
    ZIO
      .attemptBlocking {
        val p   = ProcessBuilder("/usr/sbin/scutil", "--proxy").redirectErrorStream(true).start()
        val out = String(p.getInputStream.readAllBytes(), "UTF-8")
        p.waitFor()
        out
      }
      .timeoutFail(new RuntimeException("scutil 超时"))(10.seconds)
      .mapBoth(e => AppError.internal(s"scutil --proxy 失败：${e.getMessage}", e), ProxySettings.parse)

