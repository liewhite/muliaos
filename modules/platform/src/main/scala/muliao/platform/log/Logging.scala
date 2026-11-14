package muliao.platform.log

import muliao.platform.config.ConfigCodecs.given
import muliao.platform.config.ConfigSection
import zio.*
import zio.json.*
import zio.logging.{ConsoleLoggerConfig, LogFilter, LogFormat, consoleLogger}
import zio.logging.slf4j.bridge.Slf4jBridge

import java.io.{BufferedWriter, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}

/** 配置文件 `[log]` 段。
  * @param level      最低级别：DEBUG、INFO、WARN、ERROR
  * @param file       日志文件；不写则打到标准输出
  * @param maxSizeMb  单个文件上限，超过就轮转
  * @param keep       轮转保留的旧文件份数（file.1 … file.N）
  */
@jsonMemberNames(SnakeCase)
final case class LogSettings(
    level: String = "INFO",
    file: Option[Path] = None,
    maxSizeMb: Int = 20,
    keep: Int = 5
) derives JsonCodec

object LogSettings:
  val section: ConfigSection[LogSettings] = ConfigSection("log")

object Logging:

  private val format: LogFormat =
    LogFormat.timestamp.fixed(32) |-| LogFormat.level.fixed(5) |-| LogFormat.fiberId.fixed(14) |-|
      LogFormat.annotations |-| LogFormat.line |-| LogFormat.cause

  private def parseLevel(s: String): Either[String, LogLevel] =
    List(LogLevel.Debug, LogLevel.Info, LogLevel.Warning, LogLevel.Error)
      .find(l => l.label.equalsIgnoreCase(s) || (s.equalsIgnoreCase("WARN") && l == LogLevel.Warning))
      .toRight(s"日志级别只能是 DEBUG、INFO、WARN、ERROR，不是 $s")

  /** 替换默认日志：按配置写文件（按大小轮转）或标准输出，并把 SLF4J（Flyway、连接池等库）接进来。 */
  def layer(s: LogSettings): ZLayer[Any, String, Unit] =
    ZLayer.fromZIO(ZIO.fromEither(parseLevel(s.level))).flatMap { env =>
      val level = env.get[LogLevel]
      val sink = s.file match
        case Some(path) => fileLogger(path, s.maxSizeMb.toLong * 1024 * 1024, s.keep, level)
        case None       => consoleLogger(ConsoleLoggerConfig(format, LogFilter.LogLevelByNameConfig(level)))
      Runtime.removeDefaultLoggers >>> sink >+> Slf4jBridge.initialize
    }

  private def fileLogger(path: Path, maxBytes: Long, keep: Int, level: LogLevel): ZLayer[Any, Nothing, Unit] =
    ZLayer
      .scoped(ZIO.acquireRelease(ZIO.succeed(RotatingFile(path, maxBytes, keep)))(f => ZIO.succeed(f.close())))
      .flatMap { env =>
        val file = env.get[RotatingFile]
        Runtime.addLogger(format.toLogger.map(file.writeLine).filterLogLevel(_ >= level))
      }

/** 追加写的日志文件，超过上限就把 `x.log` 依次挪成 `x.log.1`…`x.log.keep`。 */
final class RotatingFile(path: Path, maxBytes: Long, keep: Int):
  private var out: BufferedWriter = open()
  private var size: Long          = Files.size(path)

  private def open(): BufferedWriter =
    Option(path.getParent).foreach(Files.createDirectories(_))
    BufferedWriter(
      OutputStreamWriter(
        Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.APPEND),
        StandardCharsets.UTF_8
      )
    )

  def writeLine(line: String): Unit = synchronized {
    val bytes = line.getBytes(StandardCharsets.UTF_8).length + 1L
    if size > 0 && size + bytes > maxBytes then rotate()
    out.write(line)
    out.write('\n')
    out.flush()
    size += bytes
  }

  private def rotate(): Unit =
    out.close()
    def nth(i: Int) = path.resolveSibling(s"${path.getFileName}.$i")
    Files.deleteIfExists(nth(keep))
    (keep - 1 to 1 by -1).foreach { i =>
      if Files.exists(nth(i)) then Files.move(nth(i), nth(i + 1), StandardCopyOption.REPLACE_EXISTING): Unit
    }
    if keep > 0 then Files.move(path, nth(1), StandardCopyOption.REPLACE_EXISTING): Unit
    else Files.delete(path)
    out = open()
    size = 0

  def close(): Unit = synchronized(out.close())
