package muliao.models.mcp

import io.modelcontextprotocol.json.{McpJsonMapper, TypeRef}
import io.modelcontextprotocol.spec.McpClientTransport
import io.modelcontextprotocol.spec.McpSchema
import io.modelcontextprotocol.spec.McpSchema.JSONRPCMessage
import muliao.exec.{AttachedCommand, Masker}
import reactor.core.publisher.Mono
import reactor.core.scheduler.Schedulers
import zio.*

import java.util.function.Function

/** 把平台执行层起的进程（[[AttachedCommand]]）接成 MCP SDK 的 stdio 传输：一行一条 JSON-RPC 消息。
  *
  * 不用 SDK 自带的 stdio 传输：它自己起进程，进不了写沙箱、也不能整组结束。进程的生命周期归调用方的 scope。
  */
private[mcp] final class ExecStdioTransport(
    command: AttachedCommand,
    mapper: McpJsonMapper,
    runtime: Runtime[Any],
    serverName: String,
    masker: Masker
) extends McpClientTransport:

  /** 标准错误最近几行（已打码）：连接失败时附在原因后面（如命令不存在）。 */
  private val tail: Ref[Chunk[String]] = Unsafe.unsafe(implicit u => Ref.unsafe.make(Chunk.empty[String]))

  def recentErrors: UIO[Chunk[String]] = tail.get

  private def unsafeRun[A](zio: ZIO[Any, Throwable, A]): A =
    Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrowFiberFailure())

  private def unsafeFork(zio: UIO[Unit]): Unit =
    Unsafe.unsafe(implicit u => runtime.unsafe.fork(zio)): Unit

  def connect(handler: Function[Mono[JSONRPCMessage], Mono[JSONRPCMessage]]): Mono[Void] =
    Mono.fromRunnable[Void] { () =>
      unsafeFork(
        command.stdout.runForeach { line =>
          ZIO
            .attempt(McpSchema.deserializeJsonRpcMessage(mapper, line))
            .foldZIO(
              e => ZIO.logWarning(s"MCP server $serverName 输出了读不懂的一行：${e.getMessage}"),
              msg => ZIO.succeed(handler.apply(Mono.just(msg)).subscribe(): Unit)
            )
        }
      )
      unsafeFork(command.stderr.map(masker.mask).runForeach { l =>
        tail.update(t => (t :+ l).takeRight(5)) *> ZIO.logDebug(s"MCP server $serverName：$l")
      })
    }

  def sendMessage(message: JSONRPCMessage): Mono[Void] =
    Mono
      .fromRunnable[Void](() => unsafeRun(ZIO.attempt(mapper.writeValueAsString(message)).flatMap(l => command.send(l))))
      .subscribeOn(Schedulers.boundedElastic())

  def closeGracefully(): Mono[Void] =
    Mono.fromRunnable[Void](() => unsafeRun(command.closeInput)).subscribeOn(Schedulers.boundedElastic())

  def unmarshalFrom[T](data: Object, typeRef: TypeRef[T]): T = mapper.convertValue(data, typeRef)
