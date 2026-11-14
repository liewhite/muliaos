package muliao.models.llm

import zio.*
import zio.stream.*

/** Server-Sent Events 的一条事件。 */
final case class SseEvent(event: Option[String], data: String)

object Sse:
  /** 字节流 → 事件流：按空行分隔，多行 `data:` 用换行拼接，注释行（`:` 开头）与没有 data 的事件忽略。 */
  val decode: ZPipeline[Any, Throwable, Byte, SseEvent] =
    ZPipeline.utf8Decode >>> ZPipeline.splitLines >>> lines

  private final case class Pending(event: Option[String], data: Chunk[String]):
    def toEvent: Option[SseEvent] = Option.when(data.nonEmpty)(SseEvent(event, data.mkString("\n")))

  private val emptyPending = Pending(None, Chunk.empty)

  private def field(line: String): (String, String) =
    line.indexOf(':') match
      case -1 => (line, "")
      case i  => (line.take(i), line.drop(i + 1).stripPrefix(" "))

  private val lines: ZPipeline[Any, Nothing, String, SseEvent] =
    ZPipeline.fromChannel {
      def loop(p: Pending): ZChannel[Any, ZNothing, Chunk[String], Any, Nothing, Chunk[SseEvent], Any] =
        ZChannel.readWithCause(
          (in: Chunk[String]) =>
            val (next, out) = in.foldLeft((p, Chunk.empty[SseEvent])) { case ((cur, acc), line) =>
              if line.isEmpty then (emptyPending, acc ++ Chunk.fromIterable(cur.toEvent))
              else if line.startsWith(":") then (cur, acc)
              else
                field(line) match
                  case ("event", v) => (cur.copy(event = Some(v)), acc)
                  case ("data", v)  => (cur.copy(data = cur.data :+ v), acc)
                  case _            => (cur, acc)
            }
            ZChannel.write(out) *> loop(next),
          (cause: Cause[ZNothing]) => ZChannel.refailCause(cause),
          (_: Any) => p.toEvent.fold(ZChannel.unit)(e => ZChannel.write(Chunk.single(e)))
        )
      loop(emptyPending)
    }
