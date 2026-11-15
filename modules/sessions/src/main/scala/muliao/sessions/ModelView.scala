package muliao.sessions

import muliao.contract.{CallId, CallState, FileId, RunId}
import muliao.models.llm.{Message, ReplyPart, UserPart}
import muliao.platform.AppError
import muliao.sessions.store.{EntryRow, ToolCallRow, UploadRow}
import muliao.toolkit.ToolContent
import zio.*
import zio.json.*

import java.nio.file.Path

/** 时间线里的一条（解码后）。 */
final case class Entry(id: Long, run: Option[RunId], body: EntryBody, startedAt: Long, finishedAt: Long)

/** 一次工具调用（解码后）。 */
final case class Call(entryId: Long, callId: CallId, seq: Int, name: String, args: String, status: CallStatus):
  def key: CallKey = CallKey(entryId, callId)

private[sessions] object Decode:
  private def json[A: JsonDecoder](what: String, s: String): IO[AppError, A] =
    ZIO.fromEither(s.fromJson[A]).mapError(e => AppError.Internal(s"库里的$what 解不开：$e"))

  def entry(r: EntryRow): IO[AppError, Entry] =
    json[EntryBody]("时间线", r.body).map(Entry(r.id, r.runId, _, r.startedAt, r.finishedAt))

  def entries(rs: List[EntryRow]): IO[AppError, List[Entry]] = ZIO.foreach(rs)(entry)

  def call(r: ToolCallRow): IO[AppError, Call] =
    status(r).map(Call(r.entryId, r.callId, r.seq, r.name, r.args, _))

  /** 库里平铺的状态列还原成 [[CallStatus]]（写入只经 [[CallTx.save]]，对不上的组合只可能来自库被外部改过）。 */
  private def status(r: ToolCallRow): IO[AppError, CallStatus] =
    def pause = ZIO.fromOption(r.pause).orElseFail(broken(r)).flatMap(json[Pause]("挂起", _))
    def started = ZIO.fromOption(r.startedAt).orElseFail(broken(r))
    (r.state, r.result, r.finishedAt) match
      case (CallState.Pending, _, _)   => ZIO.succeed(CallStatus.Pending)
      case (CallState.Running, _, _)   => started.map(CallStatus.Running(_))
      case (CallState.Suspended, _, _) => pause.zipWith(started)(CallStatus.Suspended(_, _))
      case (CallState.Approved | CallState.ApprovedAlways, _, _) =>
        pause.flatMap {
          case a: Pause.Approval => started.map(CallStatus.Approved(a, r.state == CallState.ApprovedAlways, _))
          case _                 => ZIO.fail(broken(r))
        }
      case (CallState.Done, Some(res), Some(t)) => callResult(res).map(CallStatus.Done(_, r.startedAt, t))
      case (CallState.Done, _, _)               => ZIO.fail(broken(r))

  def callResult(s: String): IO[AppError, CallResult] = json[CallResult]("工具结果", s)

  private def broken(r: ToolCallRow): AppError =
    AppError.Internal(s"库里的工具调用 ${r.callId.value} 状态不完整（${r.state.label}）")

  def calls(rs: List[ToolCallRow]): IO[AppError, List[Call]] = ZIO.foreach(rs)(call)

  def letter(s: String): IO[AppError, Letter] = json[Letter]("来信", s)

  def replyTos(s: String): IO[AppError, List[ReplyTo]] = json[List[ReplyTo]]("回答去向", s)

/** 时间线 → 给模型的消息。纯函数：附件内容由调用方事先取好。 */
private[sessions] object ModelView:

  /** 压缩点切出的窗口：最近一个压缩点的摘要，和它之后的各条（不含压缩点本身）。 */
  final case class Cut(summary: Option[String], entries: List[Entry])

  def window(entries: List[Entry]): Cut =
    entries.reverseIterator.collectFirst { case Entry(_, _, c: EntryBody.Compaction, _, _) => c } match
      case Some(c) => Cut(Some(c.summary), entries.filter(e => e.id >= c.from && !e.body.isInstanceOf[EntryBody.Compaction]))
      case None    => Cut(None, entries.filterNot(_.body.isInstanceOf[EntryBody.Compaction]))

  /** 附件：上传记录与内容。 */
  final case class Attachments(uploads: Map[FileId, UploadRow], bytes: Map[String, Chunk[Byte]], path: String => Path):
    def shas: Set[String] = uploads.values.map(_.sha256).toSet

  def summaryText(summary: String): String =
    s"[之前的对话已压缩，下面是摘要；原文仍保存在会话里，需要细节可用 session_read 读本会话。]\n$summary"

  /** 来信给模型看的文字：非用户来源的标明来源。 */
  def letterText(l: Letter): String =
    val head = l.source match
      case Source.User                => ""
      case Source.Trigger(n)          => s"[触发器 $n]\n"
      case Source.Issue(i)            => s"[issue #$i]\n"
      case Source.Delegation(from, a) => s"[委派：来自 agent $a（会话 $from）]\n"
      case Source.Event(_)            => ""
    head + l.text

  private val imageTypes = Set("image/png", "image/jpeg", "image/gif", "image/webp")

  private def attachmentParts(id: FileId, a: Attachments): Chunk[UserPart] =
    a.uploads.get(id) match
      case None => Chunk(UserPart.Text(s"[附件 ${id.value} 不存在]"))
      case Some(u) =>
        val where = s"[附件 ${u.name}（${u.mediaType}，${u.size} 字节）：${a.path(u.sha256)}]"
        (u.mediaType, a.bytes.get(u.sha256)) match
          case (t, Some(b)) if imageTypes(t)         => Chunk(UserPart.Text(where), UserPart.Image(t, b))
          case ("application/pdf", Some(b))          => Chunk(UserPart.Text(where), UserPart.Pdf(b))
          case _                                     => Chunk(UserPart.Text(where))

  private def resultParts(r: CallResult, a: Attachments): Chunk[ToolContent] =
    r.content.map {
      case StoredPart.Text(t) => ToolContent.Text(t)
      case StoredPart.Image(m, sha) =>
        a.bytes.get(sha).fold[ToolContent](ToolContent.Text(s"[图片 $sha 已丢失]"))(ToolContent.Image(m, _))
    }

  /** 一条消息的各段：先工具结果（协议要求紧跟在调用之后），再其他内容。 */
  private sealed trait Side
  private final case class U(parts: Chunk[UserPart])   extends Side
  private final case class A(parts: Chunk[ReplyPart])  extends Side

  def messages(summary: Option[String], entries: List[Entry], calls: Map[Long, List[Call]], a: Attachments): Chunk[Message] =
    val sides: List[Side] = summary.map(s => U(Chunk(UserPart.Text(summaryText(s))))).toList ++ entries.flatMap { e =>
      e.body match
        case EntryBody.Context(t) => List(U(Chunk(UserPart.Text(t))))
        case EntryBody.Mail(l)    => List(U(Chunk(UserPart.Text(letterText(l))) ++ Chunk.fromIterable(l.files).flatMap(attachmentParts(_, a))))
        case EntryBody.Reply(parts, _, _, _, _) =>
          val cs      = calls.getOrElse(e.id, Nil)
          val known   = cs.map(_.callId.value).toSet
          // 没落库成调用的（被打断的半截）不发；有调用的必须带结果
          val visible = parts.filter {
            case c: ReplyPart.ToolCall => known(c.id)
            case _                     => true
          }
          val results = cs.sortBy(_.seq).map { c =>
            val r = c.status.outcome.getOrElse(CallResult.error(Interrupted.restartedBeforeStart))
            UserPart.ToolResult(c.callId.value, resultParts(r, a), r.isError)
          }
          List(A(visible)).filter(_.parts.nonEmpty) ++ Option.when(results.nonEmpty)(U(Chunk.fromIterable(results))).toList
        case _: EntryBody.Compaction | _: EntryBody.Notice => Nil
    }
    // 相邻同一方的合并成一条
    val merged = sides.foldLeft(List.empty[Side]) {
      case (U(p) :: rest, U(q)) => U(p ++ q) :: rest
      case (A(p) :: rest, A(q)) => A(p ++ q) :: rest
      case (acc, s)             => s :: acc
    }.reverse
    Chunk.fromIterable(merged.map {
      case U(p) => Message.User(p)
      case A(p) => Message.Assistant(p)
    })

  /** 一条回复里可搜的文字：正文与工具调用（名字和参数）。 */
  def searchText(parts: Chunk[ReplyPart]): String =
    parts.collect {
      case ReplyPart.Text(t, _)            => t
      case ReplyPart.ToolCall(_, n, a, _)  => s"$n $a"
    }.mkString("\n")

  /** 回复的正文（交回给委派方、通知用户）。 */
  def answerText(parts: Chunk[ReplyPart]): String =
    parts.collect { case ReplyPart.Text(t, _) => t }.mkString("\n\n").trim
