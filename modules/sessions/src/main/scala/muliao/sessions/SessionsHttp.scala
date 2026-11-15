package muliao.sessions

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.server.{Endpoints, Route}
import muliao.resources.agents.Agents
import zio.*
import zio.stream.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** 本领域的 HTTP 接口：会话列表与搜索、详情、时间线、实时事件、发消息、上传下载附件、停止、重试、压缩、删除、已读、答复、用量。 */
final case class SessionsHttp(sessions: Sessions, console: SessionConsole, files: Files, agents: Agents):
  import Endpoints.*

  private def view(e: LiveEvent, d: Timeline.Shows): LiveView = e match
    case LiveEvent.Begin         => LiveView.Begin
    case LiveEvent.Part(i, p)    => LiveView.Part(i, Timeline.part(p, Map.empty, d))
    case LiveEvent.Grew(i, g, t) => LiveView.Grew(i, g, t)
    case LiveEvent.Changed       => LiveView.Changed

  /** 会话页的实时事件：先给正在生成的部分，再给之后的事件。 */
  private def live: Route =
    SessionsApi.live.securedFeed { id =>
      (sessions.info(id) *> console.display).map { d =>
        console.subscribe(id).map { (snap, stream) =>
          ZStream.fromIterable(snap.map(ps => LiveView.Snapshot(ps.toList.map(Timeline.part(_, Map.empty, d))))) ++ stream.map(view(_, d))
        }
      }
    }

  private def create(req: NewSession): IO[AppError, Created] =
    for
      _ <- agents.requireActive(req.agent)
      d <- sessions
             .deliver(Address.Fresh(req.agent, Origin.User, Titles.of(req.text)), Letter(Source.User, req.text, Chunk.fromIterable(req.files), Some(ReplyTo.User)))
             .someOrFail(AppError.Internal("新建会话失败"))
    yield Created(d.session)

  private def send(id: SessionId, m: MessageIn): IO[AppError, Unit] =
    ZIO.when(m.text.isBlank && m.files.isEmpty)(ZIO.fail(AppError.invalid("消息不能为空"))) *>
      sessions.deliver(Address.To(id), Letter(Source.User, m.text, Chunk.fromIterable(m.files), Some(ReplyTo.User))).unit

  private def answer(in: CallAnswer): IO[AppError, Unit] =
    val key = CallKey(in.entry, in.call)
    (in.answer.answers, in.answer.approved) match
      case (Some(as), None) => console.answer(in.session, key, Answer.Answers(as))
      case (None, Some(ok)) => console.answer(in.session, key, Answer.Decision(ok, in.answer.always))
      case _                => ZIO.fail(AppError.invalid("answers 与 approved 给且只给一个"))

  private def compact(id: SessionId): IO[AppError, Unit] =
    console.compact(id).flatMap(done => ZIO.unless(done)(ZIO.fail(AppError.conflict("上次压缩之后不到两步，没什么可压缩的"))).unit)

  /** 只有这些图片内联显示，其余一律作为下载（附件按不可信内容处理）。 */
  private val inlineTypes = Set("image/png", "image/jpeg", "image/gif", "image/webp")

  private def disposition(mediaType: String, name: String): (String, String) =
    val encoded = URLEncoder.encode(name, StandardCharsets.UTF_8).replace("+", "%20")
    if inlineTypes(mediaType) then (mediaType, s"inline; filename*=UTF-8''$encoded")
    else ("application/octet-stream", s"attachment; filename*=UTF-8''$encoded")

  private def download(id: FileId): IO[AppError, (String, String, Array[Byte])] =
    for
      u     <- files.find(List(id)).map(_.head).mapError(_ => AppError.notFound("没有这个附件"))
      bytes <- files.read(u.sha256)
      (t, d) = disposition(u.mediaType, u.name)
    yield (t, d, bytes.toArray)

  private def blob(sha: String): IO[AppError, (String, String, Array[Byte])] =
    for
      _     <- ZIO.unless(sha.matches("^[0-9a-f]{64}$"))(ZIO.fail(AppError.notFound("没有这个内容")))
      bytes <- files.read(sha)
      (t, d) = disposition(sniff(bytes), sha)
    yield (t, d, bytes.toArray)

  /** 按开头的字节认图片种类。 */
  private def sniff(b: Chunk[Byte]): String =
    def starts(sig: Int*) = b.size >= sig.size && sig.zipWithIndex.forall((x, i) => (b(i) & 0xff) == x)
    if starts(0x89, 0x50, 0x4e, 0x47) then "image/png"
    else if starts(0xff, 0xd8, 0xff) then "image/jpeg"
    else if starts(0x47, 0x49, 0x46, 0x38) then "image/gif"
    else if starts(0x52, 0x49, 0x46, 0x46) && b.size > 12 && new String(b.slice(8, 12).toArray, StandardCharsets.US_ASCII) == "WEBP" then "image/webp"
    else "application/octet-stream"

  def routes: List[Route] =
    List(
      SessionsApi.list.secured(q => console.list(q.agent, q.q, q.before, q.limit.getOrElse(50), q.origin)),
      SessionsApi.create.secured(create),
      SessionsApi.get.secured(console.detail),
      SessionsApi.timeline.secured(sessions.timeline),
      live,
      SessionsApi.send.secured((id, m) => send(id, m)),
      SessionsApi.stop.secured(sessions.stop),
      SessionsApi.retry.secured(console.retry),
      SessionsApi.compact.secured(compact),
      SessionsApi.read.secured(console.markRead),
      SessionsApi.delete.secured(console.delete),
      SessionsApi.answer.secured(answer),
      SessionsApi.upload.secured((name, ct, bytes) => files.upload(name, ct, Chunk.fromArray(bytes)).map(Timeline.file)),
      SessionsApi.download.secured(download),
      SessionsApi.blob.secured(blob),
      SessionsApi.usage.secured(r => console.usage(r.from, r.to))
    )

