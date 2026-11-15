package muliao.sessions

import muliao.contract.*
import muliao.models.llm.Growth
import muliao.platform.AppError
import muliao.platform.server.{ApiEndpoint, Auth, Endpoints, Sse}
import muliao.resources.agents.Agents
import sttp.capabilities.zio.ZioStreams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.{CodecFormat, Endpoint}
import sttp.tapir.ztapir.*
import zio.*
import zio.json.*
import zio.stream.*

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** 本领域的 HTTP 接口：会话列表与搜索、详情、时间线、实时事件、发消息、上传下载附件、停止、重试、压缩、删除、已读、答复、用量。 */
final case class SessionsHttp(sessions: Sessions, files: Files, agents: Agents, hooks: SessionHooks):
  import Endpoints.*

  /** 会话页的实时事件（SSE）：先给正在生成的部分，再给之后的事件；连接帧（ready、ping）见 [[Sse]]。 */
  val liveEndpoint: Endpoint[Option[String], SessionId, ApiError, ZStream[Any, Throwable, Byte], ZioStreams] =
    SessionsApi.one.get
      .in("live")
      .out(streamTextBody(ZioStreams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8)))
      .name("sessionLive")
      .description("会话页的实时事件（SSE），事件 data 为 LiveView 的 JSON。")

  private def display: UIO[String => Display] =
    hooks.tools.map(ts => ts.map(t => t.spec.name -> t.spec.display).toMap).map(m => n => m.getOrElse(n, Display.Plain))

  private def view(e: LiveEvent, d: String => Display): LiveView = e match
    case LiveEvent.Begin      => LiveView("begin")
    case LiveEvent.Part(i, p) => LiveView("part", index = Some(i), part = Some(Timeline.part(p, Map.empty, d)))
    case LiveEvent.Grew(i, g, t) =>
      val growth = g match
        case Growth.Text     => "Text"
        case Growth.Thinking => "Thinking"
        case Growth.ToolArgs => "ToolArgs"
      LiveView("grew", index = Some(i), growth = Some(growth), text = Some(t))
    case LiveEvent.Changed => LiveView("changed")

  private def live(auth: Auth): ApiEndpoint =
    liveEndpoint.zServerSecurityLogic(auth.authenticate).serverLogic[Any] { _ => id =>
      (sessions.info(id) *> display).mapError(AppError.toApi).map { d =>
        Sse.frames(sessions.subscribe(id).map { (snap, stream) =>
          (ZStream.fromIterable(snap.map(ps => LiveView("snapshot", parts = ps.toList.map(Timeline.part(_, Map.empty, d))))) ++ stream.map(view(_, d)))
            .map(v => ServerSentEvent(data = Some(v.toJson), eventType = Some("live")))
        })
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

  private def answer(id: SessionId, entry: Long, call: CallId, a: AnswerIn): IO[AppError, Unit] =
    (a.answers, a.approved) match
      case (Some(as), None) => sessions.answer(id, CallKey(entry, call), Answer.Answers(as))
      case (None, Some(ok)) => sessions.answer(id, CallKey(entry, call), Answer.Decision(ok, a.always))
      case _                => ZIO.fail(AppError.invalid("answers 与 approved 给且只给一个"))

  private def compact(id: SessionId): IO[AppError, Unit] =
    sessions.compact(id).flatMap(done => ZIO.unless(done)(ZIO.fail(AppError.conflict("上次压缩之后不到两步，没什么可压缩的"))).unit)

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

  def endpoints(auth: Auth): List[ApiEndpoint] =
    List(
      SessionsApi.list.secured(auth)(_ => (agent, origins, q, before, limit) => sessions.list(agent, q, before, limit.getOrElse(50), origins)),
      SessionsApi.create.secured(auth)(_ => create),
      SessionsApi.get.secured(auth)(_ => sessions.detail),
      SessionsApi.timeline.secured(auth)(_ => sessions.timeline),
      live(auth),
      SessionsApi.send.secured(auth)(_ => (id, m) => send(id, m)),
      SessionsApi.stop.secured(auth)(_ => sessions.stop),
      SessionsApi.retry.secured(auth)(_ => sessions.retry),
      SessionsApi.compact.secured(auth)(_ => compact),
      SessionsApi.read.secured(auth)(_ => sessions.markRead),
      SessionsApi.delete.secured(auth)(_ => sessions.delete),
      SessionsApi.answer.secured(auth)(_ => (id, e, c, a) => answer(id, e, c, a)),
      SessionsApi.upload.secured(auth)(_ => (name, ct, bytes) => files.upload(name, ct, Chunk.fromArray(bytes)).map(Timeline.file)),
      SessionsApi.download.secured(auth)(_ => download),
      SessionsApi.blob.secured(auth)(_ => blob),
      SessionsApi.usage.secured(auth)(_ => (from, to) => sessions.usage(from, to))
    )

object SessionsHttp:
  val layer: URLayer[Sessions & Files & Agents & SessionHooks, SessionsHttp] = ZLayer.derive[SessionsHttp]
