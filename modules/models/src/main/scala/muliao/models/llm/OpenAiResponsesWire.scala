package muliao.models.llm

import muliao.models.llm.JsonPath.*
import muliao.platform.AppError
import muliao.toolkit.ToolContent
import zio.Chunk
import zio.json.ast.Json

import java.util.Base64

/** OpenAI Responses 协议（https://platform.openai.com/docs/api-reference/responses），也用于兼容它的网关。
  *
  *   - 思考档位放 `reasoning`，并要回摘要（`summary: auto`）、让模型看得到以前各轮的推理（`context: all_turns`）、回传加密的推理
  *     内容（`include: reasoning.encrypted_content`）；不思考的模型和网关忽略这些；
  *   - 输出项的编号、加密推理、`phase` 放在各段的 [[Seal]] 里，回传时原样带上；只有收完的项才有 Seal；
  *   - 推理项给人看的文字：有摘要用摘要，没有（开源推理模型只回原始推理）用原始推理文字，回传时放回原处。
  */
object OpenAiResponsesWire extends Wire:
  val protocol: Protocol      = Protocol.OpenAiResponses
  val maxImages: Option[Int]  = None

  def endpoint(base: String): String                   = s"$base/responses"
  def headers(apiKey: String): Seq[(String, String)]   = Seq("Authorization" -> s"Bearer $apiKey")
  def modelsUrl(base: String, cursor: Option[String]): String = s"$base/models"
  def modelsPage(page: Json): (Chunk[String], Option[String]) = (page.at("data").arr.flatMap(_.at("id").str), None)

  // ---- 请求体 ----

  def body(model: String, req: ModelRequest, maxTokens: Option[Int]): Json =
    val input = req.messages.flatMap {
      case Message.User(parts)      => userItems(parts)
      case Message.Assistant(parts) => assistantItems(parts)
    }
    // 不用 strict：它要求所有参数都必填，带可选参数的工具用不了
    val tools = req.tools.map(t =>
      obj("type" -> s("function"), "name" -> s(t.name), "description" -> s(t.description), "parameters" -> t.inputSchema, "strict" -> b(false))
    )
    val shown = obj("summary" -> s("auto"), "context" -> s("all_turns"))
    val reasoning = req.thinking match
      case Thinking.Default => shown
      case Thinking.Off     => obj("effort" -> s("none"))
      case other            => shown.withField("effort", s(other.id))
    objOpt(
      "model"             -> Some(s(model)),
      "stream"            -> Some(b(true)),
      "instructions"      -> Option.when(req.system.nonEmpty)(s(req.system)),
      "input"             -> Some(arr(input)),
      "tools"             -> Option.when(tools.nonEmpty)(arr(tools)),
      "max_output_tokens" -> maxTokens.map(m => n(m.toLong)),
      // 没有它时连续两步常落到不同的缓存上，前缀不变也命中不了
      "prompt_cache_key"  -> req.cacheKey.map(s),
      "reasoning"         -> Some(reasoning),
      "include"           -> Option.when(req.thinking != Thinking.Off)(arr(List(s("reasoning.encrypted_content"))))
    )

  private def dataUrl(media: String, data: Chunk[Byte]): String =
    s"data:$media;base64,${Base64.getEncoder.encodeToString(data.toArray)}"

  private def image(media: String, data: Chunk[Byte]): Json =
    obj("type" -> s("input_image"), "image_url" -> s(dataUrl(media, data)), "detail" -> s("auto"))

  /** 工具结果是独立的输入项，排在这条消息的用户内容之前。 */
  private def userItems(parts: Chunk[UserPart]): Chunk[Json] =
    val results = parts.collect { case UserPart.ToolResult(id, content, _) =>
      obj("type" -> s("function_call_output"), "call_id" -> s(id), "output" -> toolOutput(content))
    }
    val content = parts.collect {
      case UserPart.Text(t)     => obj("type" -> s("input_text"), "text" -> s(t))
      case UserPart.Image(m, d) => image(m, d)
      case UserPart.Pdf(d)      => obj("type" -> s("input_file"), "filename" -> s("file.pdf"), "file_data" -> s(dataUrl("application/pdf", d)))
    }
    results ++ Chunk.fromIterable(Option.when(content.nonEmpty)(obj("role" -> s("user"), "content" -> arr(content))))

  /** 只有文字时是一段文本（多段首尾相接），带图片时是内容列表。 */
  private def toolOutput(content: Chunk[ToolContent]): Json =
    val texts = content.collect { case ToolContent.Text(t) => t }
    if texts.size == content.size then s(texts.mkString)
    else
      arr(content.map {
        case ToolContent.Text(t)     => obj("type" -> s("input_text"), "text" -> s(t))
        case ToolContent.Image(m, d) => image(m, d)
      })

  private def ours(seal: Option[Seal]): Option[Json] = seal.collect { case Seal(Protocol.OpenAiResponses, d) => d }

  private def assistantItems(parts: Chunk[ReplyPart]): Chunk[Json] = parts.flatMap {
    case ReplyPart.Text(t, seal) =>
      val meta = ours(seal)
      val item = meta.flatMap(_.at("id").str) match
        case Some(id) =>
          obj("type" -> s("message"), "role" -> s("assistant"), "id" -> s(id), "status" -> s("completed"),
            "content" -> arr(List(obj("type" -> s("output_text"), "text" -> s(t), "annotations" -> arr(Nil)))))
        case None => obj("role" -> s("assistant"), "content" -> s(t))
      Chunk.single(meta.map(_.at("phase")).filterNot(_.isNull).fold(item)(item.withField("phase", _)))
    // 只回传本协议、收完的推理项；别家的思考和没收完的半截不发
    case ReplyPart.Thinking(t, seal) =>
      Chunk.fromIterable(ours(seal).flatMap(m => m.at("id").str.map(m -> _)).map { (m, id) =>
        val raw  = m.at("raw").bool.contains(true)
        val base = obj("type" -> s("reasoning"), "id" -> s(id), "summary" -> arr(Nil))
        val withText =
          if t.isEmpty then base
          else if raw then base.withField("content", arr(List(obj("type" -> s("reasoning_text"), "text" -> s(t)))))
          else base.withField("summary", arr(List(obj("type" -> s("summary_text"), "text" -> s(t)))))
        val enc = m.at("encrypted_content")
        if enc.isNull then withText else withText.withField("encrypted_content", enc)
      })
    case ReplyPart.ToolCall(id, name, args, seal) =>
      val item = obj("type" -> s("function_call"), "call_id" -> s(id), "name" -> s(name), "arguments" -> s(args))
      Chunk.single(ours(seal).flatMap(_.at("id").str).fold(item)(i => item.withField("id", s(i))))
  }

  // ---- 读流 ----

  final case class State(emitted: Emitted, index: Map[Long, Int], model: String, id: Option[String])

  def initial: State = State(Emitted.from(ReplyBuilder.empty), Map.empty, "", None)

  /** 流内错误码里属于服务端暂时性的（分别对应 5xx 与 429）。 */
  private val transientCodes = Set("server_error", "rate_limit_exceeded")

  private def streamError(code: String, message: String): AppError =
    val msg = s"$code：$message"
    if transientCodes(code) then AppError.Unavailable(msg) else AppError.External(msg)

  private def texts(list: Json, kind: String): Chunk[String] =
    list.arr.filter(c => kind.isEmpty || c.at("type").str.contains(kind)).flatMap(_.at("text").str)

  /** 一个输出项对应的段。`done`：这是结束时的完整项，只有它带 Seal。 */
  private def partOf(item: Json, done: Boolean): Either[AppError, ReplyPart] =
    val id = item.at("id").str.map(i => "id" -> s(i)).toList
    def seal(extra: List[(String, Json)]): Option[Seal] =
      val fields = id ++ extra
      Option.when(done && fields.nonEmpty)(Seal(protocol, obj(fields*)))
    item.at("type").strOr() match
      case "message" =>
        val phase = Option(item.at("phase")).filterNot(_.isNull).map("phase" -> _).toList
        Right(ReplyPart.Text(texts(item.at("content"), "").mkString, seal(phase))) // 正文与拒答都当文字
      case "reasoning" =>
        val summary = texts(item.at("summary"), "summary_text")
        val raw     = texts(item.at("content"), "reasoning_text")
        val enc     = Option(item.at("encrypted_content")).filterNot(_.isNull).map("encrypted_content" -> _).toList
        val useRaw  = summary.isEmpty && raw.nonEmpty
        val text    = if useRaw then raw.mkString("\n\n") else summary.mkString("\n\n")
        Right(ReplyPart.Thinking(text, seal(enc ++ (if useRaw then List("raw" -> b(true)) else Nil))))
      case "function_call" =>
        Right(ReplyPart.ToolCall(item.at("call_id").strOr(), item.at("name").strOr(), item.at("arguments").strOr(), seal(Nil)))
      case other => Left(Wire.outOfProtocol(s"不支持的输出项类型 $other"))

  def step(st: State, e: SseEvent): Either[AppError, (State, Chunk[ReplyEvent])] =
    val fresh = st.copy(emitted = st.emitted.copy(events = Chunk.empty))
    parse(e.data).left.map(why => Wire.outOfProtocol(s"事件不是 JSON（$why）")).flatMap { v =>
      def at: Either[AppError, Int] =
        val n = v.at("output_index").long.getOrElse(0L)
        fresh.index.get(n).toRight(Wire.outOfProtocol(s"第 $n 个输出项没开始就有事件"))
      def emit(ev: ReplyEvent) = fresh.emitted.emit(ev).map(em => done(fresh.copy(emitted = em)))
      def grow(g: Growth, t: String) = if t.isEmpty then ok(fresh) else at.flatMap(i => emit(ReplyEvent.Grew(i, g, t)))
      val delta = v.at("delta").strOr()
      v.at("type").strOr() match
        case "response.output_text.delta" | "response.refusal.delta"                    => grow(Growth.Text, delta)
        case "response.reasoning_summary_text.delta" | "response.reasoning_text.delta"   => grow(Growth.Thinking, delta)
        case "response.reasoning_summary_part.added" if v.at("summary_index").long.exists(_ > 0) => grow(Growth.Thinking, "\n\n")
        case "response.function_call_arguments.delta"                                  => grow(Growth.ToolArgs, delta)
        case "response.created" =>
          val r = v.at("response")
          ok(fresh.copy(model = r.at("model").strOr(), id = r.at("id").str))
        case "response.output_item.added" =>
          val ours = fresh.emitted.parts.size
          partOf(v.at("item"), done = false).flatMap(p => fresh.emitted.emit(ReplyEvent.Started(ours, p)))
            .map(em => done(fresh.copy(emitted = em, index = fresh.index + (v.at("output_index").long.getOrElse(0L) -> ours))))
        case "response.output_item.done" =>
          for
            i    <- at
            part <- partOf(v.at("item"), done = true)
            out  <- if fresh.emitted.parts(i) == part then ok(fresh) else emit(ReplyEvent.Replaced(i, part))
          yield out
        case "response.completed" | "response.incomplete" =>
          val r = v.at("response")
          val u = r.at("usage")
          val reply = Reply(
            fresh.emitted.parts,
            protocol,
            fresh.model,
            fresh.id,
            r.at("incomplete_details").at("reason").str.orElse(r.at("status").str),
            Usage(u.at("input_tokens").long.getOrElse(0L), u.at("output_tokens").long.getOrElse(0L), u.at("input_tokens_details").at("cached_tokens").long.getOrElse(0L), 0L)
          )
          emit(ReplyEvent.Finished(reply))
        case "response.failed" =>
          val err = v.at("response").at("error")
          Left(streamError(err.at("code").strOr(), err.at("message").strOr()))
        case "error" =>
          // 规范的 error 事件字段在顶层；有的网关包在 error 对象里
          val err = if v.at("error").isInstanceOf[Json.Obj] then v.at("error") else v
          Left(streamError(err.at("code").strOr(), err.at("message").strOr()))
        case _ => ok(fresh)
    }

  private def ok(s: State): Either[AppError, (State, Chunk[ReplyEvent])] = Right(done(s))
  private def done(s: State): (State, Chunk[ReplyEvent])                 = (s, s.emitted.events)
