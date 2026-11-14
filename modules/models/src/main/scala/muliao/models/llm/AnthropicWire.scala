package muliao.models.llm

import muliao.models.llm.JsonPath.*
import muliao.platform.AppError
import muliao.toolkit.ToolContent
import zio.Chunk
import zio.json.ast.Json

import java.util.Base64

/** Anthropic Messages 协议（https://docs.anthropic.com/en/api/messages）。
  *
  *   - 三个缓存断点：固定说明、工具定义（最后一个工具）、消息末尾（最后一块），让每次原样重发的前缀命中提示缓存（5 分钟档）；
  *   - 思考档位：`adaptive` 并 `display: summarized` 要回摘要，力度放 `output_config.effort`；
  *   - 历史里的思考块只发本协议签过名的（带签名或 redacted 数据，见 [[Seal]]）：Anthropic 拒收没签名的思考块。
  */
object AnthropicWire extends Wire:
  val protocol: Protocol = Protocol.Anthropic
  val ApiVersion         = "2023-06-01"
  val maxImages: Option[Int] = Some(100)

  def endpoint(base: String): String = s"$base/v1/messages"

  def headers(apiKey: String): Seq[(String, String)] =
    Seq("x-api-key" -> apiKey, "anthropic-version" -> ApiVersion)

  def modelsUrl(base: String, cursor: Option[String]): String =
    s"$base/v1/models?limit=1000${cursor.fold("")(c => s"&after_id=${java.net.URLEncoder.encode(c, "UTF-8")}")}"

  def modelsPage(page: Json): (Chunk[String], Option[String]) =
    val names = page.at("data").arr.flatMap(_.at("id").str)
    val next  = if page.at("has_more").bool.contains(true) then page.at("last_id").str else None
    (names, next)

  // ---- 请求体 ----

  private val cache = obj("type" -> s("ephemeral"), "ttl" -> s("5m"))

  /** Anthropic 必须给 `max_tokens`：没配置时用 Claude 5 一代的上限，更老的模型在服务商配置里调小。 */
  val DefaultMaxTokens = 128_000

  def body(model: String, req: ModelRequest, maxTokens: Option[Int]): Json =
    val messages = merged(req.messages.flatMap {
      case Message.User(parts)      => Chunk.fromIterable(Option(userBlocks(parts)).filter(_.nonEmpty).map("user" -> _))
      case Message.Assistant(parts) => Chunk.fromIterable(Option(assistantBlocks(parts)).filter(_.nonEmpty).map("assistant" -> _))
    })
    val withLastCached = messages.lastOption match
      case Some((role, blocks)) => messages.dropRight(1) :+ (role -> (blocks.dropRight(1) :+ blocks.last.withField("cache_control", cache)))
      case None                 => messages
    val tools = req.tools.map(t => obj("name" -> s(t.name), "description" -> s(t.description), "input_schema" -> t.inputSchema))
    val toolsCached = tools.lastOption.fold(tools)(last => tools.dropRight(1) :+ last.withField("cache_control", cache))
    val summarized  = obj("type" -> s("adaptive"), "display" -> s("summarized"))
    val (thinking, effort) = req.thinking match
      case Thinking.Default => (summarized, None)
      case Thinking.Off     => (obj("type" -> s("disabled")), None)
      case other            => (summarized, Some(obj("effort" -> s(other.id))))
    objOpt(
      "model"         -> Some(s(model)),
      "max_tokens"    -> Some(n(maxTokens.getOrElse(DefaultMaxTokens).toLong)),
      "stream"        -> Some(b(true)),
      "system"        -> Option.when(req.system.nonEmpty)(arr(List(obj("type" -> s("text"), "text" -> s(req.system), "cache_control" -> cache)))),
      "tools"         -> Option.when(toolsCached.nonEmpty)(arr(toolsCached)),
      "messages"      -> Some(arr(withLastCached.map((role, blocks) => obj("role" -> s(role), "content" -> arr(blocks))))),
      "thinking"      -> Some(thinking),
      "output_config" -> effort
    )

  /** 相邻同一方的消息并成一条（跳过空消息后可能出现）。 */
  private def merged(ms: Chunk[(String, Chunk[Json])]): Chunk[(String, Chunk[Json])] =
    ms.foldLeft(Chunk.empty[(String, Chunk[Json])]) { (acc, m) =>
      acc.lastOption match
        case Some((role, blocks)) if role == m._1 => acc.dropRight(1) :+ (role -> (blocks ++ m._2))
        case _                                    => acc :+ m
    }

  private def base64(data: Chunk[Byte]): String = Base64.getEncoder.encodeToString(data.toArray)

  private def image(media: String, data: Chunk[Byte]): Json =
    obj("type" -> s("image"), "source" -> obj("type" -> s("base64"), "media_type" -> s(media), "data" -> s(base64(data))))

  private def text(t: String): Option[Json] = Option.when(t.nonEmpty)(obj("type" -> s("text"), "text" -> s(t))) // 拒收空文字块

  private def userBlocks(parts: Chunk[UserPart]): Chunk[Json] = parts.flatMap {
    case UserPart.Text(t)     => Chunk.fromIterable(text(t))
    case UserPart.Image(m, d) => Chunk.single(image(m, d))
    case UserPart.Pdf(d) =>
      Chunk.single(obj("type" -> s("document"), "source" -> obj("type" -> s("base64"), "media_type" -> s("application/pdf"), "data" -> s(base64(d)))))
    case UserPart.ToolResult(id, content, isError) =>
      val blocks = content.flatMap {
        case ToolContent.Text(t)     => Chunk.fromIterable(text(t))
        case ToolContent.Image(m, d) => Chunk.single(image(m, d))
      }
      Chunk.single(obj("type" -> s("tool_result"), "tool_use_id" -> s(id), "content" -> arr(blocks), "is_error" -> b(isError)))
  }

  private def assistantBlocks(parts: Chunk[ReplyPart]): Chunk[Json] = parts.flatMap {
    case ReplyPart.Text(t, _) => Chunk.fromIterable(text(t))
    case ReplyPart.Thinking(t, Some(Seal(Protocol.Anthropic, data))) =>
      Chunk.fromIterable(
        data.at("signature").str.map(sig => obj("type" -> s("thinking"), "thinking" -> s(t), "signature" -> s(sig)))
          .orElse(data.at("redacted").str.map(r => obj("type" -> s("redacted_thinking"), "data" -> s(r))))
      )
    case _: ReplyPart.Thinking => Chunk.empty // 被打断的、别家协议的思考不发
    case ReplyPart.ToolCall(id, name, args, _) =>
      // 参数不是 JSON 对象（模型生成坏了，执行时已按「请重试」处理）时发空对象：Anthropic 只收对象
      val input = parse(args).toOption.collect { case o: Json.Obj => o }.getOrElse(Json.Obj())
      Chunk.single(obj("type" -> s("tool_use"), "id" -> s(id), "name" -> s(name), "input" -> input))
  }

  // ---- 读流 ----

  final case class State(
      emitted: Emitted,
      /** Anthropic 的块序号 → 我们的段序号 */
      index: Map[Long, Int],
      model: String,
      id: Option[String],
      usage: Map[String, Long],
      stopReason: Option[String]
  )

  def initial: State = State(Emitted.from(ReplyBuilder.empty), Map.empty, "", None, Map.empty, None)

  /** 流内 error 事件的类型是否暂时性。契约外的类型（如中转站上游空闲超时）一律当作中途断流。 */
  private val permanentErrors =
    Set("invalid_request_error", "authentication_error", "permission_error", "not_found_error", "request_too_large", "billing_error")

  private def usageOf(u: Json): Map[String, Long] = u match
    case Json.Obj(fs) => fs.collect { case (k, Json.Num(v)) => k -> v.longValue }.toMap
    case _            => Map.empty

  private def signature(sig: String): Option[Seal] =
    Option.when(sig.nonEmpty)(Seal(Protocol.Anthropic, obj("signature" -> s(sig))))

  def step(st: State, e: SseEvent): Either[AppError, (State, Chunk[ReplyEvent])] =
    val fresh = st.copy(emitted = st.emitted.copy(events = Chunk.empty))
    parse(e.data).left.map(why => Wire.outOfProtocol(s"事件不是 JSON（$why）")).flatMap { v =>
      v.at("type").strOr() match
        case "message_start" =>
          val m = v.at("message")
          ok(fresh.copy(model = m.at("model").strOr(), id = m.at("id").str, usage = usageOf(m.at("usage"))))
        case "content_block_start" =>
          val block = v.at("content_block")
          val part: Either[AppError, ReplyPart] = block.at("type").strOr() match
            case "text"              => Right(ReplyPart.Text(block.at("text").strOr()))
            case "thinking"          => Right(ReplyPart.Thinking(block.at("thinking").strOr(), signature(block.at("signature").strOr())))
            case "redacted_thinking" => Right(ReplyPart.Thinking("", Some(Seal(Protocol.Anthropic, obj("redacted" -> block.at("data"))))))
            case "tool_use"          => Right(ReplyPart.ToolCall(block.at("id").strOr(), block.at("name").strOr(), ""))
            case other               => Left(Wire.outOfProtocol(s"不支持的内容块类型 $other"))
          val ours = fresh.emitted.parts.size
          part.flatMap(p => fresh.emitted.emit(ReplyEvent.Started(ours, p)))
            .map(em => fresh.copy(emitted = em, index = fresh.index + (v.at("index").long.getOrElse(0L) -> ours)))
            .map(done)
        case "content_block_delta" =>
          val at = v.at("index").long.getOrElse(0L)
          fresh.index.get(at).toRight(Wire.outOfProtocol(s"第 $at 块没开始就有增量")).flatMap { ours =>
            val d = v.at("delta")
            def grow(g: Growth, t: String) =
              if t.isEmpty then ok(fresh) else fresh.emitted.emit(ReplyEvent.Grew(ours, g, t)).map(em => done(fresh.copy(emitted = em)))
            d.at("type").strOr() match
              case "text_delta"       => grow(Growth.Text, d.at("text").strOr())
              case "thinking_delta"   => grow(Growth.Thinking, d.at("thinking").strOr())
              case "input_json_delta" => grow(Growth.ToolArgs, d.at("partial_json").strOr())
              case "signature_delta" =>
                fresh.emitted.parts(ours) match
                  case t: ReplyPart.Thinking =>
                    // 签名可能分几段到，接在已有的后面
                    val before = t.seal.flatMap(_.data.at("signature").str).getOrElse("")
                    val part   = t.copy(seal = signature(before + d.at("signature").strOr()))
                    fresh.emitted.emit(ReplyEvent.Replaced(ours, part)).map(em => done(fresh.copy(emitted = em)))
                  case other => Left(Wire.outOfProtocol(s"签名增量指向非思考块 ${other.getClass.getSimpleName}"))
              case _ => ok(fresh) // 引用等用不到的增量
          }
        case "message_delta" =>
          // message_delta 的用量是到此为止的累计，覆盖 message_start 里的同名字段
          ok(fresh.copy(stopReason = v.at("delta").at("stop_reason").str.orElse(fresh.stopReason), usage = fresh.usage ++ usageOf(v.at("usage"))))
        case "message_stop" =>
          // 没有参数的调用不发 input_json_delta
          val parts = fresh.emitted.parts.map {
            case c: ReplyPart.ToolCall if c.args.isEmpty => c.copy(args = "{}")
            case p                                       => p
          }
          val u               = (k: String) => fresh.usage.getOrElse(k, 0L)
          val (read, written) = (u("cache_read_input_tokens"), u("cache_creation_input_tokens"))
          val reply = Reply(parts, protocol, fresh.model, fresh.id, fresh.stopReason, Usage(u("input_tokens") + read + written, u("output_tokens"), read, written))
          fresh.emitted.emit(ReplyEvent.Finished(reply)).map(em => done(fresh.copy(emitted = em)))
        case "error" =>
          val kind = v.at("error").at("type").strOr()
          val msg  = s"$kind：${v.at("error").at("message").strOr()}"
          Left(if permanentErrors(kind) then AppError.External(msg) else AppError.Unavailable(msg))
        case _ => ok(fresh) // ping、content_block_stop
    }

  private def ok(s: State): Either[AppError, (State, Chunk[ReplyEvent])] = Right(done(s))
  private def done(s: State): (State, Chunk[ReplyEvent])                 = (s, s.emitted.events)
