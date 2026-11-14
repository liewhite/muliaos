package muliao.models.llm

import muliao.models.llm.JsonPath.*
import muliao.platform.AppError
import muliao.platform.http.OutboundHttp
import muliao.toolkit.ToolContent
import zio.*
import zio.http.{Body, Request, Response}
import zio.json.*
import zio.json.ast.Json
import zio.stream.*
import zio.test.*

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

/** 协议客户端：用真实服务商录下的响应（`resources/fixtures/` 下的 `.sse`，录法见 docs/stack.md「模型接入」）测读流，用构造的请求测请求体。 */
object ModelsSpec extends ZIOSpecDefault:

  def fixture(name: String): Chunk[Byte] =
    Chunk.fromArray(getClass.getResourceAsStream(s"/fixtures/$name").readAllBytes())

  def decode(wire: Wire, bytes: Chunk[Byte]): IO[AppError, Chunk[ReplyEvent]] =
    Models.decode(wire, ZStream.fromChunk(bytes).rechunk(97), 1.minute, "test").runCollect

  def finished(events: Chunk[ReplyEvent]): Reply = events.last match
    case ReplyEvent.Finished(r) => r
    case other                  => throw RuntimeException(s"最后一个事件不是 Finished：$other")

  def folded(events: Chunk[ReplyEvent]): Chunk[ReplyPart] =
    events.init.foldLeft(ReplyBuilder.empty)((b, e) => b(e).fold(m => throw RuntimeException(m), identity)).parts

  def sse(events: String*): Chunk[Byte] =
    Chunk.fromArray(events.map(e => s"data: $e\n\n").mkString.getBytes("UTF-8"))

  def png(w: Int, h: Int): Chunk[Byte] =
    val img = BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    for x <- 0 until w by 7; y <- 0 until h by 5 do img.setRGB(x, y, (x * 31 + y * 17) & 0xffffff)
    val out = ByteArrayOutputStream()
    ImageIO.write(img, "png", out)
    Chunk.fromArray(out.toByteArray)

  val tool = ToolDef("get_weather", "Get the weather.", obj("type" -> s("object")))

  def spec = suite("模型接入")(
    suite("读流（录下的真实响应）")(
      test("Anthropic：工具调用、用量含缓存写") {
        for events <- decode(AnthropicWire, fixture("anthropic-tool-call.sse"))
        yield
          val r = finished(events)
          assertTrue(
            r.parts == Chunk(ReplyPart.ToolCall("toolu_01NeqNScbKM9mDkSksu59Bdq", "get_weather", """{"city": "Paris"}""")),
            r.usage == Usage(input = 122 + 808, output = 50, cacheRead = 0, cacheWrite = 808),
            r.stopReason.contains("tool_use"),
            r.model == "claude-sonnet-5-5",
            folded(events) == r.parts
          )
      },
      test("Anthropic：思考带签名，正文在后") {
        for events <- decode(AnthropicWire, fixture("anthropic-thinking.sse"))
        yield
          val r = finished(events)
          val thinking = r.parts.collectFirst { case t: ReplyPart.Thinking => t }.get
          assertTrue(
            thinking.text.nonEmpty,
            thinking.seal.exists(s => s.protocol == Protocol.Anthropic && s.data.at("signature").str.exists(_.nonEmpty)),
            r.parts.last.isInstanceOf[ReplyPart.Text],
            folded(events) == r.parts
          )
      },
      test("OpenAI Responses：工具调用带输出项编号") {
        for events <- decode(OpenAiResponsesWire, fixture("openai-tool-call.sse"))
        yield
          val r    = finished(events)
          val call = r.toolCalls.head
          assertTrue(
            r.toolCalls.size == 1,
            call.name == "get_weather",
            call.args.fromJson[Json].toOption.flatMap(_.at("city").str).contains("Paris"),
            call.seal.flatMap(_.data.at("id").str).exists(_.startsWith("fc_")),
            r.usage.input > 0,
            folded(events) == r.parts
          )
      },
      test("OpenAI Responses：推理摘要与加密推理，正文带编号") {
        for events <- decode(OpenAiResponsesWire, fixture("openai-reasoning.sse"))
        yield
          val r        = finished(events)
          val thinking = r.parts.collectFirst { case t: ReplyPart.Thinking => t }.get
          val text     = r.parts.collectFirst { case t: ReplyPart.Text => t }.get
          assertTrue(
            thinking.text.nonEmpty,
            thinking.seal.exists(_.data.at("encrypted_content").str.exists(_.nonEmpty)),
            text.seal.flatMap(_.data.at("id").str).exists(_.nonEmpty),
            folded(events) == r.parts
          )
      },
      test("流没收完就断了：暂时性失败") {
        val whole = String(fixture("anthropic-tool-call.sse").toArray, "UTF-8")
        val cut   = Chunk.fromArray(whole.take(whole.indexOf("event: message_stop")).getBytes("UTF-8"))
        for e <- decode(AnthropicWire, cut).flip
        yield assertTrue(e.isTransient)
      },
      test("流内错误：过载是暂时性的，请求错误不是") {
        val start = """{"type":"message_start","message":{"model":"m","id":"x","usage":{}}}"""
        for
          a <- decode(AnthropicWire, sse(start, """{"type":"error","error":{"type":"overloaded_error","message":"busy"}}""")).flip
          b <- decode(AnthropicWire, sse(start, """{"type":"error","error":{"type":"invalid_request_error","message":"bad"}}""")).flip
          c <- decode(OpenAiResponsesWire, sse("""{"type":"response.failed","response":{"error":{"code":"rate_limit_exceeded","message":"slow"}}}""")).flip
        yield assertTrue(a.isTransient, !b.isTransient, c.isTransient)
      },
      test("事件对不上已有的段：协议错误") {
        for e <- decode(AnthropicWire, sse("""{"type":"content_block_delta","index":3,"delta":{"type":"text_delta","text":"x"}}""")).flip
        yield assertTrue(!e.isTransient, e.message.contains("不合协议"))
      },
      test("空闲超时按两次数据之间算：持续有数据的长回复不超时，卡住的才超时") {
        val whole  = fixture("anthropic-tool-call.sse")
        val pieces = Chunk.fromIterable(whole.grouped(whole.size / 3 + 1).toList)
        for
          slow  <- Models.decode(AnthropicWire, ZStream.fromIterable(pieces).schedule(Schedule.spaced(40.seconds)).flattenChunks, 1.minute, "t").runCollect.fork
          _     <- TestClock.adjust(5.minutes)
          ok    <- slow.join
          stuck <- Models.decode(AnthropicWire, ZStream.fromChunk(pieces.head) ++ ZStream.never, 1.minute, "t").runCollect.flip.fork
          _     <- TestClock.adjust(61.seconds)
          e     <- stuck.join
        yield assertTrue(finished(ok).toolCalls.size == 1, e.isTransient, e.message.contains("没有动静"))
      },
      test("SSE：多行 data、注释、CRLF") {
        val raw = Chunk.fromArray(": comment\r\nevent: a\r\ndata: 1\r\ndata: 2\r\n\r\ndata: x\n\n".getBytes("UTF-8"))
        for events <- ZStream.fromChunk(raw).rechunk(3).via(Sse.decode).runCollect
        yield assertTrue(events == Chunk(SseEvent(Some("a"), "1\n2"), SseEvent(None, "x")))
      }
    ),
    suite("请求体")(
      test("Anthropic：三个缓存断点，只回传本协议签过名的思考") {
        val signed   = ReplyPart.Thinking("想过了", Some(Seal(Protocol.Anthropic, obj("signature" -> s("sig")))))
        val unsigned = ReplyPart.Thinking("被打断的", None)
        val foreign  = ReplyPart.Thinking("别家的", Some(Seal(Protocol.OpenAiResponses, obj("id" -> s("rs_1")))))
        val req = ModelRequest(
          "说明",
          Chunk(tool, tool.copy(name = "b")),
          Chunk(
            Message.User(Chunk(UserPart.Text("hi"))),
            Message.Assistant(Chunk(signed, unsigned, foreign, ReplyPart.ToolCall("c1", "get_weather", "not json"))),
            Message.User(Chunk(UserPart.ToolResult("c1", Chunk(ToolContent.Text("晴")), isError = false), UserPart.Text("")))
          ),
          Thinking.High
        )
        val body = AnthropicWire.body("m", req, None)
        val json = body.toJson
        val assistant = body.at("messages").arr(1).at("content").arr
        assertTrue(
          "cache_control".r.findAllIn(json).size == 3,
          body.at("tools").arr.last.at("cache_control") != Json.Null,
          body.at("messages").arr.last.at("content").arr.last.at("cache_control") != Json.Null,
          assistant.size == 2,
          assistant(0).at("signature").str.contains("sig"),
          assistant(1).at("input") == Json.Obj(),
          body.at("output_config").at("effort").str.contains("high"),
          body.at("max_tokens").long.contains(AnthropicWire.DefaultMaxTokens.toLong),
          !json.contains("被打断的"),
          !json.contains("别家的")
        )
      },
      test("OpenAI：工具结果在前、回传推理与编号、别家思考不发、关思考不要加密推理、带缓存键") {
        val reasoning = ReplyPart.Thinking("摘要", Some(Seal(Protocol.OpenAiResponses, obj("id" -> s("rs_1"), "encrypted_content" -> s("enc")))))
        val foreign   = ReplyPart.Thinking("别家的", Some(Seal(Protocol.Anthropic, obj("signature" -> s("sig")))))
        val call      = ReplyPart.ToolCall("call_1", "get_weather", "{}", Some(Seal(Protocol.OpenAiResponses, obj("id" -> s("fc_1")))))
        val req = ModelRequest(
          "说明",
          Chunk(tool),
          Chunk(
            Message.Assistant(Chunk(reasoning, foreign, call)),
            Message.User(Chunk(UserPart.Text("继续"), UserPart.ToolResult("call_1", Chunk(ToolContent.Text("晴")), isError = false)))
          ),
          Thinking.Off,
          cacheKey = Some("sess-1")
        )
        val body  = OpenAiResponsesWire.body("m", req, Some(100))
        val input = body.at("input").arr
        assertTrue(
          input.map(i => i.at("type").str.getOrElse("role:" + i.at("role").strOr())) == Chunk("reasoning", "function_call", "function_call_output", "role:user"),
          input(0).at("encrypted_content").str.contains("enc"),
          input(1).at("id").str.contains("fc_1"),
          body.at("reasoning").at("effort").str.contains("none"),
          body.at("include") == Json.Null,
          body.at("max_output_tokens").long.contains(100L),
          body.at("prompt_cache_key").str.contains("sess-1"),
          OpenAiResponsesWire.body("m", req.copy(cacheKey = None), None).at("prompt_cache_key") == Json.Null,
          !body.toJson.contains("别家的")
        )
      }
    ),
    suite("图片")(
      test("小图原样交回，类型按内容") {
        val data = png(100, 50)
        assertTrue(ImageFit.fit(data) == Right(ImageFit.Fitted.AsIs(ImageFit.Tile("image/png", data))))
      },
      test("长截图切段，每段缩进框里") {
        ImageFit.fit(png(1080, 11499)) match
          case Right(ImageFit.Fitted.Reworked(tiles, note)) =>
            val first = ImageIO.read(java.io.ByteArrayInputStream(tiles.head.data.toArray))
            assertTrue(tiles.size == 6, note.contains("切成 6 段"), first.getWidth <= ImageFit.Box, first.getHeight <= ImageFit.Box)
          case other => assertNever(s"$other")
      },
      test("按 EXIF 方向转正：左上角的点转到对的位置，宽高按需对调") {
        // 3x2 的图，左上角 (0,0) 标红
        val img = BufferedImage(3, 2, BufferedImage.TYPE_INT_ARGB)
        img.setRGB(0, 0, 0xffff0000)
        def red(o: Int) =
          val r = ImageFit.orient(img, o)
          (r.getWidth, r.getHeight, (for x <- 0 until r.getWidth; y <- 0 until r.getHeight if r.getRGB(x, y) == 0xffff0000 yield (x, y)).toList)
        assertTrue(
          red(1) == (3, 2, List((0, 0))),
          red(2) == (3, 2, List((2, 0))),
          red(3) == (3, 2, List((2, 1))),
          red(4) == (3, 2, List((0, 1))),
          red(5) == (2, 3, List((0, 0))),
          red(6) == (2, 3, List((1, 0))),
          red(7) == (2, 3, List((1, 2))),
          red(8) == (2, 3, List((0, 2)))
        )
      },
      test("大图整张缩小一次") {
        ImageFit.fit(png(3000, 2000)) match
          case Right(ImageFit.Fitted.Reworked(tiles, note)) => assertTrue(tiles.size == 1, note.contains("1568x1045"))
          case other                                        => assertNever(s"$other")
      },
      test("坏图换成说明，不让请求失败；超过张数上限的较早图片换成说明") {
        val msgs = Chunk(
          Message.User(Chunk(UserPart.Image("image/png", Chunk.fromArray("not an image".getBytes)))),
          Message.User(Chunk(UserPart.Image("image/png", png(10, 10)))),
          Message.User(Chunk(UserPart.ToolResult("c", Chunk(ToolContent.Image("image/png", png(10, 10))), isError = false)))
        )
        val out = ImagePrep.prepare(msgs, Some(1))
        val texts = out.flatMap {
          case Message.User(ps) => ps.flatMap {
              case UserPart.Text(t)              => Chunk(t)
              case UserPart.ToolResult(_, cs, _) => cs.collect { case ToolContent.Text(t) => t }
              case _                             => Chunk.empty
            }
          case _ => Chunk.empty
        }
        val images = out.flatMap {
          case Message.User(ps) => ps.flatMap {
              case i: UserPart.Image             => Chunk(i)
              case UserPart.ToolResult(_, cs, _) => cs.collect { case i: ToolContent.Image => i }
              case _                             => Chunk.empty
            }
          case _ => Chunk.empty
        }
        assertTrue(images.size == 1, texts.exists(_.contains("发不了")), texts.exists(_.contains("张数上限")))
      }
    ),
    suite("服务商")(
      test("按服务商限并发：名额占到流结束；请求带鉴权与协议头") {
        val settings = ModelSettings(ModelRef("p", "m"), Map("p" -> ProviderSettings(Protocol.Anthropic, "https://example.test/", "k", maxConcurrency = 1)))
        for
          gate    <- Promise.make[Nothing, Unit]
          started <- Ref.make(0)
          seen    <- Ref.make(Chunk.empty[Request])
          http = new OutboundHttp:
                   def send(req: Request) = ZIO.dieMessage("unused")
                   def exchange(req: Request) = open(req)
                   def open(req: Request) =
                     seen.update(_ :+ req) *> started.update(_ + 1) *>
                       ZIO.succeed(Response(body = Body.fromStreamChunked(ZStream.fromZIO(gate.await).drain ++ ZStream.fromChunk(fixture("anthropic-tool-call.sse")))))
          models <- Models.layer.build.provideSomeLayer[Scope](ZLayer.succeed(settings) ++ ZLayer.succeed(http)).map(_.get[Models])
          req     = ModelRequest("s", Chunk.empty, Chunk(Message.User(Chunk(UserPart.Text("hi")))))
          f1     <- models.request(ModelRef("p", "m"), req).runCollect.fork
          f2     <- models.request(ModelRef("p", "m"), req).runCollect.fork
          _      <- ZIO.sleep(200.millis)
          before <- started.get
          _      <- gate.succeed(())
          r1     <- f1.join
          r2     <- f2.join
          after  <- started.get
          reqs   <- seen.get
        yield assertTrue(
          models.problem("p:any").isEmpty,
          models.problem("q:any").exists(_.contains("没有配置服务商 q")),
          models.problem("nocolon").exists(_.contains("<服务商>:<模型名>")),
          before == 1,
          after == 2,
          finished(r1).toolCalls.size == 1,
          finished(r2).toolCalls.size == 1,
          reqs.head.url.encode == "https://example.test/v1/messages",
          reqs.head.rawHeader("x-api-key").contains("k"),
          reqs.head.rawHeader("anthropic-version").contains(AnthropicWire.ApiVersion)
        )
      } @@ TestAspect.withLiveClock,
      test("配置校验：默认模型的服务商要存在，并发至少 1") {
        val p = ProviderSettings(Protocol.OpenAiResponses, "https://x", "k", maxConcurrency = 0)
        val r = ModelSettings.validate(ModelSettings(ModelRef("q", "m"), Map("p" -> p)))
        assertTrue(r.left.exists(m => m.contains("max_concurrency") && m.contains("默认模型")), !p.toString.contains("k)"))
      }
    )
  )
