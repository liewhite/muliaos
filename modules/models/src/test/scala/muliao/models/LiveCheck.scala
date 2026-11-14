package muliao.models

import muliao.models.llm.*
import muliao.models.speech.{TranscriptionSettings, Transcriber}
import muliao.models.web.{DeepSeekWeb, Web, WebSettings}
import muliao.platform.config.ConfigFile
import muliao.platform.http.{HttpSettings, OutboundHttp, SystemProxy}
import muliao.toolkit.ToolContent
import zio.*
import zio.json.ast.Json

import java.nio.file.{Files, Paths}

/** 对真实服务商各跑一次小请求（不在 `sbt test` 里，手动跑）：
  * {{{
  * sbt "models/Test/runMain muliao.models.LiveCheck <配置文件> [服务商:模型[@思考档位],… [录音文件 媒体类型]]"
  * }}}
  * 配置文件要有 `[models]`，可选 `[web]`、`[transcription]`。只打印回复内容、用量与结论，不打印密钥。
  */
object LiveCheck extends ZIOAppDefault:

  val weather = ToolDef("get_weather", "Get the current weather for a city.",
    Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj("city" -> Json.Obj("type" -> Json.Str("string"))), "required" -> Json.Arr(Json.Str("city"))))

  def show(r: Reply): String =
    val parts = r.parts.map {
      case ReplyPart.Text(t, s)            => s"文字「${t.take(80)}」${s.fold("")(_ => " +seal")}"
      case ReplyPart.Thinking(t, s)        => s"思考 ${t.length} 字${s.fold("")(_ => " +seal")}"
      case ReplyPart.ToolCall(_, n, a, s)  => s"调用 $n($a)${s.fold("")(_ => " +seal")}"
    }
    s"${r.model} ${parts.mkString("；")}，用量 ${r.usage}，结束 ${r.stopReason.getOrElse("?")}"

  def finished(events: Chunk[ReplyEvent]): Reply = events.collectFirst { case ReplyEvent.Finished(r) => r }.get

  /** 两轮：先让它调工具，再把回复（含思考与 seal）和工具结果发回去，确认服务端收下回传的内容。 */
  def roundTrip(models: Models, model: ModelRef, thinking: Thinking): Task[Unit] =
    // 指定了思考档位时出一道要想一想的题，好让回复里有思考、验证回传
    val question =
      if thinking == Thinking.Default then "What's the weather in Paris? Use the tool, then answer in one short sentence."
      else "Which European capital has a population closest to 37 * 59 * 1009 people? Reason it through carefully, then use the tool for that city and answer in one short sentence."
    val ask   = Message.User(Chunk(UserPart.Text(question)))
    val first = ModelRequest("Be brief.", Chunk(weather), Chunk(ask), thinking, Some(2048))
    for
      r1 <- models.request(model, first).runCollect.map(finished)
      _  <- Console.printLine(s"[$model] 第一轮：${show(r1)}")
      results = r1.toolCalls.map(c => UserPart.ToolResult(c.id, Chunk(ToolContent.Text("Sunny, 21°C")), isError = false))
      r2 <- models.request(model, first.copy(messages = Chunk(ask, Message.Assistant(r1.parts), Message.User(results)))).runCollect.map(finished)
      _  <- Console.printLine(s"[$model] 第二轮：${show(r2)}")
    yield ()

  def run =
    getArgs.flatMap { args =>
      val path = Paths.get(args.head)
      val all  = (Module.sections ++ Module.optionalSections).map(_.key).toSet
      (for
        file   <- ConfigFile.load(path, all)
        ms     <- file.section(ModelSettings.section)
        webS   <- file.optionalSection(WebSettings.section)
        speech <- file.optionalSection(TranscriptionSettings.section)
        http   <- (ZLayer.succeed(HttpSettings()) ++ SystemProxy.macOS() >>> OutboundHttp.layer).build.map(_.get[OutboundHttp])
        models <- Models.layer.build.provideSomeLayer[Scope](ZLayer.succeed(ms) ++ ZLayer.succeed(http)).map(_.get[Models])
        _ <- ZIO.foreachDiscard(models.providers)(p =>
               models.listModels(p).foldZIO(e => Console.printLine(s"[$p] 模型列表失败：${e.message}"), ns => Console.printLine(s"[$p] 模型列表 ${ns.size} 个：${ns.take(5).mkString(", ")}…"))
             )
        // 模型写成 <服务商>:<模型名>[@思考档位]，逗号分隔
        targets = args.drop(1).headOption.toList.flatMap(_.split(',')).map { t =>
                    val (m, level) = t.split("@", 2) match
                      case Array(m, l) => (m, Thinking.parse(l).toOption.get)
                      case Array(m)    => (m, Thinking.Default)
                    (ModelRef.parse(m).toOption.get, level)
                  }
        _ <- ZIO.foreachDiscard(targets)((m, level) => roundTrip(models, m, level).catchAll(e => Console.printLine(s"[$m] 失败：$e")))
        _ <- ZIO.foreachDiscard(webS) { w =>
               val web: Web = DeepSeekWeb(w, http)
               locally {
                 web.search("Scala 3 ZIO", 2).foldZIO(e => Console.printLine(s"[web] 搜索失败：${e.message}"), hs => Console.printLine(s"[web] 搜索：${hs.map(_.title).mkString(" | ")}")) *>
                   web.fetch("https://example.com/").foldZIO(e => Console.printLine(s"[web] 抓取失败：${e.message}"), p => Console.printLine(s"[web] 抓取：${p.title} ${p.content.take(80).replace('\n', ' ')}"))
               }
             }
        _ <- ZIO.foreachDiscard(speech.zip(args.drop(2).headOption).zip(args.drop(3).headOption)) { case ((settings, audio), media) =>
               Transcriber.Dashscope(settings, http)
                 .transcribe(Chunk.fromArray(Files.readAllBytes(Paths.get(audio))), media)
                 .foldZIO(e => Console.printLine(s"[语音] 失败：${e.message}"), text => Console.printLine(s"[语音] 转写：$text"))
             }
      yield ()).provideSomeLayer[ZIOAppArgs](Scope.default)
    }
