package muliao.models

import muliao.contract.{ModelsApi, ModelsOverview, Transcript, TranscriptionInfo}
import muliao.models.llm.Models
import muliao.models.speech.Transcriber
import muliao.platform.AppError
import muliao.platform.server.{ApiEndpoint, Auth}
import muliao.platform.server.Endpoints.*
import zio.*

/** 本领域的 HTTP 接口。`transcriber` 为空：没配置语音转写。 */
object ModelsHttp:
  def endpoints(auth: Auth, models: Models, transcriber: Option[Transcriber]): List[ApiEndpoint] =
    List(
      ModelsApi.overview.secured(auth)(_ => _ => ZIO.succeed(ModelsOverview(models.defaultModel.toString, models.providers.toList))),
      ModelsApi.listModels.secured(auth)(_ => provider => models.listModels(provider).map(_.toList)),
      ModelsApi.transcriptionInfo.secured(auth)(_ => _ => ZIO.succeed(TranscriptionInfo(transcriber.isDefined, transcriber.map(_.maxBytes)))),
      ModelsApi.transcribe.secured(auth) { _ => (mediaType, audio) =>
        ZIO
          .fromOption(transcriber)
          .orElseFail(AppError.notFound("没有配置语音转写（配置文件 [transcription] 段）"))
          .flatMap(_.transcribe(Chunk.fromArray(audio), mediaType))
          .map(Transcript(_))
      }
    )
