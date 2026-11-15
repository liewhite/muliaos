package muliao.models

import muliao.models.llm.ModelSettings
import muliao.models.speech.TranscriptionSettings
import muliao.models.web.WebSettings
import muliao.platform.DomainModule
import muliao.platform.config.ConfigSection

/** 模型接入与外部服务。
  *
  *   - `llm`：统一的消息与回复模型（[[llm.Message]]、[[llm.Reply]]、流式事件 [[llm.ReplyEvent]]）；Anthropic Messages 与 OpenAI
  *     Responses 两种协议（[[llm.Wire]]）；[[llm.Models]] 请求一次得到一条回复、服务商并发上限、模型列表；发给模型前整理图片。
  *   - `mcp`：[[mcp.McpConnector]] 把 MCP server 连成给 agent 的工具。
  *   - `web`：`web_search`、`web_fetch` 工具（[[web.WebTools]]）。
  *   - `speech`：语音转文字（[[speech.Transcriber]]）。
  *
  * 本领域没有自己的表。
  */
object Module extends DomainModule:
  val name = "models"

  /** 必填的配置段。 */
  override val sections: List[ConfigSection[?]] = List(ModelSettings.section)

  /** 配了才有对应功能的配置段（用 `ConfigFile.optionalSection` 读）。 */
  override val optionalSections: List[ConfigSection[?]] = List(WebSettings.section, TranscriptionSettings.section)
