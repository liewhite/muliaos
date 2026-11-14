package muliao.models

import muliao.platform.DomainModule

/** 模型接入与外部服务：Anthropic Messages / OpenAI Responses 流式客户端、服务商并发上限、MCP 客户端、网页搜索与抓取、语音转写。
  *
  * 本领域自己的表结构迁移放在 `resources/db/models/`。
  */
object Module extends DomainModule:
  val name = "models"
