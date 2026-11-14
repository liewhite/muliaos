package muliao.resources

import muliao.platform.DomainModule

/** 资源、agent 与权限：记忆、skill、MCP 配置、secret（vault）、agent 配置、角色与分发规则、写沙箱可写范围。
  *
  * 本领域自己的表结构迁移放在 `resources/db/resources/`。
  */
object Module extends DomainModule:
  val name = "resources"
