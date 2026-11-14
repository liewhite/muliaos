package muliao.workspace

import muliao.platform.DomainModule

/** 工作区：agent 工作目录、文件工具、命令与后台任务、skill 脚本、页面（site）、文件浏览。
  *
  * 本领域自己的表结构迁移放在 `resources/db/workspace/`。
  */
object Module extends DomainModule:
  val name = "workspace"
