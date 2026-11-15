package muliao.triggers

import muliao.platform.DomainModule
import muliao.platform.config.ConfigSection

/** 触发器：定时（cron，按配置时区）、一次性、常驻流；前置检查、防重叠与跳过计数、挡路通知、停机错过补一次、脚本故障与告警会话、
  * 立即运行、归档 agent 的处理。
  *
  * 给别的领域与组装入口用的接口：
  *   - [[Triggers]]：界面与工具的入口；`start` 在 `Sessions.start` 之后调用，起到期扫描与常驻流托管（随 layer 的 scope 停下）；
  *   - [[Triggers.contribution]]：交给会话与资源领域的 6 个工具（静态定义在 [[Triggers.specs]]）与 agent 删除时的清理；
  *   - [[TriggersHttp]]：界面的接口。
  *
  * 本领域自己的表结构迁移放在 `resources/db/triggers/`。
  */
object Module extends DomainModule:
  val name = "triggers"

  override val sections: List[ConfigSection[?]] = List(TriggerSettings.section)
