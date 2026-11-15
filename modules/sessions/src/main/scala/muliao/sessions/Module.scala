package muliao.sessions

import muliao.platform.DomainModule
import muliao.platform.config.ConfigSection

/** 会话与运行：来信、运行驱动（请求模型 → 执行工具，每步落库、重启续跑）、挂起（提问、等批准、等别的 agent）、委派与 thread、
  * 异步工作的等待与心跳、定时唤醒、停止、删除、上下文压缩、时间线投影、实时展示、附件存储、用量记账。
  *
  * 给别的领域用的接口：
  *   - [[Sessions]]：投信 `deliver`（各来源统一入口，投不进说明原因）、工作状态 `states`（全系统唯一判断）、`stop`、
  *     会话等的后台任务 `openJobWait` / `closeJobWait`、时间线 `timeline`（界面与 `session_read` 共用）；
  *   - [[SessionHooks]]：组装时往本领域接东西——各领域给 agent 的工具（上下文 [[RunContext]]）、[[SessionListener]]（订阅
  *     [[SessionEvent]]：回到空闲/最终失败/被停止、等用户、删除）、[[JobControl]]（后台任务的状态与取消）、[[ApprovalHandler]]
  *     （批准后接着执行）、[[UnreadSource]]（未读）；
  *   - 工具挂起时交出 [[Pause]]（提问、等批准）；
  *   - [[Files]]：附件按内容存一份；
  *   - [[SessionsHttp]]：界面的会话、附件、用量接口与会话页实时事件（SSE）。
  *
  * 内部按子领域分成独立服务，依赖只朝下：[[Kernel]]（库、广播、读会话）、[[Slots]] / [[Doorbell]] / [[States]] → [[Mailbox]]（投信、取信、回答去向）
  * → [[Waits]]（等待项 [[WaitRef]]）→ [[Delegations]]（委派与交回）→ [[Requests]]（准备运行、请求模型）/ [[Compaction]] / [[CallRunner]]
  * → [[Driver]]（应门铃驱动运行）、[[Control]]（停止、重试、答复、删除）、[[WaitClock]]（唤醒与心跳）。下层要让会话动起来只按门铃，
  * 不依赖驱动，所以没有环。下一步做什么由纯函数 [[Plan.next]] 从库里的现状算出。
  * 本领域自己的表结构迁移放在 `resources/db/sessions/`。
  */
object Module extends DomainModule:
  val name = "sessions"

  val sections: List[ConfigSection[?]] = List(SessionSettings.section)
