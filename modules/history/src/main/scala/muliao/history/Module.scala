package muliao.history

import muliao.platform.DomainModule

/** 工作历史与活动回顾：只追加的工作记录（库里拒绝改、删）、关键字检索、按天活动汇总、读会话时间线。用量在会话领域。
  *
  * 给别的领域用的接口：
  *   - [[History]]：记、查工作历史，某天的活动，读会话；
  *   - [[HistoryLink.attach]]：组装时把 4 个工具接到会话领域（工具名在 [[HistoryTools.toolNames]]），要在 `Sessions.start` 之前；
  *   - [[HistoryHttp]]：界面的接口（接口声明在 contract 的 `HistoryApi`）；
  *   - 纯函数：检索相关度 [[Search]]、活动回顾的写法 [[OverviewText]]、读会话的条目与筛选 [[Reading]]、长输出分页 [[Pages]]。
  *
  * 本领域自己的表结构迁移放在 `resources/db/history/`。
  */
object Module extends DomainModule:
  val name = "history"
