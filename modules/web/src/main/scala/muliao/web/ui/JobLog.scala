package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.{JobId, WorkspaceApi}
import muliao.web.core.*

/** 后台任务日志的最后几行：随接口声明的主题刷新，也可以手动刷新。会话的等待面板与 agent 的后台任务页共用。 */
object JobLog:
  /** @param note 标题行里追加的说明（如「运行中」） */
  def view(id: JobId, lines: Int, note: Modifier[HtmlElement] = emptyMod): HtmlElement =
    val reload = new EventBus[Unit]
    val text   = Remote.following(WorkspaceApi.jobLog, reload.events)(Http.call(WorkspaceApi.jobLog)((id, Some(lines))))
    div(
      cls := "flex flex-col gap-1",
      div(
        cls := "flex items-center gap-2 text-xs text-muted",
        span(s"最后 $lines 行"),
        note,
        Ui.btn(Variant.Ghost, Size.Sm)(Icon("RefreshCw"), "刷新", onClick --> (_ => reload.emit(())))
      ),
      Ui.loading(text)(t => pre(cls := Ui.CodeBlock, child.text <-- t.map(s => if s.isEmpty then "（还没有输出）" else s)))
    )
