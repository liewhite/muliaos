package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*

/** 配置文件：各 agent 的 agent.toml 与平台的 access.toml（角色与各 agent 的权限），可直接编辑。 */
object ConfigTab:
  def view(list: Signal[List[AssetItem]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      p(
        cls := "text-sm text-muted",
        "agent 的配置和权限也是普通文件，平时在 agent 页与角色页里改；这里可以直接改原文（如修好写坏的文件）。保存前服务端会校验格式。"
      ),
      AssetsPage.listOf(list, "没有配置文件") { s =>
        AssetUi.fileRow(
          Seq(div(cls := "font-mono font-medium", s.now().path), div(cls := "truncate text-xs text-muted", child.text <-- s.map(_.description.getOrElse(""))))
        )(FileEditor.view(s.now().path, s.now().path, rows = 12))
      }
    )
