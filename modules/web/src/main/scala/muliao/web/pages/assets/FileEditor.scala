package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** 编辑一个资源文件（记忆、SKILL.md、MCP 配置、agent.toml、access.toml）：随 assets 主题刷新；保存带打开编辑时的指纹，期间被别人改过就冲突、提示重来。 */
object FileEditor:
  /** 保存失败的提示：冲突时说明怎么重来。 */
  def failureText(t: Throwable): String = Http.failureOf(t) match
    case Failure.Api(e) if e.kind == ErrorKind.Conflict =>
      s"${e.message}\n文件在你编辑期间被别人改过：先复制你的修改，点「放弃修改」载入最新内容，再改一次。"
    case f => f.message

  /** @param hint 文本框下的说明（拿到正在改的内容，如显示字数）
    * @param extra 按钮行里追加的操作（如删除）
    */
  def view(
      path: String,
      labelText: String,
      rows: Int = 14,
      hint: Signal[String] => Modifier[HtmlElement] = _ => emptyMod,
      saveLabel: String = "保存",
      extra: Modifier[HtmlElement] = emptyMod
  ): HtmlElement =
    val file = Remote(ResourcesApi.readAssetFile)(path)
    div(
      cls := "flex flex-col gap-3",
      Ui.loading(file)(data => Draft.form(data, identity[AssetFile])(draft => editor(draft, labelText, rows, hint, saveLabel, extra)))
    )

  private def editor(
      draft: Draft[AssetFile],
      labelText: String,
      rows: Int,
      hint: Signal[String] => Modifier[HtmlElement],
      saveLabel: String,
      extra: Modifier[HtmlElement]
  ): HtmlElement =
    val content = draft.value.zoom(_.content)((f, c) => f.copy(content = c))
    val busy    = Var(false)
    val error   = Var(Option.empty[String])
    def save(): Unit =
      busy.set(true)
      error.set(None)
      val f = draft.now
      Http.call(ResourcesApi.writeAssetFile)(AssetFileWrite(f.path, f.content, draft.opened.fingerprint)).onComplete { r =>
        busy.set(false)
        r.fold(
          t => error.set(Some(failureText(t))),
          saved =>
            draft.saved(saved)
            Toast.success("已保存")
            Events.refreshNow.emit(())
        )
      }
    div(
      cls := "flex flex-col gap-3",
      Fields.area(labelText, content, hint = Some(hint(content.signal)), rowsN = rows, mono = true),
      Ui.errorAlert(error.signal, "保存失败"),
      div(
        cls := "flex flex-wrap gap-2",
        AssetUi.submit(saveLabel, busy.signal, draft.dirty)(onClick --> (_ => save())),
        child.maybe <-- draft.dirty.map(d =>
          Option.when(d)(Ui.btn(Variant.Ghost)("放弃修改", onClick --> { _ => draft.discard(); error.set(None) }))
        ),
        extra
      ),
      div(cls := "font-mono text-xs break-all text-muted", draft.now.path)
    )
