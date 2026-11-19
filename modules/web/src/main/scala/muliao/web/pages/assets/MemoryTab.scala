package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** 记忆：全局、共享（平台的）与各 agent 的私有记忆。 */
object MemoryTab:
  def view(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      p(
        cls := "text-sm text-muted",
        "agent 能用（use）的每一块记忆每次运行都全文放进指令，能改（write）的直接改它的文件。私有记忆只属于它的 agent；共享记忆在角色里写 ",
        code(cls := "font-mono", "*:memory:名字"),
        " 分给一组 agent（成员能用也能改），只读写 ",
        code(cls := "font-mono", "use:memory:名字"),
        "。全局记忆总是存在、人人可读。"
      ),
      Ui.collapsibleCard("新建共享记忆")(form()),
      AssetsPage.listOf(list, "没有记忆")(item(_, agents))
    )

  private def form(): Modifier[HtmlElement] =
    val name = Var("")
    val act  = new Action
    def save(): Unit =
      val n = name.now().trim
      act
        .run(Http.call(ResourcesApi.writeAssetFile)(AssetFileWrite(s"${Owner.Platform}/memory/$n.md", "", None)))
        .foreach(ok => if ok then { Toast.success(s"已新建记忆 $n"); name.set("") })
    Seq(
      Fields.text("名字", name, hint = Some("小写字母、数字、- 和 _"), placeholderText = "trading", mono = true),
      act.errorView(),
      div(Ui.btn(pending = act.busy.signal, disabledIf = name.signal.map(_.trim.isEmpty))("新建", onClick --> (_ => save())))
    )

  /** 能删的：平台的共享记忆（全局记忆与私有记忆不能删）。 */
  def deletable(i: AssetItem): Boolean = i.owner == Owner.Platform && i.name != "global"

  private def item(s: StrictSignal[AssetItem], agents: Signal[List[AgentSummary]]): HtmlElement =
    val act = new Action
    AssetUi.fileRow(
      Seq(div(cls := "font-mono font-medium", s.now().name), div(cls := "truncate font-mono text-xs text-muted", s.now().path)),
      actions = Option.when(deletable(s.now()))(AssetsPage.deleteButton(s, "用它的 agent 从下一次运行起就看不到这些内容了。", act)),
      below = Seq(AssetUi.access(s, agents), act.errorView())
    )(MemoryEditor.view(s.now().path, s.now().name, "能用它的 agent 每次运行都会看到全文；能改它的 agent 也会直接改它的文件。"))
