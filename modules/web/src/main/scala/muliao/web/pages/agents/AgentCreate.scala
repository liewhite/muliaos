package muliao.web.pages.agents

import com.raquo.laminar.api.L.*
import muliao.contract.{AgentDefaults, ResourcesApi}
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** /agents/new：新建 agent，提交前预览它最终能用的工具与资源。 */
object AgentCreate:
  def view(): HtmlElement =
    val defaults = Remote(ResourcesApi.agentDefaults)(())
    Ui.page("新建 agent", back = Some(Route.Agents))(
      Ui.card(
        div(
          cls := "card__content flex flex-col gap-4",
          Ui.loading(defaults) { d =>
            // 默认值只在打开时取一次，之后由表单自己管
            child <-- d.distinct.map(form)
          }
        )
      )
    )

  private def form(defaults: AgentDefaults): HtmlElement =
    val value = Var(AgentForm.blank(defaults))
    val act   = new Action
    def submit(): Unit =
      act
        .run(Http.call(ResourcesApi.createAgent)(AgentForm.create(value.now())).map { d =>
          Toast.success("已创建")
          Router.replace(Route.Agent(d.summary.id.value).url)
        })
        .foreach(_ => ())
    div(
      cls := "flex flex-col gap-4",
      AgentForm.fields(None, value, defaults.thinking),
      act.errorView(),
      div(Ui.btn(pending = act.busy.signal)("保存", onClick --> (_ => submit())))
    )
