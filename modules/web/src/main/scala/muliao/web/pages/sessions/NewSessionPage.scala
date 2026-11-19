package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** 新会话页：选 agent（地址 `?agent=` 或默认 agent）、写消息、附件、语音，开始后跳到会话页。 */
object NewSessionPage:
  /** @param preset 预选的 agent（地址 `?agent=`） */
  def view(preset: Option[AgentId]): HtmlElement =
    val agents  = AgentsData.all
    val chosen  = Var(preset)
    val prompt  = Var("")
    val uploads = new Uploads
    val action  = new Action
    // 新会话还要选 agent：转写结果先填进输入框，由用户点「开始」
    val voice   = new Voice(text => prompt.update(p => Model.appendTranscript(p, text)))
    val list    = AgentsData.list
    val agent   = chosen.signal.combineWith(list).map((c, as) => c.orElse(as.find(_.isDefault).map(_.id)))
    val canStart = agent.combineWith(prompt.signal, uploads.ids, uploads.blocking).map((a, p, ids, b) => a.isDefined && (p.trim.nonEmpty || ids.nonEmpty) && !b)
    val canNow = Var(false)
    val agentNow = Var(Option.empty[AgentId])
    def start(): Unit =
      // 快捷键不经过按钮的禁用状态
      if !action.busy.now() && canNow.now() then
        agentNow.now().foreach { a =>
          action
            .fire(
              Http.call(SessionsApi.create)(NewSession(a, prompt.now().trim, uploads.done.map(_._2.id))).map(c => Router.update(Route.session(c.id))),
              leave = Some(() => ())
            )
        }
    Ui.page("新会话", back = Some(Route.home))(
      canStart --> canNow,
      agent --> agentNow,
      Ui.loading(agents) { data =>
        div(
          cls := "contents",
          child <-- data.map(_.isEmpty).distinct.map {
            case true =>
              div(cls := "flex flex-col items-center gap-3", Ui.emptyHint("还没有 agent。"), Ui.linkButton(Route.agentNew.url)("创建 agent"))
            case false =>
              Ui.card(
                div(
                  cls := "card__content flex flex-col gap-4",
                  AgentPicker[AgentId]("Agent", agent, a => chosen.set(Some(a)), data, _.id),
                  div(uploads.onPasteFiles, Fields.area("消息", prompt, rowsN = 6, autoFocusOn = true, onSubmit = Some(() => start()))),
                  uploads.pendingView,
                  action.errorView(),
                  Ui.errorAlert(voice.error.signal),
                  div(
                    cls := "flex items-center gap-2",
                    uploads.attachButton(action.busy.signal),
                    voice.button(action.busy.signal),
                    Ui.btn(pending = action.busy.signal, disabledIf = canStart.map(!_))("开始", onClick --> (_ => start()))
                  )
                )
              )
          }
        )
      }
    )
