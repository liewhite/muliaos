package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.ui.*

import scala.concurrent.Future

/** 会话里在等用户的调用做成待办卡片（[[Todo]]，#1097）：等批准的调用、ask_user 的提问。会话时间线与通知页「待你处理」共用；
  * `answer` 把答复交给这次调用（会话页经它的上下文，通知页直接调答复接口）。
  */
object CallAsks:
  def anchor(call: CallId): String = s"ask-${call.value}"

  /** 在等的这次调用的卡片；不是在等用户的（等别的 agent 回答）没有。 */
  def card(c: CallView, place: Todo.Place, answer: AnswerIn => Future[Unit]): Option[HtmlElement] =
    c.pending.collect {
      case PendingView.Approval(summary) => approval(c, summary, place, answer)
      case PendingView.Question(qs)      => question(c, qs, place, answer)
    }

  /** 拒绝、始终允许（以后同样的调用不再问，设置页可撤销）、批准。 */
  private def approval(c: CallView, summary: String, place: Todo.Place, answer: AnswerIn => Future[Unit]): HtmlElement =
    def decide(ok: Boolean, always: Boolean) = () => answer(AnswerIn(approved = Some(ok), always = always))
    Todo.card(
      Todo.Kind.Approval,
      place,
      Seq(span("要你批准"), span(cls := "font-mono text-muted", c.name)),
      c.startedAt,
      Seq(div(cls := "text-sm whitespace-pre-wrap", summary), ToolDetail.view(c)),
      List(
        Todo.Choice("拒绝", decide(false, false)),
        Todo.Choice("始终允许", decide(true, true), hint = Some("以后同样的调用不再问（设置页可撤销）")),
        Todo.Choice("批准", decide(true, false), primary = true)
      ),
      anchor = Some(anchor(c.id))
    )

  /** 单选或多选，每题都可以在「其他」里自己写；全部答完才能提交。 */
  private def question(c: CallView, qs: List[QuestionView], place: Todo.Place, answer: AnswerIn => Future[Unit]): HtmlElement =
    final case class Draft(selected: List[String], other: String):
      def answered: Boolean = selected.nonEmpty || other.trim.nonEmpty
      def text: String       = (selected ++ Option(other.trim).filter(_.nonEmpty)).mkString("、")
    val drafts = Var(qs.map(_ => Draft(Nil, "")))
    def toggle(qi: Int, label: String): Unit = drafts.update(_.zipWithIndex.map { (d, i) =>
      if i != qi then d
      else
        val on = d.selected.contains(label)
        if qs(qi).multiSelect then d.copy(selected = if on then d.selected.filterNot(_ == label) else d.selected :+ label)
        else d.copy(selected = if on then Nil else List(label))
    })
    val form = qs.zipWithIndex.map { (q, qi) =>
      fieldSet(
        cls := "flex flex-col gap-2 text-sm",
        legend(
          cls := "mb-2 flex items-center gap-2",
          Ui.chip(q.header)(),
          span(cls := "font-medium", q.question),
          Option.when(q.multiSelect)(span(cls := "text-xs text-muted", "可多选"))
        ),
        q.options.map { o =>
          val on = drafts.signal.map(_(qi).selected.contains(o.label))
          button(
            tpe := "button",
            role := (if q.multiSelect then "checkbox" else "radio"),
            aria.checked <-- on.map(_.toString),
            onClick --> (_ => toggle(qi, o.label)),
            cls <-- on.map(x => "flex flex-col items-start rounded-xl border px-3 py-2 text-left " + (if x then "border-accent bg-accent-soft" else "border-separator hover:bg-surface-hover")),
            span(o.label),
            Option.when(o.description.nonEmpty)(span(cls := "text-xs text-muted", o.description))
          )
        },
        input(
          placeholder := "其他（自己写）",
          aria.label := s"${q.header}：其他",
          cls := "rounded-xl border border-separator bg-transparent px-3 py-2 outline-none focus:border-accent",
          controlled(value <-- drafts.signal.map(_(qi).other), onInput.mapToValue --> (v => drafts.update(ds => ds.updated(qi, ds(qi).copy(other = v)))))
        )
      )
    }
    val skip = place match
      case Todo.Place.InSession => Some(div(cls := "text-xs text-muted", "也可以不回答，直接在下面发消息。"))
      case _: Todo.Place.Away   => None
    Todo.card(
      Todo.Kind.Question,
      place,
      span("需要你回答"),
      c.startedAt,
      Seq[Modifier[HtmlElement]](form, skip),
      List(Todo.Choice("提交回答", () => answer(AnswerIn(answers = Some(drafts.now().map(_.text)))), primary = true, enabled = drafts.signal.map(_.forall(_.answered)))),
      anchor = Some(anchor(c.id))
    )
