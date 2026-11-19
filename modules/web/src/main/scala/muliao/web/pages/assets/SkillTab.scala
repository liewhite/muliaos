package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

/** Skill：点进编辑页。 */
object SkillTab:
  def view(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement =
    div(
      cls := "flex flex-col gap-3",
      div(Ui.linkButton(Route.skill(None).url, Variant.Secondary)(Icon("Plus"), "新建 skill")),
      AssetsPage.listOf(list, "没有 skill") { s =>
        val url = SkillEditor.url(s.now().name, s.now().owner)
        Ui.card(
          div(
            cls := "card__header",
            Ui.appLink(url)(cls := "hover:underline", h3(cls := "card__title font-mono text-base", s.now().name)),
            p(cls := "card__description", child.text <-- s.map(_.description.getOrElse("")))
          ),
          div(cls := "card__content", AssetUi.access(s, agents))
        )
      }
    )
