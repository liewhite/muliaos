package muliao.web.pages.issues

import com.raquo.laminar.api.L.*
import muliao.web.ui.Ui

object IssueView:
  def view(id: Long): HtmlElement = Ui.page("IssueView")(Ui.emptyHint("待实现"))
