package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.contract.ProblemView

/** 没加载的资源文件：路径、归属（平台或 agent 名）与原因。AI 资产页与 agent 详情共用。 */
object Problems:
  def alert(ps: List[ProblemView]): HtmlElement =
    Ui.alert(Tone.Warning, "TriangleAlert", s"有 ${ps.size} 个资源文件没加载")(
      cls := "flex flex-col gap-1",
      ps.map(p => div(span(cls := "font-mono break-all", p.path), p.owner.fold("")(o => s"（${Owner.label(o)}）"), "：", p.reason))
    )
