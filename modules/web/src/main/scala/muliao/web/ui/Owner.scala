package muliao.web.ui

/** 资源的归属：平台或某个 agent（按名字）。 */
object Owner:
  val Platform = "platform"

  def label(owner: String): String = if owner == Platform then "平台" else owner
