package muliao.workspace

/** 给人看的时长：45s、3m12s、1h02m。 */
object Durations:
  def human(ms: Long): String =
    val s = (ms / 1000).max(0)
    if s < 60 then s"${s}s"
    else if s < 3600 then f"${s / 60}m${s % 60}%02ds"
    else f"${s / 3600}h${s % 3600 / 60}%02dm"
