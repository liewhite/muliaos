package muliao.sessions

/** 运行驱动下一步做什么。由会话现状纯函数算出（[[Plan.next]]），驱动只照做，每做完一步重新算。 */
private[sessions] enum Next:
  /** 没事可做 */
  case Rest
  /** 有排队的来信、没有进行中的运行：开一次新运行 */
  case Start
  /** 有还没执行（或批准后待执行）的调用 */
  case Execute(calls: List[Call])
  /** 有调用在等用户、又来了新信：那些提问和批准当作没答复，接着跑 */
  case Decline(calls: List[Call])
  /** 有调用在等（用户或别的 agent），没有别的可做 */
  case Pause
  /** 请求模型 */
  case Request

private[sessions] object Plan:

  /** @param run    最近一次运行的状态
    * @param open   还没有结果的调用
    * @param queued 有排队的来信
    */
  def next(run: Option[RunState], open: List[Call], queued: Boolean): Next =
    run match
      case Some(RunState.Running | RunState.Suspended) =>
        val runnable  = open.filter(c => c.state == CallState.Pending || c.state == CallState.Approved || c.state == CallState.ApprovedAlways)
        val suspended = open.filter(_.state == CallState.Suspended)
        val declinable = suspended.filter(_.pause.exists {
          case _: Pause.AskUser | _: Pause.Approval => true
          case _: Pause.AwaitThread                 => false
        })
        if runnable.nonEmpty then Next.Execute(runnable)
        else if queued && declinable.nonEmpty then Next.Decline(declinable)
        else if suspended.nonEmpty then Next.Pause
        else Next.Request
      case _ => if queued then Next.Start else Next.Rest
