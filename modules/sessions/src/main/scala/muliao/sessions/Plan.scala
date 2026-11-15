package muliao.sessions

import muliao.contract.RunId

/** 进行中的运行：编号与状态。 */
private[sessions] final case class ActiveRun(id: RunId, suspended: Boolean)

/** 运行驱动下一步做什么。由会话现状纯函数算出（[[Plan.next]]），驱动只照做，每做完一步重新算。 */
private[sessions] enum Next:
  /** 没事可做 */
  case Rest
  /** 有排队的来信、没有进行中的运行：开一次新运行 */
  case Start
  /** 有还没执行（或批准后待执行）的调用 */
  case Execute(run: ActiveRun, calls: List[Call])
  /** 有调用在等用户、又来了新信：那些提问和批准当作没答复，接着跑 */
  case Decline(run: ActiveRun, calls: List[Call])
  /** 有调用在等（用户或别的 agent），没有别的可做 */
  case Pause(run: ActiveRun)
  /** 请求模型 */
  case Request(run: ActiveRun)

private[sessions] object Plan:

  /** @param run    进行中的运行（运行中或挂起；最近一次已结束则为 None）
    * @param open   还没有结果的调用
    * @param queued 有排队的来信
    */
  def next(run: Option[ActiveRun], open: List[Call], queued: Boolean): Next =
    run match
      case None => if queued then Next.Start else Next.Rest
      case Some(r) =>
        val runnable   = open.filter(_.status.runnable)
        val suspended  = open.collect { case c @ Call(_, _, _, _, _, CallStatus.Suspended(p, _)) => c -> p }
        val declinable = suspended.collect { case (c, p) if p.declinable => c }
        if runnable.nonEmpty then Next.Execute(r, runnable)
        else if queued && declinable.nonEmpty then Next.Decline(r, declinable)
        else if suspended.nonEmpty then Next.Pause(r)
        else Next.Request(r)
