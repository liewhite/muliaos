package muliao.sessions

import muliao.contract.*
import muliao.models.llm.ReplyPart
import muliao.sessions.store.UploadRow

/** 时间线投影：界面的会话页与 `session_read` 共用。纯函数。 */
object Timeline:

  /** 一次调用怎么展示：按工具名与参数原文（JSON）。 */
  type Shows = (tool: String, args: String) => Display

  def file(u: UploadRow): FileView = FileView(u.id, u.name, u.mediaType, u.size)

  def result(r: CallResult): ResultView =
    ResultView(r.text, r.content.collect { case StoredPart.Image(m, s) => ImageRef(m, s) }.toList, r.isError)

  def pending(p: Pause): PendingView = p match
    case Pause.AskUser(qs) =>
      PendingView.Question(qs.map(q => QuestionView(q.question, q.header, q.options.map(o => OptionView(o.label, o.description)), q.multiSelect)))
    case Pause.Approval(summary, _)    => PendingView.Approval(summary)
    case Pause.AwaitThread(_, th, ag)  => PendingView.Agent(th, ag)

  def call(c: Call, display: Timeline.Shows): CallView =
    CallView(
      id = c.callId,
      name = c.name,
      args = c.args,
      display = display(c.name, c.args),
      state = c.status.state,
      result = c.status.outcome.map(result),
      pending = c.status.waitingOn.map(pending),
      startedAt = c.status.started,
      finishedAt = c.status.finished
    )

  /** 一段回复。工具调用有记录时带上状态与结果；还没落库的（正在生成）是待执行。思考块只为回传给模型，不展示。 */
  def part(p: ReplyPart, calls: Map[CallId, Call], display: Timeline.Shows): Option[PartView] = p match
    case ReplyPart.Text(t, _)     => Some(PartView.Text(t))
    case _: ReplyPart.Reasoning   => None
    case ReplyPart.ToolCall(id, name, args, _) =>
      val cid = CallId.wrap(id)
      Some(PartView.Call(calls.get(cid).map(call(_, display)).getOrElse(CallView(cid, name, args, display(name, args), CallState.Pending, None, None, None, None))))

  def usage(u: muliao.models.llm.Usage): UsageView = UsageView(u.input, u.output, u.cacheRead, u.cacheWrite)

  def items(entries: List[Entry], calls: Map[Long, List[Call]], uploads: Map[FileId, UploadRow], display: Timeline.Shows): List[TimelineItem] =
    entries.map { e =>
      val view = e.body match
        case EntryBody.Context(t) => EntryView.Context(t)
        case EntryBody.Mail(l) =>
          EntryView.Letter(l.source.view, l.text, l.files.toList.flatMap(uploads.get).map(file), l.checkIn)
        case EntryBody.Reply(parts, model, u, _, interrupted) =>
          val byId = calls.getOrElse(e.id, Nil).map(c => c.callId -> c).toMap
          EntryView.Reply(parts.toList.flatMap(part(_, byId, display)), model, usage(u), interrupted)
        case EntryBody.Compaction(s, _) => EntryView.Compaction(s)
        case EntryBody.Notice(k, t)     => EntryView.Notice(k, t)
      TimelineItem(e.id, e.run.map(_.value), e.startedAt, e.finishedAt, view)
    }
