package muliao.sessions

import muliao.contract.*
import muliao.models.llm.ReplyPart
import muliao.sessions.store.UploadRow

/** 时间线投影：界面的会话页与 `session_read` 共用。纯函数。 */
object Timeline:

  def file(u: UploadRow): FileView = FileView(u.id, u.name, u.mediaType, u.size)

  def result(r: CallResult): ResultView =
    ResultView(r.text, r.content.collect { case StoredPart.Image(m, s) => ImageRef(m, s) }.toList, r.isError)

  def pending(p: Pause): PendingView = p match
    case Pause.AskUser(qs) =>
      PendingView.Question(qs.map(q => QuestionView(q.question, q.header, q.options.map(o => OptionView(o.label, o.description)), q.multiSelect)))
    case Pause.Approval(_, summary, _) => PendingView.Approval(summary)
    case Pause.AwaitThread(_, th, ag)  => PendingView.Agent(th, ag)

  def call(c: Call, display: String => Display): CallView =
    CallView(
      id = c.callId,
      name = c.name,
      args = c.args,
      display = display(c.name),
      state = c.state,
      result = c.result.map(result),
      pending = c.pause.filter(_ => c.state == CallState.Suspended).map(pending),
      startedAt = c.startedAt,
      finishedAt = c.finishedAt
    )

  /** 一段回复。工具调用有记录时带上状态与结果。 */
  def part(p: ReplyPart, calls: Map[CallId, Call], display: String => Display): PartView = p match
    case ReplyPart.Text(t, _)     => PartView("text", Some(t), None)
    case ReplyPart.Thinking(t, _) => PartView("thinking", Some(t), None)
    case ReplyPart.ToolCall(id, name, args, _) =>
      val view = calls.get(CallId.wrap(id)).map(call(_, display)).getOrElse(CallView(CallId.wrap(id), name, args, display(name), CallState.Pending, None, None, None, None))
      PartView("call", None, Some(view))

  def usage(u: muliao.models.llm.Usage): UsageView = UsageView(u.input, u.output, u.cacheRead, u.cacheWrite)

  def letterSource(l: Letter): Option[String] = l.source match
    case Source.User                => None
    case Source.Trigger(n)          => Some(n.value)
    case Source.Issue(i)            => Some(s"#${i.value}")
    case Source.Delegation(from, a) => Some(s"${a.value}（会话 ${from.value}）")
    case Source.Event(k)            => Some(k.label)

  def items(entries: List[Entry], calls: Map[Long, List[Call]], uploads: Map[FileId, UploadRow], display: String => Display): List[TimelineItem] =
    entries.map { e =>
      val base = TimelineItem(e.id, e.body.entryKind.label, e.run.map(_.value), e.startedAt, e.finishedAt)
      e.body match
        case EntryBody.Context(t) => base.copy(text = Some(t))
        case EntryBody.Mail(l) =>
          base.copy(
            source = Some(l.source.label),
            sourceDetail = letterSource(l),
            text = Some(l.text),
            files = l.files.toList.flatMap(uploads.get).map(file),
            checkIn = l.checkIn
          )
        case EntryBody.Reply(parts, model, u, _, interrupted) =>
          val byId = calls.getOrElse(e.id, Nil).map(c => c.callId -> c).toMap
          base.copy(parts = parts.toList.map(part(_, byId, display)), model = Some(model), usage = Some(usage(u)), interrupted = interrupted)
        case EntryBody.Compaction(s, _) => base.copy(text = Some(s))
        case EntryBody.Notice(k, t)     => base.copy(noticeKind = Some(k.toString), text = Some(t))
    }
