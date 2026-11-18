package muliao.history

import muliao.contract.{EntryView, PartView, SourceView, TimelineItem}

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import scala.util.matching.Regex

/** `session_read` 读出的一条。`label`：USER / TRIGGER / ISSUE / DELEGATION / EVENT（来信）、ASSISTANT（回答正文）、`call <工具>`、`result <工具>`、
  * `error <工具>`、CONTEXT（运行上下文）、COMPACTION（压缩摘要）、`NOTICE <种类>`（失败、停止等）。`clippable`：正文按 clip 截断（工具调用、结果、事件等）。
  */
/** 读出的一个会话：基本信息与按时间排的条目。 */
final case class SessionRead(info: muliao.sessions.SessionInfo, items: List[ReadItem])

final case class ReadItem(label: String, detail: Option[String], at: Long, text: String, clippable: Boolean)

/** 筛选条件：`labels` 里任一个等于标签或是它加空格的开头（`call` 匹配 `call run_shell`）；`grep` 在标签与正文里找（不分大小写）；`since` 之后（含）的。 */
final case class ReadFilter(labels: List[String] = Nil, grep: Option[Regex] = None, since: Option[Long] = None):
  def accepts(i: ReadItem): Boolean =
    (labels.isEmpty || labels.exists(l => i.label == l || i.label.startsWith(l + " "))) &&
      grep.forall(r => r.findFirstIn(i.label).isDefined || r.findFirstIn(i.text).isDefined) &&
      since.forall(i.at >= _)

/** 会话时间线 → 按时间排的可读条目（纯函数，复用会话领域给界面的时间线投影）。 */
object Reading:

  def items(timeline: List[TimelineItem]): List[ReadItem] =
    timeline.flatMap { t =>
      t.entry match
        case EntryView.Context(text)     => List(ReadItem("CONTEXT", None, t.startedAt, text, clippable = true))
        case EntryView.Compaction(text)  => List(ReadItem("COMPACTION", None, t.startedAt, text, clippable = true))
        case EntryView.Notice(kind, text) => List(ReadItem(s"NOTICE ${kind.label}", None, t.startedAt, text, clippable = false))
        case EntryView.Letter(source, text, fs, _) =>
          val files = if fs.isEmpty then "" else "\n附件：" + fs.map(f => s"${f.name}（${f.mediaType}，${f.size} 字节）").mkString("，")
          val (label, detail, clippable) = Reading.source(source)
          List(ReadItem(label, detail, t.startedAt, text + files, clippable))
        case EntryView.Reply(parts, _, _, interrupted) =>
          val cut = if interrupted then List(ReadItem("NOTICE Interrupted", None, t.finishedAt, "这条回复生成到一半被打断，上面是已生成的部分", clippable = false)) else Nil
          parts.flatMap {
              case PartView.Text(text) => List(ReadItem("ASSISTANT", None, t.startedAt, text, clippable = false))
              case PartView.Call(c) =>
                val call = ReadItem(s"call ${c.name}", None, c.startedAt.getOrElse(t.finishedAt), c.args, clippable = true)
                val result = c.result.map { r =>
                  val images = if r.images.isEmpty then "" else s"\n[图片 ${r.images.size} 张]"
                  ReadItem(s"${if r.isError then "error" else "result"} ${c.name}", None, c.finishedAt.getOrElse(t.finishedAt), r.text + images, clippable = true)
                }
                call :: result.toList
          } ++ cut
    }

  /** 来信的标签、补充（触发器名、issue 号、委派方、事件种类）、正文能不能截断（只有事件能）。 */
  private def source(s: SourceView): (String, Option[String], Boolean) = s match
    case SourceView.User                => ("USER", None, false)
    case SourceView.Trigger(n)          => ("TRIGGER", Some(n.value), false)
    case SourceView.Issue(i)            => ("ISSUE", Some(s"#${i.value}"), false)
    case SourceView.Delegation(from, a) => ("DELEGATION", Some(s"${a.value}，会话 ${from.value}"), false)
    case SourceView.Event(k)            => ("EVENT", Some(k.label), true)

  /** 一条的写法：编号、时间、标签，正文另起一行；可截断的超过 `clip` 字（0 不截）时截断并说明。 */
  def render(n: Int, i: ReadItem, clip: Int, zone: ZoneId): String =
    val time = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(zone).format(Instant.ofEpochMilli(i.at))
    val body =
      if i.clippable && clip > 0 && i.text.length > clip then i.text.take(clip) + s"\n[截断：共 ${i.text.length} 字，clip=0 看全文]"
      else i.text
    s"[$n] $time ${i.label}${i.detail.fold("")(d => s"（$d）")}\n$body"
