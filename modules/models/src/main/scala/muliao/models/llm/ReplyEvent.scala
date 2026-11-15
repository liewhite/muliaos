package muliao.models.llm

import zio.Chunk

/** 增量长在段的哪一部分（定义在 contract，实时事件里原样给界面）。 */
export muliao.contract.Growth

/** 一次请求的流式事件。最后一个总是 [[ReplyEvent.Finished]]（流中途出错则以失败结束、没有它）。
  *
  * 段按出现顺序编号（从 0 起）。用 [[ReplyBuilder]] 把事件折叠成到目前为止的各段：界面实时展示、被打断时保存已生成的部分都靠它。
  */
enum ReplyEvent:
  case Started(index: Int, part: ReplyPart)
  case Grew(index: Int, growth: Growth, text: String)

  /** 整段换成新的（协议给出完整项、签名等时）。 */
  case Replaced(index: Int, part: ReplyPart)

  case Finished(reply: Reply)

/** 按事件长出来的各段。纯函数：事件与已有的段对不上时报协议错误。 */
final case class ReplyBuilder(parts: Chunk[ReplyPart]):

  def apply(e: ReplyEvent): Either[String, ReplyBuilder] = e match
    case ReplyEvent.Started(i, p) =>
      Either.cond(i == parts.size, ReplyBuilder(parts :+ p), s"第 $i 段开始时已有 ${parts.size} 段")
    case ReplyEvent.Replaced(i, p) =>
      Either.cond(parts.isDefinedAt(i), ReplyBuilder(parts.updated(i, p)), s"替换不存在的第 $i 段")
    case ReplyEvent.Grew(i, g, t) =>
      parts.lift(i).toRight(s"增量指向不存在的第 $i 段").flatMap(p => ReplyBuilder.grow(p, g, t)).map(p => ReplyBuilder(parts.updated(i, p)))
    case ReplyEvent.Finished(r) => Right(ReplyBuilder(r.parts))

object ReplyBuilder:
  val empty: ReplyBuilder = ReplyBuilder(Chunk.empty)

  private def grow(p: ReplyPart, g: Growth, t: String): Either[String, ReplyPart] = (p, g) match
    case (x: ReplyPart.Text, Growth.Text)         => Right(x.copy(text = x.text + t))
    case (x: ReplyPart.Thinking, Growth.Thinking) => Right(x.copy(text = x.text + t))
    case (x: ReplyPart.ToolCall, Growth.ToolArgs) => Right(x.copy(args = x.args + t))
    case _                                        => Left(s"${g} 增量对不上段的种类 ${p.getClass.getSimpleName}")
