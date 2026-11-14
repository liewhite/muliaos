package muliao.models.llm

import muliao.toolkit.ToolContent
import zio.Chunk

/** 发给模型前整理全部图片（附件与工具结果里的）：每张按 [[ImageFit]] 整理；解不开的换成一句说明（一张坏图不能让会话之后每次
  * 请求都失败）；超过一次请求的张数上限时，较早的换成说明、保留最近的。原件在会话历史里不动。
  */
object ImagePrep:

  def prepare(messages: Chunk[Message], maxImages: Option[Int]): Chunk[Message] =
    capped(messages.map(fitAll), maxImages)

  private def fitAll(m: Message): Message = m match
    case Message.User(parts) =>
      Message.User(parts.flatMap {
        case UserPart.Image(_, data) => fitted(data, UserPart.Text(_), (t: ImageFit.Tile) => UserPart.Image(t.mediaType, t.data))
        case UserPart.ToolResult(id, content, err) =>
          Chunk.single(UserPart.ToolResult(id, content.flatMap {
            case ToolContent.Image(_, data) => fitted(data, ToolContent.Text(_), (t: ImageFit.Tile) => ToolContent.Image(t.mediaType, t.data))
            case other                       => Chunk.single(other)
          }, err))
        case other => Chunk.single(other)
      })
    case a: Message.Assistant => a

  private def fitted[A](data: Chunk[Byte], text: String => A, image: ImageFit.Tile => A): Chunk[A] =
    ImageFit.fit(data) match
      case Right(ImageFit.Fitted.AsIs(t))              => Chunk.single(image(t))
      case Right(ImageFit.Fitted.Reworked(tiles, note)) => text(note) +: tiles.map(image)
      case Left(why)                                   => Chunk.single(text(s"[这张图片发不了给模型：$why]"))

  private val dropped = "[较早的一张图片没有发送：超过一次请求的图片张数上限]"

  /** 从后往前数，超过上限的图片换成说明。 */
  private def capped(messages: Chunk[Message], max: Option[Int]): Chunk[Message] = max match
    case None => messages
    case Some(limit) =>
      // 从最后一条消息往前处理，`left` 是还能保留的张数
      val (_, out) = messages.reverse.foldLeft((limit, List.empty[Message])) { case ((left, acc), m) =>
        m match
          case Message.User(parts) =>
            val (rest, kept) = parts.reverse.foldLeft((left, List.empty[UserPart])) { case ((l, ps), p) =>
              p match
                case i: UserPart.Image => if l > 0 then (l - 1, i :: ps) else (0, UserPart.Text(dropped) :: ps)
                case UserPart.ToolResult(id, content, err) =>
                  val (l2, cs) = content.reverse.foldLeft((l, List.empty[ToolContent])) { case ((ll, cc), c) =>
                    c match
                      case i: ToolContent.Image => if ll > 0 then (ll - 1, i :: cc) else (0, ToolContent.Text(dropped) :: cc)
                      case other                => (ll, other :: cc)
                  }
                  (l2, UserPart.ToolResult(id, Chunk.fromIterable(cs), err) :: ps)
                case other => (l, other :: ps)
            }
            (rest, Message.User(Chunk.fromIterable(kept)) :: acc)
          case a => (left, a :: acc)
      }
      Chunk.fromIterable(out)
