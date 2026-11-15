package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*
import org.scalajs.dom

import scala.util.{Failure as TryFailure, Success}

import Http.ec

/** 消息里的附件：图片显示缩略图（点开看原图），其他文件是下载链接。 */
object Attachments:
  def url(f: FileView): String = Http.href(SessionsApi.download)(f.id)

  private val inlineTypes = Set("image/png", "image/jpeg", "image/gif", "image/webp")
  def isImage(f: FileView): Boolean = inlineTypes(f.mediaType)

  def list(files: List[FileView], alignEnd: Boolean = false): Option[HtmlElement] =
    Option.when(files.nonEmpty) {
      val (images, others) = files.partition(isImage)
      div(
        cls := s"flex max-w-[88%] flex-col gap-1.5 " + (if alignEnd then "items-end self-end" else "items-start self-start"),
        Option.when(images.nonEmpty)(
          div(
            cls := "flex flex-wrap gap-1.5" + (if alignEnd then " justify-end" else ""),
            images.map(f =>
              a(href := url(f), target := "_blank", rel := "noreferrer",
                img(src := url(f), alt := f.name, Ui.attr("loading") := "lazy", cls := "max-h-48 max-w-full rounded-xl border border-separator object-contain"))
            )
          )
        ),
        others.map(f =>
          a(
            href := url(f),
            cls := "flex max-w-full items-center gap-2 rounded-xl border border-separator bg-surface px-3 py-2 text-sm hover:bg-surface-hover",
            Icon("FileText", "size-4 shrink-0 text-muted"),
            span(cls := "truncate", f.name)
          )
        )
      )
    }

  /** 工具结果里的图片（按内容摘要取）。 */
  def images(refs: List[ImageRef]): Option[HtmlElement] =
    Option.when(refs.nonEmpty)(
      div(
        cls := "flex flex-wrap gap-1.5",
        refs.map(r =>
          val u = Http.href(SessionsApi.blob)(r.sha256)
          a(href := u, target := "_blank", rel := "noreferrer", img(src := u, alt := "", Ui.attr("loading") := "lazy", cls := "max-h-48 max-w-full rounded-xl border border-separator object-contain"))
        )
      )
    )

/** 发送前的附件：选中或粘贴即上传（先上传拿到编号，发消息时引用），失败的留在列表里显示原因。 */
final class Uploads:
  enum State:
    case Uploading
    case Done(file: FileView)
    case Failed(error: String)

  final case class Upload(key: Int, name: String, state: State)

  val items: Var[List[Upload]] = Var(Nil)
  private var next              = 0

  def add(files: Seq[dom.File]): Unit = files.foreach { file =>
    next += 1
    val key = next
    items.update(_ :+ Upload(key, file.name, State.Uploading))
    val mediaType = if file.`type`.isEmpty then "application/octet-stream" else file.`type`
    Http.bytesOf(file).flatMap(bytes => Http.call(SessionsApi.upload)(muliao.contract.Upload(file.name, mediaType, bytes))).onComplete { r =>
      val st = r match
        case Success(info) => State.Done(info)
        case TryFailure(e) => State.Failed(Http.messageOf(e))
      items.update(_.map(u => if u.key == key then u.copy(state = st) else u))
    }
  }

  def remove(keys: Set[Int]): Unit = items.update(_.filterNot(u => keys(u.key)))

  /** 已上传好的一个附件：列表里的键与文件。 */
  final case class Ready(key: Int, file: FileView)

  /** 已上传好的：发消息时引用它们的编号。 */
  def done: List[Ready] = items.now().collect { case Upload(k, _, State.Done(f)) => Ready(k, f) }

  val ids: Signal[List[FileId]] = items.signal.map(_.collect { case Upload(_, _, State.Done(f)) => f.id })

  /** 有正在上传或失败未移除的：此时不能发送，免得以为附件发出去了。 */
  val blocking: Signal[Boolean] = items.signal.map(_.exists(_.state match
    case State.Done(_) => false
    case _             => true))

  /** 粘贴截图等文件时直接作为附件。 */
  val onPasteFiles: Modifier[HtmlElement] = onPaste --> { (e: dom.ClipboardEvent) =>
    val files = e.clipboardData.files
    if files.length > 0 then
      e.preventDefault()
      add((0 until files.length).map(files(_)))
  }

  def attachButton(disabledIf: Signal[Boolean]): HtmlElement =
    val picker = input(
      tpe := "file",
      multiple := true,
      hidden := true,
      onChange --> { e =>
        val in = e.target.asInstanceOf[dom.html.Input]
        add((0 until in.files.length).map(in.files(_)))
        // 允许再次选择同一个文件
        in.value = ""
      }
    )
    span(
      cls := "contents",
      picker,
      Ui.iconBtn("Paperclip", "添加附件", size = Size.Md, iconCls = "size-4", disabledIf = disabledIf)(onClick --> (_ => picker.ref.click()))
    )

  val pendingView: HtmlElement = div(
    cls := "flex flex-wrap gap-1.5 empty:hidden",
    children <-- items.signal.splitSeq(_.key) { u =>
      val x = u.now()
      div(
        cls <-- u.map(v =>
          "flex max-w-full items-center gap-1.5 rounded-lg border px-2 py-1 text-xs " + (v.state match
            case State.Failed(_) => "border-danger text-danger"
            case _               => "border-separator")
        ),
        title <-- u.map(v => v.state match
          case State.Failed(e) => e
          case _               => v.name),
        child <-- u.map(_.state).map {
          case State.Uploading                          => Ui.spinner("spinner--sm")
          case State.Done(f) if Attachments.isImage(f) => img(src := Attachments.url(f), alt := "", cls := "size-6 rounded object-cover")
          case _                                        => Icon("FileText", "size-3.5 shrink-0")
        },
        span(cls := "truncate", child.text <-- u.map(v => v.state match
          case State.Failed(e) => s"${v.name}：$e"
          case _               => v.name)),
        button(
          tpe := "button",
          cls := "shrink-0 text-muted hover:text-foreground",
          aria.label := s"移除 ${x.name}",
          onClick --> (_ => remove(Set(x.key))),
          Icon("X", "size-3.5")
        )
      )
    }
  )
