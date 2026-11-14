package muliao.web.ui

import com.raquo.laminar.api.L.*
import muliao.web.core.*

import scala.concurrent.Future
import scala.scalajs.js
import scala.util.{Failure as TryFailure, Success, Try}

import Http.ec

/** 目录里的一项。大小与修改时间取不到（如 skill 目录只给文件名）时为空。 */
final case class FileEntry(name: String, dir: Boolean, size: Option[Long], modifiedAt: Option[Long])

/** 一层目录的内容；`truncated`：条目太多只列了前面一部分。 */
final case class DirContent(entries: List[FileEntry], truncated: Boolean)

/** 浏览的是哪个目录。路径一律相对目录根（空串是根本身），只由列出的名字拼出；能不能走出目录由服务端把关。 */
trait DirSource:
  def list(path: String): Future[DirContent]
  /** 文件的原始内容（文本预览用）。 */
  def read(path: String): Future[Array[Byte]]
  /** 文件的地址（图片内联显示、下载）。 */
  def url(path: String): String

/** 文件预览的方式。 */
enum PreviewKind:
  case Image
  case Markdown
  /** 文本；`language` 是 highlight.js 的语言名，认不出为空（按纯文本显示） */
  case Text(language: Option[String])
  case TooBig

/** 文件浏览的纯函数：路径拼接、预览方式、高亮语言、大小写法。 */
object FilePaths:
  /** 超过它的文件不预览，只给下载 */
  val MaxPreviewBytes: Long = 1_000_000
  /** 超过它的文本不做语法高亮（高亮在主线程同步执行） */
  val MaxHighlightChars: Int = 200_000

  def join(dir: String, name: String): String = if dir.isEmpty then name else s"$dir/$name"

  /** join 的反向：(所在目录, 名字) */
  def split(path: String): (String, String) =
    path.lastIndexOf('/') match
      case -1 => ("", path)
      case i  => (path.substring(0, i), path.substring(i + 1))

  def ext(name: String): String =
    name.lastIndexOf('.') match
      case i if i > 0 => name.substring(i + 1).toLowerCase
      case _          => ""

  /** 服务端只对这些图片内联显示，其余一律当下载 */
  private val Images = Set("png", "jpg", "jpeg", "gif", "webp")

  private val Languages = Map(
    "py" -> "python", "ts" -> "typescript", "tsx" -> "typescript", "mts" -> "typescript",
    "js" -> "javascript", "jsx" -> "javascript", "mjs" -> "javascript", "cjs" -> "javascript",
    "json" -> "json", "jsonl" -> "json", "toml" -> "ini", "ini" -> "ini", "cfg" -> "ini",
    "yaml" -> "yaml", "yml" -> "yaml", "sh" -> "bash", "bash" -> "bash", "zsh" -> "bash",
    "css" -> "css", "scss" -> "scss", "html" -> "xml", "htm" -> "xml", "xml" -> "xml", "svg" -> "xml", "plist" -> "xml",
    "sql" -> "sql", "go" -> "go", "rs" -> "rust", "java" -> "java", "kt" -> "kotlin", "scala" -> "scala", "sbt" -> "scala",
    "c" -> "c", "h" -> "c", "cpp" -> "cpp", "rb" -> "ruby", "php" -> "php", "lua" -> "lua",
    "diff" -> "diff", "patch" -> "diff", "makefile" -> "makefile"
  )

  /** 高亮用的语言（扩展名，或无扩展名时的文件名）；认不出为空，不做自动识别（慢且常认错） */
  def languageOf(name: String): Option[String] =
    val e = ext(name)
    Languages.get(if e.isEmpty then name.toLowerCase else e)

  def previewKind(entry: FileEntry): PreviewKind =
    val e = ext(entry.name)
    if Images(e) then PreviewKind.Image
    else if entry.size.exists(_ > MaxPreviewBytes) then PreviewKind.TooBig
    else if e == "md" || e == "markdown" then PreviewKind.Markdown
    else PreviewKind.Text(languageOf(entry.name))

  /** 解码出的文字里有 NUL 即是二进制文件 */
  def isBinary(text: String): Boolean = text.contains('\u0000')

  def formatSize(bytes: Long): String =
    if bytes < 1024 then s"$bytes B"
    else if bytes < 1024 * 1024 then f"${bytes / 1024.0}%.1f KB"
    else f"${bytes / 1024.0 / 1024.0}%.1f MB"

/** 只读的目录浏览（agent 工作目录、skill 目录）：左边目录树（展开时才读那一层），右边预览（Markdown、代码高亮、图片）与下载。
  * 不随服务端事件重列（agent 干活时文件一直在变，列表会跳），点刷新才重新读。
  */
object FileBrowser:
  import FilePaths.*

  /** 打开的文件在预览区的状态。 */
  private enum Target:
    case Nothing
    /** 所在目录还在读（刷新中） */
    case Waiting
    case Gone(path: String)
    case File(path: String, entry: FileEntry, version: Int)

  def view(source: DirSource, label: String, emptyText: String): HtmlElement =
    // 读过的目录（键是相对路径）；收起时保留，再展开不重读，点刷新才重读
    val dirs     = Var(Map.empty[String, Load[DirContent]])
    val expanded = Var(Set.empty[String])
    val opened   = Var(Option.empty[String])
    // 刷新时递增：预览跟着重新读
    val version = Var(0)

    def load(path: String): Unit =
      dirs.update(m => m.updated(path, m.get(path).flatMap(_.data).fold(Load.Pending: Load[DirContent])(Load.Ready(_))))
      source.list(path).onComplete { r =>
        dirs.update(_.updated(path, r.fold(t => Load.Failed(Http.messageOf(t)), Load.Ready(_))))
      }

    def toggle(path: String): Unit =
      val open = !expanded.now().contains(path)
      expanded.update(s => if open then s + path else s - path)
      if open && !dirs.now().contains(path) then load(path)

    def refresh(): Unit =
      // 展开着的与打开的文件所在的目录重读（读到前照常显示旧的）；其余丢掉，下次展开再读
      val again = expanded.now() ++ opened.now().map(p => split(p)._1) + ""
      dirs.update(_.filter((k, _) => again(k)))
      again.foreach(load)
      version.update(_ + 1)

    def targetOf(d: Map[String, Load[DirContent]], o: Option[String], v: Int): Target = o match
      case None => Target.Nothing
      case Some(p) =>
        val (parent, name) = split(p)
        d.get(parent).flatMap(_.data) match
          case None => Target.Waiting
          case Some(c) =>
            c.entries.find(e => e.name == name && !e.dir).fold(Target.Gone(p))(Target.File(p, _, v))

    def level(dir: String, depth: Int): HtmlElement =
      div(
        role := "group",
        children <-- dirs.signal.map(_.get(dir).flatMap(_.data).fold(Nil)(_.entries)).splitSeq(e => (e.name, e.dir))(e => row(dir, e.now(), depth)),
        child.maybe <-- dirs.signal.map(_.get(dir)).map {
          case Some(Load.Failed(m))                     => Some(p(cls := "px-2 py-1 text-xs text-danger", m))
          case Some(Load.Pending) if depth > 0          => Some(div(cls := "px-2 py-1", Ui.spinner("spinner--sm")))
          case Some(Load.Ready(c)) if c.truncated       => Some(p(cls := "px-2 py-1 text-xs text-muted", "条目太多，只列了前面一部分"))
          case _                                        => None
        }
      )

    def row(dir: String, entry: FileEntry, depth: Int): HtmlElement =
      val path   = join(dir, entry.name)
      val isOpen = expanded.signal.map(_.contains(path))
      div(
        role := "treeitem",
        button(
          tpe := "button",
          cls <-- opened.signal.map(o =>
            "flex w-full items-center gap-1 rounded-md py-1 pe-2 text-left text-sm outline-none focus-visible:ring-2 focus-visible:ring-accent " +
              (if o.contains(path) then "bg-accent-soft text-accent-soft-foreground" else "hover:bg-surface-secondary")
          ),
          styleAttr := s"padding-inline-start:${depth * 14 + 4}px",
          onClick --> (_ => if entry.dir then toggle(path) else opened.set(Some(path))),
          if entry.dir then
            span(cls <-- isOpen.map(o => "shrink-0 text-muted transition-transform " + (if o then "rotate-90" else "")), Icon("ChevronRight", "size-3.5"))
          else span(cls := "w-3.5 shrink-0"),
          Icon(if entry.dir then "Folder" else "File", "size-4 shrink-0 text-muted"),
          span(cls := "truncate", entry.name)
        ),
        Option.when(entry.dir)(child.maybe <-- isOpen.distinct.map(o => Option.when(o)(level(path, depth + 1))))
      )

    val root = dirs.signal.map(_.get(""))
    div(
      cls := "flex flex-col gap-3",
      onMountCallback(_ => load("")),
      div(
        cls := "flex items-center justify-between gap-2",
        span(cls := "text-sm text-muted", "只读；展开目录时才读取那一层"),
        Ui.btn(Variant.Ghost, Size.Sm)(Icon("RefreshCw"), "刷新", onClick --> (_ => refresh()))
      ),
      div(
        cls := "grid gap-3 md:grid-cols-[minmax(14rem,1fr)_3fr]",
        Ui.card(
          cls := "max-h-[75vh] overflow-auto p-1",
          role := "tree",
          aria.label := label,
          child <-- root.map {
            case None | Some(Load.Pending)                       => Ui.centeredSpinner
            case Some(Load.Failed(m))                            => Ui.errorAlert("加载失败", m)
            case Some(Load.Ready(c)) if c.entries.isEmpty        => Ui.emptyHint(emptyText)
            case Some(_)                                         => div()
          },
          level("", 0)
        ),
        Ui.card(
          cls := "min-w-0 p-3",
          child <-- opened.signal.combineWith(dirs.signal, version.signal).map((o, d, v) => targetOf(d, o, v)).distinct.map {
            case Target.Nothing           => Ui.emptyHint("选一个文件预览")
            case Target.Waiting           => Ui.centeredSpinner
            case Target.Gone(p)           => Ui.emptyHint(s"$p 已不在${label}里")
            case Target.File(p, entry, _) => preview(source, p, entry)
          }
        )
      )
    )

  private def preview(source: DirSource, path: String, entry: FileEntry): HtmlElement =
    val url = source.url(path)
    div(
      cls := "flex flex-col gap-2",
      div(
        cls := "flex flex-wrap items-center gap-2 text-sm",
        code(cls := "min-w-0 break-all font-mono", path),
        span(cls := "text-muted", (entry.size.map(formatSize) ++ entry.modifiedAt.map(Ui.formatTime)).mkString(" · ")),
        a(
          href := url,
          Ui.attr("download") := entry.name,
          cls := "ms-auto inline-flex items-center gap-1 text-accent",
          Icon("Download"),
          "下载"
        )
      ),
      previewKind(entry) match
        case PreviewKind.Image  => img(src := url, alt := entry.name, cls := "max-h-[70vh] max-w-full self-start rounded-lg")
        case PreviewKind.TooBig => Ui.emptyHint(s"文件超过 ${formatSize(MaxPreviewBytes)}，不预览，请下载查看")
        case kind               => textPreview(source, path, kind)
    )

  private def textPreview(source: DirSource, path: String, kind: PreviewKind): HtmlElement =
    div(
      child <-- EventStream
        .fromFuture(source.read(path).map(b => new String(b, java.nio.charset.StandardCharsets.UTF_8)).transform(Success(_)))
        .map[HtmlElement] {
          case TryFailure(t)                   => Ui.errorAlert("读取失败", Http.messageOf(t))
          case Success(t) if isBinary(t)       => Ui.emptyHint("二进制文件，不预览，请下载查看")
          case Success(t) => kind match
              case PreviewKind.Markdown       => Markdown.view(t)
              case PreviewKind.Text(language) => highlighted(t, language)
              case _                          => highlighted(t, None)
        }
        .toSignal(Ui.centeredSpinner)
    )

  /** 预览比对话里的代码块高 */
  private val Code = s"${Ui.CodeBlock} max-h-[70vh]!"

  /** 代码高亮：认不出语言或太长时按纯文本显示。highlight.js 的输出已转义，可以直接插入。 */
  def highlighted(text: String, language: Option[String]): HtmlElement =
    val html = language.filter(l => text.length <= MaxHighlightChars && Hljs.getLanguage(l).isDefined).flatMap { l =>
      Try(Hljs.highlight(text, js.Dynamic.literal(language = l, ignoreIllegals = true)).value.asInstanceOf[String]).toOption
    }
    html match
      case Some(h) => pre(cls := s"$Code hljs", inContext(el => { el.ref.innerHTML = h; emptyMod }))
      case None    => pre(cls := Code, text)
