package muliao.workspace.tools

import muliao.resources.ToolInfo
import muliao.contract.FileView
import muliao.platform.AppError
import muliao.sessions.{Files, RunContext}
import muliao.toolkit.*
import muliao.workspace.{FileAccess, FileContent, FileWrite, WriteMode}
import muliao.workspace.text.*
import zio.*
import zio.json.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{description, fieldName}

/** 文件工具：read_file、write_file、edit_file、share_file。都与同一回复里其他调用保持先后顺序。 */
object FileTools:

  final case class ReadPart(
      @description("文件路径") path: String,
      @description("从第几行起（1 起，按 read_file 输出的行号）") offset: Int = 1,
      @description("最多几行") limit: Int = 2000
  )
  given Schema[ReadPart] = DeriveSchema.gen[ReadPart]

  final case class ReadIn(
      @description("文件路径（用 reads 时不给）；相对路径按工作目录解析") path: Option[String] = None,
      @description("从第几行起（1 起）") offset: Int = 1,
      @description("最多几行") limit: Int = 2000,
      @description("一次读几个文件或一个文件的几段：给 reads 时不给 path/offset/limit；每段前有 `==> 路径 lines 起-止 of 总数 <==` 标题") reads: List[ReadPart] = Nil
  )
  given Schema[ReadIn] = DeriveSchema.gen[ReadIn]

  final case class WriteIn(
      @description("文件路径；父目录自动建") path: String,
      @description("完整的文件内容") content: String
  )
  given Schema[WriteIn] = DeriveSchema.gen[WriteIn]

  final case class EditItem(
      @description("这处修改的文件；不给时用顶层的 path") path: Option[String] = None,
      @fieldName("old_text") @description("要替换的原文：与文件当前内容逐字一致，必须恰好出现一次（除非 replace_all）") oldText: Option[String] = None,
      @fieldName("new_text") @description("替换成的内容") newText: String,
      @fieldName("replace_all") @description("替换 old_text 的全部出现") replaceAll: Boolean = false,
      @fieldName("start_line") @description("按行号改：第一行（1 起），与 end_line、anchor 一起给") startLine: Option[Int] = None,
      @fieldName("end_line") @description("按行号改：最后一行（含）") endLine: Option[Int] = None,
      @description("第 start_line 行的全文（不只是开头；首尾空白忽略），对不上就拒绝") anchor: Option[String] = None
  )
  given Schema[EditItem] = DeriveSchema.gen[EditItem]

  final case class EditIn(
      @description("文件路径（用 edits 且每处都自带 path 时可不给）") path: Option[String] = None,
      @fieldName("old_text") @description("要替换的原文（用 edits 时不给）") oldText: Option[String] = None,
      @fieldName("new_text") @description("替换成的内容；为空表示删掉定位到的部分（用 edits 时不给）") newText: Option[String] = None,
      @fieldName("replace_all") @description("替换 old_text 的全部出现（用 edits 时不给）") replaceAll: Boolean = false,
      @fieldName("start_line") @description("按行号改：第一行（与 end_line、anchor 一起给）") startLine: Option[Int] = None,
      @fieldName("end_line") @description("按行号改：最后一行（含）") endLine: Option[Int] = None,
      @description("第 start_line 行的全文（首尾空白忽略）") anchor: Option[String] = None,
      @description("一次改多处、多个文件：按顺序执行；old_text 在前面的修改之后的文件里找，行号与 anchor 总是指这次调用之前的文件；任何一处失败都不写") edits: List[EditItem] = Nil
  )
  given Schema[EditIn] = DeriveSchema.gen[EditIn]

  final case class ShareIn(@description("文件路径") path: String)
  given Schema[ShareIn] = DeriveSchema.gen[ShareIn]

  /** 一次 read_file 最多输出多少字符。 */
  val ReadBudget = 60000

  private def rejected(e: AppError): ToolFailure = ToolFailure.fromAppError(e)

  def all(access: FileAccess, files: Files): List[Tool[RunContext]] =
    List(readFile(access), writeFile(access), editFile(access), shareFile(access, files))

  // ---- read_file ----

  /** 读的结果：文本一段、一张图、或说明为什么读不了。 */
  private enum Got:
    case Text(slice: Listing.Slice)
    case Picture(mediaType: String, bytes: Chunk[Byte])
    case Problem(message: String)

  def readFile(access: FileAccess): Tool[RunContext] =
    Tool[RunContext, ReadIn](
      "read_file",
      "读文本文件，输出是 `cat -n` 格式：每行是右对齐占 6 格的行号、一个制表符、该行原文。相对路径按工作目录解析。" +
        "一次读几个文件或一个文件的几段时给 reads（每段前有 `==> 路径 lines 起-止 of 总数 <==` 标题，读不了的段显示原因，其余照常）。" +
        "输出有上限：停在整行处，最后单独一行在方括号里说明从哪个 offset 续读。图片文件（PNG、JPEG、GIF、WebP）用 path 读时直接作为图片返回，可以看截图、图表。",
      Access.Read,
      Scheduling.Ordered,
      Display.Code
    ) { (in, ctx) =>
      (in.path, in.reads) match
        case (Some(p), Nil) =>
          readOne(access, ctx, p, in.offset, in.limit, ReadBudget).flatMap {
            case Got.Text(s) =>
              val body = if s.total == 0 then "（空文件）" else s.text.stripSuffix("\n")
              ZIO.succeed(ToolOutcome.text((body :: Listing.continuation(s).toList).mkString("\n")))
            case Got.Picture(t, b) => ZIO.succeed(ToolOutcome.Completed(Chunk(ToolContent.Image(t, b))))
            case Got.Problem(m)    => ZIO.fail(ToolFailure.Rejected(m))
          }
        case (None, parts) if parts.nonEmpty => readMany(access, ctx, parts).map(ToolOutcome.text)
        case _                               => ZIO.fail(ToolFailure.Rejected("给 path，或给 reads（一组 {path, offset, limit}），二选一。"))
    }

  private def readOne(access: FileAccess, ctx: RunContext, raw: String, offset: Int, limit: Int, budget: Int): IO[ToolFailure, Got] =
    access
      .load(ctx.agent, Some(ctx.run), FileAccess.resolve(ctx.workspace, raw))
      .map {
        case FileContent.Text(t) =>
          Listing.slice(LineIndex(t), offset, limit, budget).fold(Got.Problem(_), Got.Text(_))
        case FileContent.Image(m, b) => Got.Picture(m, b)
        case FileContent.Binary(n)   => Got.Problem(s"$raw 是二进制文件（$n 字节），不能按文本显示")
      }
      .catchSome { case e: AppError.Domain => ZIO.succeed(Got.Problem(e.message)) }
      .mapError(rejected)

  /** 多段读取：共享一个输出上限，到上限后余下的段列在最后。 */
  private def readMany(access: FileAccess, ctx: RunContext, parts: List[ReadPart]): IO[ToolFailure, String] =
    final case class Acc(out: Vector[String], used: Int, skipped: Vector[ReadPart])
    ZIO
      .foldLeft(parts)(Acc(Vector.empty, 0, Vector.empty)) { (acc, part) =>
        val left = ReadBudget - acc.used
        if acc.skipped.nonEmpty || left <= 200 then ZIO.succeed(acc.copy(skipped = acc.skipped :+ part))
        else
          readOne(access, ctx, part.path, part.offset, part.limit, left).map {
            case Got.Text(s) =>
              val text = Vector(Listing.header(part.path, s), s.text.stripSuffix("\n")) ++ Listing.continuation(s).toVector
              val block = text.filter(_.nonEmpty).mkString("\n")
              s.stop match
                case Listing.Stop.Budget(next) => Acc(acc.out :+ block, ReadBudget, acc.skipped)
                case _                         => Acc(acc.out :+ block, acc.used + block.length, acc.skipped)
            case Got.Picture(_, _) => acc.copy(out = acc.out :+ Listing.errorHeader(part.path, "是图片：单独用 path 读才能看"))
            case Got.Problem(m)    => acc.copy(out = acc.out :+ Listing.errorHeader(part.path, m))
          }
      }
      .map { acc =>
        val rest =
          if acc.skipped.isEmpty then Nil
          else List(s"[没读到（到了输出上限），另起一次调用读：${acc.skipped.map(p => s"${p.path} offset=${p.offset}").mkString("; ")}]")
        (acc.out ++ rest).mkString("\n")
      }

  // ---- write_file ----

  def writeFile(access: FileAccess): Tool[RunContext] =
    Tool[RunContext, WriteIn](
      "write_file",
      "新建文件或整体覆盖（父目录自动建）。只改一部分用 edit_file。资源目录里已有的文件（记忆、skill 等），要本次运行先 read_file 读过、且之后没被别人改，才能整体覆盖。",
      Access.Write,
      Scheduling.Ordered
    ) { (in, ctx) =>
      val path = FileAccess.resolve(ctx.workspace, in.path)
      access
        .store(ctx.agent, ctx.run, List(FileWrite(path, in.content)), WriteMode.Overwrite)
        .map(out => ToolOutcome.text(s"已写入 ${in.path}（${LineIndex(out.head).count} 行，${out.head.length} 个字符）。"))
        .mapError(rejected)
    }

  // ---- edit_file ----

  /** 参数里的一处修改换成 [[Edit]]：按原文或按行号，二选一。 */
  private def toEdit(oldText: Option[String], newText: String, all: Boolean, start: Option[Int], end: Option[Int], anchor: Option[String]): Either[String, Edit] =
    (oldText, start, end, anchor) match
      case (Some(o), None, None, None)          => Right(Edit(Locator.Text(o, all), newText))
      case (None, Some(s), Some(e), Some(a)) if !all => Right(Edit(Locator.Lines(s, e, a), newText))
      case _ => Left("每处修改要么给 old_text（可加 replace_all），要么给 start_line + end_line + anchor 三个，二选一")

  /** 一个文件（参数里的写法）要做的修改，按给出的顺序。 */
  final case class FileEdits(path: String, edits: List[Edit])

  /** 整理参数：得到按文件分组的修改（文件按第一次出现的顺序）。 */
  def plan(in: EditIn): Either[String, List[FileEdits]] =
    val items: Either[String, List[FileEdits]] =
      if in.edits.isEmpty then
        for
          path <- in.path.toRight("缺 path")
          text <- in.newText.toRight("缺 new_text")
          e    <- toEdit(in.oldText, text, in.replaceAll, in.startLine, in.endLine, in.anchor)
        yield List(FileEdits(path, List(e)))
      else if in.oldText.isDefined || in.newText.isDefined || in.replaceAll || in.startLine.isDefined || in.endLine.isDefined || in.anchor.isDefined then
        Left("用 edits 时顶层只给 path（作为各处修改的默认文件），其余都写在 edits 里")
      else
        in.edits.zipWithIndex.foldLeft[Either[String, List[FileEdits]]](Right(Nil)) { case (acc, (it, i)) =>
          for
            done <- acc
            path <- it.path.orElse(in.path).toRight(s"第 ${i + 1} 处修改没有 path，顶层也没给")
            e    <- toEdit(it.oldText, it.newText, it.replaceAll, it.startLine, it.endLine, it.anchor).left.map(m => s"第 ${i + 1} 处修改：$m")
          yield done :+ FileEdits(path, List(e))
        }
    items.map(list => list.map(_.path).distinct.map(p => FileEdits(p, list.filter(_.path == p).flatMap(_.edits))))

  /** 改好、还没写的一个文件。 */
  private final case class Planned(raw: String, path: java.nio.file.Path, edited: Edited)

  def editFile(access: FileAccess): Tool[RunContext] =
    Tool[RunContext, EditIn](
      "edit_file",
      "改文件的一部分：用 old_text 定位（逐字一致、恰好出现一次，除非 replace_all），或用 start_line + end_line + anchor 定位整行（anchor 是第 start_line 行的全文，防止按过时的行号改错）。" +
        "一次改多处、多个文件时给 edits：按顺序执行，old_text 在前面的修改之后的内容里找，行号和 anchor 总是指这次调用之前的文件，与前面改动重叠的行号范围被拒绝；任何一处失败，所有文件都不写。" +
        "成功后回显每个文件改动处及上下文（`cat -n` 格式，不相连的块用 -- 隔开），可以据此接着按行号改，不必重读。",
      Access.Write,
      Scheduling.Ordered,
      Display.Diff
    ) { (in, ctx) =>
      for
        groups <- ZIO.fromEither(plan(in)).mapError(m => ToolFailure.Rejected(m))
        edited <- ZIO.foreach(groups) { case FileEdits(raw, edits) =>
                    val path = FileAccess.resolve(ctx.workspace, raw)
                    access.load(ctx.agent, None, path).mapError(rejected).flatMap {
                      case FileContent.Text(t) =>
                        ZIO.fromEither(TextEdit(t, edits)).mapError(m => ToolFailure.Rejected(s"$raw：$m。没有改任何文件。")).map(Planned(raw, path, _))
                      case _ => ZIO.fail(ToolFailure.Rejected(s"$raw 不是文本文件，不能 edit_file。没有改任何文件。"))
                    }
                  }
        written <- access.store(ctx.agent, ctx.run, edited.map(p => FileWrite(p.path, p.edited.text)), WriteMode.Patch).mapError(rejected)
      yield
        // 资源文件写入口可能规整了内容（如记忆的空白），回显按实际写入的
        val reports = edited.zip(written).map { case (Planned(raw, _, e), out) =>
          EditReport.render(raw, if out == e.text then e else Edited(out, e.replacements, List(Span(0, out.length))))
        }
        ToolOutcome.text(reports.mkString("\n\n"))
    }

  // ---- share_file ----

  def shareFile(access: FileAccess, files: Files): Tool[RunContext] =
    Tool[RunContext, ShareIn](
      "share_file",
      "把工作目录里的文件（如做好的图表、写好的文档）发给用户：随你的回复显示给他（图片内联，其他文件可下载），你不用再贴链接或嵌入，提到文件名即可。用户拿到的是现在的快照，之后改文件不会反映过去。",
      Access.Write,
      Scheduling.Ordered,
      Display.SharedFile
    ) { (in, ctx) =>
      val path = FileAccess.resolve(ctx.workspace, in.path)
      (for
        bytes <- access.bytes(ctx.agent, path)
        name   = Option(path.getFileName).map(_.toString).getOrElse(in.path)
        row   <- files.upload(name, FileContent.mediaTypeOf(name, bytes), Chunk.fromArray(bytes))
      yield ToolOutcome.text(FileView(row.id, row.name, row.mediaType, row.size).toJson)).mapError(rejected)
    }

  /** 本组工具的名字与类别（组装时汇进全部内置工具目录）。 */
  val catalog: List[ToolInfo] =
    List(ToolInfo("read_file", Access.Read), ToolInfo("write_file", Access.Write), ToolInfo("edit_file", Access.Write), ToolInfo("share_file", Access.Write))
