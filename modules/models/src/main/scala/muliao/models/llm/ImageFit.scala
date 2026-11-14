package muliao.models.llm

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.exif.ExifIFD0Directory
import zio.Chunk

import java.awt.RenderingHints
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.imageio.{IIOImage, ImageIO, ImageReader, ImageWriteParam}
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** 把一张图片整理成服务商一定收的样子（纯函数）。
  *
  * 服务商对单张图片有硬上限，超了整次请求被拒（Anthropic：任一边超过 8000px、一次超过 20 张时任一边超过 2000px、base64 后
  * 超过 5MB）。图片在会话历史里每次请求都要再发，所以一张超限的图会让这个会话之后每次都失败；这里在发给模型的唯一出口统一整理，
  * 历史里的原件不动。
  *
  * 规则：长边不超过 [[Box]]、大小不超过 [[MaxBytes]]、格式在表里的原样交回（类型按内容而不按声明）；否则先按 EXIF 方向转正，
  * 沿长边切成几段（每段长边不短于短边的两倍、也不短于 `Box`，最多 [[MaxTiles]] 段），每段等比缩进 `Box`×`Box`。长截图切段而不是
  * 整张缩小：1080×11499 整张缩进框里只剩 147px 宽，字看不清。`Box` 取 1568：Anthropic 会把长边超过它的图在服务端缩小，发更大的
  * 没有收益，也在多图请求的 2000px 之内。重新编码：JPEG 仍存 JPEG，其余存 PNG，PNG 仍超过 `MaxBytes` 的改存 JPEG。
  */
object ImageFit:
  val Box      = 1568
  /** Anthropic 单张 5MB 按 base64 编码后计 */
  val MaxBytes: Int = 5 * 1024 * 1024 * 3 / 4
  val MaxTiles = 8
  /** 解码前按像素数拦下的上限（防内存打爆） */
  val MaxPixels: Long = 200_000_000L

  final case class Tile(mediaType: String, data: Chunk[Byte])

  enum Fitted:
    case AsIs(tile: Tile)
    /** 做过处理：`note` 说明做了什么，放在图片前给模型看。 */
    case Reworked(tiles: Chunk[Tile], note: String)

  private val passThrough = Map("png" -> "image/png", "jpeg" -> "image/jpeg", "gif" -> "image/gif", "webp" -> "image/webp")

  /** 解不开（损坏、不是图片、像素太多）时给出原因。 */
  def fit(data: Chunk[Byte]): Either[String, Fitted] =
    val bytes = data.toArray
    withReader(bytes) { reader =>
      val format = reader.getFormatName.toLowerCase
      val (w, h) = (reader.getWidth(0), reader.getHeight(0))
      passThrough.get(format) match
        case Some(media) if w.max(h) <= Box && bytes.length <= MaxBytes => Right(Fitted.AsIs(Tile(media, data)))
        case _ if w.toLong * h > MaxPixels                              => Left(s"图片太大（${w}x$h）")
        case _ =>
          val upright = orient(reader.read(0), orientation(bytes))
          Right(rework(upright, if format == "jpeg" then "jpeg" else "png"))
    }

  private def withReader[A](bytes: Array[Byte])(f: ImageReader => Either[String, A]): Either[String, A] =
    Try {
      val in      = ImageIO.createImageInputStream(ByteArrayInputStream(bytes))
      val readers = ImageIO.getImageReaders(in).asScala
      if !readers.hasNext then Left("不是认得的图片格式")
      else
        val reader = readers.next()
        try
          reader.setInput(in)
          f(reader)
        finally
          reader.dispose()
          in.close()
    }.toEither.left.map(e => s"图片解不开：${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}").flatten

  private def rework(img: BufferedImage, format: String): Fitted =
    val (w, h)        = (img.getWidth, img.getHeight)
    val (short, long) = (w.min(h), w.max(h))
    val count         = math.ceil(long.toDouble / (2 * short).max(Box)).toInt.max(1).min(MaxTiles)
    val pieces        = split(img, count).map(shrink)
    val tiles         = pieces.map(encode(_, format))
    val note =
      if count == 1 then s"[图片 ${w}x$h 为符合模型限制改为 ${pieces(0).getWidth}x${pieces(0).getHeight} ${tiles(0).mediaType} 发送]"
      else
        val direction = if h >= w then "从上到下" else "从左到右"
        s"[图片 ${w}x$h 为符合模型限制切成 $count 段，$direction 依次给出，每段缩进 ${Box}x$Box]"
    Fitted.Reworked(tiles, note)

  private def split(img: BufferedImage, count: Int): Chunk[BufferedImage] =
    val (w, h)   = (img.getWidth, img.getHeight)
    val vertical = h >= w
    val along    = if vertical then h else w
    val cuts     = (0 to count).map(i => (along.toLong * i / count).toInt)
    Chunk.fromIterable(cuts.zip(cuts.tail).map { (a, b) =>
      if vertical then img.getSubimage(0, a, w, b - a) else img.getSubimage(a, 0, b - a, h)
    })

  private def shrink(img: BufferedImage): BufferedImage =
    val scale = (Box.toDouble / img.getWidth.max(img.getHeight)).min(1.0)
    val w     = (img.getWidth * scale).round.toInt.max(1)
    val h     = (img.getHeight * scale).round.toInt.max(1)
    val out   = BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val g     = out.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g.drawImage(img.getScaledInstance(w, h, java.awt.Image.SCALE_AREA_AVERAGING), 0, 0, null)
    g.dispose()
    out

  private def encode(img: BufferedImage, format: String): Tile =
    format match
      case "jpeg" => Tile("image/jpeg", jpeg(img))
      case _ =>
        val png = write(img, "png")
        if png.size > MaxBytes then Tile("image/jpeg", jpeg(img)) else Tile("image/png", png)

  private def write(img: BufferedImage, format: String): Chunk[Byte] =
    val out = ByteArrayOutputStream()
    ImageIO.write(img, format, out)
    Chunk.fromArray(out.toByteArray)

  /** JPEG 不带透明：铺白底，质量 0.85。 */
  private def jpeg(img: BufferedImage): Chunk[Byte] =
    val rgb = BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_RGB)
    val g   = rgb.createGraphics()
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, rgb.getWidth, rgb.getHeight)
    g.drawImage(img, 0, 0, null)
    g.dispose()
    val writer = ImageIO.getImageWritersByFormatName("jpeg").next()
    val param  = writer.getDefaultWriteParam
    param.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
    param.setCompressionQuality(0.85f)
    val out = ByteArrayOutputStream()
    val ios = ImageIO.createImageOutputStream(out)
    try
      writer.setOutput(ios)
      writer.write(null, IIOImage(rgb, null, null), param)
    finally
      ios.close()
      writer.dispose()
    Chunk.fromArray(out.toByteArray)

  /** EXIF 方向（1–8）；没有或读不出为 1。 */
  private def orientation(bytes: Array[Byte]): Int =
    Try {
      val meta = ImageMetadataReader.readMetadata(ByteArrayInputStream(bytes))
      Option(meta.getFirstDirectoryOfType(classOf[ExifIFD0Directory]))
        .filter(_.containsTag(ExifIFD0Directory.TAG_ORIENTATION))
        .map(_.getInt(ExifIFD0Directory.TAG_ORIENTATION))
        .getOrElse(1)
    }.getOrElse(1)

  /** 按 EXIF 方向转正：切段与缩放不看方向，存出来也不带它，不转的话竖拍的照片发出去是横的。 */
  private[llm] def orient(img: BufferedImage, o: Int): BufferedImage =
    transformFor(o, img.getWidth.toDouble, img.getHeight.toDouble).fold(img) { (t, swap) =>
      val (nw, nh) = if swap then (img.getHeight, img.getWidth) else (img.getWidth, img.getHeight)
      val out      = BufferedImage(nw, nh, BufferedImage.TYPE_INT_ARGB)
      val g        = out.createGraphics()
      g.drawImage(img, t, null)
      g.dispose()
      out
    }

  /** EXIF 方向 → 把原图转正的变换，以及宽高是否对调。方向 1 或不认识的值不用转。 */
  private def transformFor(o: Int, w: Double, h: Double): Option[(AffineTransform, Boolean)] =
    val t = AffineTransform()
    val swap = o match
      case 2 => t.translate(w, 0); t.scale(-1, 1); Some(false)                     // 左右翻
      case 3 => t.translate(w, h); t.rotate(math.Pi); Some(false)                  // 转 180°
      case 4 => t.translate(0, h); t.scale(1, -1); Some(false)                     // 上下翻
      case 5 => t.rotate(math.Pi / 2); t.scale(1, -1); Some(true)                  // 沿主对角线翻
      case 6 => t.translate(h, 0); t.rotate(math.Pi / 2); Some(true)               // 顺时针 90°
      case 7 => t.translate(h, w); t.rotate(math.Pi / 2); t.scale(-1, 1); Some(true)    // 沿副对角线翻
      case 8 => t.translate(0, w); t.rotate(3 * math.Pi / 2); Some(true)           // 逆时针 90°
      case _ => None
    swap.map(t -> _)
