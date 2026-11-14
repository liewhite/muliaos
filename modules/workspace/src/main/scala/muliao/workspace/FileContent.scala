package muliao.workspace

import zio.Chunk

import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.ByteBuffer

/** 一个文件的内容按什么看。 */
enum FileContent:
  case Text(text: String)
  /** 常见图片（PNG、JPEG、GIF、WebP），按开头的字节认 */
  case Image(mediaType: String, bytes: Chunk[Byte])
  /** 其他二进制文件 */
  case Binary(size: Long)

object FileContent:
  /** 开头这么多字节里有 0 就当二进制。 */
  private val Probe = 8000

  def of(bytes: Array[Byte]): FileContent =
    imageType(bytes) match
      case Some(t) => Image(t, Chunk.fromArray(bytes))
      case None =>
        if bytes.iterator.take(Probe).contains(0.toByte) then Binary(bytes.length.toLong)
        else Text(decode(bytes))

  /** UTF-8 解码，坏字节换成替换字符（不因个别坏字节读不了整个文件）。 */
  def decode(bytes: Array[Byte]): String =
    StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPLACE)
      .onUnmappableCharacter(CodingErrorAction.REPLACE)
      .decode(ByteBuffer.wrap(bytes))
      .toString

  /** 只认这几种图片：界面内联显示与发给模型都只用它们。 */
  def imageType(b: Array[Byte]): Option[String] =
    def starts(sig: Int*) = b.length >= sig.size && sig.zipWithIndex.forall((x, i) => (b(i) & 0xff) == x)
    if starts(0x89, 0x50, 0x4e, 0x47) then Some("image/png")
    else if starts(0xff, 0xd8, 0xff) then Some("image/jpeg")
    else if starts(0x47, 0x49, 0x46, 0x38) then Some("image/gif")
    else if starts(0x52, 0x49, 0x46, 0x46) && b.length > 12 && String(b.slice(8, 12), StandardCharsets.US_ASCII) == "WEBP" then Some("image/webp")
    else None

  /** 按文件名猜的媒体类型（分享、下载时用）。 */
  def mediaTypeOf(name: String, bytes: Array[Byte]): String =
    imageType(bytes).orElse(Option(java.net.URLConnection.guessContentTypeFromName(name))).getOrElse("application/octet-stream")
