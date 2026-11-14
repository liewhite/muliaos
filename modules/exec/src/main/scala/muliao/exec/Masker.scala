package muliao.exec

import zio.*
import zio.stream.*

/** 注入命令的 secret：名字（即环境变量名）和值。值不出现在 toString 里。 */
final case class SecretEnv(name: String, value: String):
  override def toString: String = s"SecretEnv($name)"

/** 把输出里出现的 secret 值换成 `[secret:名字]`。按流处理：每块末尾留下可能是半个 secret 的一段，与下一块拼起来再换。 */
final class Masker(secrets: Chunk[SecretEnv]):
  // 长的先换，免得一个 secret 是另一个的一部分时只换掉一半
  private val ordered = secrets.filter(_.value.nonEmpty).sortBy(-_.value.length)
  private val keep    = ordered.headOption.fold(0)(_.value.length - 1)

  def mask(s: String): String =
    ordered.foldLeft(s)((acc, sec) => acc.replace(sec.value, s"[secret:${sec.name}]"))

  def pipeline: ZPipeline[Any, Nothing, String, String] =
    if ordered.isEmpty then ZPipeline.identity
    else
      def loop(carry: String): ZChannel[Any, ZNothing, Chunk[String], Any, Nothing, Chunk[String], Any] =
        ZChannel.readWithCause(
          (in: Chunk[String]) =>
            val text = mask(carry + in.mkString)
            val cut0 = (text.length - keep).max(0)
            // 不把一对代理字符拆开
            val cut = if cut0 > 0 && Character.isHighSurrogate(text.charAt(cut0 - 1)) then cut0 - 1 else cut0
            val (out, rest) = text.splitAt(cut)
            (if out.isEmpty then ZChannel.unit else ZChannel.write(Chunk.single(out))) *> loop(rest),
          (cause: Cause[ZNothing]) => ZChannel.refailCause(cause),
          (_: Any) => if carry.isEmpty then ZChannel.unit else ZChannel.write(Chunk.single(carry))
        )
      ZPipeline.fromChannel(loop(""))
