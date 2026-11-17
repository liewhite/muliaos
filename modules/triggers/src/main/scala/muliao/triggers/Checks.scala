package muliao.triggers

import muliao.contract.{AgentId, FireWhen, SecretUse}
import muliao.exec.{Exec, Line, Program, Stderr}
import muliao.platform.AppError
import muliao.resources.AgentExec
import zio.*

/** 前置检查跑出来的结论。 */
enum Verdict:
  /** 没事：不触发 */
  case Quiet
  /** changed 模式第一次：只记下基线，不触发 */
  case Baseline(output: String)
  /** 触发，`event` 是交给 agent 的事件；changed 模式带上新的基线 */
  case Fire(event: String, baseline: Option[String])

/** 前置检查：同 Hermes 的 script 模式，先跑一条便宜、确定性的命令，有事才唤醒 agent。 */
object Checks:

  /** 由检查的输出与上一次的基线得出结论（纯函数）。输出末尾的空白不算。 */
  def decide(when: FireWhen, baseline: Option[String], rawOutput: String): Verdict =
    val output = rawOutput.stripTrailing
    when match
      case FireWhen.Output => if output.isEmpty then Verdict.Quiet else Verdict.Fire(output, None)
      case FireWhen.Changed =>
        baseline match
          case None                    => Verdict.Baseline(output)
          case Some(b) if b == output  => Verdict.Quiet
          case Some(b)                 => Verdict.Fire(s"${LineDiff(b, output)}\n\n当前输出：\n$output", Some(output))

  /** 失败说明里最多带多少字的输出。 */
  private val ShowMax = 2000

  /** 以 agent 身份跑检查命令：`bash` 开 pipefail（否则 `curl … | jq` 里 curl 失败会变成静默的「没事」），`stdin` 每项一行喂给它。
    * 退出码 0 得到标准输出；非 0、超时或起不来得到失败说明（Left）。
    */
  def run(exec: AgentExec, agent: AgentId, p: Precheck, stdin: List[String], timeout: Duration): IO[AppError, Either[String, String]] =
    val program = Program.Shell("set -o pipefail\n" + p.command)
    exec.command(agent, program, p.secrets, SecretUse.Shell).either.flatMap {
      case Left(e) => ZIO.succeed(Left(s"起不来：${e.message}"))
      case Right(cmd) =>
        Ref
          .make(Chunk.empty[Line])
          .flatMap(seen => Exec.follow(cmd.spec, Stderr.Separate, Chunk.fromIterable(stdin))(l => seen.update(_ :+ l)).timeout(timeout).zip(seen.get))
          .flatMap {
            case (None, _)        => ZIO.succeed(Left(s"超过 ${timeout.toSeconds} 秒没跑完，已结束"))
            case (Some(0), lines) => ZIO.succeed(Right(text(lines) { case Line.Out(t) => t }))
            case (Some(code), lines) =>
              val both = List(text(lines) { case Line.Err(t) => t }, text(lines) { case Line.Out(t) => t }).filter(_.trim.nonEmpty).mkString("\n")
              exec.annotate(agent, cmd.scope, both).map { shown =>
                Left(s"退出码 $code" + (if shown.trim.isEmpty then "" else s"，输出：\n${clip(shown.trim)}"))
              }
          }
          .catchAll(e => ZIO.succeed(Left(s"起不来：${e.message}")))
    }

  private def text(lines: Chunk[Line])(pick: PartialFunction[Line, String]): String = lines.collect(pick).mkString("\n")

  private def clip(s: String): String =
    if s.length <= ShowMax then s else "…" + s.takeRight(ShowMax)
