package muliao.workspace.tools

import muliao.contract.{AskPurpose, JobId}
import muliao.exec.{Ending, Exec, Program}
import muliao.platform.AppError
import muliao.resources.AgentExec
import muliao.resources.vault.SecretUse
import muliao.sessions.RunContext
import muliao.toolkit.*
import muliao.workspace.jobs.{JobOwner, Jobs}
import zio.*
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{description, fieldName}

/** 命令怎么跑：前台等到结束（必须给超时），或后台立即返回任务号。 */
enum RunMode:
  case Foreground(timeout: Duration)
  case Background(heartbeat: Option[Duration])

object RunMode:
  /** 从工具参数得到运行方式：前台必须给 timeout，后台不给。 */
  def of(background: Boolean, timeout: Option[Double], heartbeat: Option[Double]): Either[String, RunMode] =
    (background, timeout, heartbeat) match
      case (false, Some(t), None) if t > 0 => Right(Foreground(Duration.fromMillis((t * 1000).toLong)))
      case (false, Some(_), None)          => Left("timeout 要大于 0（秒）")
      case (false, None, _)                => Left("前台运行必须给 timeout（秒，按命令预计要多久估）；要长时间跑就用 background=true")
      case (false, _, Some(_))             => Left("heartbeat_seconds 只用于后台运行")
      case (true, Some(_), _)              => Left("后台运行不给 timeout（它没有时限，用 job_kill 结束）")
      case (true, None, Some(h)) if h <= 0 => Left("heartbeat_seconds 要大于 0")
      case (true, None, h)                 => Right(Background(h.map(s => Duration.fromMillis((s * 1000).toLong.max(1000)))))

/** 以 agent 身份执行一条命令（run_shell 与 skill_run 共用）：前台收齐输出，后台登记成后台任务。 */
final class CommandRunner(exec: AgentExec, jobs: Jobs):

  def run(ctx: RunContext, program: Program, secrets: List[String], use: SecretUse, mode: RunMode, label: String): IO[AppError, String] =
    for
      cmd <- exec.command(ctx.agent, program, secrets, use, AskPurpose(Some(ctx.session), AgentExec.describe(program)))
      out <- mode match
               case RunMode.Foreground(timeout) =>
                 Exec.run(cmd.spec, timeout).flatMap { f =>
                   val head = f.ending match
                     case Ending.TimedOut     => s"killed after ${timeout.toSeconds}s timeout"
                     case Ending.Exited(code) => s"exit_code: $code"
                   exec.annotate(ctx.agent, cmd.scope, f.output).map(o => if o.isEmpty then head else s"$head\n$o")
                 }
               case RunMode.Background(hb) =>
                 jobs.start(JobOwner(ctx.agent, ctx.session), cmd, label, hb).map { id =>
                   val beat = hb.fold("")(d => s"；运行期间每 ${d.toSeconds} 秒给你投一条检查")
                   s"已在后台运行，任务号 ${id.value}；输出写进工作目录下的 ${Jobs.logName(id)}。结束时退出码和最后几行输出会作为事件回到这里$beat。期间这个会话处于等待中，可以先结束这一轮回答。"
                 }
    yield out

object CommandRunner:
  /** 后台任务的标签：命令的第一行，太长截断。 */
  def label(text: String): String =
    val first = text.linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
    val more  = if text.trim.linesIterator.size > 1 then " …" else ""
    (if first.length > 200 then first.take(200) + "…" else first) + more

/** 命令工具：run_shell、job_status、job_kill。 */
object ShellTools:

  final case class ShellIn(
      @description("要执行的命令（bash）") command: String,
      @description("前台必须给：最多等多少秒，到时结束整组进程（按命令预计要多久估）；后台不给") timeout: Option[Double] = None,
      @description("true：后台运行，立即返回任务号；输出写进工作目录的 jobs/<任务号>.log，结束时结果作为事件回到这个会话") background: Boolean = false,
      @fieldName("heartbeat_seconds") @description("仅后台：每隔多少秒给你投一条检查（运行时长与最后几行输出）") heartbeatSeconds: Option[Double] = None,
      @description("要作为同名环境变量注入的 secret（vault 里的 key）；输出里出现它们的值会被换成 [secret:KEY]") secrets: List[String] = Nil
  )
  given Schema[ShellIn] = DeriveSchema.gen[ShellIn]

  final case class JobIn(@fieldName("job_id") @description("run_shell 后台运行返回的任务号") jobId: JobId)
  given Schema[JobIn] = DeriveSchema.gen[JobIn]

  /** job_status 显示日志的最后几行。 */
  val StatusLines = 30

  private def rejected(e: AppError): ToolFailure = ToolFailure.fromAppError(e)

  val RunShellDef = ToolDef[RunContext, ShellIn](
    "run_shell",
    "在你的工作目录里用 bash 执行命令，返回退出码和标准输出与标准错误合在一起的文字。命令只能写你的可写范围（工作目录、常用临时与缓存目录、获授权的目录），被拦下时输出里会说明被什么拦、找谁授权；读和网络不受限。" +
      "前台运行必须给 timeout（秒，自己估），到时结束整组进程并返回已有的输出。要跑很久或想边跑边做别的，用 background=true：立即返回任务号，输出写进 jobs/<任务号>.log，结束时退出码和最后几行作为事件回到这个会话，期间会话处于等待中；用 job_status 看进度、job_kill 结束。" +
      "secrets 里的 key 作为同名环境变量注入，输出中出现的值会被打码。",
    Access.Write,
    Scheduling.Concurrent,
    in => Display.Terminal(Some(in.command))
  )

  val JobStatusDef = ToolDef[RunContext, JobIn](
    "job_status",
    s"看一个后台任务（只能看自己起的）：状态（运行中/已结束及退出码/被结束/被中断）、运行时长、日志最后 $StatusLines 行。全部输出在工作目录的 jobs/<任务号>.log。",
    Access.Read,
    Scheduling.Concurrent,
    _ => Display.Terminal(None)
  )

  val JobKillDef = ToolDef[RunContext, JobIn](
    "job_kill",
    "结束一个在跑的后台任务（整组进程：先 TERM，等几秒再 KILL）。它的结局（被结束）随后作为事件回到发起它的会话。",
    Access.Write,
    Scheduling.Ordered
  )

  /** 本领域工具的静态定义（进全部内置工具目录）。 */
  val specs: List[ToolSpec] = List(RunShellDef, JobStatusDef, JobKillDef).map(_.spec)

  def all(runner: CommandRunner, jobs: Jobs): List[Tool[RunContext, Nothing]] = List(runShell(runner), jobStatus(jobs), jobKill(jobs))

  def runShell(runner: CommandRunner): Tool[RunContext, Nothing] =
    RunShellDef { (in, ctx) =>
      for
        _    <- ZIO.when(in.command.isBlank)(ZIO.fail(ToolFailure.Rejected("command 不能为空")))
        mode <- ZIO.fromEither(RunMode.of(in.background, in.timeout, in.heartbeatSeconds)).mapError(ToolFailure.Rejected(_))
        out  <- runner.run(ctx, Program.Shell(in.command), in.secrets, SecretUse.Shell, mode, CommandRunner.label(in.command)).mapError(rejected)
      yield ToolOutcome.text(out)
    }

  def jobStatus(jobs: Jobs): Tool[RunContext, Nothing] =
    JobStatusDef { (in, ctx) =>
      jobs.get(ctx.agent, in.jobId).flatMap(jobs.describe(_, StatusLines)).map(ToolOutcome.text).mapError(rejected)
    }

  def jobKill(jobs: Jobs): Tool[RunContext, Nothing] =
    JobKillDef { (in, ctx) =>
      val id = in.jobId
      (for
        _      <- jobs.get(ctx.agent, id)
        killed <- jobs.kill(id)
      yield ToolOutcome.text(
        if killed then s"已结束后台任务 ${in.jobId.value}；结局会作为事件回到发起它的会话。" else s"后台任务 ${in.jobId.value} 本来就已经结束了。"
      )).mapError(rejected)
    }

