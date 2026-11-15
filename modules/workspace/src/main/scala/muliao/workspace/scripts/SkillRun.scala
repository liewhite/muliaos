package muliao.workspace.scripts

import muliao.resources.ToolInfo
import muliao.exec.{Program, RealPath}
import muliao.resources.SkillEntry
import muliao.resources.vault.SecretUse
import muliao.contract.SkillName
import muliao.sessions.{ApprovalHandler, ApproverName, Pause, RunContext}
import muliao.toolkit.*
import muliao.workspace.tools.{CommandRunner, RunMode}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{description, fieldName}

import java.nio.file.{Files, Path}

/** skill_run：运行 skill 目录里的脚本。SKILL.md 的 `approval:` 里列出的脚本每次执行前等用户批准（用户可选「始终允许」同样的调用）。 */
object SkillRun:

  final case class SkillRunIn(
      @description("skill 名（skill 索引里的）") skill: String,
      @description("脚本相对 skill 目录的路径，如 scripts/deploy.py；要有可执行位和 shebang 行") script: String,
      @description("传给脚本的参数，原样传入（不经 shell 解析）") args: List[String] = Nil,
      @description("作为同名环境变量注入的 secret；可以用只允许脚本使用（allow_shell = false）的 secret") secrets: List[String] = Nil,
      @description("前台必须给：最多等多少秒；后台不给") timeout: Option[Double] = None,
      @description("true：后台运行，同 run_shell") background: Boolean = false,
      @fieldName("heartbeat_seconds") @description("仅后台：每隔多少秒投一条检查") heartbeatSeconds: Option[Double] = None
  ) derives JsonCodec
  given Schema[SkillRunIn] = DeriveSchema.gen[SkillRunIn]

  /** 等批准时记下的：调用本身与当时 skill 文件的摘要（批准的就是用户当时看到的那份）。 */
  final case class Approval(call: SkillRunIn, filesHash: String) derives JsonCodec

  /** 批准后的执行者名。 */
  val Handler: ApproverName = ApproverName.wrap("skill_run")

  /** 一次调用验证过的样子。 */
  private final case class Checked(entry: SkillEntry, script: String, real: RealPath, mode: RunMode, needsApproval: Boolean)

  /** 规整后的脚本相对路径；走出 skill 目录、绝对路径都不行。 */
  def normalize(script: String): Either[String, String] =
    val p = Path.of(script.trim).normalize
    if script.trim.isEmpty then Left("script 不能为空")
    else if p.isAbsolute || p.toString.startsWith("..") then Left("script 要写相对 skill 目录的路径，不能走出 skill 目录")
    else Right(p.toString)

  private def check(in: SkillRunIn, ctx: RunContext): IO[ToolFailure, Checked] =
    for
      entry <- ZIO
                 .fromOption(ctx.resources.skills.find(_.doc.name == SkillName.wrap(in.skill)))
                 .orElseFail(ToolFailure.Rejected(s"没有 skill ${in.skill}，或你不能用它：只能用 skill 索引里的"))
      script <- ZIO.fromEither(normalize(in.script)).mapError(ToolFailure.Rejected(_))
      mode   <- ZIO.fromEither(RunMode.of(in.background, in.timeout, in.heartbeatSeconds)).mapError(ToolFailure.Rejected(_))
      dir    <- RealPath.resolve(entry.doc.dir).mapError(ToolFailure.fromAppError)
      real   <- RealPath.resolve(entry.doc.dir.resolve(script)).mapError(ToolFailure.fromAppError)
      _ <- ZIO.unless(RealPath.within(real, dir))(ZIO.fail(ToolFailure.Rejected("script 指向了 skill 目录之外")))
      problem <- ZIO
                   .attemptBlocking {
                     val p = real.path
                     if !Files.isRegularFile(p) then Some(s"skill ${in.skill} 里没有脚本 $script")
                     else if !Files.isExecutable(p) then Some(s"$script 没有可执行位（chmod +x）")
                     else
                       val head = scala.util.Using.resource(Files.newInputStream(p))(_.readNBytes(2))
                       Option.unless(head.sameElements("#!".getBytes))(s"$script 第一行不是 shebang（如 #!/usr/bin/env python3）")
                   }
                   .mapError(e => ToolFailure.Aborted(s"读脚本失败：${e.getMessage}", Some(e)))
      _ <- ZIO.foreachDiscard(problem)(m => ZIO.fail(ToolFailure.Rejected(m)))
      approvals = entry.doc.approval.flatMap(a => normalize(a).toOption)
    yield Checked(entry, script, real, mode, approvals.contains(script))

  private def key(ctx: RunContext, c: Checked, in: SkillRunIn, hash: String): GrantKey =
    GrantKey(ctx.agent, c.entry.doc.name, c.script, in.args, in.secrets.distinct.sorted, hash)

  private def summary(in: SkillRunIn, c: Checked): String =
    val args    = if in.args.isEmpty then "" else in.args.map(a => if a.exists(_.isWhitespace) || a.isEmpty then s"'$a'" else a).mkString(" ", " ", "")
    val secrets = if in.secrets.isEmpty then "" else s"，注入 secret：${in.secrets.distinct.mkString("、")}"
    val how = c.mode match
      case RunMode.Foreground(t) => s"（前台，最多 ${t.toSeconds} 秒）"
      case RunMode.Background(_) => "（后台）"
    s"运行 skill ${c.entry.doc.name} 的脚本 ${c.script}$args$secrets$how"

  private def execute(runner: CommandRunner, ctx: RunContext, in: SkillRunIn, c: Checked): IO[ToolFailure, ToolOutcome] =
    val label = s"skill ${c.entry.doc.name}/${c.script}" + (if in.args.isEmpty then "" else in.args.mkString(" ", " ", ""))
    runner
      .run(ctx, Program.Argv(c.real.path.toString, Chunk.fromIterable(in.args)), in.secrets, SecretUse.Script, c.mode, label)
      .map(ToolOutcome.text)
      .mapError(ToolFailure.fromAppError)

  def tool(runner: CommandRunner, grants: ScriptGrants): Tool[RunContext] =
    Tool[RunContext, SkillRunIn](
      "skill_run",
      "运行你的某个 skill 目录里的脚本：直接执行（不经 shell 解析，要有 shebang 行和可执行位），工作目录、写沙箱、超时、后台运行与 run_shell 相同。" +
        "secrets 可以用只允许脚本使用（allow_shell = false）的 secret：这种 key 只能经包好检查的脚本用，不能注入自己写的命令。" +
        "SKILL.md 的 frontmatter 里 `approval:` 列出的脚本，每次执行前等用户批准（这轮回答就此结束，结果或拒绝作为这个调用的结果回来）；用户选了「始终允许」的，同样的脚本、参数、secret 再调用且 skill 文件没改时直接执行。",
      Access.Write,
      Scheduling.Ordered,
      Display.Terminal
    ) { (in, ctx) =>
      check(in, ctx).flatMap { c =>
        if !c.needsApproval then execute(runner, ctx, in, c)
        else
          for
            hash <- ScriptGrants.filesHash(c.entry.doc.dir).mapError(ToolFailure.fromAppError)
            ok   <- grants.allowed(key(ctx, c, in, hash)).mapError(ToolFailure.fromAppError)
            out <-
              if ok then execute(runner, ctx, in, c)
              else ZIO.succeed(ToolOutcome.Suspended(Pause.Approval(Handler, summary(in, c), Approval(in, hash).toJsonAST.getOrElse(Json.Null))))
          yield out
      }
    }

  /** 用户批准后：确认 skill 文件和请求批准时一样，选了「始终允许」就记下，再执行。 */
  def approval(runner: CommandRunner, grants: ScriptGrants): ApprovalHandler = new ApprovalHandler:
    val name = Handler
    def approved(payload: Json, always: Boolean, ctx: RunContext): IO[ToolFailure, ToolOutcome] =
      for
        a    <- ZIO.fromEither(payload.as[Approval]).mapError(e => ToolFailure.Aborted(s"批准记录读不懂：$e"))
        c    <- check(a.call, ctx)
        hash <- ScriptGrants.filesHash(c.entry.doc.dir).mapError(ToolFailure.fromAppError)
        _ <- ZIO.when(hash != a.filesHash)(
               ZIO.fail(ToolFailure.Rejected("等批准期间这个 skill 的文件被改过了，没有执行：重新调用 skill_run，让用户批准现在这份"))
             )
        _   <- ZIO.when(always)(grants.grant(key(ctx, c, a.call, hash)).mapError(ToolFailure.fromAppError))
        out <- execute(runner, ctx, a.call, c)
      yield out

  val catalog: List[ToolInfo] = List(ToolInfo("skill_run", Access.Write))
