package muliao.resources

import muliao.contract.{AgentId, AskPurpose, SecretUse, SessionId}
import muliao.exec.{CommandSpec, Program, RealPath, Sandbox, SecretEnv}
import muliao.platform.AppError
import muliao.resources.vault.Vault
import zio.*

import java.nio.file.{Files, Path}

/** 以 agent 身份要起的一条命令：规格（工作目录、写沙箱、注入的 secret）和算出它时的可写范围。 */
final case class AgentCommand(spec: CommandSpec, scope: Sandbox.Confined)

/** 以 agent 身份起命令的唯一入口：run_shell、skill_run、触发器的前置检查与常驻命令都经这里得到命令规格，
  * 保证工作目录、写沙箱与 secret 权限只有一种算法。
  */
trait AgentExec:
  /** 算出命令规格：在 agent 的工作目录执行（不存在就建），写沙箱是它的可写范围，`secrets` 按用途检查权限后作为环境变量注入。
    * `session`：在哪个会话里跑（触发器为空）；passkey secret 等授权时，用户在那个会话里看到要跑的命令并批准。
    */
  def command(agent: AgentId, program: Program, secrets: List[String], use: SecretUse, session: Option[SessionId]): IO[AppError, AgentCommand]

  /** 命令输出里有写被拦下（Operation not permitted）且路径确实不在可写范围时，补一句被什么拦、找谁授权；没有就原样返回。 */
  def annotate(agent: AgentId, scope: Sandbox.Confined, output: String): UIO[String]

object AgentExec:
  val layer: URLayer[Resources & Vault & ResourcePaths, AgentExec] = ZLayer.derive[Live]

  /** 系统报「不允许」的路径：`touch: /x/y: Operation not permitted`、`bash: /x/y: Operation not permitted` 之类。 */
  private val Denied = """(/[^\s:'"]+)['"]?:? Operation not permitted""".r

  /** 输出里被拒绝写的路径（去重，最多几个）。 */
  /** 给用户看的命令：脚本原文，或程序与参数。 */
  def describe(p: Program): String = p match
    case Program.Shell(script)    => script
    case Program.Argv(prog, args) => (prog +: args).mkString(" ")

  def deniedPaths(output: String): List[String] = Denied.findAllMatchIn(output).map(_.group(1)).toList.distinct.take(5)

  /** 带 setuid 位的文件：macOS 不让沙箱里的进程执行它，报的也是 Operation not permitted。 */
  def setuid(p: Path): Boolean =
    scala.util.Try(Files.getAttribute(p, "unix:mode").asInstanceOf[Int] & 0x800).toOption.exists(_ != 0)

  final case class Live(resources: Resources, vault: Vault, paths: ResourcePaths) extends AgentExec:

    def command(agent: AgentId, program: Program, secrets: List[String], use: SecretUse, session: Option[SessionId]): IO[AppError, AgentCommand] =
      for
        dir    <- paths.ensureWorkspace(agent)
        scope  <- resources.writableScope(agent)
        values <- if secrets.isEmpty then ZIO.succeed(Map.empty[String, String]) else vault.reveal(agent, secrets.distinct, use, AskPurpose(session, AgentExec.describe(program)))
        env     = Chunk.fromIterable(secrets.distinct.flatMap(k => values.get(k).map(SecretEnv(k, _))))
      yield AgentCommand(CommandSpec(program, dir, scope, secrets = env), scope)

    def annotate(agent: AgentId, scope: Sandbox.Confined, output: String): UIO[String] =
      ZIO
        .foreach(deniedPaths(output)) { p =>
          RealPath.resolve(Path.of(p)).flatMap { real =>
            if setuid(real.path) then ZIO.some(s"[沙箱里不能执行 setuid 程序 $p（与写权限无关）；常用的话找 admin（muliao）放进沙箱外执行的名单]")
            else if scope.permits(real) then ZIO.none
            else resources.explainDenied(agent, Path.of(p)).map(why => Some(s"[写沙箱拦下了 $p：$why]"))
          }
        }
        .map(_.flatten)
        .map(notes => if notes.isEmpty then output else output.stripSuffix("\n") + "\n" + notes.mkString("\n"))
        .catchAll(_ => ZIO.succeed(output))
