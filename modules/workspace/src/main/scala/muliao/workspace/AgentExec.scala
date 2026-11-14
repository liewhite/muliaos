package muliao.workspace

import muliao.contract.AgentId
import muliao.exec.{CommandSpec, Program, RealPath, SecretEnv}
import muliao.platform.AppError
import muliao.resources.{ResourcePaths, Resources, WritableScope}
import muliao.resources.vault.{SecretUse, Vault}
import zio.*

import java.nio.file.{Files, Path}

/** 以 agent 身份要起的一条命令：规格（工作目录、写沙箱、注入的 secret）和算出它时的可写范围。 */
final case class AgentCommand(spec: CommandSpec, scope: WritableScope)

/** 以 agent 身份起命令的唯一入口：run_shell、skill_run、触发器的前置检查与常驻命令都经这里得到命令规格，
  * 保证工作目录、写沙箱与 secret 权限只有一种算法。
  */
trait AgentExec:
  /** 算出命令规格：在 agent 的工作目录执行（不存在就建），写沙箱是它的可写范围，`secrets` 按用途检查权限后作为环境变量注入。 */
  def command(agent: AgentId, program: Program, secrets: List[String], use: SecretUse): IO[AppError, AgentCommand]

  /** agent 的工作目录（不存在就建）。 */
  def workspace(agent: AgentId): IO[AppError, Path]

  /** 命令输出里有写被拦下（Operation not permitted）且路径确实不在可写范围时，补一句被什么拦、找谁授权；没有就原样返回。 */
  def annotate(agent: AgentId, scope: WritableScope, output: String): UIO[String]

object AgentExec:
  val layer: URLayer[Resources & Vault & ResourcePaths, AgentExec] = ZLayer.derive[Live]

  /** 系统报「不允许」的路径：`touch: /x/y: Operation not permitted`、`bash: /x/y: Operation not permitted` 之类。 */
  private val Denied = """(/[^\s:'"]+)['"]?:? Operation not permitted""".r

  /** 输出里被拒绝写的路径（去重，最多几个）。 */
  def deniedPaths(output: String): List[String] = Denied.findAllMatchIn(output).map(_.group(1)).toList.distinct.take(5)

  final case class Live(resources: Resources, vault: Vault, paths: ResourcePaths) extends AgentExec:

    def workspace(agent: AgentId): IO[AppError, Path] =
      val dir = paths.workspace(agent)
      ZIO.attemptBlocking(Files.createDirectories(dir)).mapError(e => AppError.internal(s"建工作目录 $dir 失败", e)).as(dir)

    def command(agent: AgentId, program: Program, secrets: List[String], use: SecretUse): IO[AppError, AgentCommand] =
      for
        dir    <- workspace(agent)
        scope  <- resources.writableScope(agent)
        values <- if secrets.isEmpty then ZIO.succeed(Map.empty[String, String]) else vault.reveal(agent, secrets.distinct, use)
        env     = Chunk.fromIterable(secrets.distinct.flatMap(k => values.get(k).map(SecretEnv(k, _))))
      yield AgentCommand(CommandSpec(program, dir, scope.sandbox, secrets = env), scope)

    def annotate(agent: AgentId, scope: WritableScope, output: String): UIO[String] =
      ZIO
        .foreach(deniedPaths(output)) { p =>
          RealPath.resolve(Path.of(p)).flatMap { real =>
            if scope.permits(real) then ZIO.none
            else resources.explainDenied(agent, Path.of(p)).map(why => Some(s"[写沙箱拦下了 $p：$why]"))
          }
        }
        .map(_.flatten)
        .map(notes => if notes.isEmpty then output else output.stripSuffix("\n") + "\n" + notes.mkString("\n"))
        .catchAll(_ => ZIO.succeed(output))
