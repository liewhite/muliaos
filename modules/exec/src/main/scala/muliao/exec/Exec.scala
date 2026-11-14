package muliao.exec

import muliao.platform.AppError
import zio.*
import zio.process.{Command, ProcessInput}
import zio.stream.*

import java.nio.file.Path

/** 要执行什么。 */
enum Program:
  /** 一段 bash 脚本（`/bin/bash -c`）。 */
  case Shell(script: String)

  /** 直接执行的程序和参数。 */
  case Argv(program: String, args: Chunk[String])

/** 以某身份起一条命令：在哪个目录、带哪些环境变量和 secret、写权限多大。
  * @param env     在服务自己的环境变量之上追加的变量
  * @param secrets 作为环境变量注入；输出里出现它们的值会被打码
  */
final case class CommandSpec(
    program: Program,
    cwd: Path,
    sandbox: Sandbox,
    env: Map[String, String] = Map.empty,
    secrets: Chunk[SecretEnv] = Chunk.empty
)

/** 起来了的命令。 */
trait RunningCommand:
  /** 进程号，也是它自成一组的进程组号。 */
  def pid: Long

  /** 标准输出与标准错误合在一起的文本（已打码）。主进程退出后再限时读完：留在后台的子进程占着输出不放时，到时就不再等。 */
  def output: ZStream[Any, Nothing, String]

  /** 等主进程退出，得到退出码。 */
  def exitCode: UIO[Int]

  /** 结束整个进程组：先 TERM，等一会儿还在就 KILL。 */
  def kill: UIO[Unit]

/** 前台执行的结果。 */
final case class Finished(exitCode: Int, output: String, timedOut: Boolean)

object Exec:
  /** 主进程退出后最多再等多久读完输出。 */
  val DrainGrace: Duration = 2.seconds

  /** TERM 之后等多久再 KILL。 */
  val KillGrace: Duration = 3.seconds

  // 让命令自成进程组（便于整组结束），再 exec 成真正的命令，进程号不变。
  private val setpgrp = Chunk("/usr/bin/perl", "-e", "setpgrp(0,0); exec { $ARGV[0] } @ARGV or die qq(exec $ARGV[0]: $!\\n)")

  private[exec] def argv(spec: CommandSpec): Chunk[String] =
    val command = spec.program match
      case Program.Shell(script)  => Chunk("/bin/bash", "-c", script)
      case Program.Argv(p, args)  => p +: args
    val sandboxed = spec.sandbox match
      case Sandbox.Unrestricted   => command
      case Sandbox.Confined(w, h, r) => Chunk("/usr/bin/sandbox-exec", "-p", Sandbox.profile(w, h, r)) ++ command
    setpgrp ++ sandboxed

  /** 起命令；scope 结束时主进程还没退出就结束整组。 */
  def start(spec: CommandSpec): ZIO[Scope, AppError, RunningCommand] =
    val masker = Masker(spec.secrets)
    val all    = argv(spec)
    for
      process <- Command(all.head, all.tail*)
                   .workingDirectory(spec.cwd.toFile)
                   .env(spec.env ++ spec.secrets.map(s => s.name -> s.value))
                   .redirectErrorStream(true)
                   .stdin(ProcessInput.fromUTF8String(""))
                   .run
                   .mapError(e => AppError.internal(s"起命令失败：${e.getMessage}", e))
      pid    <- process.pid.orElseSucceed(-1L)
      exit   <- process.exitCode.map(_.code).orElseSucceed(-1).forkDaemon
      queue  <- Queue.bounded[Take[Nothing, String]](256)
      reader <- process.stdout.stream
                  .via(ZPipeline.utf8Decode)
                  .orElse(ZStream.empty)
                  .via(masker.pipeline)
                  .runForeachChunk(c => queue.offer(Take.chunk(c)))
                  .ensuring(queue.offer(Take.end))
                  .forkDaemon
      // 主进程退出后限时等读完；读不完（后台子进程还占着输出）就结束输出流，读的那一路放弃
      _ <- (exit.join *> reader.join.timeout(DrainGrace).flatMap {
             case Some(_) => ZIO.unit
             case None    => reader.interruptFork *> queue.offer(Take.end)
           }).forkDaemon
      running = Live(pid, queue, exit)
      _ <- ZIO.addFinalizer(exit.poll.flatMap(done => ZIO.when(done.isEmpty)(running.kill)))
    yield running

  /** 前台执行到结束或超时（超时结束整组），收齐输出。 */
  def run(spec: CommandSpec, timeout: Duration): IO[AppError, Finished] =
    ZIO.scoped {
      for
        r      <- start(spec)
        out    <- r.output.runCollect.map(_.mkString).fork
        code   <- r.exitCode.timeout(timeout)
        _      <- ZIO.when(code.isEmpty)(r.kill)
        status <- r.exitCode
        text   <- out.join
      yield Finished(status, text, code.isEmpty)
    }

  private final case class Live(pid: Long, queue: Queue[Take[Nothing, String]], exit: Fiber[Nothing, Int])
      extends RunningCommand:
    val output: ZStream[Any, Nothing, String] = ZStream.fromQueue(queue).flattenTake
    def exitCode: UIO[Int]                    = exit.join

    def kill: UIO[Unit] =
      signal("TERM") *> exit.join.timeout(KillGrace).flatMap {
        case Some(_) => signal("KILL") // 主进程走了，组里可能还有别的
        case None    => signal("KILL") *> exit.join.unit
      }

    // 先按进程组发；组还没建好（极早期）就发给进程本身
    private def signal(sig: String): UIO[Unit] =
      ZIO
        .attemptBlocking {
          val group = ProcessBuilder("/bin/kill", s"-$sig", "--", s"-$pid").start().waitFor()
          if group != 0 then ProcessBuilder("/bin/kill", s"-$sig", pid.toString).start().waitFor(): Unit
        }
        .ignore
