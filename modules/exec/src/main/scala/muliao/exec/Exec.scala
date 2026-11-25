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

/** 命令输出的一行（已打码，不带换行）。 */
enum Line:
  def text: String
  case Out(text: String)
  case Err(text: String)

/** 标准错误怎么交出。 */
enum Stderr(private[exec] val merged: Boolean):
  /** 并进标准输出（两者的先后保持原样），都是 [[Line.Out]]。 */
  case Merged extends Stderr(true)
  /** 单独成 [[Line.Err]]。 */
  case Separate extends Stderr(false)

/** 前台执行怎么结束的。 */
enum Ending:
  case Exited(code: Int)
  /** 到时没跑完，已结束整组 */
  case TimedOut

/** 前台执行的结果：输出是各行以换行连起来。 */
final case class Finished(output: String, ending: Ending)

/** 走标准输入输出对话的命令（如本地 MCP server）：标准输出是协议数据，按行原样交出；标准错误是给人看的日志，已打码。 */
trait AttachedCommand:
  def pid: Long

  /** 写一行到标准输入（自动加换行并立即刷出）。进程已退出或关了输入时失败（暂时不可用）。 */
  def send(line: String): IO[AppError, Unit]

  /** 关闭标准输入（许多程序以此为退出信号）。 */
  def closeInput: UIO[Unit]

  /** 标准输出的行（未打码）。只能读一次。 */
  def stdout: ZStream[Any, Nothing, String]

  /** 标准错误的行（已打码）。只能读一次。 */
  def stderr: ZStream[Any, Nothing, String]

  def exitCode: UIO[Int]

  /** 结束整个进程组：先 TERM，等一会儿还在就 KILL。 */
  def kill: UIO[Unit]

object Exec:
  /** 主进程退出后最多再等多久读完输出。 */
  val DrainGrace: Duration = 2.seconds

  /** TERM 之后等多久再 KILL。 */
  val KillGrace: Duration = 3.seconds

  /** 命令的标准输入接什么。 */
  private enum Stdin:
    /** 管道：由服务写入（写完关闭，或对话中一直开着）。 */
    case Piped
    /** /dev/null：没有要喂的。不用空管道，是因为 ripgrep 这类命令见标准输入是管道就去搜它、不搜当前目录（#1318）。 */
    case Null

  // 让命令自成进程组（便于整组结束），按需把标准输入换成 /dev/null，再 exec 成真正的命令，进程号不变。
  private def prelude(stdin: Stdin): Chunk[String] =
    val reopen = stdin match
      case Stdin.Piped => ""
      case Stdin.Null  => """open(STDIN, "<", "/dev/null"); """
    Chunk("/usr/bin/perl", "-e", "setpgrp(0,0); " + reopen + "exec { $ARGV[0] } @ARGV or die qq(exec $ARGV[0]: $!\\n)")

  private def argv(spec: CommandSpec, stdin: Stdin): Chunk[String] =
    val command = spec.program match
      case Program.Shell(script)  => Chunk("/bin/bash", "-c", script)
      case Program.Argv(p, args)  => p +: args
    val sandboxed = spec.sandbox match
      case Sandbox.Unrestricted   => command
      case c: Sandbox.Confined     => Chunk("/usr/bin/sandbox-exec", "-p", Sandbox.profile(c)) ++ command
    prelude(stdin) ++ sandboxed

  /** 起命令，逐行读完它的输出（先打码再分行，跨行的 secret 也换得掉），每行交给 `each`，得到退出码。
    *   - `stdin` 每项一行写进标准输入，写完关闭；没有要写的，标准输入接 /dev/null。
    *   - 主进程退出后限时读完余下的输出：留在后台的子进程占着输出不放时，到时就不再等，也不杀它。
    *   - 被中断（超时、取消、服务关停）时主进程还没退出就结束整组。
    */
  def follow(spec: CommandSpec, stderr: Stderr, stdin: Chunk[String] = Chunk.empty)(each: Line => UIO[Any]): IO[AppError, Int] =
    val masker = Masker(spec.secrets)
    ZIO.scoped(
      for
        process <- launch(spec, stderr.merged, if stdin.isEmpty then Stdin.Null else Stdin.Piped)
        pid     <- process.pid.orElseSucceed(-1L)
        exit    <- process.exitCode.map(_.code).orElseSucceed(-1).forkDaemon
        _       <- ZIO.addFinalizer(exit.poll.flatMap(done => ZIO.when(done.isEmpty)(killGroup(pid, exit))))
        _       <- feed(process, stdin).forkDaemon
        queue   <- Queue.bounded[Take[Nothing, Line]](256)
        _       <- ZIO.addFinalizer(queue.shutdown)
        pump = (bytes: ZStream[Any, Any, Byte], as: String => Line) =>
                 bytes
                   .via(decode)
                   .orElse(ZStream.empty)
                   .via(masker.pipeline)
                   .via(ZPipeline.splitLines)
                   .map(as)
                   .runForeachChunk(c => queue.offer(Take.chunk(c)))
        // 读管道是阻塞调用、中断要等它返回：读的两路不跟着 scope 等它们结束，只发中断；进程组随后被结束、管道关闭，它们自然收尾
        out <- pump(process.stdout.stream, Line.Out(_)).forkDaemon
        err <- (stderr match
                 case Stderr.Merged   => ZIO.unit
                 case Stderr.Separate => pump(process.stderr.stream, Line.Err(_))
               ).forkDaemon
        _    <- ZIO.addFinalizer(out.interruptFork *> err.interruptFork)
        _    <- (exit.join *> (out.join *> err.join).timeout(DrainGrace) *> queue.offer(Take.end)).forkScoped
        _    <- ZStream.fromQueue(queue).flattenTake.runForeach(each)
        code <- exit.join
      yield code
    )

  // 不是合法 UTF-8 的字节换成 �（`utf8Decode` 遇到就失败，读的一路断掉、命令随后被 SIGPIPE 杀掉）
  private def decode: ZPipeline[Any, java.nio.charset.CharacterCodingException, Byte, String] =
    ZPipeline
      .decodeCharsWithDecoder(
        java.nio.charset.StandardCharsets.UTF_8
          .newDecoder()
          .onMalformedInput(java.nio.charset.CodingErrorAction.REPLACE)
          .onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPLACE)
      )
      >>> ZPipeline.mapChunks((cs: Chunk[Char]) => Chunk.single(String(cs.toArray)))

  // 写完就关；命令不读输入、先退出了时写不进去，不算错
  private def feed(process: zio.process.Process, lines: Chunk[String]): UIO[Unit] =
    ZIO.attemptBlocking {
      scala.util.Using.resource(process.getOutputStream) { o =>
        lines.foreach(l => o.write((l + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)))
      }
    }.ignore

  /** 起一条走标准输入输出对话的命令（同样自成进程组、进写沙箱、注入 secret）；scope 结束时关输入并结束整组。 */
  def attach(spec: CommandSpec): ZIO[Scope, AppError, AttachedCommand] =
    for
      process <- launch(spec, mergeStderr = false, Stdin.Piped)
      pid     <- process.pid.orElseSucceed(-1L)
      exit    <- process.exitCode.map(_.code).orElseSucceed(-1).forkDaemon
      lock    <- Semaphore.make(1)
      attached = Attached(pid, process, exit, lock, Masker(spec.secrets))
      _       <- ZIO.addFinalizer(attached.closeInput *> exit.poll.flatMap(done => ZIO.when(done.isEmpty)(attached.kill)))
    yield attached

  // Java 那头总是管道（zio-process 不提供从文件重定向）；Stdin.Null 时子进程在 exec 前换成 /dev/null，管道没人读，写空关掉即可
  private def launch(spec: CommandSpec, mergeStderr: Boolean, stdin: Stdin): IO[AppError, zio.process.Process] =
    val all = argv(spec, stdin)
    Command(all.head, all.tail*)
      .workingDirectory(spec.cwd.toFile)
      .env(spec.env ++ spec.secrets.map(s => s.name -> s.value))
      .redirectErrorStream(mergeStderr)
      .stdin(ProcessInput.Pipe)
      .run
      .mapError(e => AppError.internal(s"起命令失败：${e.getMessage}", e))

  /** 前台执行到结束或超时（超时结束整组），收齐输出（标准输出与标准错误合在一起）；超时也带上已有的输出。 */
  def run(spec: CommandSpec, timeout: Duration): IO[AppError, Finished] =
    for
      seen <- Ref.make(Chunk.empty[String])
      code <- follow(spec, Stderr.Merged)(l => seen.update(_ :+ l.text)).timeout(timeout)
      out  <- seen.get
    yield Finished(out.mkString("\n"), code.fold(Ending.TimedOut)(Ending.Exited(_)))

  private final case class Attached(
      pid: Long,
      process: zio.process.Process,
      exit: Fiber[Nothing, Int],
      lock: Semaphore,
      masker: Masker
  ) extends AttachedCommand:
    private val input = process.getOutputStream

    def send(line: String): IO[AppError, Unit] =
      lock
        .withPermit(ZIO.attemptBlocking {
          input.write((line + "\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))
          input.flush()
        })
        .mapError(e => AppError.Unavailable(s"写不进命令的标准输入：${e.getMessage}", Some(e)))

    def closeInput: UIO[Unit] = lock.withPermit(ZIO.attemptBlocking(input.close()).ignore)

    def stdout: ZStream[Any, Nothing, String] = process.stdout.linesStream.orElse(ZStream.empty)
    def stderr: ZStream[Any, Nothing, String] = process.stderr.linesStream.orElse(ZStream.empty).map(masker.mask)
    def exitCode: UIO[Int]                    = exit.join
    def kill: UIO[Unit]                       = killGroup(pid, exit)

  private def killGroup(pid: Long, exit: Fiber[Nothing, Int]): UIO[Unit] =
    signal(pid, "TERM") *> exit.join.timeout(KillGrace).flatMap {
      case Some(_) => signal(pid, "KILL") // 主进程走了，组里可能还有别的
      case None    => signal(pid, "KILL") *> exit.join.unit
    }

  // 先按进程组发；组还没建好（极早期）就发给进程本身
  private def signal(pid: Long, sig: String): UIO[Unit] =
    ZIO
      .attemptBlocking {
        val group = ProcessBuilder("/bin/kill", s"-$sig", "--", s"-$pid").start().waitFor()
        if group != 0 then ProcessBuilder("/bin/kill", s"-$sig", pid.toString).start().waitFor(): Unit
      }
      .ignore
