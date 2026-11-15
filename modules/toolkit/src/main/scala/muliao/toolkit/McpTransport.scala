package muliao.toolkit

import zio.Chunk

/** 一个 MCP server 的连接方式：资源领域从资源文件读出、模型领域照它连接，两边用这同一份。
  * 值（命令参数、环境变量、地址、请求头）里可以写 `${vault:KEY}`，连接时代入 secret；命令本身不代入。
  */
enum McpTransport:
  /** 本地进程，走标准输入输出。经平台执行层起：自成进程组、进使用者的写沙箱，连接断开时整组结束。 */
  case Stdio(command: String, args: Chunk[String], env: Map[String, String])

  /** 远端 HTTP（Streamable HTTP）。 */
  case Http(url: String, headers: Map[String, String])

  /** 会代入 secret 的值里引用到的 secret 名。 */
  def secretRefs: Set[String] =
    val texts = this match
      case Stdio(_, args, env) => args.toList ++ env.values
      case Http(url, headers)  => url :: headers.values.toList
    texts.flatMap(VaultRef.keys).toSet

/** 文本里的 `${vault:KEY}` 引用。 */
object VaultRef:
  val Pattern = """\$\{vault:([A-Za-z0-9_.-]+)\}""".r

  /** 文本里引用的 secret 名。 */
  def keys(text: String): Set[String] = Pattern.findAllMatchIn(text).map(_.group(1)).toSet
