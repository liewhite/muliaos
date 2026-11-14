package muliao.migrate

import zio.*

/** 一次性迁移工具：只读地把现有 muliao 的数据复制并转换进新的数据目录（#876 实现）。 */
object Main extends ZIOAppDefault:
  def run: ZIO[Any, Any, Any] = Console.printLineError("迁移工具尚未实现（#876）").as(ExitCode(2))
