# 技术栈与开发约定（#864，用户 11-15 确认）

## 技术栈
| 用途 | 选型 |
|---|---|
| 语言与工具链 | Scala 3（最新稳定版）、JDK 25（≥24）、sbt；全部用 sdkman 安装 |
| 运行时与并发 | ZIO 2（ZLayer 装配、Scope 管资源、Ref/Queue/Hub/Semaphore、Schedule 退避重试） |
| 流 | ZStream（模型流式输出、SSE、命令输出） |
| HTTP 接口 | tapir 声明接口（服务端用 zio-http 解释器，JSON 用 zio-json，生成 OpenAPI）；接口声明放在 JVM 与 Scala.js 共用的模块，前端用 tapir 的 sttp 客户端调用，前后端同一份类型 |
| 出站 HTTP | zio-http 客户端（模型、网页、推送、语音） |
| JSON | zio-json + zio-schema（工具参数的 JSON Schema 从类型生成） |
| 数据库 | SQLite（sqlite-jdbc）+ ProtoQuill（quill-jdbc-zio）；表结构升级用 Flyway |
| 子进程 | zio-process（命令、后台任务、MCP 本地进程、触发器命令；macOS 写沙箱用 sandbox-exec） |
| 配置 | 不用 zio-config：一个 TOML 文件，用 TOML 解析库读成 case class，未知键报错 |
| 日志 / 测试 | zio-logging / zio-test（TestClock 测定时与重试） |
| 模型接入与 agent 循环 | 自写：zio-http 客户端直接对 Anthropic Messages 与 OpenAI Responses 协议（流式）；agent 循环在会话领域 |
| MCP 客户端 | 官方 MCP Java SDK，外包 ZIO |
| 加密 | 只用 JDK（vault 的 Fernet、Web Push 加密与 VAPID 签名） |
| cron | cron-utils |
| 前端 | Scala.js + Laminar，外观与交互保持现有 muliao 界面的风格（样式表可沿用） |
| 打包 | sbt-assembly 打 fat jar，再用 jlink 生成精简 JRE，合成一个可直接执行的单文件（jar + JVM 都在里面） |

## 开发约定
- 仓库：`/Users/liewhite/.muliao/data/workspaces/143cd08d29b0/muliao-scala`（本地 git，主分支 main，暂不推远端）。每个 issue 在自己的 worktree 和分支里做，测试过后合回 main（skill code-git-workflow）。
- 需求以 `docs/requirements.md` 为准；设计照 skill code-dev-standards：按领域划模块、领域之间只经接口通信；逻辑尽量表达在类型上（ADT、opaque type、穷尽匹配），少写 if 判断值。
- 不参考 Rust 版的结构和写法，只按需求和 Scala 3 + ZIO 的惯用法设计。
- 不改 Rust 版仓库、线上服务、`~/.muliao` 下的任何东西。开发用的数据目录放在 worktree 或临时目录里。
- 每个领域：自己的表和迁移、自己的工具（给 agent 的）、自己的 HTTP 接口、测试，都在同一个 issue 里做完。
