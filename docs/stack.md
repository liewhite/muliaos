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

## 工具链与开发
- 全部用 sdkman 装（brew 不装 JDK）：`sdk install java 25.0.3-tem`、`sdk install sbt`、`sdk install scala`（当前 JDK 25.0.3 Temurin、sbt 1.12.11、Scala 3.10.1）。新开的 shell 先 `source ~/.sdkman/bin/sdkman-init.sh`。
- 常用：`sbt test`（全部测试，含 Scala.js 的契约测试，要 node）、`sbt app/compile`、`sbt --client <命令>`（常驻 sbt 服务，反复编译快很多）。
- 本地起服务：`sbt "export app/Runtime/fullClasspath"` 拿到类路径，`java --enable-native-access=ALL-UNNAMED -cp <类路径> muliao.app.Main serve <配置文件>`；配置与数据放在 worktree 的 `dev-data/`（已 gitignore），端口别用 1080。导出接口描述：`muliao.app.Main openapi`。
- 编译选项开了 `-Werror`、`-Wunused:imports`、`-Wvalue-discard`：丢弃的值（尤其没跑的 ZIO）编译即报错。
- 依赖说明：quill 4.9.0 依赖 zio-json 0.10（只用于 Postgres），build.sbt 里放行被 1.x 取代；它还把 scalafmt、scala 2.13 编译器带进了运行时类路径，打包（#877）时看能否排除。

## 模块与依赖方向
按层排列，每个模块只依赖更低层的模块（具体见下表）：
```
第 0 层  contract
第 1 层  platform                    web（前端，只依赖 contract）
第 2 层  toolkit   exec
第 3 层  resources   models          （两者都依赖 exec）
第 4 层  sessions
第 5 层  workspace   notifications
第 6 层  board   triggers
第 7 层  history
第 8 层  app（组装入口）   migrate（迁移工具）
```
| 模块 | 内容 | 直接依赖（另外每个领域都依赖 platform、toolkit） |
|---|---|---|
| contract | 跨领域编号（`Newtype`/opaque type）、`ApiError`、`Change`、各领域的 tapir 接口声明 | — |
| platform | 配置、数据库与迁移、错误模型、日志、出站 HTTP 与系统代理、退避重试、「数据变了」总线、HTTP 服务骨架（登录、SSE、OpenAPI、静态前端）、单实例锁；不含领域词汇 | contract |
| toolkit | 给 agent 的工具的共同契约：`Tool`、`ToolSpec`、参数 schema 生成、失败分类 | platform |
| exec | 以某身份起命令：进程组、写沙箱、secret 注入与打码、限时读完输出 | platform |
| resources | 资源、agent 与权限 | exec（可写范围直接算成写沙箱 `Sandbox.Confined`） |
| models | 模型接入、MCP、外部服务 | exec |
| sessions | 会话与运行 | resources、models |
| workspace | 文件、命令、后台任务、skill 脚本、页面 | sessions、exec |
| notifications | 通知与推送 | sessions |
| board | issue 看板 | sessions、notifications |
| triggers | 触发器 | sessions、workspace、notifications |
| history | 工作历史、活动回顾、用量 | sessions、board |
| app | 组装全部领域、程序入口 | 全部领域 |
| migrate | 一次性数据迁移 | 全部领域 |
| web | 前端 | contract（JS） |

- 依赖方向只在 `build.sbt` 的 `domain(...)` 参数里定义；sbt 模块依赖不能成环，反向 import 编译期就找不到类。要加一条依赖先在这里讲清理由。
- 下游领域要上游做的事走上游定义的接口；上游要通知下游（如会话结束要发通知）时由上游定义一个接口（port），下游实现，app 组装时接上，不反向依赖。

## 平台基础的用法约定
- **配置**：一个 TOML 文件，每个领域声明自己的段 `ConfigSection[A]("段名")`（`A` 是带 `JsonCodec` 的 case class，键名蛇形 `@jsonMemberNames(SnakeCase)`），加进 `app/Main.sections`。未知的段、段里（含嵌套）未知的键、缺必填项都在启动时报错。程序不读环境变量。
- **数据库**：`Database.read(q)` 走只读连接池，`Database.write(tx)` 在唯一的写连接上串行执行一个事务；`tx` 以 `AppError` 失败整体回滚。查询写法：`import io.getquill.*`、`import muliao.platform.db.Sql.*`，`Sql.run(...).orInternal`。所有 `Newtype` 编号自动按底层类型存取。提交后才做的事用 `TxHooks.afterCommit`（如 `bus.publishAfterCommit(topic)`）。
- **迁移**：脚本放 `modules/<领域>/src/main/resources/db/<领域>/V<n>__<说明>.sql`，每个领域自己的版本记录表 `schema_history_<领域>`，版本号互不干扰；一个领域待执行的迁移在一个事务里，失败整体回滚；库里有程序不认识的迁移（库比程序新）拒绝启动。领域之间不建外键。
- **错误**：领域失败归入 `AppError` 四类（`Domain`/`Unavailable`/`External`/`Internal`）；接口层 `AppError.toApi` 转统一错误体；工具层 `ToolFailure.fromAppError` 转「请重试 / 暂时不可用 / 中止」，挂起用 `ToolOutcome.Suspended`。
- **重试**：外部调用 `.retryTransient()`（2 秒起翻倍、单次最长 5 分钟、累计 30 分钟放弃，遵守 Retry-After），只重试 `Unavailable`。
- **出站 HTTP**：一律经 `OutboundHttp`（系统代理及其例外列表、统一 User-Agent、状态码与连接错误分类），不直接用 zio-http 客户端。
- **接口**：在 contract 里基于 `Api.base`/`Api.secure` 声明，服务端用 `Endpoints.secured(auth)` / `.public` 接上逻辑，交给 `HttpServer.serve` 的 `domainEndpoints`。
- **工具**：`Tool[Ctx, In](名字, 说明, Access, Scheduling, Display)((in, ctx) => ...)`，参数类型 `In` 派生 zio-schema，字段说明用 `@description`，敏感字段标 `@sensitive`。
- **命令**：`Exec.run(spec, timeout)` / `Exec.start(spec)`；沙箱 `Sandbox.Confined(可写, 私有)` 只收 `RealPath`（`RealPath.resolve` 解析出的真实路径），可写范围由调用方按权限算好。

## 资源、agent 与权限（resources，#866）
- 资源目录的文件格式与布局与现有 `~/.muliao/assets` 兼容（agent 目录名就是 agent 名；`platform/access.toml` 里 `[agents.<编号>]`；secret 是 Fernet 密文，主密钥在配置 `[vault] key`）。SKILL.md 的 frontmatter 用 snakeyaml-engine 解析。
- agent 的身份（编号、名字、归档、默认、改名/删除进度）在库里（表 `agent`），可编辑配置在 `agent.toml`。改名先记库再挪目录，删除先标「正在删」再清理；两者中途重启由 `Agents.resumePending` 接着做（组装入口起动时调用）。
- 资源目录每 2 秒按指纹（路径、大小、纳秒修改时间、inode）轮询，变了才重新解析并广播主题 `assets`；平台自己写文件后立即刷新。
- access.toml 写坏时沿用上一份有效的（副本在数据目录 `resources/access.last-good.toml`，重启后仍有效），平台经 `AccessEditor` 改它会被拒绝直到修好；平台写回 access.toml 时按固定格式重排（角色、agent 按名字排序，`[agents.<编号>] # 名字`），手写的注释不保留。
- 组装时要提供：`ModelCheck`（模型领域：默认模型、模型写法能不能跑）、`ToolCatalog`（全部内置工具的名字与类别，从各工具的 `spec` 汇总）、`ResourcePaths.privatePaths`（配置文件、备份、推送私钥）、各领域的 `AgentHook`（忙碌判断与删除清理，登记到 `AgentHooks`）。
- 验收脚本：`sbt "resources/Test/runMain muliao.resources.RealAssetsCheck <资源目录副本> <agent TSV> <主密钥文件>"`，只读副本，不打印 secret 值。
