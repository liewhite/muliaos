# 架构

[English](../en/architecture.md)

给读代码、改代码的人的概览。随每次改动更新的详细设计在 [docs/stack.md](../stack.md)（技术栈、模块接法、开发约定）和
[docs/web.md](../web.md)（前端）；功能规格是 [docs/requirements.md](../requirements.md)。

## 系统形态

一台 Mac 上的一个 JVM 进程：提供网页界面和 HTTP 接口、跑全部 agent、独占一个 SQLite 数据库。

```
浏览器 / iPhone PWA ──HTTP + SSE──▶ muliao（单进程）
                                     ├─ 会话：agent 循环，每个会话同时一次运行
                                     ├─ 工具 ─▶ macOS 写沙箱里的命令、文件、别的 agent、看板、触发器
                                     ├─ 模型 ─▶ Anthropic Messages / OpenAI Responses、MCP server、网页、语音
                                     ├─ SQLite（data_dir/muliao.db）
                                     └─ 资源目录（普通文件，轮询变化）
```

基本取舍：

- **单用户、单机。** 没有账号、没有分布式状态。可以牺牲边角体验换架构简单，但绝不能出现不可恢复的错误（丢数据、重复已有副作用的操作、权限失控）。
- **数据库是唯一事实来源。** 界面的实时刷新（SSE）只是加速，重新加载总能看到真实状态。
- **每一步都落库。** 运行把每次模型回复、每批工具结果保存后才往下走，所以重启是接着跑，不是重做。
- **人要改的东西都是普通文件。** 记忆、skill、MCP 配置、secret（加密）、agent 设置和角色都是资源目录里的文件，不在数据库里。
- **一个配置文件。** 不读环境变量；未知的键就是错误。

## 技术栈

| 方面 | 选型 |
|---|---|
| 语言 | Scala 3、JDK 25，sbt 构建 |
| 副作用与并发 | ZIO 2（layer、scope、队列、hub、信号量、重试策略），流用 ZStream |
| HTTP 接口 | tapir 声明接口，放在 JVM 与 Scala.js 共用的模块；服务端 zio-http；生成 OpenAPI |
| JSON | zio-json、zio-schema（工具参数的 JSON Schema 由类型生成） |
| 数据库 | SQLite（sqlite-jdbc）+ ProtoQuill；表结构迁移用 Flyway，各模块各管各的 |
| 子进程 | zio-process；写沙箱用 macOS `sandbox-exec` |
| 模型接入 | 自写 Anthropic Messages 与 OpenAI Responses 流式客户端 |
| MCP | 官方 MCP Java SDK，外包 ZIO |
| 加密 | 只用 JDK（Fernet vault、Web Push 加密、VAPID） |
| 前端 | Scala.js + Laminar、Vite、Tailwind 与 HeroUI 样式 |
| 打包 | fat jar + jlink 裁剪的运行时，嵌进一个小 C 启动器，签名成一个可执行文件 |

## 模块

模块分层，每个只依赖更低层；sbt 不允许模块依赖成环，反向 import 编译不过。

```
第 0 层  contract
第 1 层  platform                    web（前端，只依赖 contract）
第 2 层  toolkit   exec
第 3 层  resources   models
第 4 层  sessions
第 5 层  workspace   notifications
第 6 层  board   triggers
第 7 层  history
第 8 层  app（组装与程序入口）
```

| 模块 | 职责 |
|---|---|
| `contract` | 编号、共用枚举、接口错误类型、全部 HTTP 接口声明；同时编译成 JVM 和 JS |
| `platform` | 配置、数据库与迁移、错误模型、日志、出站 HTTP（系统代理、重试）、「数据变了」总线、HTTP 服务骨架（登录、SSE、OpenAPI、静态前端）、单实例锁 |
| `toolkit` | 给 agent 的工具的共同契约：定义、参数 schema、失败分类、MCP 连接方式 |
| `exec` | 起进程：进程组、写沙箱、secret 注入与打码、限时读完输出 |
| `resources` | 资源目录、agent、角色与权限、vault 与 secret 授权、以 agent 身份起命令 |
| `models` | 模型服务商、MCP 客户端、网页搜索与抓取、语音转写 |
| `sessions` | 会话、运行与 agent 循环、委派、等待、压缩、时间线 |
| `workspace` | 文件工具、shell 与后台任务、skill 脚本与审批、agent 页面 |
| `notifications` | 通知、收件箱、Web Push |
| `board` | issue、依赖、投递到 issue 会话、审核 |
| `triggers` | 定时、一次性、常驻流、前置检查、故障 |
| `history` | 工作历史、按天活动回顾、会话读取、用量 |
| `app` | 把全部模块装起来（`ZLayer.make`，少了依赖编译期报错），程序入口 |
| `web` | 单页前端 |

上游模块要通知下游（比如会话结束了该发通知）时，由上游定义一个端口（port）、下游实现，`app` 组装时接上。

每个领域模块自己管自己的表和迁移（`modules/<模块>/src/main/resources/db/<模块>/V<n>__<说明>.sql`，有自己的版本记录表）、自己的 agent 工具、HTTP 路由和测试。

## 运行与恢复

- 一次运行循环：组装请求（固定说明、记忆、skill 索引，然后是对话）→ 流式取模型回复 → 执行工具调用 → 保存 → 再来。固定部分在同一会话的多次运行间逐字节不变，以命中服务商的提示缓存。
- 写都走唯一的写连接，一次一个事务；读用单独的连接池。
- 工具调用经过 `Pending → Running →（Suspended → Approved）→ Done`，每个状态都存库。启动时续跑没结束的运行；停在 `Running` 的调用不再执行，而是得到「被中断、没重新执行、先核实」的结果。
- 暂时性失败（`Unavailable`）指数退避重试（2 秒起翻倍，单次最长等 5 分钟，累计 30 分钟放弃）。
- 后台任务、等委派的 thread、定时唤醒都记成会话的等待项；完成时投一条事件消息，会话再跑。

启动顺序：单实例锁 → 装配模块（迁移、全局记忆、资源目录轮询）→ 续跑会话 → 接着做没做完的 agent 改名与删除 → 收尾上个进程留下的后台任务 → 启动触发器 → 启动 HTTP。关停顺序相反。

## 打包与部署

`deploy/build.sh` 用 Vite 构建前端、`sbt app/assembly` 打 fat jar、`jdeps` + `jlink` 裁出运行时，把它们压缩后嵌进 `deploy/launcher.c` 的一个 Mach-O 段。
启动器第一次运行时把载荷解到程序旁边，再以子进程启动 Java、转发信号。

`deploy/install.sh` 构建一个版本，用固定的自签证书签名（macOS 授权升级后不丢），复制成固定路径的真目录 `app/current/`；`deploy/activate.sh` 换版本并经 launchd 重启；
`deploy/service.sh` 装 launchd 服务；`deploy/selfcheck.sh` 检查部署。见 [快速上手](getting-started.md)。
