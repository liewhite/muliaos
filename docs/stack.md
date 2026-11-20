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
- 仓库：GitHub `liewhite/muliaos`，主分支 main，MIT 许可。每个 issue 在自己的 worktree 和分支里做，测试过后合回 main。面向外部贡献者的构建、测试与约定在 `CONTRIBUTING.md`，用户文档在 `docs/en`、`docs/zh-CN`（行为变了要同步更新）。
- 需求以 `docs/requirements.md` 为准；设计照 skill code-dev-standards：按领域划模块、领域之间只经接口通信；逻辑尽量表达在类型上（ADT、opaque type、穷尽匹配），少写 if 判断值。
- 不参考 Rust 版的结构和写法，只按需求和 Scala 3 + ZIO 的惯用法设计。
- 不改 Rust 版仓库、线上服务、`~/.muliao` 下的任何东西。开发用的数据目录放在 worktree 或临时目录里。
- 每个领域：自己的表和迁移、自己的工具（给 agent 的）、自己的 HTTP 接口、测试，都在同一个 issue 里做完。

## 工具链与开发
- 全部用 sdkman 装（brew 不装 JDK）：`sdk install java 25.0.3-tem`、`sdk install sbt`、`sdk install scala`（当前 JDK 25.0.3 Temurin、sbt 1.12.11、Scala 3.10.1）。新开的 shell 先 `source ~/.sdkman/bin/sdkman-init.sh`。
- 常用：`sbt test`（全部测试，含 Scala.js 的契约测试，要 node）、`sbt app/compile`、`sbt --client <命令>`（常驻 sbt 服务，反复编译快很多）。
- 本地起服务：`sbt "export app/Runtime/fullClasspath"` 拿到类路径，`java --enable-native-access=ALL-UNNAMED -cp <类路径> muliao.app.Main serve <配置文件>`；配置与数据放在 worktree 的 `dev-data/`（已 gitignore），端口别用 1080。导出接口描述：`muliao.app.Main openapi`。
- 编译选项开了 `-Werror`、`-Wunused:imports`、`-Wvalue-discard`：丢弃的值（尤其没跑的 ZIO）编译即报错。
- 依赖说明：quill 4.9.0 依赖 zio-json 0.10（只用于 Postgres），build.sbt 里放行被 1.x 取代；它还把 scalafmt、scala 2.13 编译器带进了运行时类路径，打包时没有排除（quill 运行时可能用，理由见 build.sbt 的 app 段）。

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
第 8 层  app（组装入口）
```
| 模块 | 内容 | 直接依赖（另外每个领域都依赖 platform、toolkit） |
|---|---|---|
| contract | 跨领域编号（`Newtype`/opaque type）、`ApiError`、`Change`、各领域的 tapir 接口声明、前后端都要算的编码（SSH 证书 `SshCert`） | — |
| platform | 配置、数据库与迁移、错误模型、日志、出站 HTTP 与系统代理、退避重试、「数据变了」总线、HTTP 服务骨架（登录、SSE、OpenAPI、静态前端）、单实例锁；不含领域词汇 | contract |
| toolkit | 给 agent 的工具的共同契约：`Tool`、`ToolSpec`、参数 schema 生成、失败分类；MCP 的连接方式 `McpTransport`（资源领域读出、模型领域连接，共用一份） | platform |
| exec | 以某身份起命令：进程组、写沙箱、secret 注入与打码、限时读完输出 | platform |
| resources | 资源、agent 与权限；以 agent 身份起命令的入口 `AgentExec` | exec（可写范围直接算成写沙箱 `Sandbox.Confined`） |
| models | 模型接入、MCP、外部服务 | exec |
| sessions | 会话与运行 | resources、models |
| workspace | 文件、命令、后台任务、skill 脚本、页面 | sessions、exec |
| notifications | 通知与推送 | sessions |
| board | issue 看板 | sessions、notifications |
| triggers | 触发器 | sessions、notifications |
| history | 工作历史、活动回顾、用量 | sessions、board |
| app | 组装全部领域、程序入口 | 全部领域 |
| web | 前端 | contract（JS） |

- 依赖方向只在 `build.sbt` 的 `domain(...)` 参数里定义；sbt 模块依赖不能成环，反向 import 编译期就找不到类。要加一条依赖先在这里讲清理由。
- 下游领域要上游做的事走上游定义的接口；上游要通知下游（如会话结束要发通知）时由上游定义一个接口（port），下游实现，app 组装时接上，不反向依赖。

## 平台基础的用法约定
- **配置**：一个 TOML 文件，每个领域声明自己的段 `ConfigSection[A]("段名")`（`A` 是带 `JsonCodec` 的 case class，键名蛇形 `@jsonMemberNames(SnakeCase)`），列在本领域 `Module` 的 `sections`（配了才有的功能列在 `optionalSections`）；配置文件允许出现的段名由这些派生，`AppConfig` 读成字段，`AppConfig.layer` 交给各领域。未知的段、段里（含嵌套）未知的键、缺必填项都在启动时报错。程序不读环境变量。
- **数据库**：`Database.read(q)` 走只读连接池，`Database.write(tx)` 在唯一的写连接上串行执行一个事务；`tx` 以 `AppError` 失败整体回滚。查询写法：`import io.getquill.*`、`import muliao.platform.db.Sql.*`，`Sql.run(...).orInternal`。所有 `Newtype` 编号自动按底层类型存取。提交后才做的事用 `TxHooks.afterCommit`（如 `bus.publishAfterCommit(Topic.Issues)`）。
- **「数据变了」**：主题是 contract 的 `enum Topic`，前后端共用；读接口在 contract 里用 `.refreshedBy(主题...)` 声明跟着哪些主题刷新，前端 `Remote` 据此订阅。每个领域只在一处发：会话领域的写都经 `Kernel.touched(会话, 时间)`（更新活动时间、广播 sessions、推该会话的实时事件 `Changed`），看板经记动态（`record`），后台任务经 `Jobs` 的 `write`。
- **迁移**：脚本放 `modules/<领域>/src/main/resources/db/<领域>/V<n>__<说明>.sql`，每个领域自己的版本记录表 `schema_history_<领域>`，版本号互不干扰；一个领域待执行的迁移在一个事务里，失败整体回滚；库里有程序不认识的迁移（库比程序新）拒绝启动。领域之间不建外键。
- **错误**：领域失败归入 `AppError` 四类（`Domain`/`Unavailable`/`External`/`Internal`）；接口层 `AppError.toApi` 转统一错误体；工具层 `ToolFailure.fromAppError` 转「请重试 / 暂时不可用 / 中止」，挂起用 `ToolOutcome.Suspended`。
- **重试**：外部调用 `.retryTransient()`（2 秒起翻倍、单次最长 5 分钟、累计 30 分钟放弃，遵守 Retry-After），只重试 `Unavailable`。
- **出站 HTTP**：一律经 `OutboundHttp`（系统代理及其例外列表、统一 User-Agent、状态码与连接错误分类），不直接用 zio-http 客户端。`send`/`open` 只把 2xx 当成功；要按对方契约自己解读状态码（跟随跳转、区分错误体）用 `exchange`。
- **接口**：在 contract 里基于 `Api.base`/`Api.secure` 声明，服务端用 `Endpoints.secured(逻辑)` 得到 `Route`；输入有两个以上同类型、容易传反的字段（多个查询参数、文件名加类型）用 `.mapTo[case class]` / `.mapInTo[case class]` 收成一个类型；「编号 + 请求体」这种类型各不相同的二元组照旧。实时连接（SSE）在 contract 声明为 `Feed[输入, 事件]`（地址加事件名，data 是事件的 JSON），服务端 `feed.securedFeed(建订阅)`（连接帧 ready/ping 的 data 是 contract 的 `Beat`：心跳间隔与程序版本 `Build.version`，见 `Sse`），前端 `Live.frames(feed)(输入)`，登录校验由 `HttpServer.serve` 统一套上，领域不经手登录；不要登录的用 `Route.unsecured`。各领域的 `XHttp(服务...).routes` 由组装入口直接调用，不做成 layer。请求体整个读进内存，上限 `HttpServer.MaxRequestBytes`（256MB，只防误传超大文件；zio-http 默认的约 100KB 会让附件上传 413）。前端遇到不合接口声明的回应（如 HTTP 层直接回的 413）按状态码报人话（`Http.unreadable`）。
- **工具**：静态定义 `val XxxDef = ToolDef[Ctx, In](名字, 说明, Access, Scheduling, in => Display...)`（参数类型 `In` 派生 zio-schema，字段说明用 `@description`，敏感字段标 `@sensitive`；`Labeled` 枚举与 `Newtype` 编号自带 zio-schema，直接当字段类型；界面展示 `Display` 是带数据的 ADT，命令、路径、委派对象由这个函数从参数取，前端不按字段名猜），拿到服务后 `XxxDef { (in, ctx) => ... }` 得到 `Tool`。每个领域导出 `specs`（全部定义的 `spec`），组装入口据此静态给出全部内置工具目录；`Tool(...)(...)` 只在测试里用。
- **命令**：起命令并读完输出只有一个入口 `Exec.follow(spec, Stderr.Merged | Separate, stdin)(每行 => …)`，得到退出码：先打码再分行、主进程退出后限时读完、被中断就整组结束都在里面做，前台命令（`Exec.run`）、前置检查、常驻命令、后台任务都用它，调用方要限时就 `.timeout`、要停就中断；走标准输入输出对话的（本地 MCP server）用 `Exec.attach(spec)`；沙箱 `Sandbox.Confined(可写, 私有, 只读)` 只收 `RealPath`（`RealPath.resolve` 解析出的真实路径），可写范围由调用方按权限算好；平台自己判断能不能写用同一份规则的 `permits`。「解析真实路径后是否在某根目录下」一律用 `RealPath.under(根, 路径)`。
- **可选配置段**：配了才有的功能（网页工具、语音转写）用 `ConfigFile.optionalSection`，文件里没有这一段得到 `None`。

## 模型接入与外部服务（models 模块）
- **配置**：`[models]` 必填：`default_model = "<服务商>:<模型名>"`，`[models.providers.<名字>]` 写 `protocol`（`anthropic` / `openai-responses`）、`base_url`（Anthropic 不带 `/v1`，OpenAI 带）、`api_key`、`max_concurrency`（默认 4）、`max_output_tokens`（可选）；`idle_timeout_seconds`（默认 180）是回复流多久没数据算断。`[web]`（`api_key`、`base_url`）与 `[transcription]`（`api_key`、`model`、`base_url`、`max_bytes`、`vocabulary`、`language_hints`）可选。
- **请求模型**：`Models.request(ModelRef, ModelRequest)` 得到 `ZStream[ReplyEvent]`，最后一个是 `Finished(reply)`；用 `ReplyBuilder` 折叠事件得到到目前为止的各段（被打断时保存已生成的部分）。不重试：`AppError.Unavailable` 由调用方退避重试整步。回复各段的 `Seal` 是协议要回传的数据，只在收完的段上有，回传时只发同协议的。思考（#950）：请求不带思考设置（新模型如 opus-5-5 关不掉思考，发 `disabled` 会被拒）；回来的思考/推理块读流时不报告（各协议的 `Slot.Held`），收完时整块原样成 `ReplyPart.Reasoning` 插回原位（`Slot.interleave`），只为后续请求回传（官方要求工具循环里原样带回），实时事件、时间线、读会话都不展示；`Reply.shown` 是报告过的段。
- **图片**：发给模型前在 `Models.request` 里统一整理（长图切段、超限缩小、坏图换成说明、超过张数上限的较早图片换成说明），会话历史存原件。
- **MCP**：`McpConnector.connectAll(servers, secrets, launch)` 在调用方的 scope 里连接，得到工具（`<server>_<工具名>`）与没连上的原因；`SecretLookup` 由资源领域按使用者权限实现，`Launch` 给出 stdio server 的工作目录与写沙箱。
- **测试**：协议读流用真实服务商录下的响应（`modules/models/src/test/resources/fixtures/`，录制脚本 `record_fixtures.py` 在同一目录，密钥从本机配置读、不打印）。对真实服务商各跑一次：`sbt "models/Test/runMain muliao.models.LiveCheck <配置文件> claude:<模型>,openai:<模型> <录音> audio/mp4"`（两轮工具调用验证回传、模型列表、网页搜索与抓取、语音转写）。

## 资源、agent 与权限（resources，#866）
- 资源目录的文件格式与布局与现有 `~/.muliao/assets` 兼容（agent 目录名就是 agent 名；`platform/access.toml` 里 `[agents.<编号>]`；secret 是 Fernet 密文，主密钥在配置 `[vault] key`）。SKILL.md 的 frontmatter 用 snakeyaml-engine 解析。
- agent 的身份（编号、名字、归档、默认、改名/删除进度）在库里（表 `agent`），可编辑配置在 `agent.toml`。新建先在库里记「正在建」（`AgentState.Creating`，不算现存）再写资源文件、工作目录、角色，最后改成 active，中途出错当场、中途重启在启动时按删除收拾掉（#1062）；改名先记库再挪目录，删除先标「正在删」再清理；中途重启都由 `Agents.start(各领域的 AgentHook)` 接着做（组装入口在 `Sessions.start` 之后调用；归档、删除在它之前会等它）。
- **secret 的三个维度**（#1069）：`SecretFile(说明, lock: SecretLock, content: SecretContent)`。谁能解开 `SecretLock = Server(Fernet 密文) | Passkey(PasskeyBox)`（文件里 `ciphertext` 或 `[passkey]` 表）；内容与能否取明文 `SecretContent = Opaque(RevealScope) | KeyPair(算法, KeyExposure = Reveal(RevealScope) | SignOnly)`（contract 里，前后端共用；「口令 + 只能签名」在类型上写不出），文件里 `keypair = "ed25519"`（有就是密钥对）与 `usage = shell（缺省）| scripts | sign`。签名在哪做由解锁方推出：`Vault.sign` 对 Server 的解开后用 `ServerSigner`（JDK Ed25519）当场签、不等批准，对 Passkey 的走 `Grants.sign`。设值 `Vault.set(…, ContentChange = Keep | Scope | Replace, …)`：agent 的 `vault_set` 只能 Keep 或改范围（参数 `usage`），界面整个指定（`SecretWrite.Value`）或只改说明与范围（`SecretWrite.Settings`）；服务端密钥对设值时先核对私钥认得。手改文件（`AssetGuard`）只能动说明与 shell/scripts，密文、内容类型、「只能签名」都改不了。
- **passkey secret**（#915）：`[passkey]` 表（`rp`、`credential`、`iv`、`ciphertext`：浏览器用 WebAuthn PRF 从 passkey 导出密钥、AES-GCM 加密，附加数据见 `SecretContent.aad`：能取明文的是 secret 名）。只有用户能设、改 passkey 的值（界面里加密好再交 `SecretValue.Passkey`），agent 的 `vault_set` 与手改文件都改不动。`Vault.reveal` 是唯一入口：passkey 的经 `Grants.obtain` 等用户授权——每次调用各自一个请求，带 `AskPurpose`（哪个会话、拿去跑什么：命令全文、skill 脚本与参数、要连的 MCP、触发器名与命令），批准卡片显示在那个会话页（推送点开即到），没有会话的（触发器）在 AI 资产 Secret 页（那里列全部）。用户用 passkey 解开交回明文：「一次」「拒绝」只结这一个请求；「一段时间」把这个 agent 这个 secret 记成已授权并结掉它全部在等的请求，到期前不再问（到期的在下次读写状态时删掉）。调用结束（含被中断）时请求随之撤下（`acquireReleaseWith`）。授权全在内存，按 agent 隔离，重启清空。新请求经 `Grants.asked` 由通知领域的 `VaultLink` 推送给用户（有会话的指向会话；组装时在 `Sessions.start` 之前订上）。等待不设时限，也不在前置检查的时限里（时限只包命令本身）：触发器用到它就卡在「检查中」、之后到点记跳过，常驻流卡在启动前，直到用户授权（用户定：卡住就卡住，这类 secret 不该用在没人在场的地方）。MCP 配置引用它的，每次运行开头连 MCP 都要等授权（授权「一次」就每次运行问一次）。前端只在 HTTPS 的域名地址（不是 IP、localhost）上启用 passkey。接口 `VaultApi`（`VaultHttp`），主题 `vault`。
- **passkey 密钥对与 SSH 短期证书**（#938）：passkey 加密、只能签名的密钥对（`Passkey` + `KeyPair(_, SignOnly)`）只能在浏览器里签名：`Vault.reveal` 对只能签名的直接拒绝（不发请求），加密时 AES-GCM 的附加数据是 `signing:<算法>:<名>`（`PasskeyAad`），取明文那条路（按名字解）解不开它；浏览器签名时先按这个附加数据解、解不开再按 secret 名解（能取明文的密钥对也能签，签名路只交出签名）。保证范围：网页由运行平台的机器提供，那台机器被攻破后可以换掉网页，在用户下一次用 passkey 确认时拿走私钥；所以保证的是「攻破后要等用户再确认一次」，不是「攻破也拿不到」（用户 11-18 定不另建授权站，接受这一点）。只有用户能设（界面「加密方式」选签名钥匙时先核对是认得的私钥；已有的普通 secret 要改成签名钥匙就在表单里重新设值）。签名请求是带类型的内容 `SignContent`（第一种 `SshCert(SshCertRequest)`，加一种内容加一个分支，各自要的算法 `content.algorithm`），经 `Vault.sign`（核对是签名钥匙）→ `Grants.sign` 请用户批，`AskWant.Sign(content)`；浏览器按类型自己拼待签字节、用 @noble/curves 签名，只交回结果（`AskDecision.Signed`）；每次都要批，「一段时间」的授权不管它，答复种类不符报错。工具 `ssh_certificate`：`SshKeys` 用 JDK 生成临时 ed25519 钥匙，签好后与证书写进系统临时目录下只有本用户可读的新目录（macOS 几天后自动清）。证书编码在 contract 的 `SshCert`（前后端共用），测试与 ssh-keygen 签的逐字节比对。服务器端：`TrustedUserCAKeys` 加 `AuthorizedPrincipalsFile /etc/ssh/auth_principals/%u`（文件里写「账户@主机」），签给一台的证书登不了别台。
- 资源文件的位置与路径只在 `AssetPath` 互换：`classify`（路径 → 位置）、`render`（资源文件 → 路径，测试保证往返一致）、`home`（某资源归某主人时放在哪）；不手拼 `skills/`、`secrets/` 这类路径。每种文件的规格（名字、内容解析）只在 `FileSpec.load`，扫描与写入口共用。
- 资源目录每 2 秒按指纹（路径、大小、纳秒修改时间、inode）轮询，变了才重新解析并广播主题 `assets`；平台自己写文件后立即刷新。
- access.toml 写坏时沿用上一份有效的（副本在数据目录 `resources/access.last-good.toml`，重启后仍有效），平台经 `AccessEditor` 改它会被拒绝直到修好；平台写回 access.toml 时按固定格式重排（角色、agent 按名字排序，`[agents.<编号>] # 名字`），手写的注释不保留。
- 组装时要提供：`ModelCheck`（模型领域：默认模型、模型写法能不能跑）、`ToolCatalog`（全部内置工具的名字与类别，由各领域静态的 `specs` 给出）、`ResourcePaths.privatePaths`（配置文件、备份、推送私钥）；启动时把各领域的 `AgentHook`（名字、删除清理，有「忙」的再给忙碌判断）交给 `Agents.start`。
- 验收脚本：`sbt "resources/Test/runMain muliao.resources.RealAssetsCheck <资源目录副本> <agent TSV> <主密钥文件>"`，只读副本，不打印 secret 值。

## 会话领域的接法（#868，给下游领域）
- **给 agent 的工具**：`ToolDef[RunContext, In]` 实现出的 `Tool[RunContext, S]`（`S` 是挂起时交出的说明，不会挂起的是 `Nothing`，会挂起的是 `Pause` 的某种；`muliao.sessions.RunContext`：agent、会话、运行、调用编号、来源、工作目录、本次运行的资源快照、用户在不在等），放进本领域的 `Contribution`（工具、事件订阅、`AgentHook`），由下游领域的纯函数给出；组装入口在 `Sessions.start(SessionPorts(各领域的 Contribution, jobs, unread))` 时一次交进来（只调一次；之前读这些端口的会等到这时）。工具的静态定义另汇进资源领域的 `ToolCatalog`，两者一致由 `AppSpec` 检查。界面用的操作在 `SessionConsole`，不在 `Sessions`。
- **投信**：一律经 `Sessions.deliver(Address, Letter)`：触发器用 `Address.Fresh(agent, Origin.Trigger(名), 标题)`，看板用 `Address.IssueOf(agent, issue, 标题)`（每个 issue + agent 一个会话），事件用 `Source.Event(种类)`；投不进以 `AppError` 说明原因。「回答交给谁」由信里的 `replyTo` 定（用户在等写 `ReplyTo.User`）。
- **agent 显示名**：`Agents.names(编号)` 一次查出（含已删除的，查不到显示编号）；不逐个 `get`。
- **工作状态**：`Sessions.states(ids)` 是全系统唯一的判断（running / waiting / idle），看板停滞、触发器防重叠、界面都用它。
- **会话等的后台任务**：开始时 `Sessions.openJobWait(会话, 任务号, 说明, 心跳)`，结束时 `closeJobWait(任务号, Some(事件信))`；`SessionPorts.jobs` 是必填的 `JobControl`（工作区的 `Workspace.jobControl`：心跳检查里的一句现状、会话被停止时结束任务）。等待项是 ADT `WaitRef = Job | Wake | Delegation`，唤醒与委派由会话领域自己处理。
- **状态与种类**：跨领域、存库、走接口的状态与种类都是 `Labeled` 枚举（contract 的 `Labels` 给出解析、JSON、tapir、zio-schema 编解码，`Sql` 自动按 `label` 存取，当工具参数时 JSON Schema 列出全部 label），不写字符串比较；带分支字段的接口数据（时间线条目、来信来源、会话来源、回复段、实时事件、挂起说明、等待项）是 JSON 里 `kind` 为分支名的 ADT。
- **要用户批准的执行**：工具返回 `ToolOutcome.Suspended(Pause.Approval(给用户看的摘要, 数据))`，工具本身用 `Approvable(工具)(批准后怎么做)` 包起来：用户批准后会话领域按调用的工具名找回它接着执行（`always` 表示用户选了以后不再问）。
- **工具调用的状态**：`CallStatus = Pending | Running | Suspended(挂起说明) | Approved(批准说明, always) | Done(结果)`，各步带着自己的数据和时间；库里仍是平铺的列，写入只经 `CallTx.save(调用, 状态)`，读出经 `Decode.call` 还原。
- **事件**：`Contribution.listener` 订阅 `SessionEvent`，`on(事件): Tx[Unit]` 在会话领域产生事件的写事务里按贡献的顺序逐个调用（#1062）：订阅方只写自己的表（看板记动态、经 `deliverIn` 请表态，通知领域 `notifyIn`），推送等慢的或外部的事用 `afterCommit`；它出错整个事务回滚：正常收尾由启动时补收尾重做，停止与删除报错给调用方，运行最终失败的记录没写下（运行仍是运行中到重启续跑），挂起没记下（重启后这次调用按「被打断」交给模型）。事件有 `Settled`（回到空闲/最终失败/被停止，`toUser` 表示用户在等；最终失败无论谁在等都该通知用户。会话内部：取走回答去向、交回委派、发 `Settled`、清掉「待收尾」一律经 `Delegations.settleTx`，放在调用方的收尾事务里，#1046；运行做完的事务里记下「待收尾」（`session.settle_due`），启动时待收尾或有挂起运行的会话都会被拉起，补收尾或接着跑）、`NeedsUser`（提问、等批准）、`Deleted`。未读是必填的 `SessionPorts.unread`（通知领域的 `SessionLink.unread`）。
- **停止**：`Sessions.stop(会话, also)` 先挡住它和它开出的、没空闲的 thread（逐层）的驱动，再在一个事务里记下整棵的停止；`also` 是调用方要一起提交的改动（看板叫停 issue：改状态、投叫停留言），写在停止之后，投给这个会话的信不会被丢弃。
- **时间线**：`Sessions.timeline(id)` 给出界面与 `session_read` 共用的投影（`TimelineItem`，来信的 `source` 是 ADT `SourceView`：触发器名、issue 号、发起会话等都带在类型里，前端不拆字符串；session_read 的 USER/TRIGGER/… 标签由 history 的 `Reading.source` 给出）。存库的 `Source` 与接口的 `SourceView` 分开：两边 JSON 写法不同（库里是 zio-json 默认的外包对象），分开就不用迁移，`Source.view` 穷尽映射。会话列表的 `SessionSummary.origin` 直接是 contract 的 ADT `Origin`（用户 / 触发器名 / issue 号 / thread 的发起会话与名字），库里按种类、引用、发起会话三列存，只在 `SessionInfo` 里换算，所以只有一个类型。

## 通知与推送（notifications，#870）
- **配置**：`[push]` 必填：`vapid_subject`（`mailto:` 或 `https://` 开头）；`vapid_key_file` 可选，默认数据目录下的 `vapid_private.pem`（PKCS#8 PEM，与现有文件同一格式，迁移时原样复制即可，已订阅的设备不用重订；没有就生成，权限 600；读不出来报错、不覆盖）。
- **发通知**：`Notifications.notifyIn(Notice(标题, 正文, Target))` 在调用方的写事务里写（看板交给用户、触发器告警等与自己的改动同一事务提交），提交后才广播主题 `notifications` 并推送；`notify` 自己开事务。`Target`：`Session(id)`（点开去会话页、打开即已读、随会话删除）、`Page(前端路由)`（如请用户审核的 issue 页 `/issues/<号>`，Service Worker 据此加「通过/打回」）、`Inbox`（系统通知，点开看通知页）。
- **组装**：`Notifications.layer`（要 `Database`、`ChangeBus`、`OutboundHttp`、`PushSettings`、`GeneralSettings`）；`SessionLink.contribution`：`notify_user` 工具（静态定义在 `SessionLink.specs`）、会话事件订阅（规则在纯函数 `SessionRules.wanted`）；`SessionLink.unread`：会话未读；`NotificationsHttp(...).routes` 交给 HTTP 服务。
- **推送载荷**（Service Worker 契约）：`{"title","body","url","badge"}`，`url` 没有时是 `/notifications`，`badge` 是发出那一刻的未读数；标题、正文按字截到 120 / 600。
- **测试**：`CryptoSpec`（RFC 8291 例子逐字节、VAPID 签名验签、openssl 私钥互通）、`NotificationsSpec`（事务、逐设备重试与订阅清理、已读、会话事件、HTTP）。现有私钥的验收：`sbt "notifications/Test/runMain muliao.notifications.VapidKeyCheck <私钥文件>"`（先复制再读），公钥与 `openssl pkey -pubout` 对照。

## 工作区领域的接法（#869，给下游领域与组装入口）
- **组装**：`Workspace.layer`（要 `PageSigner.fromSecret(server.cookieSecret)`）起全部服务；`Workspace.contribution` 给出 8 个工具（静态定义在 `Workspace.specs`）、skill 脚本的批准执行者 `skill_run`、agent 忙碌判断与删除清理；`Workspace.jobControl` 是会话等后台任务时的状态与取消。启动顺序：`Sessions.start` 之后调 `Jobs.recover`（上次没收尾的后台任务标为被中断、不盲杀，补交结局）。接口 `WorkspaceHttp(...).routes`，其中页面文件 `/site/<agent>/<page>/<签名>/<文件>` 不要登录，带隔离响应头（CSP sandbox、nosniff、允许跨源读取）。
- **以 agent 身份起命令**：一律经 `AgentExec.command(agent, program, secrets, SecretUse)` 得到 `CommandSpec`（工作目录及指向它的环境变量 `MULIAO_WORKSPACE`、写沙箱、secret 注入），再 `Exec.run/follow`；输出可经 `annotate` 补沙箱拦截说明（含读平台私有文件被拒）。它在资源领域（#892），工作区的命令与后台任务、触发器的前置检查与常驻命令都用它，不自己算可写范围。agent 的工作目录只经 `ResourcePaths.ensureWorkspace` 建。
- **文件读写**：`FileAccess` 按可写范围检查（平台私有文件对非 admin 读写都不行），每次写先经 `AssetGuard.check` 得到 `Vetted`（只有它能造）：`Plain` 是资源目录外的文件、照写，`Asset` 交给 `AssetStore.commit`；平台自己生成的资源文件用 `AssetStore.write(资源文件, 内容)`。读过的文本文件都记指纹；`WriteMode.Overwrite` 要求本次运行读过（读过的指纹按运行记在内存里），`Patch` 不要求。`AssetStore` 写文件覆盖已有文件时沿用原权限位（skill 脚本不丢可执行位）。
- **后台任务**：输出在工作目录 `jobs/<编号>.log`；每个任务是一个 daemon fiber，登记在 `Jobs` 的 registry 里（结束时自己删登记），`job_kill` 经它的 `stop` 结束（记被结束）；关停时 layer 的 finalizer 并行中断登记里的全部 fiber、进程由 `Exec.follow` 结束，标被中断，结局下次启动补交（表 `job` 的 `reported`）。不用并行策略的子 scope 做 `forkIn`：fiber 自己结束时关子 scope 会另起 fiber 去中断它自己而互等，留下永远挂着的 root fiber，关停卡到 launchd 强杀（#897）。边界：Jobs 的 finalizer 跑完后、会话关闭前才起的任务不在快照里，可能留在「运行中」，下次启动 `recover` 补成被中断。

## 看板（board，#871）
- **组装**：`Board.layer`（要 `Database`、`ChangeBus`、`Sessions`、`Notifications`、`Agents`、`GeneralSettings`）；`BoardLink.contribution`：6 个 issue 工具（静态定义在 `BoardTools.specs`）、会话事件订阅、删除 agent 时的清理（`AgentHook`）；`BoardHttp(...).routes` 交给 HTTP 服务（接口声明在 contract 的 `BoardApi`）。改动后广播主题 `issues`。
- **投递与事务**：每次改动在同一个写事务里写 issue、记动态、投信（会话领域的 `Sessions.deliverIn`，提交后会话才开始跑），不会出现「改了没投」或「投了没改」。投不进（agent 已归档等）只记一条「没能投给」的动态，改动照常提交。谁收信由纯函数 `Rules.recipients` 定：in_progress、指派给 agent、上游都结束 → 投给它（它在这个 issue 自己的会话里做的不投回）；否则只有用户的评论投给 owner。上游的「结束与否」变了时，在每个下游记一条动态并按同一规则投递，所以下游在最后一个上游结束时收到第一封信。
- **停滞与请表态**：停滞在读的时候算（`Rules.stalled`：该有人处理、它的 issue 会话空闲或还没有，状态取 `Sessions.states`）。issue 会话做完一段工作（会话事件 `Settled` 且回答完）后如果 issue 还该它处理，投一封请表态的信；表 `issue_nudge` 记下已请过，下次有动态投给它时清掉，所以不会反复催。会话最终失败只记一条动态，不重投。
- **审核**：`approve` / `sendBack` 只对指派给用户、还没结束的 issue 有效（否则 409）；打回给最近把指派改成用户的 agent（时间线里找），没有就给 owner。agent 把 issue 交给用户时（`Rules.handedToUser`）在同一事务里发通知，点开 `/issues/<号>`，正文取它刚写的评论。
- **时间线**：表 `issue_event` 只增不改，`actor` 是 `user` / `system` / agent 编号，`session_id` 是发生在哪个会话，`activity` 是 `Activity` 的 JSON。工作历史（#873）的「对 issue 的操作」从这里按时间、agent 取。
- **工具参数里的枚举**：接口里已有的 `Labeled` 枚举（如 `IssueStatus`）直接当字段类型；只给工具用的（如叫停后的状态）是 Scala 3 enum 加 `@caseName("in_progress")` 并派生 zio-schema，JSON Schema 里就是带 `enum` 的字符串（`ToolArgs` 取 `@caseName`），解码也认它。
- **测试**：`RulesSpec`（规则纯函数）、`BoardSpec`（真库、真会话与通知领域、假模型：投递规则、依赖放行、停滞与请表态、审核与冲突、会话失败、叫停、开始、删除、删 agent、工具、HTTP）。

## 工作历史与活动回顾（history，#873）
- **组装**：`History.layer`（要 `Database`、`ChangeBus`、`Sessions`、`Board`、`Agents`、`GeneralSettings`）；`HistoryLink.contribution`：4 个工具（静态定义在 `HistoryTools.specs`）；`HistoryHttp(...).routes` 交给 HTTP 服务（接口声明在 contract 的 `HistoryApi`，`GET /api/history`）。记下后广播主题 `history`。删 agent 不清理（工作历史保留），没有 `AgentHook`。
- **只追加**：表 `history_entry` 上的触发器拒绝 UPDATE / DELETE（库级），程序里也没有改、删的入口。一批在一个事务里写，任一条不合法（`NewEntry.make`：标题、摘要不空，日期不在今天之后；重要程度 1–3）整批不记。
- **检索**：不用 FTS（中文分词、trigram 对两个字的词无效）；SQL 先按 agent、日期、重要程度和最长的那个词（`instr`）筛，再由纯函数 `Search.rank` 要求每个词都出现、按相关度（标题与引用里出现的权重 3、摘要里 1，乘出现次数）排，同分按重要程度、日期。没有关键字时按日期倒序直接在 SQL 里取。
- **活动回顾**：按配置时区把一天换成 [from, to)，从会话领域 `Sessions.activity`（有来信/回复或运行的会话、来源为用户的来信、最终失败的运行）、看板 `Board.activity`（该 agent 做的，加用户在它当前负责或被指派的 issue 上做的；不给 agent 是用户与全部 agent 的；平台记的不算）和本领域当天的历史取数，文字由纯函数 `OverviewText` 写，按行分页（`Pages`，每次约 4 万字符，最后一行说明 offset）。
- **读会话**：`Sessions.find` 按编号或唯一前缀找会话，`Sessions.timeline` 的投影经纯函数 `Reading.items` 变成带标签的条目（USER/TRIGGER/ISSUE/DELEGATION/EVENT、ASSISTANT、`call <工具>`、`result <工具>`、`error <工具>`、CONTEXT、COMPACTION、`NOTICE <种类>`），`ReadFilter` 按标签、正则（不分大小写）、时刻筛选，用户与回答正文不截、其余按 clip 截，按条分页。
- **测试**：`PureSpec`（相关度、记录校验、分页、读会话的条目与筛选、回顾的写法）、`HistorySpec`（真库、真会话/看板/通知领域、假模型：四个工具经模型调用、整批拒绝、库级拒绝改删、按天汇总单个与全部 agent 及别的日子、前缀与筛选与续读、HTTP）。

## 触发器领域的接法（#872，给组装入口与迁移）
- **组装**：`Triggers.layer`（要 `Database`、`ChangeBus`、`Sessions`、`Agents`、`Notifications`、`AgentExec`、`ResourcePaths`、`GeneralSettings`、`TriggerSettings`）；`Triggers.contribution`：6 个工具（静态定义在 `Triggers.specs`）和 agent 删除时的清理（`forgetAgent`）。`Sessions.start`、`Jobs.recover` 之后调 `Triggers.start` 起到期扫描与常驻流托管；它们跑在 layer 的 scope 里，关停时随之停下（常驻命令整组结束）。接口 `TriggersHttp(...).routes`，主题 `triggers`。
- **配置**：`[triggers] precheck_timeout_seconds`（默认 60）。cron 与不带时区的时刻按 `[general] timezone` 解释。
- **数据**（表 `trigger_def`、`trigger_fault`）：`config` 是 `TriggerConfig` 的 JSON（带 `kind` 的 ADT：`Schedule{cron}`、`Once{at}`、`Stream{command, secrets, debounceSeconds}`，接口也是它；给模型的工具参数仍是平铺的 `ConfigIn`，按种类读成它）、`precheck`（`PrecheckSpec` 的 JSON：`command`、`fireWhen`、`secrets`）。读出时连同 `enabled`、`next_at`、`finished_at` 一次解析成 `Mode`（每种带自己的状态：定时 `Armed = Off | On(下次到期)`、一次性 `OnceState = Off | Waiting | Finished(时间)`、常驻流开关）与 `Precheck`，读不懂报内部错误（不自动停用）；写回时这三列由 `Mode` 派生。已结束的一次性 `finished_at` 非空、`enabled = 0`（#889 迁移 V2 补齐旧行），名字只在没结束的之间唯一。迁移进来的开关照搬旧版（#881 正式切换：先停旧版再迁移，两边不会同时开着）。
- **触发只有一个出口** `Core.fire`：agent 能不能接活（归档：定时跳过、一次性推送用户没做、流式事件丢弃）→ 防重叠（正在跑前置检查的靠内存集合；上次开的会话不空闲就跳过，定时计入 `skipped`，挡路会话在等待时通知用户一次）→ 前置检查（`bash` 开 pipefail，事件行从标准输入喂入）→ 在一个事务里经 `Sessions.deliverIn` 开会话并记下这次触发。
- **测试**：`ModelSpec`（纯函数）、`ScheduleSpec`（测试时钟：定时、防重叠、停机补一次、停用不补、一次性、归档）、`CommandSpec`（真实进程：前置检查各模式与失败/超时、常驻流防抖与排队、退避重启与故障、工具、HTTP）。用测试时钟拨时间的测试把资源目录轮询调大（`TestEnv.layer(assetPoll = 1.hour)`），否则拨一次时钟要跑成千上万次轮询。

## 组装入口（app，#874）
- **配置**：`AppConfig.load` 一次读出全部段（可选段 `[web]`、`[transcription]` 没配就是 None，对应工具和接口不提供）；允许的段名由平台段（general、log、server、http）加各领域 `Module` 声明的段派生，未知的段、段里未知的键启动即报错。数据目录下的位置：`workspaces/<agent 编号>`、`files/`（附件）、`resources/`（上一份有效的 access.toml）、`backups/`、`vapid_private.pem`；配置文件、备份（含 `general.offsite_backup_dir` 指的异地备份目录）、推送私钥是平台私有文件。
- **装配**：`Assembly.layer(cfg, models)` 用 `ZLayer.make` 装全部领域（少了哪个依赖编译期报错）；`models` 默认真实接入，测试换假模型。
- **内置工具目录**：由各领域静态的工具定义给出（`Assembly.toolSpecs`，每个领域一份 `specs`，网页工具配了 `[web]` 才有），装配时就是定值。实际接进会话领域的工具由 `Assembly.contributions` 给出（每个领域一行）；两者一一对应、没有重名由 `AppSpec` 检查，不在启动时核对。MCP 工具按运行连接，不在目录里。
- **启动顺序**（`Assembly.boot`）：单实例锁（`Main`，在打开数据库之前）→ 装配（迁移、全局记忆、资源目录轮询）→ 会话启动（交进各领域的贡献与后台任务、未读，续跑）→ agent 启动（交进各领域的 `AgentHook`，接着做没做完的改名与删除；删除要发会话事件、停后台任务，所以排在会话之后）→ 收尾上次的后台任务 → 触发器 → HTTP。关停是相反顺序（scope 后进先出）：HTTP 与实时连接 → 触发器与常驻命令 → 后台任务（结束并标被中断）→ 会话。
- **集成测试**：`AppSpec`（真配置文件、全部领域、真 HTTP，假模型）：工具目录与实际接上的工具一致、配置报错、接口描述、经 HTTP 与工具走一遍各领域，单实例，关停后同一份数据重启接着用。
- **本地起服务与真实模型验收**：见上文「工具链与开发」；`dev-data/` 下放配置（从现有配置取服务商密钥生成，不打印）。

## 数据迁移（migrate，#876）
- **代码不在主构建里**（#892）：11-15 正式切换后迁移工具移出主构建，最后一版在 tag `migrate-final`。要再从 Rust 版迁移：`git worktree add ../muliao-migrate migrate-final`，在那里按下面的用法跑（写出的库是那一版的表结构，之后用当前版本启动时按各领域迁移自动升级）。
- **用法**：`java -cp <migrate 类路径> muliao.migrate.Main ~/.muliao ~/.muliao-scala [--no-workspaces] [--port=1080]`（类路径 `sbt "export migrate/Runtime/fullClasspath"`）。新版主目录必须不存在或为空，不覆盖；`--no-workspaces` 只在开发试跑时用。不读工作目录约 15 秒，加工作目录（约 85G，APFS 克隆）另计。
- **做法**：旧库在线备份成 `migration/source.db` 后只读快照；用各领域自己的迁移建库（与启动时一样），在一个事务里按行类型写入（列名 = 行字段名的蛇形）；附件、资源目录、工作目录 `cp -c -R -p` 克隆，推送私钥原样复制；资源目录里写死的 `~/.muliao/{assets,data,config.toml,muliao.log}` 换成新主目录的（触发器的提示、命令、前置检查也换，#878）；配置由旧配置生成（端口按 `--port`，默认 1081、权限 600、登录签名密钥新生成）；逐表条数（旧条数 / 写入 / 新库里）与没搬的、问题写进 `migration/report.md`。
- **对应关系**：旧消息按段拆成新时间线（`Transcript`）：助手消息 → 回复（签名、OpenAI 输出项等协议数据放进 `Seal`，续聊时原样回传）+ 工具调用行；用户消息里运行上下文那段 → 上下文条目，信件文字（连同紧跟的附件段）→ 来信（来源按旧版标记），工具结果 → 对应调用的结果；失败、被停止、没结束的运行各记一条提示。迁移后每个调用都有结果（旧版缺的补说明）、运行都已结束、收件箱与等待为空，所以新版启动时不续跑、不执行任何东西。
- **不搬**：排队中的信、进行中的后台任务与委派、等批准（还在等的唤醒搬成新版的等待，到点照样唤醒）；看板的「请 agent 表态」动态（新版只记在 `issue_nudge`）。旧版早期状态 todo / waiting 在历史动态里按 in_progress / blocked 记。同一 agent 同一 issue 有多个会话的，只保留最近的为 issue 会话，其余记成用户会话。
- **验收**：迁移后起服务（无 ERROR、续跑 0 个会话），逐个读全部会话详情与时间线、全部 issue、各 agent 后台任务与各列表接口，（`modules/migrate/verify.sh <配置> <端口>`），并在 Claude、OpenAI 两种协议的旧会话里各续聊一轮。

## 打包与部署（#877）
- **位置**（与 Rust 版完全分开）：主目录 `~/.muliao-scala`（`config.toml`、`data/`、`assets/`、`muliao.log`、`launchd.log`、`app/`、`codesign/`、`build/`），launchd 服务 `com.muliao.scala`，端口 1080（#881 起是正式服务；Rust 版的 `com.muliao` 已 bootout 并 disable，`~/.muliao` 留作回退）。脚本都在 `deploy/`，路径可用 `MULIAO_HOME`、`MULIAO_LABEL`、`MULIAO_PORT` 改（演练用），说明在 `deploy/lib.sh` 开头。
- **单文件程序**：`deploy/build.sh <版本> <输出>`：Vite 构建前端 → `sbt app/assembly` 打 fat jar（前端作为 `web/` 资源，清单写版本号与 `Enable-Native-Access`）→ jdeps 算模块、jlink 裁 JRE → JRE 与 jar 打成 tar.gz 编进启动器 `deploy/launcher.c` 的 Mach-O 段。启动器第一次运行把载荷解到程序旁边的 `.muliao-<载荷编号>/`，再以子进程起 java、转发终止信号、带回退出码（不 exec，保持签名身份）。版本号 = 版本目录名 `<时间>-<commit>`，`muliao version` 打印它。
- **签名**：rcodesign 用 `~/.muliao-scala/codesign/` 下固定的自签证书、固定标识 `muliao-scala` 签名；程序路径固定为 `app/current/muliao`（真目录，换版本是整目录复制），「完全磁盘访问」授权一次后换版本不丢。
- **流程**：第一次：`deploy/install.sh` → 在服务外（agent 里经 `ssh -F /dev/null -i ~/.ssh/id_rsa localhost`）跑 `deploy/service.sh install` 写 plist 并加载 → `deploy/selfcheck.sh`。之后每次：`deploy/install.sh [ref]`（在 `build/src` 检出、打包、签名、放进 `app/<时间>-<commit>/` 并复制成 `app/current`，留最近 3 版；正在跑的旧进程的载荷 `.muliao-<旧编号>/` 一并带进新的 `app/current`，重启前它照样能起命令，#884）→ `deploy/activate.sh --restart-only`（重启并等健康）→ `deploy/selfcheck.sh <commit>`。退回上一版：`deploy/activate.sh --previous`；换成指定版本：`deploy/activate.sh <版本目录>`；这两种之后同样跑 `deploy/selfcheck.sh`（activate 只等到能应答，稳不稳由 selfcheck 查）。从本服务自己的命令里跑 activate 会自动脱离到后台，输出在 `deploy.log`。
- **注意**：两个版本的进程名都是 `muliao`，不要用 `pkill -x muliao` 之类按名字杀进程；按 `launchctl print gui/$(id -u)/com.muliao.scala` 的 pid 操作。
