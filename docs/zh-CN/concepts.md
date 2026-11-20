# 核心概念

[English](../en/concepts.md)

本文讲 muliao 由哪些部分组成、它们怎么配合。安装见 [快速上手](getting-started.md)，配置项见 [配置](configuration.md)。

## Agent

agent 是长期在岗的「员工」，有：

- **名字**（唯一、不分大小写、可改；`user` 和 `platform` 保留）和一个不变的内部编号；
- **职责描述**（它管什么，别的 agent 据此决定把活交给谁）；
- **模型**（`<服务商>:<模型>`）；
- 可选的**分组**（列表里归在一起）、**角色**和额外的允许/禁止**规则**（见 [角色与权限](#角色与权限)）；
- 私有**记忆**、自己的 **skill**、MCP server 和 secret，以及跑命令用的**工作目录**。

agent 可以归档（数据保留、不接新活）或删除（不可撤销，忙时拒绝）。可以把一个 agent 设为默认，新会话默认选它。

## 会话与运行

**会话**是属于某个 agent 的一段对话。会话由你新建、触发器开、issue 开，或由别的 agent 委派开（叫 *thread*）。
你可以往任何会话里发消息；其他来源只能续写自己开的会话。

会话收到消息就开始一次**运行**：平台组装上下文、请求模型、执行它返回的工具调用，循环到模型不再调用工具为止。

- **同一会话同时只有一次运行**；不同会话、不同 agent 并发。每个模型服务商有并发上限，全部 agent 共享。
- **每一步都落库。** 每次模型回复、每批工具结果都先写进数据库再往下走。重启或暂时失败后从上一次保存的步骤接着跑。
- **被打断的工具调用不重放。** 命令跑到一半服务停了，agent 会收到「被中断、没重新执行、可能已生效」的结果，有副作用的事它先核实再决定。
- **暂时性失败**（服务商不可用、限流、超时）指数退避重试约 30 分钟，仍失败就算最终失败：给你发通知，会话页出现「重试」按钮。
- **插话。** 运行中你发的消息在它下一次请求模型前一起交给它。
- **停止**会话：已产生的内容都保留，它开出的 thread 一并停下，后台任务被结束，在等的东西被放弃。
- **上下文压缩。** 上一次请求的输入超过配置的阈值时，把较早的消息总结掉，之后用「摘要 + 最近几步原文」接着跑；原始消息在界面上照常显示。

**回答交给谁**取决于来源：你在等的，回答完推送通知给你；委派来的，交回发起的 agent；触发器开的不通知你，除非 agent 自己调 `notify_user`。

### 工作状态

每个会话处于*运行中*、*等待中*（等后台任务、委派的 thread、定时唤醒、你的回答或批准）或*空闲*。全系统（会话列表、issue 看板、触发器防重叠）用同一个判断。

## 工具

agent 只拿到权限允许的工具。工具列表只由权限和配置的 MCP server 决定，不随会话在做什么而增减（让提示缓存一直命中）；需要特定状态的工具在调用时检查，并说明为什么不能用。

| 方面 | 工具 |
|---|---|
| 文件 | `read_file`、`write_file`、`edit_file`、`share_file` |
| 命令 | `run_shell`（前台带超时，或后台任务）、`job_status`、`job_kill`、`skill_run` |
| 你 | `ask_user`（1–4 道选择题，只在你在等时可用）、`notify_user` |
| 别的 agent | `delegate_task`、`ask_agent`、`list_threads`、`stop_thread`、`list_agents`、`get_agent`、`create_agent`\*、`delete_agent`\* |
| 时间 | `wake_me`（每个会话至多一个待唤醒） |
| issue 看板 | `issue_list`、`issue_show`、`issue_create`、`issue_update`、`issue_comment`、`issue_stop` |
| 触发器 | `trigger_kinds`、`trigger_list`、`trigger_create`、`trigger_update`、`trigger_run`、`trigger_delete` |
| 历史 | `history_append`、`history_search`、`activity_overview`、`session_read` |
| secret | `vault_set`、`ssh_certificate` |
| 网页（配了才有） | `web_search`、`web_fetch` |
| MCP | agent 能用的、已启用的 MCP server 的工具，名字带 server 前缀 |

\* 管理类工具，只给有 `admin` 角色的 agent。

后台任务（`run_shell` 带 `background`）的输出写进 agent 工作目录的 `jobs/<编号>.log`；结束时退出码和最后几行作为事件回到会话。后台任务可以配心跳检查。

## 委派

`delegate_task` 把活交给某个 agent 的具名 **thread**（同一 agent、同名 thread 就是续聊）。默认回答直接推给你；带 `report_back` 时回答作为事件交回发起的会话。
`ask_agent` 等对方回答，作为工具结果返回。agent 也可以委派给自己，并行做旁支任务。

## issue 看板

issue 有标题、Markdown 描述、状态（`backlog`、`in_progress`、`in_review`、`blocked`、`done`、`cancelled`）、优先级、**owner**（负责这个项目的 agent，必填）、
**指派人**（某个 agent、你或空）和上游**依赖**（成环会被拒绝）。每次改动、每条评论都追加到 issue 的时间线上。

- **投递。** issue 处于 `in_progress`、指派给某 agent、上游都已结束时，每条新动态投进这个 agent 专门处理这个 issue 的会话（每个 issue 和 agent 一个）。
  一串互相依赖的 issue 可以一次建好，前一步做完下一步自动开始。
- **停滞。** 该有人做、但它的会话空闲的 issue 标为停滞，并请 agent 表态一次：做完了、交给你，还是在等。
- **审核。** agent 做完设 `in_review` 并指派给你。「通过」→ `done`；「打回」写理由 → 回到 agent 手里。推送通知里也能直接操作。
- `issue_stop` 停掉正在处理这个 issue 的会话，可附一句话让它收尾或改方向。只改状态不会停下工作。

## 触发器

触发器让 agent 在没人发消息时自己醒来。每次触发新开一个会话，交给它触发器的提示词（有事件时附上事件）。

| 种类 | 什么时候触发 |
|---|---|
| `schedule` | 按 cron 表达式，按配置的时区 |
| `once` | 到某个时刻触发一次，之后移到「已结束」列表 |
| `stream` | 平台托管一条常驻命令（退出后退避重启），它每输出一行算一个事件（短时间内的多行合成一次） |

- **前置检查。** 触发前先跑一条便宜的命令：有输出才触发（输出作为事件），或输出变了才触发（事件是 diff）。检查失败、常驻命令反复崩溃记为故障，并通知所属 agent 去修。
- **不重叠。** 上一次开的会话还没做完就跳过本次并计数。停机期间错过的定时在启动时补一次；停用期间的不补。

## 记忆与 skill

agent 学到的东西存在两种文件里：

- **记忆**：有长度上限的 Markdown，每次运行全文放进上下文。有人人都读的全局记忆、按角色分发的共享记忆，以及每个 agent 的私有记忆。
- **skill**：一个目录，里面有 `SKILL.md`（frontmatter 写 `name`、`description`，可选 `approval`：每次执行都要你批准的脚本列表），加任意附件和脚本。
  上下文里只放 skill 索引（名字和描述），任务对得上描述时 agent 再去读 `SKILL.md`。

两者都是资源目录里的普通文件，你、你的编辑器和 agent 都能改，下次运行生效。

## 资源、角色与权限

可编辑的东西都在**资源目录**里（结构见 [配置](configuration.md#资源目录)）：记忆、skill、MCP server、secret、agent 配置和权限规则。
文件在谁的目录下（`platform/` 或某个 agent 的目录）就归谁；挪文件就是转移归属。

### 角色与权限

规则写成 `动作:种类:名字通配`：

- 动作是 `use`（用）或 `write`（改），种类是 `tool`、`skill`、`mcp`、`secret`、`memory`，名字可带通配，如 `use:skill:deploy-*`、`use:secret:GITHUB_*`、`write:memory:team`。动作和种类也可以写 `*`。
- `write:path:<目录>` 让 agent 的命令和文件工具能写工作目录以外的某个目录。
- 规则分**允许**和**禁止**两组，禁止优先；但禁止拦不住 agent 自己名下的资源，也拦不住读全局记忆。

**角色**是一组具名规则。内置两个：`base`（每个 agent 都有：全部非管理类工具、全局记忆，加上你给 `base` 角色写的规则）和 `admin`（全部工具、能改全部资源）。
agent 最终的权限 = `base` + 它的各个角色 + 它自己的额外规则；agent 页上能看到结果，以及每一项来自哪条规则。主人对自己的资源总能用、能改。

权限用来防误操作、让每个 agent 的上下文聚焦，**不是**安全边界。

### 写沙箱

agent 的命令（shell、skill 脚本、触发器的前置检查和常驻命令、本地 MCP server）跑在 macOS `sandbox-exec` 里：只能写自己的工作目录、常用的临时/缓存/工具链目录、
被授权的 `write:path` 目录，以及它有权改的资源文件。读和网络不受限制。平台私有文件（配置、备份、推送私钥）非 admin 的 agent 读写都不行。
被沙箱拦下时（写范围外，或读平台私有文件），agent 看到原本的系统报错，外加一句被什么拦、找谁授权。

## Secret

secret 加密存在资源目录里，值不会出现在会话记录、日志和界面上。agent 用 secret 的方式：在 `run_shell` / `skill_run` 里点名（`secrets=[KEY]`，作为环境变量注入，输出里自动打码），
或在 MCP 配置里写 `${vault:KEY}`。没有能读出 secret 明文的工具。

每个 secret 有三个维度，各选一项：

| 维度 | 可选 |
|---|---|
| 谁能解开 | **服务端**：平台用配置里的 `[vault] key` 解；**passkey**：只有你的浏览器用你的 passkey 解，每次使用都等你批准（一次、一段时间，或拒绝） |
| 内容 | **口令**（任意文本）；**密钥对**（ed25519 私钥，平台能用它签名，比如经 `ssh_certificate` 签 SSH 短期证书） |
| 能否取出明文 | **任意命令**；**只给脚本**（skill 脚本和 MCP server 能用，agent 自己写的命令注入不了）；**只能签名**（只有密钥对能选，取不出明文） |

签名在解开它的一方做：服务端的在平台里签（不用你批准；agent 的工具取不出明文，但这只防误用：能读到配置里主密钥的程序仍能解开，要真正隔离用 passkey），passkey 的在你的浏览器里、在你看过内容并批准后签。passkey 相关功能要 HTTPS 的域名地址。

## 通知

这些情况会产生通知：你在等的回答好了、agent 向你提问或等你批准、运行最终失败、agent 调 `notify_user`、issue 交给你、触发器被挡住或出故障。
通知进收件箱，并经 Web Push 发到每台订阅过的设备。

## 历史与用量

- **工作历史**：agent 追加记录自己做了什么（日期、标题、摘要、引用、重要程度）。只能追加、不能改删，删 agent 也保留。`history_search` 按关键字相关度检索；`activity_overview` 汇总某一天。
- **用量**：按 agent、模型、天统计 token，含提示缓存的读写量。

## agent 页面

agent 在工作目录写 `site/<页面>/index.html`（及附件）就发布了一个网页。界面里列出全部页面并内嵌打开，经签名链接在隔离的源上提供：脚本能跑，但拿不到你的登录 cookie，也调不了平台接口。
