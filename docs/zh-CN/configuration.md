# 配置

[English](../en/configuration.md)

muliao 只读一个 TOML 文件，路径在命令行给出（`muliao serve <文件>`；launchd 服务用 `~/.muliao-scala/config.toml`）。程序不读环境变量。
带注释的示例在 [`deploy/config.example.toml`](../../deploy/config.example.toml)。

规则：

- 出现未知的段、未知的键（嵌套表里也算）或缺必填项，启动时直接报错并指出是哪个。
- 所有键都有默认值的段可以不写。
- 路径可以 `~/` 开头，展开成你的主目录。
- 改完重启生效（`sh deploy/activate.sh --restart-only`）。

文件里有登录密码、vault 主密钥和各种 API key：只让自己能读（`chmod 600`）。

## `[general]`

| 键 | 默认 | 说明 |
|---|---|---|
| `data_dir` | 必填 | 数据目录：数据库、agent 工作目录、附件（结构见[下文](#数据目录)） |
| `assets_dir` | 必填 | 资源目录：记忆、skill、MCP、secret、agent 配置、角色（结构见[下文](#资源目录)） |
| `timezone` | `"Asia/Shanghai"` | cron 触发器、不带时区的时刻、按天统计用的时区 |

## `[log]`

| 键 | 默认 | 说明 |
|---|---|---|
| `level` | `"INFO"` | 最低级别：`DEBUG`、`INFO`、`WARN`、`ERROR` |
| `file` | 无 | 日志文件；不写就打到标准输出 |
| `max_size_mb` | `20` | 单个文件超过这么大就轮转 |
| `keep` | `5` | 轮转保留的旧文件份数（`file.1` … `file.N`） |

## `[server]`

| 键 | 默认 | 说明 |
|---|---|---|
| `host` | `"127.0.0.1"` | 监听地址 |
| `port` | 必填 | 监听端口（部署脚本默认 `1080`，见 [快速上手](getting-started.md#再跑一个实例) 的 `MULIAO_PORT`） |
| `public_url` | 无 | 经反向代理访问时的外部 HTTPS 地址；通知里的链接用它 |
| `password` | 必填 | 网页登录密码。改了它所有设备都要重新登录 |
| `cookie_secret` | 必填 | 签登录 cookie 和 agent 页面链接的密钥，至少 32 个字符 |
| `web_dir` | 无 | 从这个目录提供前端，而不是用打包在程序里的（开发前端时用） |

## `[http]`

出站 HTTP（模型服务、网页工具、推送、语音）。请求遵循 macOS 系统代理设置（含例外列表）。

| 键 | 默认 | 说明 |
|---|---|---|
| `user_agent` | `"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) muliao/0.1"` | User-Agent 请求头 |
| `connect_timeout_seconds` | `15` | 建连超时 |
| `request_timeout_seconds` | `300` | 整个请求（含读完响应）的超时 |

## `[vault]`

| 键 | 默认 | 说明 |
|---|---|---|
| `key` | 必填 | Fernet 密钥（32 个随机字节的 URL-safe base64），加密用服务端密钥锁的 secret。密钥不对，存过的 secret 都解不开 |

## `[models]`

| 键 | 默认 | 说明 |
|---|---|---|
| `default_model` | 必填 | 新 agent 用的 `"<服务商>:<模型>"`；建 agent 时写进它的 `agent.toml`，之后改这里不影响已有的 agent |
| `idle_timeout_seconds` | `180` | 回复流这么久没有任何数据（含心跳）就算断了 |

### `[models.providers.<名字>]`

每个服务商一张表；`<名字>` 就是模型写法 `claude:claude-sonnet-5-5` 里冒号前那部分。

| 键 | 默认 | 说明 |
|---|---|---|
| `protocol` | 必填 | `"anthropic"`（Messages API）或 `"openai-responses"`（Responses API，也适用于兼容服务） |
| `base_url` | 必填 | Anthropic 不带 `/v1`（如 `https://api.anthropic.com`）；OpenAI 带 `/v1`（如 `https://api.openai.com/v1`） |
| `api_key` | 必填 | API key |
| `max_concurrency` | `4` | 对这个服务商同时最多几个请求，全部 agent 共享，超出的排队 |
| `max_output_tokens` | 无 | 回复最多多少 token。Anthropic 必须给：不配时发 128000，模型上限更小就配小一点；OpenAI 不配时用模型自己的默认 |

走 `anthropic` 协议时 muliao 请求 adaptive thinking 并要回思考摘要（思考档位 `off` 时关闭），所以模型必须支持 adaptive thinking。

## `[sessions]`

| 键 | 默认 | 说明 |
|---|---|---|
| `compaction_threshold_tokens` | `700000` | 上一次请求的输入超过这么多 token 就压缩上下文 |
| `keep_recent_steps` | `6` | 压缩后保留原文的最近几步（模型回复） |
| `tool_output_limit` | `100000` | 一次工具结果最多多少字符，超出截断并说明 |
| `upload_max_bytes` | `52428800`（50 MB） | 上传一个文件最多多少字节 |

## `[resources]`

| 键 | 默认 | 说明 |
|---|---|---|
| `memory_limit` | `4000` | 每块记忆的长度上限（字符数） |
| `writable_dirs` | 见下 | 每个 agent 的命令都能写的目录（临时文件、缓存、工具链） |

`writable_dirs` 默认：`/tmp`、`/private/tmp`、`/private/var/folders`、`~/Library/Caches`、`~/.cache`、`~/.npm`、`~/.cargo`、
`~/.rustup`、`~/go`、`~/.local/share/uv`、`~/.ivy2`、`~/.sbt`、`~/.m2`、`~/.gradle`、`~/.bun`、`~/.pnpm-store`。
写了这个键就整份替换。只给个别 agent 多开目录用 `write:path:<目录>` 规则。

## `[triggers]`

| 键 | 默认 | 说明 |
|---|---|---|
| `precheck_timeout_seconds` | `60` | 触发器前置检查最多跑多久；超时算失败，记为故障 |

## `[push]`

| 键 | 默认 | 说明 |
|---|---|---|
| `vapid_subject` | 必填 | Web Push 的联系方式，以 `mailto:` 或 `https://` 开头 |
| `vapid_key_file` | `<data_dir>/vapid_private.pem` | VAPID 私钥（PKCS#8 PEM）。没有就在第一次启动时生成；私钥不变，已订阅的设备就一直能收到 |

## `[web]`（可选）

有这一段 agent 才有 `web_search` 和 `web_fetch`，没有就没有这两个工具。

| 键 | 默认 | 说明 |
|---|---|---|
| `api_key` | 必填 | 搜索与抓取服务（DeepSeek）的 API key |
| `base_url` | `"https://api.deepseek.com"` | 服务地址 |

## `[transcription]`（可选）

有这一段，输入框才显示语音输入的麦克风按钮。用阿里云百炼（DashScope）的录音识别接口。

| 键 | 默认 | 说明 |
|---|---|---|
| `api_key` | 必填 | API key |
| `model` | 必填 | 识别模型 |
| `base_url` | `"https://dashscope.aliyuncs.com/api/v1"` | 服务地址 |
| `max_bytes` | `10485760`（10 MB） | 一段录音最大字节数 |
| `vocabulary` | `{}` | 热词及权重，如 `{ muliao = 5 }` |
| `language_hints` | `[]` | 语种提示，如 `["zh", "en"]` |

## 数据目录

`general.data_dir` 由平台管理，不要手改。

| 路径 | 内容 |
|---|---|
| `muliao.db` | SQLite 数据库：agent、会话、运行、issue、触发器、通知、历史、用量 |
| `workspaces/<agent 编号>/` | 每个 agent 的工作目录（命令在这里跑；后台任务日志在 `jobs/`，页面在 `site/`） |
| `files/` | 上传的附件，按内容哈希存 |
| `resources/` | 上一份有效的 `access.toml`（当前那份写坏时用它） |
| `vapid_private.pem` | Web Push 私钥（`push.vapid_key_file` 没指到别处时） |
| `.lock` | 单实例锁：同一数据目录的第二个进程会退出 |

配置文件、数据目录下的 `backups/` 和推送私钥是平台私有文件：没有 `admin` 角色的 agent 读写都不行。

## 资源目录

`general.assets_dir` 放你和 agent 要改的全部东西。文件变了会被重新读入（每 2 秒查一次），下次运行生效。解析不了的文件会被跳过，
相关的 agent 每次运行开头都会被告知。

```
platform/access.toml              角色与各 agent 的规则
platform/memory/global.md         全局记忆（总是存在，每个 agent 都读）
platform/memory/<名>.md           共享记忆，按规则分发
platform/skills/<名>/SKILL.md     平台的 skill（目录里其他文件是附件）
platform/mcp/<名>.json            平台的 MCP server
platform/secrets/<KEY>.toml       平台的 secret
<agent>/agent.toml                agent 的设置
<agent>/memory.md                 它的私有记忆
<agent>/skills/<名>/SKILL.md      它的 skill
<agent>/mcp/<名>.json             它的 MCP server
<agent>/secrets/<KEY>.toml        它的 secret
```

`<agent>` 是 agent 的名字；资源在谁的目录下就归谁。`.` 开头的文件和目录不读。

### `agent.toml`

```toml
description = "Keeps an eye on this Mac: disk space, backups, long-running jobs."
model = "claude:claude-sonnet-5-5"
thinking = "default"     # default、off、low、medium、high、xhigh
group = "infra"          # 可选
```

名字就是目录名；编号、归档状态和权限不在这个文件里。

### `platform/access.toml`

```toml
[roles.deployer]
description = "Can deploy the web app"
allow = ["use:skill:webapp-deploy", "use:secret:DEPLOY_*", "write:path:~/projects/webapp"]
deny = []

[agents.c5c3979a0b2b] # ops
roles = ["deployer"]
allow = ["use:mcp:github"]
deny = ["use:tool:delete_agent"]
```

agent 按编号写（后面带名字的注释由平台写）。平台保存这个文件时按固定顺序重写，手写的注释会丢。文件写坏时沿用上一份有效的，
平台拒绝再改它直到修好。规则写法见 [核心概念](concepts.md#角色与权限)。

### `SKILL.md`

```markdown
---
name: webapp-deploy
description: Deploy the web app to production and verify it. Use when the user says deploy, release or ship.
approval: [scripts/deploy.sh]
---

# Deploying the web app
1. ...
```

`description` 必填，agent 在 skill 索引里看到的就是它；写了 `name` 就必须和目录名相同；`approval` 列出每次经 `skill_run` 执行都要你批准的脚本（相对 skill 目录）。

### MCP server（`mcp/<名>.json`）

```json
{"transport": "stdio", "command": "npx", "args": ["-y", "@modelcontextprotocol/server-github"],
 "env": {"GITHUB_TOKEN": "${vault:GITHUB_TOKEN}"}, "enabled": true}
```

```json
{"transport": "http", "url": "https://mcp.example.com/mcp", "headers": {"Authorization": "Bearer ${vault:EXAMPLE_TOKEN}"}}
```

`args`、`env`、`url`、`headers` 里的 `${vault:KEY}` 换成 secret 的值（agent 要有权用它）。本地 server 跑在 agent 的写沙箱里。

### Secret（`secrets/<KEY>.toml`）

在界面（AI 资产 → Secret）里设，或把值交给 agent 让它用 `vault_set` 存；文件里只有密文。KEY 以大写字母开头，由大写字母、数字和下划线组成。
