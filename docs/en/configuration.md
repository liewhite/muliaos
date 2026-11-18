# Configuration

[中文](../zh-CN/configuration.md)

muliao reads one TOML file, given on the command line (`muliao serve <file>`; the launchd service uses `~/.muliao-scala/config.toml`).
The program does not read environment variables. A commented example is in [`deploy/config.example.toml`](../../deploy/config.example.toml).

Rules:

- An unknown section, an unknown key (also inside nested tables) or a missing required key stops the server at startup with a message naming it.
- Sections whose keys all have defaults may be left out.
- Paths may start with `~/`, which is expanded to your home directory.
- Changes take effect after a restart (`sh deploy/activate.sh --restart-only`).

The file contains the login password, the vault key and API keys: keep it readable only by you (`chmod 600`).

## `[general]`

| Key | Default | Description |
|---|---|---|
| `data_dir` | required | Data directory: database, agent workspaces, attachments (layout [below](#data-directory)) |
| `assets_dir` | required | Assets directory: memories, skills, MCP servers, secrets, agent configs, roles (layout [below](#assets-directory)) |
| `timezone` | `"Asia/Shanghai"` | Time zone for cron triggers, times without a zone and per-day statistics |

## `[log]`

| Key | Default | Description |
|---|---|---|
| `level` | `"INFO"` | Minimum level: `DEBUG`, `INFO`, `WARN`, `ERROR` |
| `file` | none | Log file; without it the log goes to standard output. |
| `max_size_mb` | `20` | Rotate when the file exceeds this size |
| `keep` | `5` | Rotated files to keep (`file.1` … `file.N`) |

## `[server]`

| Key | Default | Description |
|---|---|---|
| `host` | `"127.0.0.1"` | Listen address |
| `port` | required | Listen port (the deploy scripts assume `1080`; see `MULIAO_PORT` in [Getting started](getting-started.md#running-a-second-instance)) |
| `public_url` | none | External HTTPS address when served behind a reverse proxy; used for links in notifications |
| `password` | required | Web login password. Changing it signs out every device |
| `cookie_secret` | required | Secret used to sign login cookies and agent page links, at least 32 characters |
| `web_dir` | none | Serve the frontend from this directory instead of the copy built into the program (frontend development) |

## `[http]`

Outbound HTTP (model providers, web tools, push, speech). Requests follow the macOS system proxy settings, including the bypass list.

| Key | Default | Description |
|---|---|---|
| `user_agent` | `"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) muliao/0.1"` | User-Agent header |
| `connect_timeout_seconds` | `15` | Connection timeout |
| `request_timeout_seconds` | `300` | Timeout for a whole request including reading the response |

## `[vault]`

| Key | Default | Description |
|---|---|---|
| `key` | required | Fernet key (URL-safe base64 of 32 random bytes) that encrypts secrets locked with the server key. Without the right key stored secrets cannot be decrypted |

## `[models]`

| Key | Default | Description |
|---|---|---|
| `default_model` | required | `"<provider>:<model>"` for new agents; it is written into each new agent's `agent.toml`, so changing it later does not move existing agents |
| `idle_timeout_seconds` | `180` | A response stream that sends nothing (not even a heartbeat) for this long counts as broken |

### `[models.providers.<name>]`

One table per provider; `<name>` is the provider part of model names such as `claude:claude-sonnet-5-5`.

| Key | Default | Description |
|---|---|---|
| `protocol` | required | `"anthropic"` (Messages API) or `"openai-responses"` (Responses API, also compatible services) |
| `base_url` | required | Anthropic: without `/v1` (e.g. `https://api.anthropic.com`). OpenAI: with `/v1` (e.g. `https://api.openai.com/v1`) |
| `api_key` | required | API key |
| `max_concurrency` | `4` | Maximum concurrent requests to this provider, shared by all agents; extra requests wait |
| `max_output_tokens` | none | Output token limit. Anthropic requires one: without this key 128000 is sent, so set a lower value for models with a smaller limit. OpenAI: the model's own default |

muliao has no thinking settings: requests leave thinking to each model's default, so older models work too. Thinking or reasoning
blocks the model returns are not shown; they are stored as-is only to be sent back on later requests, as the providers require.
Text the model writes between tool calls is shown as usual.

## `[sessions]`

| Key | Default | Description |
|---|---|---|
| `compaction_threshold_tokens` | `700000` | Compact the context when the previous request's input exceeded this many tokens |
| `keep_recent_steps` | `6` | Model replies kept verbatim after compaction |
| `tool_output_limit` | `100000` | Maximum characters of one tool result; longer output is truncated with a note |
| `upload_max_bytes` | `52428800` (50 MB) | Maximum size of one uploaded file |

## `[resources]`

| Key | Default | Description |
|---|---|---|
| `memory_limit` | `4000` | Maximum length of one memory, in characters |
| `writable_dirs` | see below | Directories every agent's commands may write (temporary files, caches, toolchains) |

Default `writable_dirs`: `/tmp`, `/private/tmp`, `/private/var/folders`, `~/Library/Caches`, `~/.cache`, `~/.npm`, `~/.cargo`,
`~/.rustup`, `~/go`, `~/.local/share/uv`, `~/.ivy2`, `~/.sbt`, `~/.m2`, `~/.gradle`, `~/.bun`, `~/.pnpm-store`.
Setting the key replaces the whole list. Grant individual agents more with `write:path:<dir>` rules instead.

## `[triggers]`

| Key | Default | Description |
|---|---|---|
| `precheck_timeout_seconds` | `60` | How long a trigger precheck may run; a timeout counts as a failure and is recorded as a fault |

## `[push]`

| Key | Default | Description |
|---|---|---|
| `vapid_subject` | required | Contact for Web Push, starting with `mailto:` or `https://` |
| `vapid_key_file` | `<data_dir>/vapid_private.pem` | VAPID private key (PKCS#8 PEM). Created on first start if missing; subscribed devices keep working as long as the key stays the same |

## `[web]` (optional)

Present: agents get `web_search` and `web_fetch`. Absent: those tools do not exist.

| Key | Default | Description |
|---|---|---|
| `api_key` | required | API key of the search and fetch service (DeepSeek) |
| `base_url` | `"https://api.deepseek.com"` | Service address |

## `[transcription]` (optional)

Present: the message box shows a microphone button for voice input. Uses the Alibaba Cloud Bailian (DashScope) speech recognition API.

| Key | Default | Description |
|---|---|---|
| `api_key` | required | API key |
| `model` | required | Recognition model |
| `base_url` | `"https://dashscope.aliyuncs.com/api/v1"` | Service address |
| `max_bytes` | `10485760` (10 MB) | Maximum size of one recording |
| `vocabulary` | `{}` | Hot words and their weights, e.g. `{ muliao = 5 }` |
| `language_hints` | `[]` | Language hints, e.g. `["zh", "en"]` |

## Data directory

`general.data_dir` is managed by the platform; do not edit it by hand.

| Path | Contents |
|---|---|
| `muliao.db` | SQLite database: agents, sessions, runs, issues, triggers, notifications, history, usage |
| `workspaces/<agent id>/` | each agent's working directory (its commands run here; background job logs in `jobs/`, pages in `site/`) |
| `files/` | uploaded attachments, stored by content hash |
| `resources/` | the last valid `access.toml` (used when the current one is broken) |
| `vapid_private.pem` | Web Push private key (unless `push.vapid_key_file` points elsewhere) |
| `.lock` | single-instance lock: a second process on the same data directory exits |

The configuration file, `backups/` in the data directory and the push private key are platform-private: agents without the `admin`
role can neither read nor write them.

## Assets directory

`general.assets_dir` holds everything you and the agents edit. Files are read again when they change (polled every 2 seconds) and
take effect on the next run. A file that cannot be parsed is skipped and the agents it concerns are told at the start of each run.

```
platform/access.toml              roles and per-agent rules
platform/memory/global.md         global memory (always present, read by every agent)
platform/memory/<name>.md         shared memories, distributed by rules
platform/skills/<name>/SKILL.md   platform skills (other files in the directory are attachments)
platform/mcp/<name>.json          platform MCP servers
platform/secrets/<KEY>.toml       platform secrets
<agent>/agent.toml                an agent's settings
<agent>/memory.md                 its private memory
<agent>/skills/<name>/SKILL.md    its skills
<agent>/mcp/<name>.json           its MCP servers
<agent>/secrets/<KEY>.toml        its secrets
```

`<agent>` is the agent's name; a resource belongs to the directory it is in. Files and directories starting with `.` are ignored.
Names of skills, MCP servers and shared memories use lowercase letters, digits and `-` (starting with a letter or digit, at most 64 characters).

### `agent.toml`

```toml
description = "Keeps an eye on this Mac: disk space, backups, long-running jobs."
model = "claude:claude-sonnet-5-5"
group = "infra"          # optional
```

The name is the directory name; the id, archive state and permissions are not in this file.

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

Agents are keyed by id (the comment with the name is written by the platform). When the platform saves this file it rewrites it in a
fixed order and hand-written comments are lost. If the file becomes invalid, the last valid version stays in effect and the platform
refuses to change it until it is fixed. Rule syntax is explained in [Concepts](concepts.md#roles-and-permissions).

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

`description` is required and is what the agent sees in its skill index; `name`, if given, must equal the directory name;
`approval` lists scripts (relative to the skill directory) that need your approval every time `skill_run` runs them.

### MCP server (`mcp/<name>.json`)

```json
{"transport": "stdio", "command": "npx", "args": ["-y", "@modelcontextprotocol/server-github"],
 "env": {"GITHUB_TOKEN": "${vault:GITHUB_TOKEN}"}, "enabled": true}
```

```json
{"transport": "http", "url": "https://mcp.example.com/mcp", "headers": {"Authorization": "Bearer ${vault:EXAMPLE_TOKEN}"}}
```

`${vault:KEY}` in `args`, `env`, `url` and `headers` is replaced by the secret's value (the agent must be allowed to use it).
Local servers run in the agent's write sandbox.

### Secrets (`secrets/<KEY>.toml`)

Set secrets in the UI (AI 资产 → Secret) or let an agent store one you give it with `vault_set`; the file holds only ciphertext.
Keys start with an uppercase letter and contain uppercase letters, digits and underscores.
