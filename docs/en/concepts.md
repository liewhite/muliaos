# Concepts

[中文](../zh-CN/concepts.md)

This page explains the building blocks of muliao and how they fit together. For setup see [Getting started](getting-started.md);
for every configuration key see [Configuration](configuration.md).

## Agents

An agent is a long-lived worker with:

- a **name** (unique, case-insensitive, renameable; `user` and `platform` are reserved) and a permanent internal id,
- a **description** of what it is responsible for (other agents read it to decide whom to delegate to),
- a **model** (`<provider>:<model>`),
- an optional **group** for the agent list, **roles** and extra allow/deny **rules** (see [Roles and permissions](#roles-and-permissions)),
- a private **memory**, its own **skills**, MCP servers and secrets, and a **workspace** directory where its commands run.

Agents can be archived (kept but not accepting new work) or deleted (irreversible; refused while the agent is busy).
One agent can be marked as the default for new conversations.

## Sessions and runs

A **session** is a conversation that belongs to one agent. Sessions are opened by you, by a trigger, by an issue,
or by another agent's delegation (a *thread*). You can write into any session; other sources only continue the sessions they opened.

When a session receives a message it starts a **run**: the platform builds the context, asks the model, executes the
tool calls it returns, and repeats until the model stops calling tools.

- **One run per session at a time**; different sessions and agents run concurrently. Each model provider has a
  concurrency limit shared by all agents.
- **Every step is saved.** Each model reply and each batch of tool results is written to the database before the run continues.
  After a restart or a transient failure the run resumes from the last saved step.
- **Interrupted tool calls are not replayed.** If the service stops while a command is running, the agent receives a result saying
  the call was interrupted, was not re-executed and may have taken effect, so it checks before repeating anything with side effects.
- **Transient failures** (provider unavailable, rate limits, timeouts) are retried with exponential backoff for about 30 minutes,
  then the run fails visibly and you get a notification and a *Retry* button.
- **Interjecting.** Messages you send while a run is in progress are given to the model before its next request.
- **Stopping** a session keeps everything produced so far, stops the threads it opened, kills its background jobs and drops what it was waiting for.
- **Context compaction.** When the previous request exceeded the configured threshold, older messages are summarised and the run
  continues with the summary plus the most recent steps. The original messages stay visible in the UI.

**Who receives the answer** depends on the source: if you are waiting, you get a push notification when the answer is ready;
a delegated task answers the delegating agent; sessions opened by triggers do not notify you unless the agent calls `notify_user`.

### Working states

Every session is *running*, *waiting* (for a background job, a delegated thread, a timed wake-up, your answer or your approval)
or *idle*. The same definition is used everywhere: the session list, the issue board and trigger overlap protection.

## Tools

Agents only get the tools their permissions allow. The list depends only on permissions and configured MCP servers, not on what the
session is doing (this keeps the prompt cache warm); tools that need a particular state check it when called and explain why they cannot run.

| Area | Tools |
|---|---|
| Files | `read_file`, `write_file`, `edit_file`, `share_file` |
| Commands | `run_shell` (foreground with timeout, or background job), `job_status`, `job_kill`, `skill_run` |
| You | `ask_user` (1–4 multiple-choice questions, only while you are waiting), `notify_user` |
| Other agents | `delegate_task`, `ask_agent`, `list_threads`, `stop_thread`, `list_agents`, `get_agent`, `create_agent`\*, `delete_agent`\* |
| Time | `wake_me` (at most one pending wake-up per session) |
| Issue board | `issue_list`, `issue_show`, `issue_create`, `issue_update`, `issue_comment`, `issue_stop` |
| Triggers | `trigger_kinds`, `trigger_list`, `trigger_create`, `trigger_update`, `trigger_run`, `trigger_delete` |
| History | `history_append`, `history_search`, `activity_overview`, `session_read` |
| Secrets | `vault_set`, `ssh_certificate` |
| Web (if configured) | `web_search`, `web_fetch` |
| MCP | tools of every enabled MCP server the agent may use, prefixed with the server name |

\* admin tools, only for agents with the `admin` role.

Background jobs (`run_shell` with `background`) write their output to `jobs/<id>.log` in the agent's workspace; when they finish,
the exit code and the last lines come back to the session as an event. Jobs can send periodic heartbeat checks.

## Delegation

`delegate_task` hands work to an agent in a named **thread** (the same agent and thread name continue the same conversation).
By default the answer goes straight to you; with `report_back` it comes back to the delegating session as an event.
`ask_agent` waits for the answer and returns it as the tool result. An agent can delegate to itself to run a side task in parallel.

## Issue board

Issues have a title, a Markdown description, a status (`backlog`, `in_progress`, `in_review`, `blocked`, `done`, `cancelled`),
a priority, an **owner** (the agent responsible for the project; required), an **assignee** (an agent, you, or nobody) and
upstream **dependencies** (cycles are rejected). Every change and comment is appended to the issue's timeline.

- **Delivery.** While an issue is `in_progress`, assigned to an agent and all its dependencies are finished, every new event is delivered
  to that agent's dedicated session for the issue (one per issue and agent). A chain of dependent issues can be created in one go
  and each step starts when the previous one finishes.
- **Stalled.** An issue that should be worked on but whose session is idle is shown as stalled, and the agent is asked once to state
  whether it is done, handing over to you, or waiting.
- **Review.** When an agent finishes it sets `in_review` and assigns you. *Approve* marks it `done`; *Send back* with a reason
  returns it to the agent. Both are available in the push notification.
- `issue_stop` stops the session working on an issue and optionally gives it a new direction. Changing the status alone does not stop work.

## Triggers

A trigger wakes an agent without anyone sending a message. Each firing opens a new session with the trigger's prompt
(plus the event, if any).

| Kind | Fires |
|---|---|
| `schedule` | on a cron expression, in the configured time zone |
| `once` | at a given time, then moves to the finished list |
| `stream` | for each line printed by a long-running command the platform keeps alive (restarted with backoff, events debounced) |

- **Precheck.** A cheap command run before firing: fire only if it prints something (the output becomes the event), or only when
  its output changes (the event is a diff). Failing prechecks and crashing stream commands are recorded as faults and the owning agent is told to fix them.
- **No overlap.** If the session opened last time is still busy, the firing is skipped and counted. Missed schedules during downtime
  fire once on startup; disabled triggers do not catch up.

## Memory and skills

Agents keep what they learn in two kinds of files:

- **Memory**: Markdown with a size limit, included in full in every run. There is a global memory everyone reads,
  shared memories distributed by role, and each agent's private memory.
- **Skills**: a directory with a `SKILL.md` (front matter with `name`, `description` and optionally `approval`, a list of scripts that
  need your approval each time) plus any attachments and scripts. Only the skill index (name and description) is in the context;
  the agent reads `SKILL.md` when the task matches the description.

Both are plain files in the assets directory, so you, your editor and the agents can change them; changes apply on the next run.

## Assets, roles and permissions

Everything editable lives in the **assets directory** (layout in [Configuration](configuration.md#assets-directory)):
memories, skills, MCP servers, secrets, agent configs and the access rules. A file belongs to whoever's directory it is in
(`platform/` or an agent's directory); moving a file transfers ownership.

### Roles and permissions

Rules have the form `action:kind:pattern`:

- `action` is `use` or `write`, `kind` is `tool`, `skill`, `mcp`, `secret` or `memory`, and `pattern` is a name with wildcards,
  e.g. `use:skill:deploy-*`, `use:secret:GITHUB_*`, `write:memory:team`. `action` and `kind` may also be `*`.
- `write:path:<dir>` lets the agent's commands and file tools write to a directory outside its workspace.
- Rules are grouped in **allow** and **deny** lists; deny wins, except that it never blocks an agent's own resources or reading the global memory.

A **role** is a named set of rules. Two roles are built in: `base` (every agent: all non-admin tools, the global memory, plus any rules
you add to the `base` role) and `admin` (all tools, write access to everything). An agent's effective permissions are `base` plus
its roles plus its own extra rules; the agent page shows the result and which rule granted each item. Owners can always use and
edit their own resources.

Permissions prevent mistakes and keep each agent's context focused. They are **not** a security boundary.

### Write sandbox

Agent commands (shell, skill scripts, trigger prechecks and stream commands, local MCP servers) run inside a macOS `sandbox-exec`
profile that only allows writing to the agent's workspace, common temp/cache/toolchain directories, granted `write:path` directories
and asset files the agent may edit. Reading and network access are not restricted. Platform-private files (config, backups,
push keys) are off limits to non-admin agents. When the sandbox blocks a command (a write outside the allowed paths, or reading a platform-private file) the agent sees the original error plus a note on what blocked it and who to ask.

## Secrets

Secrets are stored encrypted in the assets directory and never appear in session records, logs or the UI. An agent uses a secret by
naming it in `run_shell` / `skill_run` (`secrets=[KEY]`, injected as an environment variable and masked in the output) or as
`${vault:KEY}` in an MCP configuration. There is no tool that reveals a secret's value.

Each secret has three dimensions, one choice each:

| Dimension | Choices |
|---|---|
| Who can decrypt | **Server**: the platform, with the `[vault] key` from the config; **Passkey**: only your browser, with your passkey; each use waits for your approval (once, for a period, or deny) |
| Content | **Password** (any text); **Key pair** (an ed25519 private key the platform can sign with, e.g. short-lived SSH certificates via `ssh_certificate`) |
| Plaintext | **Any command**; **Scripts only** (usable by skill scripts and MCP servers, but not injectable into commands the agent writes itself); **Sign only** (key pairs only: the value can never be revealed) |

Signing happens where the secret can be decrypted: server-held keys sign inside the platform (agents never see the private key and no approval is needed),
passkey-held keys sign inside your browser after you approve the content. Passkey features require HTTPS on a domain name.

## Notifications

Notifications are created when an answer you were waiting for is ready, when an agent asks you something or needs approval,
when a run fails, when an agent calls `notify_user`, when an issue is handed to you, and when a trigger is blocked or faulty.
They appear in the inbox and are delivered by Web Push to every subscribed device.

## History and usage

- **Work history**: agents append entries (date, title, summary, references, importance) describing what they did. Entries cannot be
  edited or deleted and survive agent deletion. `history_search` ranks keyword matches; `activity_overview` summarises a day.
- **Usage**: token usage per agent, model and day, including prompt-cache reads and writes.

## Agent pages

An agent can publish a web page by writing `site/<page>/index.html` (and assets) in its workspace. Pages are listed in the UI and open
inside it, served from signed URLs in an isolated origin: scripts run, but they cannot see your login cookie or call the platform API.
