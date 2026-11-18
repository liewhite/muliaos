# Architecture

[中文](../zh-CN/architecture.md)

An overview for people reading or changing the code. The detailed design notes, kept up to date with every change, are in Chinese:
[docs/stack.md](../stack.md) (stack, module contracts, conventions) and [docs/web.md](../web.md) (frontend);
the functional specification is [docs/requirements.md](../requirements.md).

## Shape of the system

One JVM process on one Mac serves the web UI and the HTTP API, runs every agent and owns one SQLite database.

```
browser / iPhone PWA ──HTTP + SSE──▶ muliao (single process)
                                      ├─ sessions: agent loop, one run per session
                                      ├─ tools ─▶ commands in a macOS write sandbox, files, other agents, board, triggers
                                      ├─ models ─▶ Anthropic Messages / OpenAI Responses, MCP servers, web, speech
                                      ├─ SQLite (data_dir/muliao.db)
                                      └─ assets directory (plain files, polled for changes)
```

Guiding choices:

- **Single user, single machine.** No accounts, no distributed state. Edge-case comfort is traded for a simpler architecture,
  but never at the cost of unrecoverable errors (lost data, repeated side effects, permissions out of control).
- **The database is the source of truth.** Live updates in the UI (server-sent events) only speed things up; reloading always shows the real state.
- **Persist every step.** A run saves each model reply and each batch of tool results before continuing, so restarts resume instead of repeating.
- **Plain files for everything a person edits.** Memories, skills, MCP configs, secrets (encrypted), agent settings and roles are files
  in the assets directory, not database rows.
- **One configuration file.** No environment variables; unknown keys are errors.

## Stack

| Concern | Choice |
|---|---|
| Language | Scala 3 on JDK 25, built with sbt |
| Effects and concurrency | ZIO 2 (layers, scopes, queues, hubs, semaphores, retry schedules), ZStream for streaming |
| HTTP API | tapir endpoints declared in a module shared by the JVM and Scala.js, served by zio-http, OpenAPI generated |
| JSON | zio-json, zio-schema (tool parameter JSON Schemas are derived from types) |
| Database | SQLite (sqlite-jdbc) with ProtoQuill; schema migrations with Flyway, per module |
| Processes | zio-process; macOS `sandbox-exec` for the write sandbox |
| Model access | own streaming clients for Anthropic Messages and OpenAI Responses |
| MCP | official MCP Java SDK wrapped in ZIO |
| Crypto | JDK only (Fernet vault, Web Push encryption, VAPID) |
| Frontend | Scala.js + Laminar, Vite, Tailwind and HeroUI styles |
| Packaging | fat jar + jlink-trimmed runtime embedded in a small C launcher, one signed executable |

## Modules

Modules are layered; each depends only on lower layers, and sbt rejects cycles, so a reverse import does not compile.

```
layer 0  contract
layer 1  platform                    web (frontend, depends on contract only)
layer 2  toolkit   exec
layer 3  resources   models
layer 4  sessions
layer 5  workspace   notifications
layer 6  board   triggers
layer 7  history
layer 8  app (assembly and entry point)
```

| Module | Responsibility |
|---|---|
| `contract` | ids, shared enums, API error type, every HTTP endpoint declaration; compiled for both JVM and JS |
| `platform` | configuration, database and migrations, error model, logging, outbound HTTP (system proxy, retries), change bus, HTTP server skeleton (login, SSE, OpenAPI, static frontend), single-instance lock |
| `toolkit` | the common contract of agent tools: definitions, parameter schemas, failure classes, MCP transport |
| `exec` | starting processes: process groups, write sandbox, secret injection and masking, bounded output reading |
| `resources` | assets directory, agents, roles and permissions, vault and secret approvals, running commands as an agent |
| `models` | model providers, MCP client, web search/fetch, speech-to-text |
| `sessions` | sessions, runs and the agent loop, delegation, waiting, compaction, timelines |
| `workspace` | file tools, shell and background jobs, skill scripts and approvals, agent pages |
| `notifications` | notifications, inbox, Web Push |
| `board` | issues, dependencies, delivery to issue sessions, review |
| `triggers` | schedules, one-shot times, stream commands, prechecks, faults |
| `history` | work history, daily activity overview, session reader, usage |
| `app` | wires all modules together (`ZLayer.make`, so a missing dependency is a compile error) and defines `main` |
| `web` | the single-page frontend |

When an upstream module needs to tell a downstream one about something (for example, a session finished and a notification should be
sent), the upstream module defines a port and the downstream module implements it; `app` connects them.

Each domain module owns its tables and migrations (`modules/<module>/src/main/resources/db/<module>/V<n>__<name>.sql`, with its own
history table), its agent tools, its HTTP routes and its tests.

## Runs and recovery

- A run loops: build the request (fixed instructions, memories, skill index, then the conversation) → stream the model reply →
  execute tool calls → save → repeat. The fixed part is byte-identical across runs of the same session so the provider's prompt cache hits.
- Writes go through a single write connection, one transaction at a time; reads use a separate pool.
- Tool calls move through `Pending → Running → (Suspended → Approved) → Done`, each state saved. On startup, unfinished runs are resumed;
  calls found in `Running` get an "interrupted, not re-executed, verify first" result instead of being executed again.
- Transient failures (`Unavailable`) are retried with exponential backoff (2 s doubling, at most 5 min per wait, give up after 30 min).
- Background jobs, waits for delegated threads and wake-ups are recorded as waits on the session; when they complete, an event message
  is delivered and the session runs again.

Startup order: single-instance lock → assemble modules (migrations, global memory, asset polling) → resume sessions → resume pending
agent renames and deletions → settle background jobs left from the last process → start triggers → start HTTP. Shutdown is the reverse.

## Packaging and deployment

`deploy/build.sh` builds the frontend with Vite, the fat jar with `sbt app/assembly`, a trimmed runtime with `jdeps` and `jlink`, and
embeds them as a compressed payload in a Mach-O section of `deploy/launcher.c`. On first run the launcher unpacks the payload next
to the executable and starts Java as a child process, forwarding signals.

`deploy/install.sh` builds a revision, signs the executable with a fixed self-signed certificate (so macOS permissions survive upgrades)
and copies it to `app/current/` as a real directory with a fixed path; `deploy/activate.sh` switches versions and restarts through launchd;
`deploy/service.sh` installs the launchd job; `deploy/selfcheck.sh` checks a deployment. See [Getting started](getting-started.md).
