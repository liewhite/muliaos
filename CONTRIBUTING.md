# Contributing

[中文](#参与开发)

Thanks for your interest. muliao is a personal project maintained by one person, so the process is light:

- **Bugs and ideas**: open a GitHub issue. For bugs include the macOS version, the commit (`~/.muliao-scala/app/current/COMMIT`
  or `muliao version`) and the relevant lines of `muliao.log` with secrets removed.
- **Pull requests**: for anything beyond a small fix, open an issue first so we can agree on the approach. Keep one logical change per
  pull request, and make sure `sbt test` passes.
- **Security problems**: do not open a public issue, see [SECURITY.md](SECURITY.md).

## Development setup

Install the toolchain from [Getting started](docs/en/getting-started.md#requirements) (JDK 25 and sbt via SDKMAN!, Node.js, Xcode command line tools).

```zsh
export JAVA_HOME=~/.sdkman/candidates/java/current
sbt test                    # all tests, including the Scala.js contract tests (needs node)
sbt app/compile             # compile the server
sbt "sessions/testOnly *DriverSpec"   # one spec of one module
sbt --client test           # keep an sbt server running for much faster repeated builds
```

A first full compile in a fresh checkout can exceed sbt's default heap; use `sbt -J-Xmx4g test` if you see `OutOfMemoryError`.

### Run a development server

```zsh
mkdir -p dev-data                                     # ignored by git
cp deploy/config.example.toml dev-data/muliao.toml    # point data_dir, assets_dir and log.file into dev-data/, use a port other than 1080
cp=$(sbt -batch "export app/Runtime/fullClasspath" | tail -1)
java --enable-native-access=ALL-UNNAMED -cp "$cp" muliao.app.Main serve dev-data/muliao.toml
```

`muliao.app.Main openapi` prints the OpenAPI description of the HTTP API.

### Frontend

```zsh
cd modules/web
npm install
npx vite build              # links the Scala.js module through sbt; output in modules/web/dist
```

Set `server.web_dir` to `modules/web/dist` in the development config to serve the fresh build, or run the Vite dev server with
`MULIAO_BACKEND=http://127.0.0.1:<server port> npx vite --port <port>` for hot reload (it proxies `/api` and `/site`).

## Repository layout

```
build.sbt, project/         sbt build; module dependencies are declared in build.sbt
modules/<module>/           one directory per module (see docs/en/architecture.md)
  src/main/scala/           code
  src/main/resources/db/    that module's SQL migrations
  src/test/scala/           tests
modules/web/                frontend (Scala.js + Laminar, Vite project)
deploy/                     build, install, launchd and self-check scripts, single-file launcher, example config
docs/                       documentation; requirements.md, stack.md and web.md are the design documents
```

## Conventions

- **Design by domain.** Each module owns one area: its tables and migrations, its agent tools, its HTTP routes and its tests.
  Modules talk only through interfaces, and dependencies point to lower layers only. Adding a module dependency needs a reason in `docs/stack.md`.
- **Put logic in types.** Prefer ADTs, opaque types and exhaustive matches over `if` chains on values. Shared enums live in `contract`.
- **Warnings are errors.** The build uses `-Werror`, `-Wunused:imports` and `-Wvalue-discard`, so a discarded ZIO effect does not compile.
- **Migrations are append-only.** Never edit a released `V<n>__*.sql`; add a new one.
- **Keep docs in step.** Behaviour changes update `docs/requirements.md`, `docs/stack.md` or `docs/web.md` and, if user-facing, the
  guides in `docs/en` and `docs/zh-CN`. The design documents are written in Chinese; user guides exist in both languages.
- **No secrets in the repository.** Test fixtures use dummy keys. Run [gitleaks](https://github.com/gitleaks/gitleaks) before pushing.

By contributing you agree that your contributions are licensed under the [MIT License](LICENSE).

---

# 参与开发

muliao 是一个人维护的个人项目，流程从简：

- **bug 与想法**：开 GitHub issue。报 bug 请写 macOS 版本、commit（`~/.muliao-scala/app/current/COMMIT` 或 `muliao version`）和 `muliao.log` 里相关的几行（先去掉密钥）。
- **Pull request**：小修以外的改动先开 issue 商量做法。一个 PR 一个逻辑改动，`sbt test` 要通过。
- **安全问题**：不要开公开 issue，见 [SECURITY.md](SECURITY.md)。

开发环境、跑测试、起开发服务、前端构建见上文英文部分的命令（与 [快速上手](docs/zh-CN/getting-started.md#环境要求) 的环境要求相同）。

约定：

- **按领域设计**：每个模块管一块（自己的表与迁移、agent 工具、HTTP 路由、测试），模块之间只经接口通信，依赖只指向更低层；加模块依赖要在 `docs/stack.md` 写明理由。
- **逻辑写在类型上**：用 ADT、opaque type、穷尽匹配，少写判断值的 `if`；共用枚举放 `contract`。
- **警告即错误**：开了 `-Werror`、`-Wunused:imports`、`-Wvalue-discard`，丢弃的 ZIO 效果编译不过。
- **迁移只增不改**：已发布的 `V<n>__*.sql` 不改，要改就加新的。
- **文档随改动更新**：行为变了就改 `docs/requirements.md`、`docs/stack.md` 或 `docs/web.md`，面向用户的再改 `docs/en`、`docs/zh-CN` 里的指南。
- **仓库里不放密钥**：测试用假密钥；推送前跑 gitleaks。

提交贡献即表示你同意按 [MIT 许可证](LICENSE) 授权你的贡献。
