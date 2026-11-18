# Getting started

[中文](../zh-CN/getting-started.md)

This guide installs muliao as a background service on your Mac, creates your first agent and covers upgrades, rollback and removal.

## Requirements

- An **Apple Silicon Mac**. The deploy scripts expect Homebrew in `/opt/homebrew` and use macOS-only tools
  (launchd, `sandbox-exec`, Mach-O code signing).
- **Xcode command line tools** (`xcode-select --install`) for `cc`.
- **Homebrew** packages: `node` (frontend build), `openssl@3` (the signing certificate is created with `/opt/homebrew/bin/openssl`),
  `rust` (to install `rcodesign`).
- **JDK 25 and sbt** installed with [SDKMAN!](https://sdkman.io); the build uses `~/.sdkman/candidates/java/current` and `~/.sdkman/candidates/sbt/current`.
- **rcodesign** (`cargo install --locked apple-codesign`) on your `PATH`. Cargo installs into `~/.cargo/bin`, which Homebrew's Rust does not add to `PATH`.
- An API key for an **Anthropic** or **OpenAI-compatible** model provider.

```zsh
xcode-select --install
brew install node openssl@3 rust
curl -s "https://get.sdkman.io" | bash      # open a new terminal afterwards
sdk install java 25.0.5.0+8-tem             # or a newer 25.x Temurin from `sdk list java`
sdk install sbt
cargo install --locked apple-codesign
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.zprofile && export PATH="$HOME/.cargo/bin:$PATH"
```

## 1. Get the source

```zsh
git clone https://github.com/liewhite/muliaos.git
cd muliaos
```

The clone can live anywhere. `deploy/install.sh` builds from it, so keep it for upgrades.

## 2. Write the configuration

Everything muliao keeps lives in one home directory, `~/.muliao-scala` by default.

```zsh
mkdir -p ~/.muliao-scala
cp deploy/config.example.toml ~/.muliao-scala/config.toml
chmod 600 ~/.muliao-scala/config.toml
```

Edit `~/.muliao-scala/config.toml`, set `general.timezone` to your time zone and replace every `CHANGE ME` that is not commented out:

| Key | What to put there |
|---|---|
| `server.password` | the password for the web login |
| `server.cookie_secret` | a random string of at least 32 characters: `openssl rand -hex 32` |
| `vault.key` | a Fernet key: `python3 -c 'import base64,os; print(base64.urlsafe_b64encode(os.urandom(32)).decode())'` |
| `models.providers.<name>.api_key` | your model provider's API key |
| `push.vapid_subject` | a `mailto:` address or `https://` URL identifying you to push services |

Keep a copy of `vault.key` somewhere safe: without it, stored secrets cannot be decrypted. All keys are described in
[Configuration](configuration.md).

## 3. Build and install

```zsh
sh deploy/install.sh
```

The script checks out the requested revision (default `main`) of your clone into `~/.muliao-scala/build/src`, builds the frontend,
the server jar and a trimmed Java runtime, packs them into a single executable, signs it with a self-signed certificate it creates
in `~/.muliao-scala/codesign/`, and installs it as `~/.muliao-scala/app/current/muliao`. It does not start or restart anything.
The first build downloads dependencies and takes several minutes.

Keep `~/.muliao-scala/codesign/`: the program is always signed with the same certificate and installed at the same path, so macOS
permissions you grant it (for example Full Disk Access) survive upgrades.

## 4. Start the service

```zsh
sh deploy/service.sh install
```

This writes `~/Library/LaunchAgents/com.muliao.scala.plist` and loads it. launchd starts muliao now and at every login, and
restarts it if it exits. The service runs `app/current/muliao serve ~/.muliao-scala/config.toml`. Commands started by agents get
the `PATH` written into the plist (`~/.local/bin`, `~/.cargo/bin`, `/opt/homebrew/bin`, `/usr/local/bin` and the system directories).

Check that it is healthy:

```zsh
sh deploy/selfcheck.sh
```

It prints `PASS`/`FAIL` for: the process started after the install, the running version matches the installed one, no `ERROR` in the
log since startup, the port answers 200, and launchd is not restarting it in a loop (this last check waits 60 seconds). Run right
after `service.sh install`, it can report no running process because Java is still starting; run it again a few seconds later.

If the service does not come up, look at `~/.muliao-scala/muliao.log` (the program's log) and `~/.muliao-scala/launchd.log`
(startup errors, such as an invalid configuration).

## 5. Create your first agent

1. Open <http://127.0.0.1:1080> and log in with `server.password`.
2. Open the menu (☰) → **Agents** → **新建** (New). Give the agent a name and a description of what it is for.
   New agents start with `models.default_model`; pick another provider and model in the form if you like.
3. Optionally switch on the **admin** role for an assistant you want to manage the platform (create and delete agents, write any asset).
4. Open the agent and click **对话** (Chat), or use the **+** button, and send it a task.

The interface is in Chinese. The main areas are 会话 (sessions), Issues, 页面 (agent pages), 通知 (notifications), and in the menu:
Agents, 触发器 (triggers), Token 用量 (usage), AI 资产 (assets), 角色 (roles) and 设置 (settings).

## 6. Use it from your phone (optional)

muliao listens on `127.0.0.1` by default. To reach it from other devices, put it behind a reverse proxy that serves HTTPS on a domain
name you control (any proxy or tunnel that terminates TLS and forwards to `127.0.0.1:1080` works).

The proxy must run on the same Mac and add an `X-Forwarded-For` header: login rate limiting uses its last entry to tell devices apart
when a request comes from `127.0.0.1`. Without it, every device shares one counter and a few wrong passwords on one device lock out all of them for a while. Then:

1. Set `server.public_url` to that HTTPS address (notification links use it) and restart: `sh deploy/activate.sh --restart-only`.
2. Open the address in Safari on the iPhone, *Share → Add to Home Screen*, and open muliao from the home screen.
3. In **设置** (settings), turn on push notifications for this device and send a test notification.

Passkey-protected secrets and signing keys only work on an HTTPS domain name (not an IP address or `localhost`).
Voice input appears once a `[transcription]` section is configured.

## Upgrading

```zsh
git pull
sh deploy/install.sh                   # or: sh deploy/install.sh <branch, tag or commit>
sh deploy/activate.sh --restart-only   # restart and wait until it answers
sh deploy/selfcheck.sh
```

Restarting is safe at any time: runs in progress continue from their last saved step after the restart, and background jobs that
were cut off are reported to their agents as interrupted. `install.sh` keeps the three most recent versions in `~/.muliao-scala/app/`.

New versions may upgrade the database schema on first start. Back up before upgrading if you want to be able to go back:

```zsh
sqlite3 ~/.muliao-scala/data/muliao.db ".backup $HOME/muliao-backup.db"
cp -R ~/.muliao-scala/assets ~/muliao-assets-backup
cp ~/.muliao-scala/config.toml ~/muliao-config-backup.toml
```

## Rolling back

```zsh
sh deploy/activate.sh --previous                         # the most recent installed version older than the current one
sh deploy/activate.sh ~/.muliao-scala/app/<version-dir>  # a specific installed version
sh deploy/selfcheck.sh
```

A program refuses to start on a database written by a newer version. If the version you are leaving migrated the database, stop
the service, restore the database backup, then switch versions:

```zsh
launchctl bootout gui/$(id -u)/com.muliao.scala
cp ~/muliao-backup.db ~/.muliao-scala/data/muliao.db && rm -f ~/.muliao-scala/data/muliao.db-wal ~/.muliao-scala/data/muliao.db-shm
cp -R ~/.muliao-scala/app/<version-dir>/. ~/.muliao-scala/app/current/
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.muliao.scala.plist
```

Data written after the backup is lost.

## Uninstalling

```zsh
sh deploy/service.sh uninstall   # stop the service and remove the plist; data is kept
rm -rf ~/.muliao-scala           # delete all data, assets, agent workspaces and installed versions
```

## Running a second instance

Every deploy script reads three environment variables, so you can try a build without touching your main installation:

| Variable | Default | Meaning |
|---|---|---|
| `MULIAO_HOME` | `~/.muliao-scala` | home directory with `config.toml`, `data/`, `app/`, `codesign/` |
| `MULIAO_LABEL` | `com.muliao.scala` | launchd service name |
| `MULIAO_PORT` | `1080` | port used by the health checks; must match `server.port` |

In that instance's config use different `data_dir`, `assets_dir` and `server.port`, and set `log.file` to `<MULIAO_HOME>/muliao.log`
(`selfcheck.sh` reads the log from there). One data directory can only be used by one running process.
