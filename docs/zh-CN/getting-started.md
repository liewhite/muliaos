# 快速上手

[English](../en/getting-started.md)

本文把 muliao 装成你 Mac 上的后台服务、建第一个 agent，并说明升级、回滚和卸载。

## 环境要求

- **Apple Silicon 的 Mac**。部署脚本默认 Homebrew 在 `/opt/homebrew`，并用到只有 macOS 才有的东西（launchd、`sandbox-exec`、Mach-O 代码签名）。
- **Xcode 命令行工具**（`xcode-select --install`），提供 `cc`。
- **Homebrew** 装的 `node`（构建前端）、`openssl@3`（签名证书用 `/opt/homebrew/bin/openssl` 生成）、`rust`（用来装 `rcodesign`）。
- [SDKMAN!](https://sdkman.io) 装的 **JDK 25 和 sbt**；构建用 `~/.sdkman/candidates/java/current` 和 `~/.sdkman/candidates/sbt/current`。
- `PATH` 里有 **rcodesign**（`cargo install --locked apple-codesign`，装在 `~/.cargo/bin`）。
- 一个 **Anthropic** 或 **OpenAI 兼容**模型服务的 API key。走 Anthropic 协议时 muliao 总是请求 adaptive thinking，模型必须支持它
  （老模型会回 `adaptive thinking is not supported on this model`）。

```zsh
xcode-select --install
brew install node openssl@3 rust
curl -s "https://get.sdkman.io" | bash      # 装完开一个新终端
sdk install java 25.0.3-tem
sdk install sbt
cargo install --locked apple-codesign
```

## 1. 取源码

```zsh
git clone https://github.com/liewhite/muliaos.git
cd muliaos
```

放哪都行。`deploy/install.sh` 从这个仓库构建，升级时还要用，别删。

## 2. 写配置

muliao 的全部东西都在一个主目录里，默认 `~/.muliao-scala`。

```zsh
mkdir -p ~/.muliao-scala
cp deploy/config.example.toml ~/.muliao-scala/config.toml
chmod 600 ~/.muliao-scala/config.toml
```

编辑 `~/.muliao-scala/config.toml`，把没注释掉的 `CHANGE ME` 都换掉：

| 键 | 填什么 |
|---|---|
| `server.password` | 网页登录密码 |
| `server.cookie_secret` | 至少 32 个字符的随机串：`openssl rand -hex 32` |
| `vault.key` | Fernet 密钥：`python3 -c 'import base64,os; print(base64.urlsafe_b64encode(os.urandom(32)).decode())'` |
| `models.providers.<名字>.api_key` | 模型服务的 API key |
| `push.vapid_subject` | `mailto:` 邮箱或 `https://` 地址，推送服务据此认你 |

`vault.key` 另存一份：丢了它，存过的 secret 都解不开。全部配置项见 [配置](configuration.md)。

## 3. 构建并安装

```zsh
sh deploy/install.sh
```

脚本把仓库里要装的版本（默认 `main`）检出到 `~/.muliao-scala/build/src`，构建前端、服务端 jar 和裁剪过的 Java 运行时，打成一个可执行文件，
用它在 `~/.muliao-scala/codesign/` 生成的自签证书签名，装成 `~/.muliao-scala/app/current/muliao`。它不启动也不重启任何东西。
第一次构建要下载依赖，需要几分钟。

保留 `~/.muliao-scala/codesign/`：程序总用同一张证书签名、装在同一路径，你给它的 macOS 授权（比如「完全磁盘访问」）升级后不丢。

## 4. 启动服务

```zsh
sh deploy/service.sh install
```

它写 `~/Library/LaunchAgents/com.muliao.scala.plist` 并加载：launchd 现在就启动 muliao，之后每次登录都启动，进程退出会自动拉起。
服务运行的是 `app/current/muliao serve ~/.muliao-scala/config.toml`。agent 起的命令用 plist 里写的 `PATH`
（`~/.local/bin`、`~/.cargo/bin`、`/opt/homebrew/bin`、`/usr/local/bin` 和系统目录）。

确认服务正常：

```zsh
sh deploy/selfcheck.sh
```

它逐项打印 `PASS`/`FAIL`：进程晚于安装启动、在跑的版本就是装的版本、启动以来日志无 `ERROR`、端口返回 200、launchd 没在循环重启（最后一项要等 60 秒）。
第一次启动时程序要先解开内置的运行时，要几秒；自检跑得太早会报没有在跑的进程，再跑一次即可。

服务起不来时看 `~/.muliao-scala/muliao.log`（程序日志）和 `~/.muliao-scala/launchd.log`（启动前的报错，比如配置写错）。

## 5. 建第一个 agent

1. 打开 <http://127.0.0.1:1080>，用 `server.password` 登录。
2. 打开菜单（☰）→「Agents」→「新建」，填名字和职责描述。新 agent 默认用 `models.default_model`，也可以在表单里换服务商和模型。
3. 想让某个助手管理平台（建删 agent、改任何资源），打开它的 **admin** 角色。
4. 打开这个 agent 点「对话」，或点 **+** 按钮，给它派个活。

界面主要区域：会话、Issues、页面（agent 做的网页）、通知；菜单里有 Agents、触发器、Token 用量、AI 资产、角色、设置。

## 6. 在手机上用（可选）

muliao 默认只监听 `127.0.0.1`。要从别的设备访问，在前面放一个反向代理，用你自己的域名提供 HTTPS（任何终结 TLS、转发到 `127.0.0.1:1080` 的代理或隧道都行），然后：

1. 把 `server.public_url` 设成那个 HTTPS 地址（通知里的链接用它），重启：`sh deploy/activate.sh --restart-only`。
2. 在 iPhone 的 Safari 打开这个地址，「分享 → 添加到主屏幕」，之后从主屏图标打开。
3. 在「设置」里开启本设备推送，发一条测试通知。

passkey 加密的 secret 和签名钥匙只在 HTTPS 的域名地址上可用（IP 和 `localhost` 不行）。配了 `[transcription]` 段才会出现语音输入。

## 升级

```zsh
git pull
sh deploy/install.sh                   # 或 sh deploy/install.sh <分支、tag 或 commit>
sh deploy/activate.sh --restart-only   # 重启并等它能应答
sh deploy/selfcheck.sh
```

随时重启都安全：在跑的运行重启后从上一次保存的步骤接着跑，被打断的后台任务会以「被中断」告诉对应的 agent。`install.sh` 在 `~/.muliao-scala/app/` 里保留最近 3 个版本。

新版本第一次启动可能升级数据库结构。想留退路就先备份：

```zsh
sqlite3 ~/.muliao-scala/data/muliao.db ".backup $HOME/muliao-backup.db"
cp -R ~/.muliao-scala/assets ~/muliao-assets-backup
cp ~/.muliao-scala/config.toml ~/muliao-config-backup.toml
```

## 回滚

```zsh
sh deploy/activate.sh --previous                         # 退回比当前早的最近一个已装版本
sh deploy/activate.sh ~/.muliao-scala/app/<版本目录>       # 换成指定的已装版本
sh deploy/selfcheck.sh
```

数据库比程序新时程序拒绝启动。要退出的那个版本升级过数据库的话，先停服务、恢复备份的库，再换版本：

```zsh
launchctl bootout gui/$(id -u)/com.muliao.scala
cp ~/muliao-backup.db ~/.muliao-scala/data/muliao.db && rm -f ~/.muliao-scala/data/muliao.db-wal ~/.muliao-scala/data/muliao.db-shm
cp -R ~/.muliao-scala/app/<版本目录>/. ~/.muliao-scala/app/current/
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.muliao.scala.plist
```

备份之后写进库的数据会丢。

## 卸载

```zsh
sh deploy/service.sh uninstall   # 停服务、删 plist，数据保留
rm -rf ~/.muliao-scala           # 删掉全部数据、资源、agent 工作目录和已装版本
```

## 再跑一个实例

部署脚本都读三个环境变量，可以试新版本而不碰正在用的那套：

| 变量 | 默认 | 含义 |
|---|---|---|
| `MULIAO_HOME` | `~/.muliao-scala` | 主目录：`config.toml`、`data/`、`app/`、`codesign/` |
| `MULIAO_LABEL` | `com.muliao.scala` | launchd 服务名 |
| `MULIAO_PORT` | `1080` | 健康检查用的端口，须与 `server.port` 一致 |

那个实例的配置里 `data_dir`、`assets_dir`、日志文件和 `server.port` 都要换成不同的。一份数据目录同时只能有一个进程在用。
