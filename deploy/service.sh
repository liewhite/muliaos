#!/bin/sh
# 装上或卸下 Scala 版 muliao 的 launchd 服务（只在第一次或改 plist 时用；换版本、重启用 activate.sh）：
#   deploy/service.sh install    写 ~/Library/LaunchAgents/$LABEL.plist 并加载（RunAtLoad，立即启动）
#   deploy/service.sh uninstall  停掉并卸载服务、删 plist（数据不动）
# 要在 muliao 服务之外运行：从服务里起的进程做 launchctl bootstrap/bootout 会报「5: Input/output error」。
# 在 muliao 的 agent 里就经 ssh 到本机执行：ssh -F /dev/null -i ~/.ssh/id_rsa localhost 'sh <仓库>/deploy/service.sh install'
set -eu
. "$(dirname "$0")/lib.sh"
PLIST=$HOME/Library/LaunchAgents/$LABEL.plist

case ${1:?usage: deploy/service.sh install | uninstall} in
  install)
    [ -x "$APP/current/muliao" ] || die "$APP/current/muliao not found: run deploy/install.sh first"
    [ -f "$CONFIG" ] || die "$CONFIG not found"
    launchctl print "$SVC" > /dev/null 2>&1 && die "$LABEL is already loaded (uninstall first to change the plist)"
    cat > "$PLIST.tmp" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<!-- 由 muliao-scala 的 deploy/service.sh 生成 -->
<plist version="1.0">
<dict>
  <key>Label</key>
  <string>$LABEL</string>
  <key>ProgramArguments</key>
  <array>
    <string>$APP/current/muliao</string>
    <string>serve</string>
    <string>$CONFIG</string>
  </array>
  <key>WorkingDirectory</key>
  <string>$MULIAO_HOME</string>
  <key>EnvironmentVariables</key>
  <dict>
    <!-- agent 的命令与本地 MCP server（如 npx）从这里找程序 -->
    <key>PATH</key>
    <string>$HOME/.local/bin:$HOME/.cargo/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
  </dict>
  <key>RunAtLoad</key>
  <true/>
  <key>KeepAlive</key>
  <true/>
  <!-- 关停（SIGTERM）后等进程自己退出的秒数：要够结束后台任务、断开连接 -->
  <key>ExitTimeOut</key>
  <integer>60</integer>
  <key>ThrottleInterval</key>
  <integer>10</integer>
  <!-- 程序自己的日志写 config.toml 里的日志文件；这里只收 JVM 自己的输出（崩溃、启动前报错） -->
  <key>StandardOutPath</key>
  <string>$STDLOG</string>
  <key>StandardErrorPath</key>
  <string>$STDLOG</string>
</dict>
</plist>
EOF
    plutil -lint "$PLIST.tmp" > /dev/null || die "generated plist is invalid"
    mv "$PLIST.tmp" "$PLIST"
    launchctl bootstrap "gui/$(id -u)" "$PLIST" || die "launchctl bootstrap failed (run outside the muliao service, see header)"
    say "loaded $LABEL from $PLIST"
    ;;
  uninstall)
    launchctl bootout "$SVC" 2> /dev/null || say "$LABEL was not loaded"
    rm -f "$PLIST"
    say "unloaded $LABEL and removed $PLIST"
    ;;
  *) die "usage: deploy/service.sh install | uninstall" ;;
esac
