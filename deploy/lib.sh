# deploy/ 下各脚本共用的路径与服务操作（被 . 引入，不单独执行）。Scala 版 muliao：#881 起是正式服务（端口 1080）；Rust 版（~/.muliao、com.muliao）已停用，留作回退。
#
# 服务怎么启动：launchd 服务 $LABEL（plist 由 service.sh 生成）运行 $APP/current/muliao serve $CONFIG。
# $APP/current 是真目录（不是链接），换版本就是把某个版本目录整个复制成它（activate）：macOS 的文件访问授权
# （「完全磁盘访问」等）按程序的真实路径加签名记，所以程序路径固定为 $APP/current/muliao，签名用 $SIGNING 下
# 固定的自签证书、固定标识 muliao-scala，每次重装都被认成同一个程序。
# 换版本只 activate 再 kickstart -k，不重新加载 plist：从 muliao 服务里起的进程做 launchctl bootstrap 会失败，
# 只有 kickstart、kill 可用；第一次装 plist 要在服务外做（service.sh 的说明）。
#
# 路径都可以用环境变量改，演练时指到临时目录和别的 launchd label：
#   MULIAO_HOME   配置与数据所在目录（默认 ~/.muliao-scala）：config.toml、data/、muliao.log、app/、codesign/
#   MULIAO_LABEL  launchd 服务名（默认 com.muliao.scala）
#   MULIAO_PORT   服务端口（默认 1080，须与 config.toml 的 server.port 一致），健康检查用
MULIAO_HOME=${MULIAO_HOME:-$HOME/.muliao-scala}
LABEL=${MULIAO_LABEL:-com.muliao.scala}
PORT=${MULIAO_PORT:-1080}
SVC=gui/$(id -u)/$LABEL
CONFIG=$MULIAO_HOME/config.toml
LOGFILE=$MULIAO_HOME/muliao.log
# launchd 收的标准输出/错误（JVM 崩溃信息、启动前的报错）；程序自己的日志写 $LOGFILE
STDLOG=$MULIAO_HOME/launchd.log
# 安装的版本：app/<时间>-<commit>/{muliao,COMMIT}，app/current 是在用那一版的副本
APP=$MULIAO_HOME/app
# 代码签名证书与私钥（第一次安装时生成，之后不再换：换了就要重新授权）
SIGNING=$MULIAO_HOME/codesign
SIGN_ID=muliao-scala
KEEP_VERSIONS=${KEEP_VERSIONS:-3}

say() { echo "$(date '+%F %T') $*"; }
die() { say "ERROR: $*"; exit 1; }

# launchd 起的是启动器；服务的 java 进程是它的子进程（用 ps 找：pgrep -P 在 agent 的写沙箱里找不到）
svc_pid() { launchctl print "$SVC" 2>/dev/null | awk '$1=="pid" {print $3; exit}'; }
java_cmd() { p=$(svc_pid); [ -n "$p" ] && ps -axo ppid=,pid=,command= | awk -v p="$p" '$1==p {$1=""; print; exit}'; }
java_pid() { java_cmd | awk '{print $1}'; }
# 服务的 java 正在用的载荷目录名（current 下的 .muliao-<编号>）；服务没在跑时为空
running_payload() { java_cmd | awk '{print $2}' | sed -n 's#^.*/\(\.[^/]*\)/runtime/bin/java$#\1#p'; }

# 用固定的自签证书签 $1（第一次调用时生成证书）。用 rcodesign 直接拿证书文件签：系统的 codesign 只认钥匙串里
# 受信任的证书，设为受信任要在图形界面上输密码，服务里做不了。
sign_program() {
  command -v rcodesign > /dev/null || die "rcodesign not found: cargo install --locked apple-codesign"
  if [ ! -e "$SIGNING/cert.pem" ]; then
    mkdir -p "$SIGNING" && chmod 700 "$SIGNING" || return 1
    sign_out=$(/opt/homebrew/bin/openssl req -x509 -newkey rsa:2048 -nodes -days 36500 -subj "/CN=muliao-scala code signing" \
      -addext basicConstraints=critical,CA:false -addext keyUsage=critical,digitalSignature \
      -addext extendedKeyUsage=critical,codeSigning \
      -keyout "$SIGNING/key.pem.tmp" -out "$SIGNING/cert.pem.tmp" 2>&1) || { echo "$sign_out"; return 1; }
    mv "$SIGNING/key.pem.tmp" "$SIGNING/key.pem" && mv "$SIGNING/cert.pem.tmp" "$SIGNING/cert.pem" || return 1
    say "created signing certificate in $SIGNING (keep it: a new one means granting file access again)"
  fi
  # 不要时间戳（默认连 Apple 的时间戳服务器，断网就失败；本机授权用不到）；成功时的过程日志不打印
  sign_out=$(rcodesign sign --binary-identifier "$SIGN_ID" --timestamp-url none \
    --pem-file "$SIGNING/key.pem" --pem-file "$SIGNING/cert.pem" "$1" 2>&1) || { echo "$sign_out"; return 1; }
}

# 把版本目录 $1 复制成 $APP/current（先复制成 current.new 再换上），重启后生效。
# 目录里连同启动器解好的载荷（.muliao-<编号>/）一起克隆，换上后第一次启动不用再解。
# 正在跑的服务还要用它自己的载荷：JVM 按路径 $APP/current/.muliao-<旧编号>/runtime 起子进程（lib/jspawnhelper）、
# 懒加载库，路径没了 agent 的命令和 precheck 全部起不来（#884）。所以把它在用的那份载荷也带进新目录；
# 重启后没人用它，下次 activate 就不再带，current 里最多两份。
activate() {
  [ -x "$1/muliao" ] || die "activate: $1/muliao not found"
  rm -rf "$APP/current.new" "$APP/current.old"
  cp -cR "$1" "$APP/current.new" || return 1 # APFS 克隆，不占空间
  used=$(running_payload)
  if [ -n "$used" ] && [ -d "$APP/current/$used" ] && [ ! -e "$APP/current.new/$used" ]; then
    cp -cR "$APP/current/$used" "$APP/current.new/$used" || return 1
  fi
  [ -e "$APP/current" ] && { mv "$APP/current" "$APP/current.old" || return 1; }
  mv "$APP/current.new" "$APP/current" || return 1
  rm -rf "$APP/current.old"
  say "activated $1 as $APP/current ($(cat "$APP/current/COMMIT"))"
}

# 重启服务（SIGTERM，按自己的关停流程退出，最长 ExitTimeOut 秒）并等它能应答：90 秒内 /api/auth/session 有回应（不带 cookie 是 401）。
restart_and_wait() {
  launchctl kickstart -k "$SVC" || { say "launchctl kickstart -k $SVC failed"; return 1; }
  code=000
  for _ in $(seq 1 45); do
    code=$(curl -s -o /dev/null -m 5 -w '%{http_code}' "http://127.0.0.1:$PORT/api/auth/session")
    case $code in 200 | 401) break ;; esac
    sleep 2
  done
  case $code in 200 | 401) ;; *) say "unhealthy: /api/auth/session on port $PORT answered $code within 90s"; return 1 ;; esac
  # 稳不稳（进程号不变、无 ERROR、不循环重启）由随后的 deploy/selfcheck.sh 查，这里只等到能应答
  say "up: launcher pid $(svc_pid), java pid $(java_pid), /api/auth/session $code"
}

# 自己脱离调用者在后台重跑：从本服务里起的命令在重启时会被打断，所以另起会话（setsid）；输出追加到 $MULIAO_HOME/deploy.log。
detach() {
  if [ -z "${MULIAO_DETACHED:-}" ]; then
    MULIAO_DETACHED=1 nohup /usr/bin/perl -MPOSIX -e 'POSIX::setsid() or die "setsid: $!"; exec @ARGV or die "exec: $!"' sh "$@" \
      >> "$MULIAO_HOME/deploy.log" 2>&1 < /dev/null &
    echo "running in the background (pid $!); follow: tail -f $MULIAO_HOME/deploy.log"
    exit 0
  fi
}
