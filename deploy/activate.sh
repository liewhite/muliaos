#!/bin/sh
# 换版本、退回上一版、重启 Scala 版 muliao，并等它能应答（稳不稳由 deploy/selfcheck.sh 查）：
#   deploy/activate.sh <版本目录>      换成这个已装的版本（如 ~/.muliao-scala/app/<时间>-<commit>）再重启
#   deploy/activate.sh --previous      退回到比当前版本早的最近一个已装版本再重启
#   deploy/activate.sh --restart-only  只重启（install.sh 装完后用）
# 从本服务自己起的命令里运行时自动脱离到后台（重启会打断调用者），输出在 $MULIAO_HOME/deploy.log。
# 90 秒内没应答时退出码 1、不自动回滚：看日志后决定是否 --previous。
set -eu
. "$(dirname "$0")/lib.sh"
arg=${1:?usage: deploy/activate.sh <version dir> | --previous | --restart-only}

inside_service() {
  s=$(svc_pid)
  [ -n "$s" ] || return 1
  p=$$
  while [ "${p:-1}" -gt 1 ]; do
    [ "$p" = "$s" ] && return 0
    p=$(ps -o ppid= -p "$p" | tr -d ' ')
  done
  return 1
}
inside_service && detach "$0" "$@"

case $arg in
  --restart-only) ;;
  --previous)
    # 版本号就是版本目录名（install.sh 打包时写进程序）
    current=$("$APP/current/muliao" version) || die "cannot read the version of $APP/current/muliao"
    prev=$(ls -1d "$APP"/2*-* | sort | awk -v c="$APP/$current" '$0 < c' | tail -1)
    [ -n "$prev" ] || die "no version older than $current in $APP"
    activate "$prev" || die "could not activate $prev"
    ;;
  *) activate "$arg" || die "could not activate $arg" ;;
esac
restart_and_wait || die "service not answering after restart (log: $LOGFILE, $STDLOG)"
