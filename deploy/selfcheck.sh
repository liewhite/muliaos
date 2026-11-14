#!/bin/sh
# 部署后的通用自检：用法 deploy/selfcheck.sh [目标 commit]（在仓库里跑；新功能验收不在此，由调用方另做）
#
# 逐项打印 PASS/FAIL 和依据，任一失败退出码 1：
#   1. 重启了：服务的 java 进程启动时间晚于安装时间（app/current 目录的修改时间）
#   2. 版本对：日志里本进程启动行的版本 = app/current/muliao 的版本；给了目标 commit 时，已装 commit 包含它
#   3. 日志里本进程（java pid）的启动行在，之后没有 ERROR
#   4. HTTP 端口返回 200
#   5. 没有循环重启：launchctl runs 隔 RUNS_WAIT 秒（默认 60）不增长
# 只报告不处理。路径和服务名的环境变量见 deploy/lib.sh。
set -u
export LC_ALL=C # ps lstart、date 解析依赖英文格式
target=${1:-}
. "$(dirname "$0")/lib.sh"
RUNS_WAIT=${RUNS_WAIT:-60}
repo=$(cd "$(dirname "$0")/.." && pwd)
failed=0

pass() { echo "PASS $1: $2"; }
fail() { echo "FAIL $1: $2"; failed=1; }

jpid=$(java_pid)
version=$("$APP/current/muliao" version 2> /dev/null)

# 1. 重启晚于安装
if [ ! -e "$APP/current/COMMIT" ]; then
  fail restarted "没有 ${APP}/current（没用 deploy/install.sh 装过）"
elif [ -z "$jpid" ]; then
  fail restarted "${LABEL} 没有在跑的 java 进程（launchd pid $(svc_pid)）"
else
  installed_at=$(stat -f %m "$APP/current")
  started_at=$(date -j -f "%a %b %e %T %Y" "$(ps -o lstart= -p "$jpid" | sed 's/^ *//')" +%s 2> /dev/null || echo 0)
  info="java pid ${jpid} 启动于 $(date -r "$started_at" '+%F %T')，安装于 $(date -r "$installed_at" '+%F %T')"
  if [ "$started_at" -gt "$installed_at" ]; then pass restarted "$info"; else fail restarted "$info"; fi
fi

# 2 + 3. 本进程的启动行（带版本与 pid），之后无 ERROR；重启后刚好轮转过的话启动行在 .1 里，拼起来看
joined=$(mktemp)
trap 'rm -f "$joined"' EXIT
cat "$LOGFILE.1" "$LOGFILE" > "$joined" 2> /dev/null
start=$(grep -n " muliao .* 启动，pid ${jpid:-none}，" "$joined" | tail -1)
if [ -z "$start" ]; then
  fail log "${LOGFILE}(.1) 里没有本进程（pid=${jpid:-none}）的启动行"
  fail version "无启动行可对照"
else
  line=${start%%:*}
  logged=$(echo "$start" | sed 's/.* muliao \([^ ]*\) 启动.*/\1/')
  if [ "$logged" = "$version" ]; then pass version "在跑的是 ${logged}"; else fail version "在跑 ${logged}，已装 ${version:-?}"; fi
  errors=$(awk -v s="$line" 'NR >= s && / ERROR / {print NR ": " substr($0, 1, 300)}' "$joined" | head -5)
  if [ -n "$errors" ]; then fail log "第 ${line} 行（本进程启动）之后有 ERROR（前 5 条）：
${errors}"; else pass log "第 ${line} 行起本进程无 ERROR"; fi
fi
if [ -n "$target" ]; then
  installed=$(cat "$APP/current/COMMIT" 2> /dev/null)
  if git -C "$repo" merge-base --is-ancestor "$target" "$installed" 2> /dev/null; then
    pass commit "已装 ${installed} 包含 ${target}"
  else
    fail commit "已装 ${installed:-?} 不包含 ${target}（或本地仓库缺提交）"
  fi
fi

# 4. HTTP 200
code=$(curl -s -o /dev/null -m 10 -w "%{http_code}" "http://127.0.0.1:${PORT}/")
if [ "$code" = 200 ]; then pass http "127.0.0.1:${PORT} 返回 200"; else fail http "127.0.0.1:${PORT} 返回 ${code}"; fi

# 5. runs 不增长
runs() { launchctl print "$SVC" 2> /dev/null | awk '$1=="runs" {print $3; exit}'; }
r1=$(runs)
sleep "$RUNS_WAIT"
r2=$(runs)
if [ -n "$r1" ] && [ "$r1" = "$r2" ]; then pass runs "runs = ${r1}，${RUNS_WAIT}s 后不变"; else fail runs "runs ${r1:-?} → ${r2:-?}（${RUNS_WAIT}s 内），可能在循环重启"; fi

[ "$failed" = 0 ] && echo "ALL PASS" || echo "SOME FAILED"
exit "$failed"
