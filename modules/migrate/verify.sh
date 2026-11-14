#!/bin/bash
# 迁移验收：登录新版、逐个读全部会话的详情与时间线，再读各领域的列表接口。用法：verify.sh <配置文件> <端口>
set -u
cfg=$1; port=$2
base="http://127.0.0.1:$port/api"
jar=$(mktemp)
pw=$(python3 -c 'import sys,tomllib;print(tomllib.load(open(sys.argv[1],"rb"))["server"]["password"])' "$cfg")
code=$(jq -n --arg p "$pw" '{password:$p}' | curl -s -o /dev/null -w '%{http_code}' -c "$jar" -H 'content-type: application/json' -d @- "$base/auth/login")
unset pw
echo "login $code"
db=$(python3 -c 'import sys,tomllib;print(tomllib.load(open(sys.argv[1],"rb"))["general"]["data_dir"])' "$cfg")/muliao.db
ok=0; bad=0
for id in $(sqlite3 -readonly "$db" "select id from session"); do
  a=$(curl -s -o /dev/null -w '%{http_code}' -b "$jar" "$base/sessions/$id")
  b=$(curl -s -o /dev/null -w '%{http_code}' -b "$jar" "$base/sessions/$id/timeline")
  if [ "$a$b" = "200200" ]; then ok=$((ok+1)); else bad=$((bad+1)); echo "session $id: $a $b"; fi
done
echo "sessions ok=$ok bad=$bad"
check() { # 一组接口逐个读，只报非 200
  local what=$1; shift; local ok=0 bad=0
  for p in "$@"; do
    c=$(curl -s -o /tmp/verify-$$.json -w '%{http_code}' -b "$jar" "$base/$p")
    if [ "$c" = 200 ]; then ok=$((ok+1)); else bad=$((bad+1)); echo "$p: $c $(head -c 300 /tmp/verify-$$.json)"; fi
  done
  echo "$what ok=$ok bad=$bad"
}
check issues $(sqlite3 -readonly "$db" "select 'issues/'||id from issue")
check jobs $(sqlite3 -readonly "$db" "select 'agents/'||id||'/jobs' from agent")
now=$(($(date +%s)*1000))
for p in "sessions?limit=50" "issues" "triggers" "triggers?finished=true" "notifications" "history" "usage?from=0&to=$now" "agents"; do
  echo "$p $(curl -s -o /tmp/verify-$$.json -w '%{http_code}' -b "$jar" "$base/$p") $(wc -c < /tmp/verify-$$.json) bytes"
done
rm -f "$jar" /tmp/verify-$$.json
