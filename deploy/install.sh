#!/bin/sh
# 编译并安装 Scala 版 muliao：用法 deploy/install.sh [分支、tag 或 commit，默认 main]
#
# 在 $MULIAO_HOME/build/src（常驻的仓库副本，保留 sbt 编译输出与 node_modules）检出要装的版本，用 build.sh 打成单文件，
# 用固定证书签名，放进新目录 $APP/<时间>-<commit>/，再复制成 app/current（路径和签名固定，文件访问授权才不丢，见 lib.sh）。
# 不重启服务：装完用 deploy/activate.sh --restart-only 重启，或 activate.sh <版本目录> 换版本并重启。保留最近 $KEEP_VERSIONS 个版本目录。
# 仓库默认是本脚本所在的本地仓库（装本地已有的提交，不去 GitHub 远端取），可用 MULIAO_REPO 指定别的（路径或 URL）。
set -eu
. "$(dirname "$0")/lib.sh"
REF=${1:-main}
REPO=${MULIAO_REPO:-$(git -C "$(dirname "$0")" rev-parse --path-format=absolute --git-common-dir)}
SRC=$MULIAO_HOME/build/src

[ -d "$SRC/.git" ] || git clone -q "$REPO" "$SRC"
git -C "$SRC" fetch -q "$REPO" "$REF"
git -C "$SRC" checkout -q --force --detach FETCH_HEAD
# 清掉上次留下的东西，只留编译输出与依赖
git -C "$SRC" clean -q -ffdx -e target -e modules/web/node_modules
sha=$(git -C "$SRC" rev-parse HEAD)
name=$(date +%Y%m%d-%H%M%S)-$(echo "$sha" | cut -c1-10)
dest=$APP/$name
say "building $REF ($sha) as $name"

mkdir -p "$APP"
rm -rf "$dest.tmp"
mkdir "$dest.tmp"
sh "$SRC/deploy/build.sh" "$name" "$dest.tmp/muliao"
sign_program "$dest.tmp/muliao" || die "signing $dest.tmp/muliao failed"
"$dest.tmp/muliao" version > /dev/null || die "signed program does not run"
echo "$sha" > "$dest.tmp/COMMIT"
mv "$dest.tmp" "$dest"
prev=$(cat "$APP/current/COMMIT" 2>/dev/null || true)
activate "$dest" || die "could not activate $dest"
say "installed $sha into $dest (previous: ${prev:-none}); restart to apply: deploy/activate.sh --restart-only"

# 只留最近几个版本目录（服务跑的是 current 里的副本，删版本目录不影响它）
ls -1d "$APP"/2*-* 2>/dev/null | sort -r | tail -n +"$((KEEP_VERSIONS + 1))" | while read -r old; do
  rm -rf "$old"
  say "removed old version $old"
done
