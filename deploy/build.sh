#!/bin/sh
# 把当前源码树打成一个可直接执行的单文件：用法 deploy/build.sh <版本号> <输出文件>
#
# 1. Vite 构建前端（它自己调 sbt 链接 Scala.js）到 modules/app/target/web-dist/web/
# 2. sbt app/assembly 打 fat jar（带前端，清单写版本号，见 build.sbt 的 app 段）
# 3. jdeps 算出用到的 JDK 模块，jlink 裁出精简 JRE
# 4. JRE 与 jar 打成 tar.gz，编进启动器（deploy/launcher.c）的 Mach-O 段，得到单个文件（未签名，签名由 install.sh 做）
# 依赖：sdkman 装的 JDK 与 sbt（docs/stack.md）、npm、Xcode 命令行工具的 cc。
set -eu
version=${1:?usage: deploy/build.sh <version> <output file>}
out=${2:?usage: deploy/build.sh <version> <output file>}
src=$(cd "$(dirname "$0")/.." && pwd)
work=$src/target/package
# 直接用 sdkman 的当前版本（sdkman-init.sh 要 bash，这里是 sh）
export JAVA_HOME="$HOME/.sdkman/candidates/java/current"
export PATH="$JAVA_HOME/bin:$HOME/.sdkman/candidates/sbt/current/bin:/opt/homebrew/bin:$PATH"

echo "== frontend"
(cd "$src/modules/web" && npm ci --no-audit --no-fund --loglevel=error &&
  npx vite build --logLevel warn --outDir ../app/target/web-dist/web --emptyOutDir)

echo "== fat jar"
(cd "$src" && sbt -batch -Dmuliao.version="$version" app/assembly)
jar=$(ls "$src"/modules/app/target/scala-*/muliao.jar)
unzip -l "$jar" web/index.html > /dev/null || { echo "frontend missing from $jar" >&2; exit 1; }

echo "== runtime"
rm -rf "$work"
mkdir -p "$work/payload"
# jdeps 看不到反射加载的：本地化数据（中文日期格式）、字符集、zip 文件系统、DNS
modules=$(jdeps --multi-release "$(java -XshowSettings:properties -version 2>&1 | awk -F'= ' '/java.specification.version/ {print $2}')" \
  --ignore-missing-deps --print-module-deps -q "$jar"),jdk.localedata,jdk.charsets,jdk.zipfs,jdk.naming.dns
echo "modules: $modules"
jlink --add-modules "$modules" --strip-debug --no-header-files --no-man-pages --compress zip-6 --output "$work/payload/runtime"
cp "$jar" "$work/payload/muliao.jar"

echo "== single file"
(cd "$work/payload" && tar -czf ../payload.tar.gz runtime muliao.jar)
id=$(shasum -a 256 "$work/payload.tar.gz" | cut -c1-16)
cc -O2 -Wall -Werror -DPAYLOAD_ID="\"$id\"" -sectcreate __MULIAO __payload "$work/payload.tar.gz" \
  -o "$out" "$src/deploy/launcher.c"
"$out" version > /dev/null # 解一次载荷并确认能跑（解出的目录在输出文件旁边）
echo "built $out ($(du -h "$out" | cut -f1), payload $id, version $("$out" version))"
