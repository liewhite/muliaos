/*
 * muliao（Scala 版）的单文件可执行程序的启动器（#877）。
 *
 * 程序文件 = 这个启动器 + 打进 Mach-O 段 __MULIAO,__payload 的 tar.gz（精简 JRE `runtime/` 与 `muliao.jar`）。
 * 放进段里而不是接在文件末尾，是为了整个文件能被代码签名覆盖：macOS 的「完全磁盘访问」按程序路径加签名记，
 * launchd 起的就是这个文件，它起的 java 子进程（以及 agent 的命令）都算在它名下。
 *
 * 运行时：第一次把载荷解到程序旁边的 `.<程序名>-<载荷编号>/`（先解到临时目录再改名，并发启动也安全），
 * 之后直接用；再以子进程起 `runtime/bin/java ... -jar muliao.jar <参数>`，把收到的终止类信号转给它，
 * 等它退出并带回它的退出码。不 exec：exec 之后进程就成了 java，签名身份跟着变了。
 *
 * 载荷编号 PAYLOAD_ID 由 deploy/build.sh 编译时给出（载荷的 sha256 前 16 位）。
 */
#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#include <mach-o/ldsyms.h>
#include <signal.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef PAYLOAD_ID
#error "PAYLOAD_ID must be defined"
#endif

extern char **environ;

static pid_t child = 0;

static void die(const char *what) {
  fprintf(stderr, "muliao launcher: %s: %s\n", what, strerror(errno));
  exit(70);
}

static void forward(int sig) {
  if (child > 0) kill(child, sig);
}

/* 把内嵌的载荷解到 dir（不存在时）。 */
static void unpack(const char *dir) {
  struct stat st;
  if (stat(dir, &st) == 0) return;

  unsigned long size = 0;
  const uint8_t *data = getsectiondata(&_mh_execute_header, "__MULIAO", "__payload", &size);
  if (data == NULL || size == 0) {
    fprintf(stderr, "muliao launcher: payload section missing\n");
    exit(70);
  }

  char tmp[PATH_MAX];
  snprintf(tmp, sizeof tmp, "%s.tmp-%d", dir, (int)getpid());
  if (mkdir(tmp, 0755) != 0) die("mkdir");

  char cmd[PATH_MAX + 64];
  snprintf(cmd, sizeof cmd, "/usr/bin/tar -xzf - -C '%s'", tmp);
  FILE *tar = popen(cmd, "w");
  if (tar == NULL) die("popen tar");
  if (fwrite(data, 1, size, tar) != size) die("write payload to tar");
  int status = pclose(tar);
  if (status != 0) {
    fprintf(stderr, "muliao launcher: tar exited with status %d\n", status);
    exit(70);
  }
  if (rename(tmp, dir) != 0) {
    /* 别的进程同时解好了：用它的 */
    if (stat(dir, &st) != 0) die("rename unpacked dir");
    char rm[PATH_MAX + 32];
    snprintf(rm, sizeof rm, "/bin/rm -rf '%s'", tmp);
    if (system(rm) != 0) fprintf(stderr, "muliao launcher: could not remove %s\n", tmp);
  }
}

int main(int argc, char **argv) {
  char raw[PATH_MAX], self[PATH_MAX];
  uint32_t len = sizeof raw;
  if (_NSGetExecutablePath(raw, &len) != 0) {
    fprintf(stderr, "muliao launcher: executable path too long\n");
    return 70;
  }
  if (realpath(raw, self) == NULL) die("realpath");

  char dirbuf[PATH_MAX], namebuf[PATH_MAX];
  strlcpy(dirbuf, self, sizeof dirbuf);
  strlcpy(namebuf, self, sizeof namebuf);
  char unpacked[PATH_MAX];
  snprintf(unpacked, sizeof unpacked, "%s/.%s-%s", dirname(dirbuf), basename(namebuf), PAYLOAD_ID);
  unpack(unpacked);

  char java[PATH_MAX], jar[PATH_MAX];
  snprintf(java, sizeof java, "%s/runtime/bin/java", unpacked);
  snprintf(jar, sizeof jar, "%s/muliao.jar", unpacked);

  /* 固定的 JVM 参数：内存耗尽直接退出（launchd 会重启），Scala 3 的 lazy val 仍用 sun.misc.Unsafe */
  const char *fixed[] = {java, "-XX:+ExitOnOutOfMemoryError", "--sun-misc-unsafe-memory-access=allow", "-jar", jar};
  size_t nfixed = sizeof fixed / sizeof fixed[0];
  char **args = calloc(nfixed + (size_t)argc, sizeof(char *));
  if (args == NULL) die("calloc");
  for (size_t i = 0; i < nfixed; i++) args[i] = (char *)fixed[i];
  for (int i = 1; i < argc; i++) args[nfixed + (size_t)i - 1] = argv[i];

  int sigs[] = {SIGTERM, SIGINT, SIGHUP, SIGQUIT};
  for (size_t i = 0; i < sizeof sigs / sizeof sigs[0]; i++) signal(sigs[i], forward);

  int err = posix_spawn(&child, java, NULL, NULL, args, environ);
  if (err != 0) {
    errno = err;
    die("spawn java");
  }

  int status;
  while (waitpid(child, &status, 0) < 0) {
    if (errno != EINTR) die("waitpid");
  }
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
  return 70;
}
