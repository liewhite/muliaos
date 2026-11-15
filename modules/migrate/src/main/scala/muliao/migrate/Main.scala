package muliao.migrate

import muliao.contract.OriginKind
import zio.*

import java.nio.file.{Files, Path, Paths}
import java.time.ZoneId
import scala.collection.mutable

/** 一次性迁移工具：只读地把旧版（Rust 版）muliao 的数据复制并转换进新版的主目录。
  *
  * {{{
  * migrate <旧版主目录 ~/.muliao> <新版主目录 ~/.muliao-scala> [--no-workspaces]
  * }}}
  *
  * 做的事（新版主目录必须还不存在或是空的，不覆盖任何东西）：
  *   1. 把旧库在线备份成快照（`migration/source.db`），之后只读快照；
  *   2. 用新版各领域的迁移建库，在一个事务里写入全部转换后的数据（失败整体回滚）；
  *   3. APFS 克隆复制附件、资源目录、各 agent 工作目录与推送私钥；
  *   4. 生成新版配置（端口 1081、数据目录指向新目录；secret 只随资源目录复制密文，任何密钥值不打印）；
  *   5. 逐表核对条数，报告写进 `migration/report.md` 并打印。
  *
  * 触发器一律停用；旧版排队中的信、进行中的异步工作（后台任务、委派、唤醒、等批准）不搬，新版启动后没有要续跑的东西。
  * `--no-workspaces` 只在开发时试跑用（工作目录约 80G）。
  */
object Main extends ZIOAppDefault:

  def run: ZIO[ZIOAppArgs, Any, Any] =
    getArgs.flatMap { args =>
      val flags = args.filter(_.startsWith("--")).toSet
      args.filterNot(_.startsWith("--")) match
        case Chunk(from, to) if flags.subsetOf(Set("--no-workspaces")) =>
          ZIO
            .attemptBlocking(migrate(Layout(home(from), home(to)), workspaces = !flags("--no-workspaces")))
            .foldZIO(
              e => Console.printLineError(s"迁移失败：${e.getMessage}").ignore.as(ExitCode.failure),
              report => Console.printLine(report).ignore.as(ExitCode.success)
            )
        case _ =>
          Console.printLineError("用法：migrate <旧版主目录> <新版主目录> [--no-workspaces]").ignore.as(ExitCode(2))
    }

  private def home(p: String): Path =
    Paths.get(if p.startsWith("~/") then java.lang.System.getProperty("user.home") + p.drop(1) else p).toAbsolutePath.normalize

  private def fail(msg: String): Nothing = throw IllegalStateException(msg)

  def migrate(layout: Layout, workspaces: Boolean): String =
    val started = java.lang.System.currentTimeMillis()
    if Files.exists(layout.newHome) && Files.list(layout.newHome).findAny().isPresent then
      fail(s"${layout.newHome} 已存在且不是空的；迁移不覆盖，确认不要了先删掉它")
    val old = OldConfig.load(layout.oldConfig).fold(fail, identity)
    val oldDb = old.dataDir.resolve("muliao.db")
    val watched = List(layout.oldHome, old.dataDir, layout.oldAssets, old.dataDir.resolve("files"), old.dataDir.resolve("workspaces"))
    val mtimesBefore = watched.map(p => p -> Files.getLastModifiedTime(p))

    // 1. 快照
    Files.createDirectories(layout.snapshot.getParent)
    Snapshot.take(oldDb, layout.snapshot)

    // 2. 建库、转换、写入
    Files.createDirectories(layout.newData)
    zio.Unsafe.unsafe { implicit u =>
      Runtime.default.unsafe.run(NewDb.create(layout.newDb)).getOrThrowFiberFailure()
    }
    val zone   = old.str("timezone").map(ZoneId.of).getOrElse(ZoneId.of("Asia/Shanghai"))
    val src    = Old.open(layout.snapshot)
    val dst    = NewDb.open(layout.newDb)
    val report = Report()
    try
      convertAll(src, dst, zone, started, report)
      dst.commit()
      report.counted(dst)
    finally
      src.close()
      dst.close()

    // 3. 文件
    val copies = List(
      Copy.tree(old.dataDir.resolve("files"), layout.newData.resolve("files")),
      Copy.tree(layout.oldAssets, layout.newAssets),
      Copy.file(old.dataDir.resolve("vapid_private.pem"), layout.newData.resolve("vapid_private.pem"))
    ) ++ Option.when(workspaces)(Copy.tree(old.dataDir.resolve("workspaces"), layout.newData.resolve("workspaces"))).toList
    if !workspaces then Files.createDirectories(layout.newData.resolve("workspaces")): Unit

    // 4. 配置
    val (config, skippedKeys) = NewConfig.render(old, layout).fold(fail, identity)
    NewConfig.write(layout.newConfig, config)

    // 5. 核对与报告
    val mtimesAfter = watched.map(p => p -> Files.getLastModifiedTime(p))
    val text = report.render(layout, copies, skippedKeys, mtimesBefore, mtimesAfter, workspaces, java.lang.System.currentTimeMillis() - started)
    Files.writeString(layout.reportFile, text)
    text

  private def convertAll(src: Old.Db, dst: NewDb.Writer, zone: ZoneId, now: Long, report: Report): Unit =
    // agent
    val agents = src.agents
    report.table("agents", agents.size, "agent", dst.insert("agent", agents.map(Convert.agent)))
    val agentName = agents.map(a => a.id -> a.name).toMap

    // 附件
    val files = src.files
    report.table("files", files.size, "upload", dst.insert("upload", files.map(Convert.upload)))
    val fileInfo = files.map(f => f.id -> FileInfo(f.sha256, f.mediaType, f.name)).toMap

    // 触发器（会话的来源要用名字）
    val triggers = src.triggers
    val converted = triggers.map(t => Convert.trigger(t, zone, now))
    converted.collect { case Left(e) => e }.foreach(report.problem)
    val ok = converted.collect { case Right(v) => v }
    report.table("triggers", triggers.size, "trigger_def", dst.insert("trigger_def", ok.map(_._1)), "全部停用")
    report.table("triggers.faults", ok.map(_._2.size).sum, "trigger_fault", dst.insert("trigger_fault", ok.flatMap(_._2)))
    val triggerName = triggers.map(t => t.id -> t.name).toMap

    // 会话与时间线
    val sessions     = src.sessions
    val sessionAgent = sessions.map(s => s.id -> agentName.getOrElse(s.agentId, "?")).toMap
    val origins      = dedupeIssueSessions(sessions.map(s => Transcript.origin(s, triggerName, sessionAgent)), report)
    report.table("sessions", sessions.size, "session", dst.insert("session", origins.map(_.row)))
    var nextEntry = 1L
    var stats     = Stats()
    var runs, entries, calls = 0L
    val callAt = mutable.Map.empty[(String, String), (Long, Long)] // (会话, 调用) → (条目, 时间)
    origins.zip(sessions).foreach { (o, s) =>
      val c = Transcript.convert(o, src.runs(s.id), src.messages(s.id), src.compactions(s.id), fileInfo, nextEntry)
      runs += dst.insert("run", c.runs)
      entries += dst.insert("entry", c.entries)
      calls += dst.insert("tool_call", c.calls)
      nextEntry += c.entries.size
      stats = stats + c.stats
      val time = c.entries.map(e => e.id -> e.startedAt).toMap
      c.callEntry.foreach((call, entry) => callAt.update((s.id, call), (entry, time.getOrElse(entry, 0L))))
    }
    report.table("runs", src.count("runs"), "run", runs, s"没结束的 ${stats.unfinishedRuns} 次记成被停止")
    report.table("messages（助手）", src.messagesOf("assistant"), "entry（回复）", stats.replies)
    report.table("messages（用户）的文字段", src.parts("user", "text"), "entry（上下文 + 有文字的来信）", stats.contexts + stats.letters - stats.fileOnlyLetters,
      s"上下文 ${stats.contexts}、来信 ${stats.letters}（其中只有附件的 ${stats.fileOnlyLetters}）；解不开的消息 ${stats.badPayloads}")
    report.table("messages（用户）的附件段", src.parts("user", "file"), "来信里的附件", stats.attachments)
    report.table("compactions", src.count("compactions"), "entry（压缩点）", stats.compactions)
    report.table("runs 里失败、被停止、没结束的", src.endedBadly, "entry（运行结局提示）", stats.notices - stats.badPayloads)
    report.table("messages 里的工具调用段", src.parts("assistant", "tool_call"), "tool_call", calls,
      s"旧版没有结果的 ${stats.missingResults} 个补了说明")
    report.table("messages 里的工具结果段", src.parts("user", "tool_result"), "tool_call 的结果", stats.results,
      s"找不到对应调用的结果 ${stats.orphanResults}")
    report.entries(entries)
    report.stats(stats)

    // 委派
    val threadCalls = src.threadCalls
    val delegations = threadCalls.flatMap { t =>
      val hit = callAt.get((t.parent, t.callId))
      Convert.delegation(t, hit.map(_._1), hit.map(_._2).getOrElse(0L))
    }
    report.table("thread_calls", threadCalls.size, "delegation", dst.insert("delegation", delegations), "都记成已交回；回答推给用户的不记")

    // 看板
    val issues = src.issues.map(Convert.issue)
    issues.collect { case Left(e) => e }.foreach(report.problem)
    report.table("issues", issues.size, "issue", dst.insert("issue", issues.collect { case Right(r) => r }))
    val deps = src.dependencies
    report.table("issue_dependencies", deps.size, "issue_dep", dst.insert("issue_dep", deps.map(Convert.dependency)))
    val acts   = src.activities
    val events = acts.map(Convert.event)
    events.collect { case Left(e) => e }.foreach(report.problem)
    val unsettled = acts.count(_.kind == "unsettled")
    val legacy    = acts.count(a => a.change.exists(c => Convert.legacyStatus.keys.exists(k => c.contains(s"\"$k\""))))
    report.table("issue_activities", acts.size, "issue_event", dst.insert("issue_event", events.collect { case Right(Some(r)) => r }),
      s"「请 agent 表态」$unsettled 条新版不记在时间线上；早期状态 todo/waiting 按 in_progress/blocked 记的 $legacy 条")

    // 其他
    val notes = src.notifications
    report.table("notifications", notes.size, "notification", dst.insert("notification", notes.map(Convert.notification)))
    val pushes = src.pushes
    report.table("push_subscriptions", pushes.size, "push_subscription", dst.insert("push_subscription", pushes.map(Convert.push)))
    val hist = src.history.map(Convert.history)
    hist.collect { case Left(e) => e }.foreach(report.problem)
    report.table("agent_history", hist.size, "history_entry", dst.insert("history_entry", hist.collect { case Right(r) => r }))
    val usage = src.usage
    report.table("model_usage", usage.size, "usage", dst.insert("usage", usage.map(Convert.usage)),
      s"会话已删的 ${usage.count(_.sessionId.isEmpty)} 行会话记空")
    val jobs = src.jobs
    report.table("jobs", jobs.size, "job", dst.insert("job", jobs.map(Convert.job)), "只搬记录，都算结局已交回")
    val grants = src.grants
    report.table("script_grants", grants.size, "script_grant", dst.insert("script_grant", grants.map(Convert.grant)))

    report.skipped("mail（排队中）", src.queuedMail, "不搬：搬进来新版会立刻开运行")
    report.skipped("session_holds（进行中）", src.activeHolds, "不搬：后台任务、委派、唤醒由旧版自己收尾")
    report.skipped("paused_calls", src.count("paused_calls"), "不搬：挂起中的批准随运行结束")
    report.skipped("wakeups", src.count("wakeups"), "不搬")

  /** 新版每个 (agent, issue) 只有一个 issue 会话：旧版重复的，保留最近更新的那个，其余记成用户会话。 */
  private def dedupeIssueSessions(origins: List[SessionOrigin], report: Report): List[SessionOrigin] =
    val keep = origins
      .filter(_.row.origin == OriginKind.Issue)
      .groupBy(o => (o.row.agentId, o.row.originRef))
      .values
      .map(_.maxBy(_.row.updatedAt).row.id)
      .toSet
    origins.map { o =>
      if o.row.origin == OriginKind.Issue && !keep(o.row.id) then
        report.problem(s"会话 ${o.row.id.value}：同一 agent 处理 issue #${o.row.originRef.getOrElse("?")} 有多个会话，较早的这个记成用户会话")
        o.copy(row = o.row.copy(origin = OriginKind.User, originRef = None))
      else o
    }

/** 旧库的一致快照：SQLite 在线备份（读的是只读连接，原库不写）。 */
object Snapshot:
  def take(from: Path, to: Path): Unit =
    val db = Old.open(from)
    try
      val st = db.conn.createStatement()
      try st.executeUpdate(s"backup to ${to.toAbsolutePath}"): Unit
      finally st.close()
    finally db.close()

/** 逐表核对的记录与报告。 */
final class Report:
  private final case class Row(old: String, oldCount: Long, target: String, written: Long, note: String)
  private val rows     = mutable.ListBuffer.empty[Row]
  private val skips    = mutable.ListBuffer.empty[(String, Long, String)]
  private val problems = mutable.ListBuffer.empty[String]
  private var inDb     = Map.empty[String, Long]
  private var entryKinds = 0L
  private var st       = Stats()

  def table(old: String, oldCount: Long, target: String, written: Long, note: String = ""): Unit =
    rows += Row(old, oldCount, target, written, note)
  def skipped(what: String, n: Long, why: String): Unit = skips += ((what, n, why))
  def problem(p: String): Unit = problems += p
  def entries(n: Long): Unit = entryKinds = n
  def stats(s: Stats): Unit = st = s

  /** 提交后从新库数一遍，和写入时的条数对照。 */
  def counted(dst: NewDb.Writer): Unit =
    inDb = List("agent", "upload", "trigger_def", "trigger_fault", "session", "run", "entry", "tool_call", "delegation", "issue", "issue_dep",
      "issue_event", "notification", "push_subscription", "history_entry", "usage", "job", "script_grant").map(t => t -> dst.count(t)).toMap

  def render(
      layout: Layout,
      copies: List[Copy.Outcome],
      skippedKeys: List[String],
      before: List[(Path, java.nio.file.attribute.FileTime)],
      after: List[(Path, java.nio.file.attribute.FileTime)],
      workspaces: Boolean,
      tookMs: Long
  ): String =
    val b = StringBuilder()
    b ++= s"# 迁移报告\n\n旧版 ${layout.oldHome} → 新版 ${layout.newHome}，用时 ${tookMs / 1000} 秒。\n\n"
    b ++= "## 逐表条数\n\n| 旧表 | 旧条数 | 新表 | 写入 | 新库里 | 说明 |\n|---|---:|---|---:|---:|---|\n"
    rows.foreach { r =>
      val table = r.target.takeWhile(c => c != '（')
      val db    = if r.target.contains('（') then "" else inDb.get(table).fold("")(_.toString)
      b ++= s"| ${r.old} | ${r.oldCount} | ${r.target} | ${r.written} | $db | ${r.note} |\n"
    }
    b ++= s"\n时间线条目合计 $entryKinds 条（新库里 ${inDb.getOrElse("entry", 0L)}）= 回复 ${st.replies} + 来信 ${st.letters} + 上下文 ${st.contexts} + 压缩 ${st.compactions} + 提示 ${st.notices}。\n"
    b ++= "\n## 没搬的\n\n"
    skips.foreach((w, n, why) => b ++= s"- $w：$n 条，$why\n")
    b ++= "\n## 文件\n\n"
    copies.foreach(c => b ++= s"- ${c.from} → ${c.to}：${if c.ok then "完成" else s"有 ${c.problems.size} 处出错"}\n" + c.problems.take(20).map(p => s"  - $p\n").mkString)
    if !workspaces then b ++= "- 工作目录：按 --no-workspaces 跳过（只建了空目录）\n"
    b ++= s"\n## 配置\n\n已生成 ${layout.newConfig}（权限 600，端口 ${NewConfig.port}；登录签名密钥新生成，要重新登录）。"
    b ++= (if skippedKeys.isEmpty then "旧配置的键都已对应。\n" else s"旧配置里没搬的键：${skippedKeys.mkString("、")}。\n")
    b ++= "\n## 原目录\n\n"
    before.zip(after).foreach { case ((p, t0), (_, t1)) => b ++= s"- $p 修改时间${if t0 == t1 then "未变" else s"变了：$t0 → $t1（旧版服务在跑时数据目录会自己变）"}\n" }
    b ++= s"\n## 问题（${problems.size}）\n\n"
    problems.foreach(p => b ++= s"- $p\n")
    b.result()
