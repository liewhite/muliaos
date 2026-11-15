package muliao.migrate

import java.sql.{Connection, DriverManager, ResultSet}
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneOffset}
import scala.util.Try

/** 旧版（Rust 版）库里的行，按需读出；时间换成 Unix 毫秒（UTC）。只读：连接以只读方式打开。 */
object Old:
  final case class Agent(id: String, name: String, isDefault: Boolean, archived: Boolean, deleting: Boolean, createdAt: Long, updatedAt: Long)

  /** origin：user / trigger / delegation / issue；originRef：触发器编号、issue 号。 */
  final case class Session(
      id: String,
      agentId: String,
      title: String,
      origin: String,
      originRef: Option[String],
      parent: Option[String],
      thread: Option[String],
      createdAt: Long,
      updatedAt: Long
  )

  /** outcome：done / stopped / failed，没结束为空。 */
  final case class Run(id: Long, sessionId: String, startedAt: Long, endedAt: Option[Long], outcome: Option[String], error: Option[String])
  final case class Message(id: Long, sessionId: String, runId: Long, payload: String, createdAt: Long)
  /** `through` 及之前的消息由摘要代替；`at` 是写入时会话最后一条消息。 */
  final case class Compaction(id: Long, sessionId: String, through: Long, at: Long, summary: String, createdAt: Long)
  /** mode：ask / plain / report_back */
  final case class ThreadCall(id: Long, parent: String, callId: String, thread: String, mode: String)
  final case class Issue(
      id: Long,
      title: String,
      description: String,
      status: String,
      priority: String,
      owner: Option[String],
      assignee: Option[String],
      creator: String,
      createdAt: Long,
      updatedAt: Long
  )
  final case class Activity(id: Long, issueId: Long, at: Long, actor: String, kind: String, body: Option[String], change: Option[String], sessionId: Option[String])
  final case class Dependency(issueId: Long, upstream: Long)
  final case class Trigger(
      id: Long,
      name: String,
      agentId: String,
      prompt: String,
      kind: String,
      config: String,
      state: Option[String],
      enabled: Boolean,
      lastFiredAt: Option[Long],
      lastSessionId: Option[String],
      precheck: Option[String],
      precheckOutput: Option[String],
      faults: Option[String],
      finishedAt: Option[Long],
      createdAt: Long
  )
  final case class Notification(id: Long, title: String, body: String, sessionId: Option[String], unread: Boolean, createdAt: Long, link: Option[String])
  final case class History(id: Long, agentId: String, day: String, title: String, summary: String, refs: String, importance: Int, createdAt: Long)
  final case class Usage(id: Long, at: Long, agentId: String, sessionId: Option[String], model: String, input: Long, output: Long, cacheRead: Long, cacheWrite: Long)
  final case class File(id: String, sha256: String, name: String, mediaType: String, size: Long, createdAt: Long)
  final case class Push(endpoint: String, p256dh: String, auth: String, createdAt: Long)
  /** status：exited / killed / interrupted（旧版在跑的也可能是别的值，按被中断算） */
  final case class Job(id: Long, sessionId: String, agentId: String, command: String, status: String, exitCode: Option[Int], startedAt: Long, finishedAt: Option[Long])
  final case class Grant(id: Long, agentId: String, skill: String, script: String, args: String, secrets: String, fingerprint: String, createdAt: Long)
  final case class Wake(sessionId: String, dueAt: Long, note: String, createdAt: Long)

  /** 旧版的时间文本：`2026-10-19 13:58:05.487150+00:00` 或 ISO 8601（`…Z`）；不带时区的按 UTC。 */
  def millis(text: String): Long =
    val t = text.trim.replaceFirst(" ", "T")
    Try(OffsetDateTime.parse(t).toInstant)
      .orElse(Try(Instant.parse(t)))
      .orElse(Try(LocalDateTime.parse(t).toInstant(ZoneOffset.UTC)))
      .getOrElse(throw IllegalArgumentException(s"认不出的时间：$text"))
      .toEpochMilli

  def open(file: java.nio.file.Path): Db =
    val props = java.util.Properties()
    props.setProperty("open_mode", "1") // SQLITE_OPEN_READONLY
    Db(DriverManager.getConnection(s"jdbc:sqlite:${file.toAbsolutePath}", props))

  /** 一份旧库（快照）的读取。 */
  final class Db(val conn: Connection) extends AutoCloseable:
    def close(): Unit = conn.close()

    def count(table: String): Long = query(s"SELECT count(*) FROM $table")(_.getLong(1)).head

    def query[A](sql: String, params: Any*)(f: ResultSet => A): List[A] =
      val st = conn.prepareStatement(sql)
      try
        params.zipWithIndex.foreach((p, i) => st.setObject(i + 1, p))
        val rs  = st.executeQuery()
        val out = List.newBuilder[A]
        while rs.next() do out += f(rs)
        out.result()
      finally st.close()

    private def str(rs: ResultSet, c: String): Option[String] = Option(rs.getString(c))
    private def time(rs: ResultSet, c: String): Long           = millis(rs.getString(c))
    private def timeOpt(rs: ResultSet, c: String): Option[Long] = str(rs, c).filter(_.nonEmpty).map(millis)
    private def long(rs: ResultSet, c: String): Option[Long]   = Option(rs.getObject(c)).map(_ => rs.getLong(c))

    def agents: List[Agent] =
      query("SELECT * FROM agents ORDER BY created_at")(r =>
        Agent(r.getString("id"), r.getString("name"), r.getInt("is_default") != 0, str(r, "archived_at").isDefined,
          r.getInt("deleting") != 0, time(r, "created_at"), time(r, "updated_at"))
      )

    def sessions: List[Session] =
      query("SELECT * FROM sessions ORDER BY created_at, id")(r =>
        Session(r.getString("id"), r.getString("agent_id"), r.getString("title"), r.getString("origin"), str(r, "origin_ref"),
          str(r, "parent_session_id"), str(r, "thread"), time(r, "created_at"), time(r, "updated_at"))
      )

    def runs(session: String): List[Run] =
      query("SELECT * FROM runs WHERE session_id = ? ORDER BY id", session)(r =>
        Run(r.getLong("id"), r.getString("session_id"), time(r, "started_at"), timeOpt(r, "ended_at"), str(r, "outcome"), str(r, "error"))
      )

    def messages(session: String): List[Message] =
      query("SELECT id, session_id, run_id, payload, created_at FROM messages WHERE session_id = ? ORDER BY id", session)(r =>
        Message(r.getLong("id"), r.getString("session_id"), r.getLong("run_id"), r.getString("payload"), time(r, "created_at"))
      )

    def compactions(session: String): List[Compaction] =
      query("SELECT * FROM compactions WHERE session_id = ? ORDER BY id", session)(r =>
        Compaction(r.getLong("id"), r.getString("session_id"), r.getLong("through_message_id"), r.getLong("at_message_id"),
          r.getString("summary"), time(r, "created_at"))
      )

    def threadCalls: List[ThreadCall] =
      query("SELECT * FROM thread_calls ORDER BY id")(r =>
        ThreadCall(r.getLong("id"), r.getString("parent_session_id"), r.getString("call_id"), r.getString("thread_session_id"), r.getString("mode"))
      )

    def issues: List[Issue] =
      query("SELECT * FROM issues ORDER BY id")(r =>
        Issue(r.getLong("id"), r.getString("title"), r.getString("description"), r.getString("status"), r.getString("priority"),
          str(r, "owner_agent_id").filter(_.nonEmpty), str(r, "assignee").filter(_.nonEmpty), r.getString("creator"),
          time(r, "created_at"), time(r, "updated_at"))
      )

    def activities: List[Activity] =
      query("SELECT * FROM issue_activities ORDER BY id")(r =>
        Activity(r.getLong("id"), r.getLong("issue_id"), time(r, "created_at"), r.getString("actor"), r.getString("kind"),
          str(r, "body"), str(r, "change"), str(r, "session_id"))
      )

    def dependencies: List[Dependency] =
      query("SELECT issue_id, depends_on_id FROM issue_dependencies")(r => Dependency(r.getLong(1), r.getLong(2)))

    def triggers: List[Trigger] =
      query("SELECT * FROM triggers ORDER BY id")(r =>
        Trigger(r.getLong("id"), r.getString("name"), r.getString("agent_id"), r.getString("prompt"), r.getString("kind"),
          r.getString("config"), str(r, "state"), r.getInt("enabled") != 0, timeOpt(r, "last_fired_at"), str(r, "last_session_id"), str(r, "precheck"),
          str(r, "precheck_output"), str(r, "faults"), timeOpt(r, "finished_at"), time(r, "created_at"))
      )

    def notifications: List[Notification] =
      query("SELECT * FROM notifications ORDER BY id")(r =>
        Notification(r.getLong("id"), r.getString("title"), r.getString("body"), str(r, "session_id"), r.getInt("unread") != 0,
          time(r, "created_at"), str(r, "link").filter(_.nonEmpty))
      )

    def history: List[History] =
      query("SELECT * FROM agent_history ORDER BY id")(r =>
        History(r.getLong("id"), r.getString("agent_id"), r.getString("day"), r.getString("title"), r.getString("summary"),
          r.getString("refs"), r.getInt("importance"), time(r, "created_at"))
      )

    def usage: List[Usage] =
      query("SELECT * FROM model_usage ORDER BY id")(r =>
        Usage(r.getLong("id"), time(r, "created_at"), r.getString("agent_id"), str(r, "session_id"), r.getString("model"),
          r.getLong("input_tokens"), r.getLong("output_tokens"), r.getLong("cache_read_tokens"), r.getLong("cache_write_tokens"))
      )

    def files: List[File] =
      query("SELECT * FROM files ORDER BY created_at")(r =>
        File(r.getString("id"), r.getString("sha256"), r.getString("name"), r.getString("media_type"), r.getLong("size"), time(r, "created_at"))
      )

    def pushes: List[Push] =
      query("SELECT * FROM push_subscriptions")(r => Push(r.getString("endpoint"), r.getString("p256dh"), r.getString("auth"), time(r, "created_at")))

    def jobs: List[Job] =
      query("SELECT * FROM jobs ORDER BY id")(r =>
        Job(r.getLong("id"), r.getString("session_id"), r.getString("agent_id"), r.getString("command"), r.getString("status"),
          long(r, "exit_code").map(_.toInt), time(r, "started_at"), timeOpt(r, "finished_at"))
      )

    def grants: List[Grant] =
      query("SELECT * FROM script_grants ORDER BY id")(r =>
        Grant(r.getLong("id"), r.getString("agent_id"), r.getString("skill"), r.getString("script"), r.getString("args"),
          r.getString("secrets"), r.getString("fingerprint"), time(r, "created_at"))
      )

    /** 某个角色的消息里某种段的段数（与新版条目、调用核对）。 */
    def parts(role: String, tpe: String): Long =
      query(
        "SELECT count(*) FROM messages m, json_each(m.payload, '$.parts') p WHERE json_extract(m.payload, '$.role') = ? AND json_extract(p.value, '$.type') = ?",
        role, tpe
      )(_.getLong(1)).head

    /** 最终失败、被停止或没结束的运行（新版各记一条结局提示）。 */
    def endedBadly: Long = query("SELECT count(*) FROM runs WHERE outcome IS NULL OR outcome IN ('failed', 'stopped')")(_.getLong(1)).head

    def messagesOf(role: String): Long = query("SELECT count(*) FROM messages WHERE json_extract(payload, '$.role') = ?", role)(_.getLong(1)).head

    /** 还在等的唤醒（wake_me）：搬进新版的等待，到点照样唤醒（#881）。 */
    def pendingWakes: List[Wake] =
      query(
        """SELECT h.session_id, w.due_at, w.note, w.created_at FROM wakeups w JOIN session_holds h ON h.id = w.hold_id
          |WHERE h.kind = 'wake' AND h.status = 'active' AND h.ended_at IS NULL ORDER BY w.id""".stripMargin
      )(r => Wake(r.getString(1), time(r, "due_at"), r.getString(3), time(r, "created_at")))

    /** 还在排队、没交给运行的信（迁移不搬：搬进来新版会立刻开运行）。 */
    def queuedMail: Long = query("SELECT count(*) FROM mail WHERE state = 'queued'")(_.getLong(1)).head

    /** 还在进行的后台任务、委派（唤醒另搬）：新版不接。 */
    def activeHolds: Long = query("SELECT count(*) FROM session_holds WHERE status = 'active' AND kind <> 'wake'")(_.getLong(1)).head
