-- 工作区领域的表。时间都是 Unix 毫秒（UTC）。

-- 后台任务（输出在 agent 工作目录的 jobs/<id>.log）。state：running / exited / killed / interrupted；exit_code 只在 exited / killed 时有。
-- reported：结局是否已作为事件交回发起会话（没交回的在启动时补交，交回是幂等的）。
CREATE TABLE job (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  agent_id   TEXT NOT NULL,
  session_id TEXT NOT NULL,
  label      TEXT NOT NULL,
  state      TEXT NOT NULL,
  exit_code  INTEGER,
  reported   INTEGER NOT NULL,
  started_at INTEGER NOT NULL,
  ended_at   INTEGER
);
CREATE INDEX job_agent ON job(agent_id, id);
CREATE INDEX job_open ON job(state, reported);

-- skill 脚本的「始终允许」：同一 agent 以同样的参数和 secret 运行同一脚本，且 skill 里除 SKILL.md 外的文件都没变（files_hash）时不再问。
-- args、secrets 是 JSON 数组（secrets 排好序）。
CREATE TABLE script_grant (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  agent_id   TEXT NOT NULL,
  skill      TEXT NOT NULL,
  script     TEXT NOT NULL,
  args       TEXT NOT NULL,
  secrets    TEXT NOT NULL,
  files_hash TEXT NOT NULL,
  created_at INTEGER NOT NULL
);
CREATE UNIQUE INDEX script_grant_key ON script_grant(agent_id, skill, script, args, secrets, files_hash);
