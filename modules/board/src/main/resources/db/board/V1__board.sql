-- issue 看板的表。时间都是 Unix 毫秒（UTC）。agent 用不变的编号记。

-- issue。owner_id：负责项目的 agent（建时必填，agent 删除后置空）；assignee：`user` 或 agent 编号，空为没指派。
CREATE TABLE issue (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  title       TEXT NOT NULL,
  description TEXT NOT NULL,
  status      TEXT NOT NULL,
  priority    TEXT NOT NULL,
  owner_id    TEXT,
  assignee    TEXT,
  created_by  TEXT NOT NULL,
  created_at  INTEGER NOT NULL,
  updated_at  INTEGER NOT NULL
);
CREATE INDEX issue_updated ON issue(updated_at);

-- 依赖：issue_id 等 upstream_id 结束（done/cancelled）。无环由程序保证。
CREATE TABLE issue_dep (
  issue_id    INTEGER NOT NULL,
  upstream_id INTEGER NOT NULL,
  PRIMARY KEY (issue_id, upstream_id)
);
CREATE INDEX issue_dep_upstream ON issue_dep(upstream_id);

-- 时间线：只增不改。actor：`user`、`system` 或 agent 编号；session_id：发生在哪个会话；activity：发生了什么（JSON）。
CREATE TABLE issue_event (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  issue_id   INTEGER NOT NULL,
  at         INTEGER NOT NULL,
  actor      TEXT NOT NULL,
  session_id TEXT,
  activity   TEXT NOT NULL
);
CREATE INDEX issue_event_issue ON issue_event(issue_id, id);
CREATE INDEX issue_event_at ON issue_event(at);

-- 已请 agent 表态过：issue 会话一次运行结束后停滞，请它表态一次；之后再有动态投给它才清掉，避免反复催。
CREATE TABLE issue_nudge (
  issue_id INTEGER NOT NULL,
  agent_id TEXT NOT NULL,
  PRIMARY KEY (issue_id, agent_id)
);
