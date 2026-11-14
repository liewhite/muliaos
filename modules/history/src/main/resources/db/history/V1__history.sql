-- 工作历史：agent 记下的「做过的事」。只能追加：库里用触发器拒绝改、删（删 agent 也保留）。
-- day：做这件事的日期（配置时区的日历日，YYYY-MM-DD）；refs：引用（JSON 字符串数组，如 issue 号、会话编号、提交）；
-- importance：1 日常、2 值得注意、3 重大；session_id：在哪个会话里记的。时间是 Unix 毫秒（UTC）。
CREATE TABLE history_entry (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  agent_id   TEXT NOT NULL,
  day        TEXT NOT NULL,
  title      TEXT NOT NULL,
  summary    TEXT NOT NULL,
  refs       TEXT NOT NULL,
  importance INTEGER NOT NULL,
  session_id TEXT,
  created_at INTEGER NOT NULL
);
CREATE INDEX history_day ON history_entry(day);
CREATE INDEX history_agent_day ON history_entry(agent_id, day);

CREATE TRIGGER history_entry_no_update BEFORE UPDATE ON history_entry
BEGIN
  SELECT RAISE(ABORT, '工作历史只能追加，不能修改');
END;

CREATE TRIGGER history_entry_no_delete BEFORE DELETE ON history_entry
BEGIN
  SELECT RAISE(ABORT, '工作历史只能追加，不能删除');
END;
