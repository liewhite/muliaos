-- 触发器领域的表。时间都是 Unix 毫秒（UTC）。

-- 触发器。kind + config（JSON）是怎么触发，读出时一次解析成校验过的类型；precheck 是前置检查（JSON，可空）。
-- next_at：定时、一次性的下次到期（停用期间不看，重新启用时重算）；skipped：上一次的会话没做完而连续跳过的次数；
-- last_session_id：上一次触发开的会话（防重叠看它）；block_notified：已为挡路的这个会话通知过用户；
-- baseline：前置检查 changed 模式的上一次输出；finished_at：一次性的到期被消耗的时间（之后不再触发）。
-- 名字只在没结束的触发器之间唯一。
CREATE TABLE trigger_def (
  id              INTEGER PRIMARY KEY AUTOINCREMENT,
  name            TEXT NOT NULL,
  agent_id        TEXT NOT NULL,
  prompt          TEXT NOT NULL,
  kind            TEXT NOT NULL,
  config          TEXT NOT NULL,
  enabled         INTEGER NOT NULL,
  precheck        TEXT,
  next_at         INTEGER,
  skipped         INTEGER NOT NULL DEFAULT 0,
  last_session_id TEXT,
  last_fired_at   INTEGER,
  block_notified  TEXT,
  baseline        TEXT,
  finished_at     INTEGER,
  created_at      INTEGER NOT NULL,
  updated_at      INTEGER NOT NULL
);
CREATE UNIQUE INDEX trigger_def_live_name ON trigger_def(name) WHERE finished_at IS NULL;
CREATE INDEX trigger_def_agent ON trigger_def(agent_id);

-- 脚本故障：前置检查失败、常驻命令连续很快退出。同一触发器同一来源只留最新一条，来源恢复即删。
CREATE TABLE trigger_fault (
  trigger_id INTEGER NOT NULL,
  source     TEXT NOT NULL,
  error      TEXT NOT NULL,
  at         INTEGER NOT NULL,
  PRIMARY KEY (trigger_id, source)
);
