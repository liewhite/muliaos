-- 会话与运行领域的表。时间都是 Unix 毫秒（UTC）。

-- 会话。origin：user / trigger / issue / thread；origin_ref：触发器名 / issue 号 / thread 名；
-- parent_id：thread 的发起会话（发起会话删了置空，thread 保留）。
-- pending_reply：已交给模型、还没交回回答的去向（JSON 数组），会话回到空闲时交回。
CREATE TABLE session (
  id            TEXT PRIMARY KEY,
  agent_id      TEXT NOT NULL,
  origin        TEXT NOT NULL,
  origin_ref    TEXT,
  parent_id     TEXT,
  title         TEXT NOT NULL,
  pending_reply TEXT NOT NULL,
  created_at    INTEGER NOT NULL,
  updated_at    INTEGER NOT NULL
);
CREATE INDEX session_agent ON session(agent_id, updated_at);
CREATE INDEX session_updated ON session(updated_at);
CREATE INDEX session_parent ON session(parent_id);
-- issue 会话：每个 (agent, issue) 一个；thread：每个 (发起会话, agent, thread 名) 一个
CREATE UNIQUE INDEX session_issue ON session(agent_id, origin_ref) WHERE origin = 'issue';
CREATE UNIQUE INDEX session_thread ON session(parent_id, agent_id, origin_ref) WHERE origin = 'thread' AND parent_id IS NOT NULL;

-- 排队中的来信：下一次请求模型前被取走，变成时间线里的一条。
CREATE TABLE inbox (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id TEXT NOT NULL,
  letter     TEXT NOT NULL,   -- JSON：来源、正文、附件、回答去向、是否检查
  dedupe_key TEXT,
  created_at INTEGER NOT NULL
);
CREATE INDEX inbox_session ON inbox(session_id, id);

-- 运行。state：running / suspended / finished / failed / stopped。
-- retry_at / retry_note：暂时失败、正在退避时下次重试的时间与原因（界面显示）。
CREATE TABLE run (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id  TEXT NOT NULL,
  state       TEXT NOT NULL,
  error       TEXT,
  retry_at    INTEGER,
  retry_note  TEXT,
  started_at  INTEGER NOT NULL,
  finished_at INTEGER
);
CREATE INDEX run_session ON run(session_id, id);
CREATE INDEX run_state ON run(state);

-- 时间线：kind = context（本次运行上下文）/ letter（来信）/ reply（模型回复）/ compaction（压缩点）/ notice（失败、停止等提示）。
-- body 是各种类的 JSON；search 是可搜的文字（来信正文、回复正文、工具调用参数），不含工具结果。
CREATE TABLE entry (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id  TEXT NOT NULL,
  run_id      INTEGER,
  kind        TEXT NOT NULL,
  body        TEXT NOT NULL,
  search      TEXT NOT NULL,
  started_at  INTEGER NOT NULL,
  finished_at INTEGER NOT NULL
);
CREATE INDEX entry_session ON entry(session_id, id);

-- 一条回复里的工具调用。state：pending / running / suspended / approved / done。
-- pause：挂起时在等什么（JSON）；result：结果（JSON，图片存附件只留引用）。args 已打码。
CREATE TABLE tool_call (
  entry_id    INTEGER NOT NULL,
  call_id     TEXT NOT NULL,
  session_id  TEXT NOT NULL,
  seq         INTEGER NOT NULL,
  name        TEXT NOT NULL,
  args        TEXT NOT NULL,
  state       TEXT NOT NULL,
  pause       TEXT,
  result      TEXT,
  started_at  INTEGER,
  finished_at INTEGER,
  PRIMARY KEY (entry_id, call_id)
);
CREATE INDEX tool_call_session ON tool_call(session_id, state);

-- 进行中的异步工作（会话因此「等待中」）：kind = 登记方定义的种类（job / delegation / wake ...），ref 是它那边的编号。
CREATE TABLE wait (
  id                INTEGER PRIMARY KEY AUTOINCREMENT,
  session_id        TEXT NOT NULL,
  kind              TEXT NOT NULL,
  ref               TEXT NOT NULL,
  label             TEXT NOT NULL,
  heartbeat_seconds INTEGER,
  due_at            INTEGER,
  created_at        INTEGER NOT NULL
);
CREATE INDEX wait_session ON wait(session_id);
CREATE UNIQUE INDEX wait_kind_ref ON wait(kind, ref);

-- 委派：requester 交给 thread 的一件事。mode：notify（回答推给用户）/ report（交回发起方）/ ask（发起方的调用在等）。
-- call_entry / call_id：ask 时发起方挂起的调用。done：已交回结局；result：结局（JSON，ask 时调用还没挂上就先存着）。
CREATE TABLE delegation (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  requester_id TEXT NOT NULL,
  thread_id    TEXT NOT NULL,
  mode         TEXT NOT NULL,
  call_entry   INTEGER,
  call_id      TEXT,
  done         INTEGER NOT NULL,
  result       TEXT,
  created_at   INTEGER NOT NULL
);
CREATE INDEX delegation_thread ON delegation(thread_id, done);

-- 上传的文件（附件）：内容按 sha256 存一份，记录按上传各一条。
CREATE TABLE upload (
  id         TEXT PRIMARY KEY,
  sha256     TEXT NOT NULL,
  name       TEXT NOT NULL,
  media_type TEXT NOT NULL,
  size       INTEGER NOT NULL,
  created_at INTEGER NOT NULL
);

-- token 用量：每次模型请求一条，只增不改（删会话、删 agent 都保留）。
CREATE TABLE usage (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  at          INTEGER NOT NULL,
  agent_id    TEXT NOT NULL,
  session_id  TEXT NOT NULL,
  model       TEXT NOT NULL,
  input       INTEGER NOT NULL,
  output      INTEGER NOT NULL,
  cache_read  INTEGER NOT NULL,
  cache_write INTEGER NOT NULL
);
CREATE INDEX usage_at ON usage(at);
CREATE INDEX usage_session ON usage(session_id);
