-- 通知与推送领域的表。时间都是 Unix 毫秒（UTC）。

-- 发给用户的通知。session_id：关于哪个会话（打开它即已读、随它删除）；url：点开去的页面（前端路由，系统通知为空）。
-- 会话的未读不另存：有指向它的未读通知就是未读。
CREATE TABLE notification (
  id         INTEGER PRIMARY KEY AUTOINCREMENT,
  title      TEXT NOT NULL,
  body       TEXT NOT NULL,
  session_id TEXT,
  url        TEXT,
  unread     INTEGER NOT NULL,
  created_at INTEGER NOT NULL
);
CREATE INDEX notification_unread_session ON notification(session_id) WHERE unread;
CREATE INDEX notification_unread_url ON notification(url) WHERE unread;
CREATE INDEX notification_session ON notification(session_id);

-- 设备的推送订阅（浏览器 PushSubscription）。endpoint 本身就是凭据，不进日志。
CREATE TABLE push_subscription (
  endpoint   TEXT PRIMARY KEY,
  p256dh     TEXT NOT NULL,
  auth       TEXT NOT NULL,
  created_at INTEGER NOT NULL
);
