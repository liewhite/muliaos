-- settle_due：运行做完了、还没收尾（取走回答去向、交回委派、发会话事件）。做完运行的事务里置 1，收尾的事务里清 0，
-- 启动时补收尾所有置 1 的会话（#1062，取代内存里的「这轮跑过」标记）。
ALTER TABLE session ADD COLUMN settle_due INTEGER NOT NULL DEFAULT 0;
-- 旧版启动时只补收尾「还有回答去向」的会话：这些照旧补。
UPDATE session SET settle_due = 1 WHERE pending_reply != '[]';
