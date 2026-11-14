-- agent 的身份与生命周期。可编辑的配置（描述、模型等）在资源目录的 agent.toml 里，不在这里。
CREATE TABLE agent (
    id           TEXT    PRIMARY KEY,           -- 不变的内部编号
    name         TEXT    NOT NULL,              -- 显示名，也是资源目录下它的目录名
    name_key     TEXT    NOT NULL,              -- 名字的比较形式（不分大小写），用于查重
    state        TEXT    NOT NULL,              -- active / archived / deleting / deleted
    is_default   INTEGER NOT NULL DEFAULT 0,
    renamed_from TEXT,                          -- 改名后资源目录还没挪完时的旧名
    created_at   INTEGER NOT NULL,              -- epoch 毫秒
    updated_at   INTEGER NOT NULL
);

-- 名字在未删除的 agent 里唯一；删除后名字可再用（行保留，用量与工作历史仍能查到名字）
CREATE UNIQUE INDEX agent_name_live ON agent (name_key) WHERE state <> 'deleted';

-- 至多一个默认 agent
CREATE UNIQUE INDEX agent_single_default ON agent (is_default) WHERE is_default = 1;
