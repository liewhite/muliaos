-- #889：config 改成带 kind 的 ADT（JSON 里 kind 是分支名 Schedule / Once / Stream），种类只存在 config 里，删掉 kind 列；
-- 已结束的一次性触发器一律是停用（结束意味着不会再触发，之前留着 enabled = 1）。
UPDATE trigger_def SET enabled = 0 WHERE finished_at IS NOT NULL;

UPDATE trigger_def SET config = CASE kind
  WHEN 'schedule' THEN json_object('kind', 'Schedule', 'cron', json_extract(config, '$.cron'))
  WHEN 'once'     THEN json_object('kind', 'Once', 'at', json_extract(config, '$.at'))
  ELSE json_object(
    'kind', 'Stream',
    'command', json_extract(config, '$.command'),
    'secrets', json(coalesce(json_extract(config, '$.secrets'), '[]')),
    'debounceSeconds', coalesce(json_extract(config, '$.debounceSeconds'), 5.0)
  )
END;

ALTER TABLE trigger_def DROP COLUMN kind;
