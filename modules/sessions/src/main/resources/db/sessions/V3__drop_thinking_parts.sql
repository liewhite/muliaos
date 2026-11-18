-- 平台不再请求、保存思考（#950）：把旧回复里的思考段删掉，只留正文与工具调用，顺序不变。
UPDATE entry
SET body = json_set(
  body,
  '$.Reply.parts',
  (SELECT json_group_array(json(p.value) ORDER BY p.key)
   FROM json_each(entry.body, '$.Reply.parts') AS p
   WHERE json_type(p.value, '$.Thinking') IS NULL)
)
WHERE kind = 'reply' AND body LIKE '%"Thinking"%';
