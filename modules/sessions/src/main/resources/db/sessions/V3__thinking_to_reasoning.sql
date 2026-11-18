-- 平台不再展示思考，思考块只作协议数据原样回传（#950）：旧回复里的 Thinking 段换成 Reasoning（整块原样，按各协议回传的格式），
-- 没有协议数据的（生成到一半被打断的）删掉，别的段原样、顺序不变。
UPDATE entry
SET body = json_set(
  body,
  '$.Reply.parts',
  (SELECT json_group_array(
            CASE
              WHEN json_type(p.value, '$.Thinking') IS NULL THEN json(p.value)
              WHEN json_extract(p.value, '$.Thinking.seal.protocol') = 'anthropic'
                   AND json_type(p.value, '$.Thinking.seal.data.signature') IS NOT NULL
                THEN json_object('Reasoning', json_object('block', json_object('protocol', 'anthropic', 'data', json_object(
                       'type', 'thinking',
                       'thinking', json_extract(p.value, '$.Thinking.text'),
                       'signature', json_extract(p.value, '$.Thinking.seal.data.signature')))))
              WHEN json_extract(p.value, '$.Thinking.seal.protocol') = 'anthropic'
                THEN json_object('Reasoning', json_object('block', json_object('protocol', 'anthropic', 'data', json_object(
                       'type', 'redacted_thinking',
                       'data', json_extract(p.value, '$.Thinking.seal.data.redacted')))))
              ELSE json_object('Reasoning', json_object('block', json_object('protocol', 'openai-responses', 'data', json_patch(
                       json_object(
                         'type', 'reasoning',
                         'id', json_extract(p.value, '$.Thinking.seal.data.id'),
                         'summary', CASE
                                      WHEN json_extract(p.value, '$.Thinking.seal.data.raw') OR json_extract(p.value, '$.Thinking.text') = '' THEN json_array()
                                      ELSE json_array(json_object('type', 'summary_text', 'text', json_extract(p.value, '$.Thinking.text')))
                                    END,
                         'encrypted_content', json_extract(p.value, '$.Thinking.seal.data.encrypted_content')),
                       json_object('content', CASE
                                      WHEN json_extract(p.value, '$.Thinking.seal.data.raw') AND json_extract(p.value, '$.Thinking.text') <> ''
                                        THEN json_array(json_object('type', 'reasoning_text', 'text', json_extract(p.value, '$.Thinking.text')))
                                    END)))))
            END
            ORDER BY p.key)
   FROM json_each(entry.body, '$.Reply.parts') AS p
   WHERE json_type(p.value, '$.Thinking') IS NULL
      OR json_type(p.value, '$.Thinking.seal.data.signature') IS NOT NULL
      OR json_type(p.value, '$.Thinking.seal.data.redacted') IS NOT NULL
      OR (json_extract(p.value, '$.Thinking.seal.protocol') = 'openai-responses'
          AND json_type(p.value, '$.Thinking.seal.data.id') IS NOT NULL))
)
WHERE kind = 'reply' AND body GLOB '*"Thinking"*';
