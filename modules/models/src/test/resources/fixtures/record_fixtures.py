# 录制 #867 测试用的协议响应（原始 SSE 字节），密钥从本机 Rust 版配置读、不打印。
# 用法：uv run --no-project python record_fixtures.py <输出目录> <anthropic 模型> <场景> [openai 模型]
#   场景：anthropic | openai | all（工具调用）、think-anthropic | think-openai（模型自己回思考/推理，验证读流时丢掉）
import json, sys, tomllib, urllib.request, pathlib

cfg = tomllib.load(open(pathlib.Path.home() / ".muliao/config.toml", "rb"))
P = cfg["llm"]["providers"]
out = pathlib.Path(sys.argv[1]); out.mkdir(parents=True, exist_ok=True)
UA = "Mozilla/5.0 (Macintosh) muliao-fixture-recorder"

TOOL = {"name": "get_weather", "description": "Get the current weather for a city.",
        "schema": {"type": "object", "properties": {"city": {"type": "string"}}, "required": ["city"], "additionalProperties": False}}

def post(url, headers, body, name):
    req = urllib.request.Request(url, data=json.dumps(body).encode(), method="POST",
                                 headers={**headers, "content-type": "application/json", "user-agent": UA})
    try:
        with urllib.request.urlopen(req, timeout=180) as r:
            data = r.read()
    except urllib.error.HTTPError as e:
        print(name, "HTTP", e.code, e.read()[:300]); return
    (out / name).write_bytes(data)
    print(name, len(data), "bytes")

def anthropic(name, messages):
    p = P["claude"]
    body = {"model": sys.argv[2] if len(sys.argv) > 2 else "claude-sonnet-4-5", "max_tokens": 2048, "stream": True,
            "system": [{"type": "text", "text": "Be brief."}],
            "tools": [{"name": TOOL["name"], "description": TOOL["description"], "input_schema": TOOL["schema"]}],
            "messages": messages}
    post(p["base_url"].rstrip("/") + "/v1/messages", {"x-api-key": p["api_key"], "anthropic-version": "2023-06-01"}, body, name)

def responses(name, provider, model, input_):
    p = P[provider]
    body = {"model": model, "stream": True, "instructions": "Be brief.", "input": input_,
            "tools": [{"type": "function", "name": TOOL["name"], "description": TOOL["description"], "parameters": TOOL["schema"], "strict": False}]}
    post(p["base_url"].rstrip("/") + "/responses", {"Authorization": "Bearer " + p["api_key"]}, body, name)

ask = "What's the weather in Paris? Use the tool."
which = sys.argv[3] if len(sys.argv) > 3 else "all"
if which in ("all", "anthropic"):
    anthropic("anthropic-tool-call.sse", [{"role": "user", "content": ask}])
if which in ("all", "openai"):
    responses("openai-tool-call.sse", "openai", sys.argv[4] if len(sys.argv) > 4 else "gpt-5-mini",
              [{"role": "user", "content": [{"type": "input_text", "text": ask}]}])

think = "How many integers n with 1 <= n <= 60 make n^2 + n + 41 composite? Think carefully, verify each candidate, then give only the number."
if which == "think-anthropic":
    p = P["claude"]
    body = {"model": sys.argv[2], "max_tokens": 4096, "stream": True, "messages": [{"role": "user", "content": think}],
            "thinking": {"type": "adaptive", "display": "summarized"}}
    post(p["base_url"].rstrip("/") + "/v1/messages", {"x-api-key": p["api_key"], "anthropic-version": "2023-06-01"}, body, "anthropic-thinking.sse")
if which == "think-openai":
    p = P["openai"]
    body = {"model": sys.argv[4], "stream": True, "input": [{"role": "user", "content": [{"type": "input_text", "text": think}]}],
            "reasoning": {"summary": "auto", "effort": "medium"}, "include": ["reasoning.encrypted_content"]}
    post(p["base_url"].rstrip("/") + "/responses", {"Authorization": "Bearer " + p["api_key"]}, body, "openai-reasoning.sse")
