#!/usr/bin/python3
# 测试用的最小 MCP server（stdio，一行一条 JSON-RPC）。工具：echo、boom（报错）、secret（回显 TOKEN）、crash（进程退出）、
# image（回一张图）、touch（在给定路径建文件，测写沙箱）。启动时把进程号写进工作目录的 server.pid，往标准错误写一行带 TOKEN 的日志。
import json, os, sys

open("server.pid", "w").write(str(os.getpid()))
sys.stderr.write("starting with token %s\n" % os.environ.get("TOKEN", "")); sys.stderr.flush()

TOOLS = [
    {"name": n, "description": d, "inputSchema": {"type": "object", "properties": p}}
    for n, d, p in [
        ("echo", "Echo text.", {"text": {"type": "string"}}),
        ("boom", "Always fails.", {}),
        ("secret", "Leaks the token.", {}),
        ("crash", "Exits.", {}),
        ("image", "Returns an image.", {}),
        ("touch", "Creates a file.", {"path": {"type": "string"}}),
    ]
]
PNG = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="

def text(t, err=False):
    return {"content": [{"type": "text", "text": t}], "isError": err}

def call(name, args):
    if name == "echo": return text(args.get("text", ""))
    if name == "boom": return text("it broke", True)
    if name == "secret": return text("token=" + os.environ.get("TOKEN", ""))
    if name == "crash": os._exit(3)
    if name == "image": return {"content": [{"type": "image", "data": PNG, "mimeType": "image/png"}]}
    if name == "touch":
        try:
            open(args["path"], "w").write("x"); return text("ok")
        except OSError as e:
            return text("denied: %s" % e, True)
    return text("unknown tool", True)

for line in sys.stdin:
    msg = json.loads(line)
    if "id" not in msg: continue  # 通知
    m, p = msg.get("method"), msg.get("params") or {}
    if m == "initialize":
        result = {"protocolVersion": p.get("protocolVersion"), "capabilities": {"tools": {}}, "serverInfo": {"name": "demo", "version": "1"}}
    elif m == "tools/list":
        result = {"tools": TOOLS}
    elif m == "tools/call":
        result = call(p["name"], p.get("arguments") or {})
    else:
        result = {}
    sys.stdout.write(json.dumps({"jsonrpc": "2.0", "id": msg["id"], "result": result}) + "\n"); sys.stdout.flush()
