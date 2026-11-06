#!/usr/bin/env python3
"""Linux-hosted cross-check of the mock and the decoder: send one request
to tests/llm/mock_server.py as llm.svc would, and save the raw response.

capture.py PROMPT OUTPUT [KEY]

Then: tests/llm/build/llm_main --decode OUTPUT. Not CuBit: llm.svc builds
its request with Llm_Requests; this mirrors that shape only.
"""
import json
import socket
import ssl
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
prompt, output = sys.argv[1], sys.argv[2]
key = sys.argv[3] if len(sys.argv) > 3 else "sk-ant-test-0000"
body = json.dumps({"model": "claude-opus-5-5", "max_tokens": 1024, "stream": True,
                   "messages": [{"role": "user", "content": prompt}]}).encode()
head = ("POST /v1/messages HTTP/1.1\r\nhost: llm-mock.cubit.internal:18490\r\n"
        "content-type: application/json\r\naccept: text/event-stream\r\n"
        "anthropic-version: 2026-10-21\r\nx-api-key: %s\r\ncontent-length: %d\r\n"
        "connection: close\r\n\r\n" % (key, len(body))).encode()
context = ssl.create_default_context(cafile=str(HERE.parent / "tls" / "build" / "pki" / "ca.pem"))
with socket.create_connection(("127.0.0.1", 18490), timeout=30) as raw:
    with context.wrap_socket(raw, server_hostname="llm-mock.cubit.internal") as conn:
        conn.sendall(head + body)
        data = b""
        while True:
            try:
                part = conn.recv(65536)
            except (socket.timeout, ssl.SSLError):
                break
            if not part:
                break
            data += part
Path(output).write_bytes(data)
print("captured %d bytes" % len(data))
