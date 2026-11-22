#!/usr/bin/env python3
"""Host-side mock of the Messages API for the headless llm-mock and
agent-mock regressions.

mock_server.py STATE_DIR TIMEOUT_SECONDS [llm|agent]

Serves HTTPS on 127.0.0.1:18490 (the guest's 10.0.2.2:18490, named
llm-mock.cubit.internal by the test image's tls.hosts) with the test
certificate from tests/llm/make-pki.sh. Each POST /v1/messages is checked
like the API would (x-api-key, anthropic-version, content-type, a JSON body
with model, max_tokens and stream) and answered with a canned streaming
response chosen by the prompt:

  "hello"   the stream test: message_start, a thinking block, text deltas;
            after the first two deltas it writes STATE_DIR/part1 and holds
            until STATE_DIR/release exists, then sends the rest (ending in
            END-OF-ANSWER), message_delta and message_stop.
  "budget"  message_start with 25 input tokens, then text the client must
            not take: its cost ceiling stops it at message_start.
  "slow"    message_start, then nothing: the client's deadline stops it.

The agent scenario (AGENT-002 step 2) scripts conversations by their first
user text, answering each turn from the conversation so far:

  "agent-diagnose"  turn 1: four tool calls at once: logs_recent
            {"service":"acpi"} and {"service":"llm"}, files_write (no mission
            grants it) and logs_recent {"service":42} (malformed); turn 2
            checks their results (ran, ran, refused, refused) and asks
            files_read for
            notes.ccl in the mission's place (named in the system prompt);
            turn 3 checks the file's text arrived and answers, ending in
            END-OF-DIAGNOSIS.
  "agent-budget"    asks for two tool calls; the mission allows one, so the
            loop must stop (Agent_Stop.Tool_Call_Budget) before running any.

The agent-app scenario (AGENT-002 step 3, agent.app's own window):

  "agent-app-diagnose"  turn 1: logs_recent {"service":"llm"} and
            processes_list {}; turn 2 checks both ran (llm.svc's ready line,
            agent.app in the process list) and answers AGENT-APP-DONE.

Every agent request is checked for the prompt-cache breakpoints (last
tool, system, last block) and for a byte-stable prefix: each turn's tools
and system equal the first turn's, and its messages begin with the
previous turn's messages.

Writes STATE_DIR/requests.json (what each request carried, the key shown
only as whether it matched) and exits 0 when the expected requests came
with the expected key. Never sees a real key: the test types the fake one.
"""
import json
import os
import socket
import ssl
import sys
import threading
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
PKI = HERE / "build" / "pki"
PORT = 18490
EXPECTED_KEY = "sk-ant-test-0000"
API_VERSION = "2026-10-21"
MODEL = "claude-opus-5-5"
HOLD_SECONDS = 60

state_dir = Path(sys.argv[1])
timeout = float(sys.argv[2]) if len(sys.argv) > 2 else 200
scenario = sys.argv[3] if len(sys.argv) > 3 else "llm"
AGENT_MODEL = "claude-opus-5-5"
FILE_MARKER = "AGENT-FILE-MARKER-42"
requests = []
agent_checks = {}
previous_turn = {}
lock = threading.Lock()


def first_text(request):
    """The first user message's text, whether content is a string or blocks."""
    message = request["messages"][0]
    content = message["content"]
    if isinstance(content, str):
        return content
    for block in content:
        if block.get("type") == "text":
            return block["text"]
    return None


def strip_cache(value):
    """value without its cache_control members (they move turn to turn)."""
    if isinstance(value, dict):
        return {k: strip_cache(v) for k, v in value.items() if k != "cache_control"}
    if isinstance(value, list):
        return [strip_cache(v) for v in value]
    return value


def check(name, ok):
    with lock:
        agent_checks[name] = agent_checks.get(name, True) and bool(ok)
    print("llm mock: check %s %s" % (name, "ok" if ok else "FAILED"), flush=True)


def event(name, data):
    return ("event: %s\ndata: %s\n\n" % (name, json.dumps(data, separators=(",", ":")))).encode()


def chunk(data):
    return b"%x\r\n%s\r\n" % (len(data), data)


def message_start(input_tokens, model=MODEL):
    return event("message_start", {
        "type": "message_start",
        "message": {"id": "msg_mock", "type": "message", "role": "assistant", "model": model,
                    "content": [], "stop_reason": None, "stop_sequence": None,
                    "usage": {"input_tokens": input_tokens, "cache_creation_input_tokens": 0,
                              "cache_read_input_tokens": 0, "output_tokens": 1}}})


def delta(index, text):
    return event("content_block_delta", {"type": "content_block_delta", "index": index,
                                         "delta": {"type": "text_delta", "text": text}})


def read_request(conn):
    data = b""
    while b"\r\n\r\n" not in data:
        part = conn.recv(4096)
        if not part:
            return None
        data += part
    head, rest = data.split(b"\r\n\r\n", 1)
    lines = head.decode("latin-1").split("\r\n")
    method, path, version = lines[0].split(" ", 2)
    headers = {}
    for line in lines[1:]:
        name, value = line.split(":", 1)
        headers[name.strip().lower()] = value.strip()
    length = int(headers.get("content-length", "0"))
    while len(rest) < length:
        part = conn.recv(4096)
        if not part:
            break
        rest += part
    return method, path, version, headers, rest[:length]


def handle(conn):
    got = read_request(conn)
    if got is None:
        return
    method, path, version, headers, body = got
    record = {"method": method, "path": path, "version": version,
              "key_matches": headers.get("x-api-key") == EXPECTED_KEY,
              "anthropic_version": headers.get("anthropic-version"),
              "content_type": headers.get("content-type"),
              "host": headers.get("host")}
    try:
        request = json.loads(body)
        record.update(model=request.get("model"), max_tokens=request.get("max_tokens"),
                      stream=request.get("stream"), prompt=first_text(request),
                      turns=len(request["messages"]), valid_json=True)
    except (ValueError, KeyError, IndexError, TypeError):
        record["valid_json"] = False
        request = {}
    with lock:
        requests.append(record)
    print("llm mock: request %s" % json.dumps(record), flush=True)
    if (method != "POST" or path != "/v1/messages" or not record["key_matches"]
            or record["anthropic_version"] != API_VERSION or not record["valid_json"]):
        error = json.dumps({"type": "error", "error": {"type": "authentication_error",
                                                       "message": "invalid x-api-key"}}).encode()
        conn.sendall(b"HTTP/1.1 401 Unauthorized\r\ncontent-type: application/json\r\n"
                     b"content-length: %d\r\nconnection: close\r\n\r\n%s" % (len(error), error))
        return
    conn.sendall(b"HTTP/1.1 200 OK\r\ncontent-type: text/event-stream; charset=utf-8\r\n"
                 b"cache-control: no-cache\r\ntransfer-encoding: chunked\r\nconnection: close\r\n\r\n")
    prompt = record.get("prompt")
    if prompt in ("agent-diagnose", "agent-budget"):
        answer_agent(conn, request, prompt)
    elif prompt == "agent-app-diagnose":
        answer_agent_app(conn, request)
    elif prompt == "hello":
        conn.sendall(chunk(message_start(25) + event("ping", {"type": "ping"})))
        conn.sendall(chunk(
            event("content_block_start", {"type": "content_block_start", "index": 0,
                                          "content_block": {"type": "thinking", "thinking": "",
                                                            "signature": ""}}) +
            event("content_block_delta", {"type": "content_block_delta", "index": 0,
                                          "delta": {"type": "signature_delta", "signature": "c2ln"}}) +
            event("content_block_stop", {"type": "content_block_stop", "index": 0}) +
            event("content_block_start", {"type": "content_block_start", "index": 1,
                                          "content_block": {"type": "text", "text": ""}})))
        conn.sendall(chunk(delta(1, "Hello from the mock")))
        time.sleep(0.2)
        conn.sendall(chunk(delta(1, " Messages API, part one.\n")))
        (state_dir / "part1").write_text("sent\n")
        deadline = time.monotonic() + HOLD_SECONDS
        while not (state_dir / "release").exists() and time.monotonic() < deadline:
            time.sleep(0.1)
        conn.sendall(chunk(delta(1, "Part two, café \"quoted\".\n")))
        conn.sendall(chunk(delta(1, "END-OF-ANSWER")))
        conn.sendall(chunk(
            event("content_block_stop", {"type": "content_block_stop", "index": 1}) +
            event("message_delta", {"type": "message_delta",
                                    "delta": {"stop_reason": "end_turn", "stop_sequence": None},
                                    "usage": {"output_tokens": 12}}) +
            event("message_stop", {"type": "message_stop"})))
        conn.sendall(b"0\r\n\r\n")
    elif prompt == "budget":
        conn.sendall(chunk(message_start(25)))
        time.sleep(1)
        try:
            conn.sendall(chunk(delta(0, "MUST-NOT-ARRIVE")))
        except OSError:
            pass
    elif prompt == "slow":
        conn.sendall(chunk(message_start(25)))
        time.sleep(15)
    else:
        conn.sendall(chunk(event("error", {"type": "error", "error": {
            "type": "invalid_request_error", "message": "unknown prompt"}})) + b"0\r\n\r\n")


def tool_use(index, call_id, name, arguments):
    """A tool_use block streamed as the API does (input in partial_json)."""
    text = json.dumps(arguments, separators=(",", ":"))
    half = len(text) // 2
    return (event("content_block_start", {"type": "content_block_start", "index": index,
                                          "content_block": {"type": "tool_use", "id": call_id,
                                                            "name": name, "input": {}}}) +
            event("content_block_delta", {"type": "content_block_delta", "index": index,
                                          "delta": {"type": "input_json_delta", "partial_json": text[:half]}}) +
            event("content_block_delta", {"type": "content_block_delta", "index": index,
                                          "delta": {"type": "input_json_delta", "partial_json": text[half:]}}) +
            event("content_block_stop", {"type": "content_block_stop", "index": index}))


def text_block(index, text):
    return (event("content_block_start", {"type": "content_block_start", "index": index,
                                          "content_block": {"type": "text", "text": ""}}) +
            delta(index, text) +
            event("content_block_stop", {"type": "content_block_stop", "index": index}))


def thinking_block(index):
    return (event("content_block_start", {"type": "content_block_start", "index": index,
                                          "content_block": {"type": "thinking", "thinking": "",
                                                            "signature": ""}}) +
            event("content_block_delta", {"type": "content_block_delta", "index": index,
                                          "delta": {"type": "signature_delta", "signature": "c2lnLWFnZW50"}}) +
            event("content_block_stop", {"type": "content_block_stop", "index": index}))


def finish(stop_reason, output_tokens):
    return (event("message_delta", {"type": "message_delta",
                                    "delta": {"stop_reason": stop_reason, "stop_sequence": None},
                                    "usage": {"output_tokens": output_tokens}}) +
            event("message_stop", {"type": "message_stop"}))


def tool_results(message):
    """{tool_use_id: (content, is_error)} of a user message."""
    content = message.get("content")
    if not isinstance(content, list):
        return {}
    return {b["tool_use_id"]: (b.get("content"), b.get("is_error", False))
            for b in content if b.get("type") == "tool_result"}


def answer_agent(conn, request, prompt):
    messages = request["messages"]
    turn = (len(messages) + 1) // 2
    tools = [t["name"] for t in request.get("tools", [])]
    system = request.get("system")
    check("agent model", request.get("model") == AGENT_MODEL)
    check("tools are the mission's", tools == ["logs_recent", "processes_list", "files_list", "files_read"])
    check("cache breakpoint on the last tool",
          bool(request.get("tools")) and "cache_control" in request["tools"][-1])
    check("cache breakpoint on the system prompt",
          isinstance(system, list) and "cache_control" in system[-1])
    last = messages[-1]["content"]
    check("cache breakpoint on the last block", isinstance(last, list) and "cache_control" in last[-1])
    check("max_tokens last", list(request.keys())[-1] == "max_tokens")
    check("conversation alternates", all(m["role"] == ("user" if i % 2 == 0 else "assistant")
                                         for i, m in enumerate(messages)))
    stripped = strip_cache(request)
    with lock:
        before = previous_turn.get(prompt)
        previous_turn[prompt] = stripped
    if before is not None:
        check("prefix stable: tools", stripped.get("tools") == before.get("tools"))
        check("prefix stable: system", stripped.get("system") == before.get("system"))
        check("prefix stable: messages", stripped["messages"][:len(before["messages"])] == before["messages"])
    place = None
    if isinstance(system, list):
        for line in system[0]["text"].splitlines():
            if line.startswith("- @"):
                place = line[2:].strip()
    body = message_start(400 * turn, AGENT_MODEL)
    if prompt == "agent-budget":
        body += (thinking_block(0) +
                 tool_use(1, "toolu_budget_1", "logs_recent", {"service": "acpi"}) +
                 tool_use(2, "toolu_budget_2", "logs_recent", {"service": "llm"}) +
                 finish("tool_use", 30))
    elif turn == 1:
        body += (thinking_block(0) + text_block(1, "Checking the ACPI logs first.") +
                 tool_use(2, "toolu_logs_1", "logs_recent", {"service": "acpi"}) +
                 tool_use(3, "toolu_logs_2", "logs_recent", {"service": "llm"}) +
                 tool_use(4, "toolu_write_1", "files_write", {"path": "/etc/x", "text": "pwned"}) +
                 tool_use(5, "toolu_bad_1", "logs_recent", {"service": 42}) +
                 finish("tool_use", 60))
    elif turn == 2:
        results = tool_results(messages[-1])
        acpi = results.get("toolu_logs_1", (None, True))
        check("logs_recent acpi ran (records, or logs.recent's own typed failure)",
              isinstance(acpi[0], str) and (acpi[0].startswith("[") or "logs.recent" in acpi[0]))
        logs = results.get("toolu_logs_2", (None, True))
        check("logs_recent llm ran with the console's log-observer grant",
              logs[1] is False and isinstance(logs[0], str) and logs[0].startswith("[") and "llm: ready" in logs[0])
        write = results.get("toolu_write_1", ("", False))
        check("files_write refused as not granted", write[1] is True and "not a tool of this" in write[0])
        bad = results.get("toolu_bad_1", ("", False))
        check("malformed input refused", bad[1] is True and "wrong type at service" in bad[0])
        check("a place in the system prompt", place is not None)
        body += (text_block(0, "Now the notes.") +
                 tool_use(1, "toolu_read_1", "files_read",
                          {"child": {"place": {"root": place or "", "path": ""}, "name": "notes.ccl"},
                           "offset": 0, "length": 4096}) +
                 finish("tool_use", 40))
    else:
        results = tool_results(messages[-1])
        read = results.get("toolu_read_1", ("", True))
        check("files_read ran in the mission's place", read[1] is False and FILE_MARKER in (read[0] or ""))
        body += (text_block(0, "DIAGNOSIS: the ACPI log and the notes agree. END-OF-DIAGNOSIS") +
                 finish("end_turn", 20))
    conn.sendall(chunk(body) + b"0\r\n\r\n")


def answer_agent_app(conn, request):
    messages = request["messages"]
    turn = (len(messages) + 1) // 2
    tools = [t["name"] for t in request.get("tools", [])]
    check("app: model", request.get("model") == AGENT_MODEL)
    check("app: tools are the Diagnose mission's",
          tools == ["logs_recent", "processes_list", "files_list", "files_read"])
    check("app: cache breakpoint on the last block",
          isinstance(messages[-1]["content"], list) and "cache_control" in messages[-1]["content"][-1])
    body = message_start(400 * turn, AGENT_MODEL)
    if turn == 1:
        body += (thinking_block(0) + text_block(1, "Looking at llm.svc and what runs.") +
                 tool_use(2, "toolu_app_logs", "logs_recent", {"service": "llm"}) +
                 tool_use(3, "toolu_app_ps", "processes_list", {}) +
                 finish("tool_use", 40))
    else:
        results = tool_results(messages[-1])
        logs = results.get("toolu_app_logs", (None, True))
        check("app: logs_recent llm ran", logs[1] is False and isinstance(logs[0], str) and "llm: ready" in logs[0])
        ps = results.get("toolu_app_ps", (None, True))
        check("app: processes_list ran and lists agent.app",
              ps[1] is False and isinstance(ps[0], str) and "com.cubit.agent" in ps[0])
        body += (text_block(0, "AGENT-APP-DONE: llm.svc is ready and agent.app is running.") +
                 finish("end_turn", 20))
    conn.sendall(chunk(body) + b"0\r\n\r\n")


def serve(connection, context):
    try:
        with context.wrap_socket(connection, server_side=True) as conn:
            handle(conn)
    except (OSError, ssl.SSLError, ValueError) as error:
        print("llm mock: connection ended: %s" % error, flush=True)


def main():
    state_dir.mkdir(parents=True, exist_ok=True)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_2
    context.load_cert_chain(PKI / "llm.pem", PKI / "llm.key")
    listener = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    listener.bind(("127.0.0.1", PORT))
    listener.listen(8)
    listener.settimeout(0.5)
    print("llm mock: listening on 127.0.0.1:%d" % PORT, flush=True)
    end = time.monotonic() + timeout
    while time.monotonic() < end and not (state_dir / "stop").exists():
        try:
            connection, _ = listener.accept()
        except socket.timeout:
            continue
        threading.Thread(target=serve, args=(connection, context), daemon=True).start()
    listener.close()
    with lock:
        (state_dir / "requests.json").write_text(json.dumps({"requests": requests, "checks": agent_checks},
                                                            indent=1))
        prompts = [r.get("prompt") for r in requests]
        model = AGENT_MODEL if scenario in ("agent", "agent-app") else MODEL
        good = all(r["key_matches"] and r["anthropic_version"] == API_VERSION and r["valid_json"]
                   and r.get("model") == model and r.get("stream") is True for r in requests)
        if scenario == "agent-app":
            app_turns = sorted(r.get("turns") for r in requests if r.get("prompt") == "agent-app-diagnose")
            ok = good and app_turns == [1, 3] and bool(agent_checks) and all(agent_checks.values())
        elif scenario == "agent":
            diagnose_turns = sorted(r.get("turns") for r in requests if r.get("prompt") == "agent-diagnose")
            ok = (good and diagnose_turns == [1, 3, 5] and "agent-budget" in prompts and
                  bool(agent_checks) and all(agent_checks.values()))
        else:
            ok = good and {"hello", "budget", "slow"} <= set(prompts)
    print("llm mock: %s (prompts %s, checks %s)" % ("PASS" if ok else "FAIL", prompts, agent_checks),
          flush=True)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
