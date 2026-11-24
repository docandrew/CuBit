#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Host-side HTTPS fixture for the headless net-fetch regression (SELF-003
step 1).

server.py STATE_DIR TIMEOUT_SECONDS

Serves HTTPS on 127.0.0.1:18496 (the guest's 10.0.2.2:18496, named
fetch-test.cubit.internal by the test image's tls.hosts) with the test
certificate from tests/net-fetch/make-pki.sh. Every body is made here at
start (no stored fixtures):

  /files/hello.txt   200, Content-Length: a line of text
  /files/big.bin     200, chunked in uneven pieces: 300000 bytes of a
                     deterministic pattern (several TLS records and reads)
  /files/pkg.tar     200, Content-Length: a tar (Python's tarfile, pax) of
                     pkg/README, pkg/src/main.adb, pkg/sub/deep/x.txt
  /files/evil.tar    200: a tar whose second entry is ../evil.txt
  /redirect/big      302 to the absolute URL of /files/big.bin
  /redirect/loop     302 to itself (an absolute path)
  /missing           404

and git's smart HTTP, protocol version 2 (SELF-003 step 2), for two
repositories made here by the host's git (hermetic: no user config) in
STATE_DIR/repos: sha1.git and, when the host's git can make one, sha256.git
(--object-format=sha256). Each answer is `git upload-pack --stateless-rpc`'s:

  GET  /git/NAME/info/refs?service=git-upload-pack   the advertisement
       (sha1.git's after the "# service=" preamble, sha256.git's without)
  POST /git/NAME/git-upload-pack                       ls-refs and fetch
       (the fetch's answer chunked)
  GET  /git/moved.git/info/refs?service=...            302 to sha1.git's

Before listening it writes STATE_DIR/hashes.json (each body's SHA-256, for
the harness to pin) and STATE_DIR/ready. It writes STATE_DIR/requests.json
at the end and exits 0 when the expected requests came: each path the
harness asks for, and none outside the list.
"""
import hashlib
import io
import json
import os
import shutil
import socket
import ssl
import subprocess
import sys
import tarfile
import threading
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
PKI = HERE / "build" / "pki"
PORT = 18496
NAME = "fetch-test.cubit.internal"

state_dir = Path(sys.argv[1])
timeout = float(sys.argv[2])
lock = threading.Lock()
requests = []


def tar_of(entries):
    out = io.BytesIO()
    with tarfile.open(fileobj=out, mode="w", format=tarfile.PAX_FORMAT) as tar:
        for name, data in entries:
            info = tarfile.TarInfo(name)
            info.mtime = 1700000000
            if data is None:
                info.type = tarfile.DIRTYPE
                tar.addfile(info)
            else:
                info.size = len(data)
                tar.addfile(info, io.BytesIO(data))
    return out.getvalue()


HELLO = b"hello from the net-fetch fixture\n"
BIG = bytes((i * 7 + (i >> 8)) & 0xFF for i in range(300000))
PKG = tar_of([("pkg", None), ("pkg/README", b"NET-FETCH-README\n"),
              ("pkg/src/main.adb", b"procedure Main is begin null; end Main;\n"),
              ("pkg/sub/deep/x.txt", b"NET-FETCH-DEEP-FILE\n")])
EVIL = tar_of([("ok.txt", b"fine\n"), ("../evil.txt", b"NET-FETCH-MUST-NOT-LAND\n")])
BODIES = {"/files/hello.txt": HELLO, "/files/big.bin": BIG, "/files/pkg.tar": PKG, "/files/evil.tar": EVIL}

GIT_ENV = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_SYSTEM="/dev/null", GIT_AUTHOR_NAME="CuBit Test",
               GIT_AUTHOR_EMAIL="test@cubit.invalid", GIT_COMMITTER_NAME="CuBit Test",
               GIT_COMMITTER_EMAIL="test@cubit.invalid", GIT_AUTHOR_DATE="2026-11-23T00:00:00Z",
               GIT_COMMITTER_DATE="2026-11-23T00:00:00Z", LC_ALL="C")
SERVICE = "/info/refs?service=git-upload-pack"
REPOS = {}


def git(*args, cwd=None, input=None, extra=None):
    return subprocess.run(["git", *args], cwd=cwd, input=input, env=dict(GIT_ENV, **(extra or {})),
                          check=True, capture_output=True).stdout


def make_repo(path, object_format):
    """History with deltas, a tag, a branch, a nested file, an executable."""
    git("init", "-q", "-b", "main", "--object-format=" + object_format, str(path))
    text = "".join("line %d of the clone fixture, words to delta\n" % i for i in range(2000))
    (path / "src" / "deep").mkdir(parents=True)
    for i in range(4):
        (path / "notes.txt").write_text(text[: 20000 + 6000 * i] + "edit %d\n" % i)
        (path / "src" / "deep" / "x.txt").write_text("GIT-CLONE-DEEP %s %d\n" % (object_format, i))
        if i == 1:
            (path / "run").write_text("#!/bin/sh\necho hi\n")
            (path / "run").chmod(0o755)
        git("add", "-A", cwd=path)
        git("commit", "-q", "-m", "commit %d" % i, cwd=path)
        if i == 2:
            git("tag", "-a", "v1", "-m", "one", cwd=path)
            git("branch", "side", cwd=path)


def make_repos():
    root = state_dir / "repos"
    shutil.rmtree(root, ignore_errors=True)
    root.mkdir(parents=True)
    make_repo(root / "sha1.git", "sha1")
    REPOS["sha1.git"] = root / "sha1.git"
    try:
        make_repo(root / "sha256.git", "sha256")
        REPOS["sha256.git"] = root / "sha256.git"
    except subprocess.CalledProcessError:
        shutil.rmtree(root / "sha256.git", ignore_errors=True)


def upload_pack(repo, protocol, request=None):
    extra = {"GIT_PROTOCOL": protocol} if protocol else {}
    if request is None:
        return git("upload-pack", "--stateless-rpc", "--advertise-refs", str(repo), extra=extra)
    return git("upload-pack", "--stateless-rpc", str(repo), input=request, extra=extra)


def pkt(text):
    return b"%04x" % (len(text) + 4) + text


def respond(conn, status, reason, headers, body=b""):
    head = "HTTP/1.1 %d %s\r\n" % (status, reason)
    for key, value in headers:
        head += "%s: %s\r\n" % (key, value)
    conn.sendall(head.encode() + b"\r\n" + body)


def handle(conn):
    data = b""
    while b"\r\n\r\n" not in data:
        piece = conn.recv(4096)
        if not piece:
            return
        data += piece
    head, _, rest = data.partition(b"\r\n\r\n")
    line, *fields = head.decode().split("\r\n")
    method, target, version = line.split(" ")
    headers = {k.lower(): v.strip() for k, v in (f.split(":", 1) for f in fields)}
    body = rest
    while len(body) < int(headers.get("content-length", "0")):
        piece = conn.recv(65536)
        if not piece:
            return
        body += piece
    with lock:
        requests.append({"method": method, "target": target, "host": headers.get("host"),
                         "accept_encoding": headers.get("accept-encoding"),
                         "connection": headers.get("connection"), "git_protocol": headers.get("git-protocol"),
                         "content_type": headers.get("content-type")})
    print("fetch fixture: %s %s" % (method, target), flush=True)
    git_name = target.split("/")[2] if target.startswith("/git/") and target.count("/") >= 3 else None
    if version != "HTTP/1.1":
        respond(conn, 400, "Bad Request", [("Content-Length", "0")])
    elif method == "GET" and target == "/git/moved.git" + SERVICE:
        respond(conn, 302, "Found", [("Location", "https://%s:%d/git/sha1.git%s" % (NAME, PORT, SERVICE)),
                                     ("Content-Length", "0")])
    elif method == "GET" and git_name in REPOS and target == "/git/%s%s" % (git_name, SERVICE):
        answer = upload_pack(REPOS[git_name], headers.get("git-protocol"))
        if git_name == "sha1.git":
            answer = pkt(b"# service=git-upload-pack\n") + b"0000" + answer
        respond(conn, 200, "OK", [("Content-Type", "application/x-git-upload-pack-advertisement"),
                                  ("Content-Length", str(len(answer))), ("Connection", "close")], answer)
    elif method == "POST" and git_name in REPOS and target == "/git/%s/git-upload-pack" % git_name:
        answer = upload_pack(REPOS[git_name], headers.get("git-protocol"), body)
        if b"command=fetch" in body:
            respond(conn, 200, "OK", [("Content-Type", "application/x-git-upload-pack-result"),
                                      ("Transfer-Encoding", "chunked"), ("Connection", "close")])
            for offset in range(0, len(answer), 7000):
                piece = answer[offset:offset + 7000]
                conn.sendall(b"%x\r\n" % len(piece) + piece + b"\r\n")
            conn.sendall(b"0\r\n\r\n")
        else:
            respond(conn, 200, "OK", [("Content-Type", "application/x-git-upload-pack-result"),
                                      ("Content-Length", str(len(answer))), ("Connection", "close")], answer)
    elif method != "GET":
        respond(conn, 400, "Bad Request", [("Content-Length", "0")])
    elif target == "/files/big.bin":
        respond(conn, 200, "OK", [("Transfer-Encoding", "chunked"), ("Connection", "close")])
        offset, size = 0, 1
        while offset < len(BIG):
            piece = BIG[offset:offset + size]
            conn.sendall(b"%x;n=%d\r\n" % (len(piece), offset) + piece + b"\r\n")
            offset += len(piece)
            size = size * 3 + 17 if size < 40000 else 1
        conn.sendall(b"0\r\nX-Trailer: done\r\n\r\n")
    elif target in BODIES:
        body = BODIES[target]
        respond(conn, 200, "OK", [("Content-Length", str(len(body))), ("Content-Type", "application/octet-stream"),
                                  ("Connection", "close")], body)
    elif target == "/redirect/big":
        respond(conn, 302, "Found", [("Location", "https://%s:%d/files/big.bin" % (NAME, PORT)),
                                     ("Content-Length", "0")])
    elif target == "/redirect/loop":
        respond(conn, 302, "Found", [("Location", "/redirect/loop"), ("Content-Length", "5")], b"again")
    else:
        respond(conn, 404, "Not Found", [("Content-Length", "9")], b"not here\n")


def serve(connection, context):
    try:
        with context.wrap_socket(connection, server_side=True) as conn:
            handle(conn)
            try:
                conn.unwrap()
            except (OSError, ssl.SSLError):
                pass
    except (OSError, ssl.SSLError, ValueError) as error:
        print("fetch fixture: connection ended: %s" % error, flush=True)


def main():
    state_dir.mkdir(parents=True, exist_ok=True)
    make_repos()
    (state_dir / "hashes.json").write_text(json.dumps(
        {path: hashlib.sha256(body).hexdigest() for path, body in BODIES.items()}))
    (state_dir / "repos.json").write_text(json.dumps({name: str(path) for name, path in REPOS.items()}))
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.minimum_version = ssl.TLSVersion.TLSv1_2
    context.load_cert_chain(PKI / "fetch.pem", PKI / "fetch.key")
    listener = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    listener.bind(("127.0.0.1", PORT))
    listener.listen(8)
    listener.settimeout(0.5)
    (state_dir / "ready").write_text("")
    print("fetch fixture: listening on 127.0.0.1:%d" % PORT, flush=True)
    end = time.monotonic() + timeout
    while time.monotonic() < end and not (state_dir / "stop").exists():
        try:
            connection, _ = listener.accept()
        except socket.timeout:
            continue
        threading.Thread(target=serve, args=(connection, context), daemon=True).start()
    listener.close()
    with lock:
        (state_dir / "requests.json").write_text(json.dumps(requests, indent=1))
        targets = [r["target"] for r in requests]
        expected = {"/files/pkg.tar", "/files/hello.txt", "/redirect/big", "/files/big.bin", "/files/evil.tar",
                    "/redirect/loop", "/missing", "/git/moved.git" + SERVICE, "/git/missing.git" + SERVICE}
        for name in REPOS:
            expected |= {"/git/%s%s" % (name, SERVICE), "/git/%s/git-upload-pack" % name}
        well_formed = all(r["host"] == "%s:%d" % (NAME, PORT) and r["accept_encoding"] == "identity"
                          and r["connection"] == "close" for r in requests)
        # git's requests ask for protocol version 2; its POSTs carry the
        # request media type; everything else is a GET.
        git_formed = all((r["git_protocol"] == "version=2") == r["target"].startswith("/git/")
                         and (r["method"] == "POST") == r["target"].endswith("/git-upload-pack")
                         and (r["method"] != "POST" or r["content_type"] == "application/x-git-upload-pack-request")
                         for r in requests)
        ok = well_formed and git_formed and expected <= set(targets) and set(targets) <= expected
    print("fetch fixture: %s (targets %s)" % ("PASS" if ok else "FAIL", targets), flush=True)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
