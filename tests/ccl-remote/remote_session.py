#!/usr/bin/env python3
"""The headless ccl-remote-session test's host side (tests/headless/run.sh).

Usage: remote_session.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS PORT

Waits for the guest's ccl-remote to show its pairing code on the kernel
console (display => Kernel_Console: the console is this machine's screen),
pairs through QEMU's user-net forward like a LAN host would, and drives the
real endpoint with tools/cubit-remote: evaluation, logs, processes, a
chunked upload and download checked by SHA-256, a refused damaged commit,
the mailbox both ways (the image seeds mail/to-host as the device's agent
would), a build job's lifecycle, refusals (wrong token, a place no grant
admits) and the lockout after too many wrong tokens; that each operation
the host performs reaches the device's activity list as a "ccl-remote: host
<op> ...: <outcome>" line (console and logstore); and that a list result
too large for one response comes back whole in slices (cubit-remote's
evaluate_whole). Then quits QEMU.
Prints PASS/FAIL lines; exits 1 on any failure.
"""
import hashlib
import json
import os
import re
import socket
import struct
import sys
import tempfile
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "tools" / "cubit-remote"))
import cubit_remote as cr  # noqa: E402

serial, qmp_socket, timeout, port = sys.argv[1], sys.argv[2], float(sys.argv[3]), int(sys.argv[4])
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
SEEDED_MAIL = b"hello host, from the device's agent\n"   # run.sh writes it
BUILD_DEADLINE_S = 120
OBSERVER_PAGE_RECORDS = 32   # CuBit.Process_Observer.Page_Records
failures = 0


def check(condition, what):
    global failures
    print(("PASS: " if condition else "FAIL: ") + what, flush=True)
    if not condition:
        failures += 1


def quit_qemu():
    try:
        with socket.socket(socket.AF_UNIX) as connection:
            connection.settimeout(3)
            connection.connect(qmp_socket)
            stream = connection.makefile("rwb")
            json.loads(stream.readline())
            for command in ("qmp_capabilities", "quit"):
                stream.write(json.dumps({"execute": command}).encode() + b"\n")
                stream.flush()
                stream.readline()
    except (OSError, ValueError) as error:
        print(f"remote-session: could not quit QEMU: {error}", file=sys.stderr)


def serial_bytes():
    try:
        return Path(serial).read_bytes()
    except OSError:
        return b""


def wait_for_serial(marker, seconds=5):
    end = time.monotonic() + seconds
    while marker not in serial_bytes() and time.monotonic() < end:
        time.sleep(0.2)
    return marker in serial_bytes()


def status_of(call):
    try:
        call()
        return 0
    except cr.RemoteError as e:
        return e.status


def main():
    code = None
    while time.monotonic() < deadline:
        data = serial_bytes()
        found = CODE.search(data)
        if found and LISTENING in data:
            code = found.group(1).decode()
            break
        if b"CUBIT KERNEL PANIC" in data:
            break
        time.sleep(0.5)
    check(code is not None, "the guest showed a pairing code and listens")
    if code is None:
        return

    token = cr.decode_token_text(code)
    host = cr.Host("guest", "127.0.0.1", port, token)
    # The endpoint may need a moment between listening and the forward.
    for _ in range(20):
        try:
            bits, chunk, server = cr.hello(host)
            break
        except (OSError, cr.RemoteError):
            time.sleep(0.5)
    else:
        check(False, "hello with the pairing token")
        return
    check(bits == 0b1111 and chunk == cr.MAXIMUM_CHUNK_BYTES and server.startswith("ccl-remote"),
          "hello: grants %s, chunk %d, %s" % (cr.grant_names(bits), chunk, server))

    wrong = cr.Host("guest", "127.0.0.1", port, bytes(16))
    check(status_of(lambda: cr.hello(wrong)) == 2, "a wrong token is Unauthorized")

    ok, text = cr.evaluate(host, "(+ 40 2)")
    check(ok and text == "Integer: 42", "eval (+ 40 2) -> %r" % text)
    ok, text = cr.evaluate(host, "(+ 1 \"a\")")
    check(not ok, "a type error is reported, not run: %r" % text[:80])
    ok, text = cr.evaluate(host, "(define (twice (n Integer)) Integer (* 2 n))")
    ok2, text2 = cr.evaluate(host, "(twice 21)")
    check(ok and ok2 and text2 == "Integer: 42", "the session keeps definitions: %r" % text2)
    ok, text = cr.evaluate_whole(host, "(proc.list)")
    check(ok and "ccl-remote" in text, "proc.list sees ccl-remote")
    ok, text = cr.evaluate(host, '(logs.recent "ccl-remote")')
    check(ok and "a host paired" in text, "logs.recent shows the pairing")
    check(code.replace("-", "") not in text.replace("-", ""), "the token is not in the log")
    ok, text = cr.evaluate(host, '(logs.set-minimum Severity.Debug)')
    check(not ok, "logs.set-minimum is refused to a remote session: %r" % text[:100])

    # A list too large for one response: the device says so (with its
    # length), and cubit-remote takes it whole in slices.
    ok, _ = cr.evaluate(host, "(type Row (record (n Integer) (s String)))")
    rows = '(each (fn ((n Integer)) (Row n "a text long enough to fill the result")) (range 1 150))'
    ok, text = cr.evaluate(host, rows)
    check(not ok and text.startswith(cr.TOO_LARGE_TEXT) and "150 elements" in text,
          "a too large list is named so, with its length: %r" % text[:120])
    ok, text = cr.evaluate_whole(host, rows)
    shown = cr.parse_list(text) if ok else None
    check(shown is not None and len(shown[1]) == 150 and shown[1][0].startswith("(Row 1 ")
          and shown[1][-1].startswith("(Row 150 "), "evaluate_whole returns all 150 rows in order")
    # More processes run than one observer page holds (the init profile
    # starts twelve sleep.app): every page is read, none is left out.
    ok, text = cr.evaluate_whole(host, "(proc.list)")
    shown = cr.parse_list(text) if ok else None
    listed = len(shown[1]) if shown else -1
    ok, counted = cr.evaluate(host, "(length (proc.list))")
    check(shown is not None and ok and counted == "Integer: %d" % listed and listed > OBSERVER_PAGE_RECORDS,
          "ps lists every process, past one page (%s, %d listed)" % (counted, listed))
    check(shown is not None and sum('"sleep.app"' in p for p in shown[1]) == 12 and
          any('"ccl-remote.app"' in p for p in shown[1]), "ps shows all twelve sleep.app and ccl-remote.app")

    # The device owner sees what the host did: each operation is an
    # activity line on the console (the screen here) and in logstore.
    for marker in (b"ccl-remote: host hello: ok",
                   b"ccl-remote: host hello with a wrong pairing token (1 of 32): refused",
                   b"ccl-remote: host evaluate (+ 40 2): ok",
                   b"ccl-remote: host evaluate (+ 1 \"a\"): failed",
                   b"ccl-remote: host evaluate (logs.set-minimum Severity.Debug): failed",
                   b"ccl-remote: host evaluate (proc.list): ok"):
        check(wait_for_serial(marker), "activity: %s" % marker.decode())
    ok, text = cr.evaluate(host, '(logs.recent "ccl-remote")')
    check(ok and "host evaluate (+ 40 2): ok" in text, "the activity is in logstore")
    long_source = "(+ 1 %s)" % " ".join(["1"] * 80)
    cr.evaluate(host, long_source)
    check(wait_for_serial(("ccl-remote: host evaluate " + long_source[:60]).encode()) and
          not (long_source.encode() in serial_bytes()), "a long source is shown cut, with an ellipsis")

    # Files: chunked upload, stat, download, all checked by SHA-256.
    work = tempfile.mkdtemp(prefix="cubit-remote-session-")
    blob = os.urandom(3 * cr.MAXIMUM_CHUNK_BYTES + 1234)
    local = os.path.join(work, "blob.bin")
    Path(local).write_bytes(blob)
    size, digest = cr.put_file(host, "workspace", local, "upload/blob.bin")
    check(size == len(blob) and digest == hashlib.sha256(blob).hexdigest(), "put %d bytes" % size)
    remote_size, remote_digest = cr.remote_stat(host, "workspace", "upload/blob.bin")
    check(remote_size == len(blob) and remote_digest == hashlib.sha256(blob).digest(),
          "stat matches size and SHA-256")
    back = os.path.join(work, "back.bin")
    cr.get_file(host, "workspace", "upload/blob.bin", back)
    check(Path(back).read_bytes() == blob, "get returns the same bytes")
    listing = host.call("list", "workspace", cr.name_field("upload")).decode()
    check("blob.bin" in listing and cr.STAGING_SUFFIX not in listing, "ls upload: %r" % listing.strip())
    # A commit whose hash does not match keeps the old file.
    host.call("put", "workspace", cr.name_field("upload/blob.bin") + struct.pack(">Q", 0) + b"damaged")
    status = status_of(lambda: host.call(
        "commit", "workspace", cr.name_field("upload/blob.bin") + struct.pack(">Q", 7) + bytes(32)))
    check(status == 9, "a commit with the wrong SHA-256 is Integrity_Mismatch")
    remote_size, _ = cr.remote_stat(host, "workspace", "upload/blob.bin")
    check(remote_size == len(blob), "the committed file is unchanged")
    check(status_of(lambda: host.call("stat", "workspace", cr.name_field("../etc"))) == 1,
          "a name outside the place is Malformed")
    check(status_of(lambda: host.call("put", "to-host", cr.name_field("x") + struct.pack(">Q", 0)))
          == 3, "no grant writes mail/to-host from the host")
    for marker in (b"ccl-remote: host put workspace/upload/blob.bin (%d bytes, SHA-256 checked): ok" % len(blob),
                   b"ccl-remote: host get workspace/upload/blob.bin: ok",
                   b"ccl-remote: host stat workspace/upload/blob.bin: ok",
                   b"ccl-remote: host list workspace/upload: ok",
                   b"ccl-remote: host put workspace/upload/blob.bin (7 bytes): failed",
                   b"ccl-remote: host mail put mail/to-host (not granted): refused"):
        check(wait_for_serial(marker), "activity: %s" % marker.decode())

    # Mailbox: the host's message reaches mail/to-device; the device's
    # message (seeded in the image) comes back and is taken.
    note = os.path.join(work, "note.txt")
    Path(note).write_text("hello device, from the host\n")
    cr.put_file(host, "to-device", note, "note.txt")
    to_device = host.call("list", "to-device", cr.name_field("")).decode()
    check("note.txt" in to_device, "mail send: mail/to-device holds note.txt")
    inbox = host.call("list", "to-host", cr.name_field("")).decode()
    check("greeting.txt" in inbox, "mail/to-host holds the device's message")
    got = os.path.join(work, "greeting.txt")
    cr.get_file(host, "to-host", "greeting.txt", got)
    check(Path(got).read_bytes() == SEEDED_MAIL, "mail recv: the device's message arrives intact")
    host.call("remove", "to-host", cr.name_field("greeting.txt"))
    inbox = host.call("list", "to-host", cr.name_field("")).decode()
    check("greeting.txt" not in inbox, "mail recv takes the message")

    # A build job: started, followed to its end, then released.
    job = struct.unpack(">I", host.call("build_start", "workspace", cr.name_field("missing/component.ccl")))[0]
    check(job >= 1, "build start returns job %d" % job)
    offset, output, state, code_value = 0, b"", 1, 0
    end = time.monotonic() + BUILD_DEADLINE_S
    while time.monotonic() < end:
        state, code_value, total, first_kept, out = cr.build_poll(host, job, offset)
        output += out
        offset += len(out)
        if state != 1 and offset >= total:
            break
        time.sleep(0.5)
    print("remote-session: job %d ended %s (exit code %d); output:\n%s" % (
        job, cr.JOB_STATES.get(state, "?"), code_value, output.decode("utf-8", "replace")), flush=True)
    check(state in (2, 3), "the job reached an end (%s)" % cr.JOB_STATES.get(state, "?"))
    check(state == 3 or code_value != 0, "a missing component does not build")
    check(len(output) > 0, "the job's output reached the host")
    check(status_of(lambda: cr.build_poll(host, job, offset)) == 4,
          "a job polled to its end is released")
    for marker in (b"ccl-remote: host mail put mail/to-device/note.txt (",
                   b"ccl-remote: host mail get mail/to-host/greeting.txt: ok",
                   b"ccl-remote: host mail rm mail/to-host/greeting.txt: ok",
                   b"ccl-remote: host build missing/component.ccl (job %d): ok" % job):
        check(wait_for_serial(marker), "activity: %s" % marker.decode())

    # Lockout: after too many wrong tokens even the right one is refused.
    for _ in range(40):
        status_of(lambda: cr.hello(wrong))
    check(status_of(lambda: cr.hello(host)) == 12, "too many wrong tokens lock the endpoint")
    end = time.monotonic() + 5
    while b"ccl-remote: too many wrong tokens" not in serial_bytes() and time.monotonic() < end:
        time.sleep(0.2)
    check(b"ccl-remote: too many wrong tokens" in serial_bytes(), "the lockout is logged")


try:
    main()
finally:
    quit_qemu()
print("REMOTE-SESSION: %s" % ("PASS" if failures == 0 else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
