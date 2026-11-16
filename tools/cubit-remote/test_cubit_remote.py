#!/usr/bin/env python3
"""Hosted tests of cubit-remote: token text (the same vectors as
tests/ccl-remote/remote_wire_tests.adb), names, framing, the host store's
permissions, and whole commands against a stand-in endpoint that speaks
the wire profile. The real endpoint is tested in QEMU (tests/ccl-remote/
remote_session.py)."""

import hashlib
import io
import os
import re
import socket
import stat
import struct
import sys
import tempfile
import threading
import unittest
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cubit_remote as cr  # noqa: E402

COUNTING = bytes(range(16))


class TokenText(unittest.TestCase):
    def test_vectors(self):
        self.assertEqual(cr.token_image(COUNTING), "000G40R40M30E209185GR38E1W")
        self.assertEqual(cr.token_image(b"\xff" * 16), "ZZZZZZZZZZZZZZZZZZZZZZZZZW")
        self.assertEqual(cr.token_image(bytes([0xDE, 0xAD, 0xBE, 0xEF] * 4)), "VTPVXVYYNPZEZQNDQVQXXBDYXW")

    def test_typed_forms(self):
        self.assertEqual(cr.decode_token_text("000g4-0r40m-30e20-9185g-r38e1-w"), COUNTING)
        self.assertEqual(cr.decode_token_text("OOOG40R40M30E2O9185GR38E1W"), COUNTING)

    def test_refusals(self):
        for text in ("ZZZZZZZZZZZZZZZZZZZZZZZZZX", "ZZZZZZZZZZZZZZZZZZZZZZZZZ",
                     "ZZZZZZZZZZZZZZZZZZZZZZZZZW0", "000G40R40M30E209185GR38E1U", ""):
            with self.assertRaises(ValueError):
                cr.decode_token_text(text)

    def test_round_trip(self):
        for _ in range(500):
            token = os.urandom(16)
            self.assertEqual(cr.decode_token_text(cr.token_image(token)), token)


class Names(unittest.TestCase):
    def test_names(self):
        for good in ("a", "src/ps2/component.ccl", "a-b_c.d+e", "..a"):
            self.assertTrue(cr.valid_name(good), good)
        for bad in ("", "/a", "a/", "a//b", "../a", "a/..", "a/./b", ".", "@nvme:0/x", "a b",
                    "a\\b", "x" + cr.STAGING_SUFFIX, "a" * 241):
            self.assertFalse(cr.valid_name(bad), bad)


class Framing(unittest.TestCase):
    def test_request_header(self):
        frame = cr.request_bytes("put", "workspace", COUNTING, b"xyz")
        self.assertEqual(len(frame), cr.REQUEST_HEADER_BYTES + 3)
        self.assertEqual(frame[:4], b"CBRM")
        self.assertEqual(frame[4:8], bytes([1, 6, 1, 0]))
        self.assertEqual(struct.unpack(">I", frame[8:12])[0], 3)
        self.assertEqual(frame[12:28], COUNTING)


class Apps(unittest.TestCase):
    # The same bytes as tests/ccl-remote/remote_wire_tests.adb's vector.
    VECTOR = bytes([0, 0, 0, 0, 2, 0, 0, 0, 1, 1, 1]) + b"\xab" * 32 + b"\x01" * 32 + b"\x00"
    START_FAILED = bytes([4, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0]) + b"\x00" * 64 + b"\x09"

    def test_response_vector(self):
        report = cr.parse_app(self.VECTOR)
        self.assertEqual((report["result"], report["current"], report["previous"], report["instance"],
                          report["stop"]), (0, 2, 1, 1, 1))
        self.assertEqual(report["current_sha256"], "ab" * 32)
        self.assertEqual(report["previous_sha256"], "01" * 32)
        self.assertEqual(report["failure"], 0)
        with self.assertRaises(cr.RemoteError):
            cr.parse_app(self.VECTOR[:-1])

    def test_start_failure_names_the_reason(self):
        report = cr.parse_app(self.START_FAILED)
        self.assertEqual((report["result"], report["failure"]), (4, 9))
        refused = cr.AppRefused(report)
        self.assertEqual(refused.name, "Start_Failed")
        self.assertEqual(refused.failure, ("Manifest_Refused", "its manifest was refused"))
        self.assertIn("procmgr: Manifest_Refused, its manifest was refused", str(refused))

    def test_request_frames(self):
        place, body = cr.app_request("app_replace", "out/x/build/swap-probe.app")
        frame = cr.request_bytes("app_replace", place, COUNTING, body)
        self.assertEqual(frame[4:8], bytes([1, 13, 1, 0]))
        self.assertEqual(frame[28:], b"\x00\x1aout/x/build/swap-probe.app")
        place, body = cr.app_request("app_rollback", "swap-probe")
        self.assertEqual(cr.request_bytes("app_rollback", place, COUNTING, body)[4:8], bytes([1, 14, 0, 0]))
        self.assertEqual(cr.OPERATIONS["app_status"], 15)
        self.assertEqual(cr.STATUSES[13], "Capacity")

    def test_names(self):
        self.assertEqual(cr.app_of_product("out/userspace/apps/swap-probe/build/swap-probe.app"), "swap-probe")
        for bad in (".app", "a/.app", "swap-probe", "a.app/b", "x/" + "a" * 65 + ".app"):
            self.assertEqual(cr.app_of_product(bad), "", bad)
        self.assertEqual(len(cr.app_of_product("x/" + "a" * 64 + ".app")), 64)
        self.assertFalse(cr.valid_app_name("a/b"))
        with self.assertRaises(SystemExit):
            cr.app_request("app_status", "apps/swap-probe")
        with self.assertRaises(SystemExit):
            cr.app_request("app_replace", "out/swap-probe")


class StandIn:
    """The endpoint's semantics, enough to drive whole commands."""

    def __init__(self, token, grants=0b1111):
        self.token, self.grants = token, grants
        self.files = {}
        self.jobs = {}
        # Lists the stand-in's "CCL" knows: expression -> (type, elements,
        # scalar). A result is as long as its value, as the device's is; a
        # literal over literal_limit (the device's LITERAL_CEILING when a
        # test sets one) is refused (too_large_text); a scalar list carries
        # at most scalar_carry elements and says "... N more".
        self.lists = {}
        # Results that are not lists (a record), by expression.
        self.values = {}
        self.literal_limit = 1 << 27
        self.scalar_carry = 3
        self.too_large_text = cr.TOO_LARGE_TEXT + ": %d elements"
        self.evaluations = []
        # Screenshots: what the next capture answers (Capture_Result), the
        # PNG it saves, and the requests it saw.
        self.capture_result = 0
        self.capture_png = b"\x89PNG\r\n\x1a\nstand-in frame"
        self.captures = []
        # Apps: what the next app operation answers (App_Result), and the
        # generations the stand-in keeps: app -> [current, previous].
        self.app_result = 0
        self.app_failure = 0
        self.apps = {}
        # System generations: what the next activation answers
        # (Generation_Result, detail), the records (service -> sha256 hex
        # per generation, with its previous) and the current number; the
        # requests seen (service, product, grace, health).
        self.generation_result = (0, 0)
        self.generations = {0: ({}, 0)}
        self.current = 0
        self.activations = []
        # The last evaluation's text, kept for read_result, and its reads.
        self.last_result = b""
        self.result_reads = 0
        self.server = socket.socket()
        self.server.bind(("127.0.0.1", 0))
        self.server.listen(8)
        self.port = self.server.getsockname()[1]
        threading.Thread(target=self.run, daemon=True).start()

    def run(self):
        while True:
            try:
                connection, _ = self.server.accept()
            except OSError:
                return
            with connection:
                header = cr._read_exact(connection, 28)
                body = cr._read_exact(connection, struct.unpack(">I", header[8:12])[0])
                status, payload = self.answer(header[5], header[6], header[12:28], body)
                connection.sendall(b"CBRM" + bytes([1, status, 0, 0]) + struct.pack(">I", len(payload)) + payload)

    def answer(self, op, place, token, body):
        if token != self.token:
            return 2, b""

        def name():
            n = struct.unpack(">H", body[:2])[0]
            return (place, body[2:2 + n].decode()), body[2 + n:]
        if op == 1:
            return 0, bytes([self.grants]) + struct.pack(">I", cr.MAXIMUM_CHUNK_BYTES) + b"stand-in"
        if op == 2:
            # As the device: ok, the whole text's length, its first chunk;
            # the rest is kept for read_result (op 19).
            answered = self.evaluate(body.decode())
            self.last_result = answered[1:]
            return 0, (answered[:1] + struct.pack(">Q", len(self.last_result))
                       + self.last_result[:cr.MAXIMUM_CHUNK_BYTES])
        if op == 19:
            offset = struct.unpack(">Q", body)[0]
            self.result_reads += 1
            return 0, (struct.pack(">Q", len(self.last_result))
                       + self.last_result[offset:offset + cr.MAXIMUM_CHUNK_BYTES])
        if op == 3:
            lines = "".join("file %d %s\n" % (len(v), k[1]) for k, v in sorted(self.files.items()) if k[0] == place)
            return 0, lines.encode()
        if op == 4:
            key, _ = name()
            if key not in self.files:
                return 4, b""
            data = self.files[key]
            return 0, struct.pack(">Q", len(data)) + hashlib.sha256(data).digest()
        if op == 5:
            key, rest = name()
            offset = struct.unpack(">Q", rest)[0]
            return 0, self.files.get(key, b"")[offset:offset + cr.MAXIMUM_CHUNK_BYTES]
        if op == 6:
            (p, n), rest = name()
            offset = struct.unpack(">Q", rest[:8])[0]
            staged = self.files.get((p, n + cr.STAGING_SUFFIX), b"") if offset else b""
            self.files[(p, n + cr.STAGING_SUFFIX)] = staged[:offset] + rest[8:]
            return 0, b""
        if op == 7:
            (p, n), rest = name()
            size, digest = struct.unpack(">Q", rest[:8])[0], rest[8:]
            staged = self.files.get((p, n + cr.STAGING_SUFFIX))
            if staged is None or len(staged) != size or hashlib.sha256(staged).digest() != digest:
                return 9, b""
            self.files[(p, n)] = self.files.pop((p, n + cr.STAGING_SUFFIX))
            return 0, b""
        if op == 8:
            key, _ = name()
            return (0 if self.files.pop(key, None) is not None else 4), b""
        if op == 9:
            self.jobs[1] = b"ran  abc  compile\nccl-build: 1 ran, 0 answered by the cache\n"
            return 0, struct.pack(">I", 1)
        if op == 10:
            job, offset = struct.unpack(">IQ", body)
            out = self.jobs[job]
            return 0, struct.pack(">BIQQ", 2, 0, len(out), 0) + out[offset:]
        if op == 12:
            if not self.grants & 16:
                return 3, b""
            output, x, y, w, h = struct.unpack(">BHHHH", body)
            self.captures.append((output, x, y, w, h))
            if self.capture_result != 0:
                return 0, bytes([self.capture_result])
            shot = "output-%d.png" % output
            self.files[(4, shot)] = self.capture_png
            return 0, (bytes([0]) + struct.pack(">IIQ", w or 1024, h or 768, len(self.capture_png))
                       + hashlib.sha256(self.capture_png).digest() + shot.encode())
        if op in (13, 14, 15):
            (p, n), _ = name()
            app = cr.app_of_product(n) if op == 13 else n
            gens = self.apps.setdefault(app, [0, 0])
            result = self.app_result
            if result == 0 and op == 13:
                gens[:] = [gens[0] + 1, gens[0]]
            elif result == 0 and op == 14:
                if not gens[1]:
                    result = 6
                else:
                    gens.reverse()
                    result = 1
            elif result == 0:
                result = 2
            return 0, (struct.pack(">BIIBB", result, gens[0], gens[1], 1 if gens[0] else 0, 1 if op != 15 else 0)
                       + b"\x11" * 32 + b"\x22" * 32 + bytes([self.app_failure]))
        if op in (16, 17):
            if op == 16:
                (p, product), rest = name()
                grace, health = struct.unpack(">II", rest[:8])
                service = rest[8:].decode()
                digest = hashlib.sha256(self.files.get((p, product), b"")).hexdigest()
                pins = dict(self.generations[self.current][0])
                pins[service] = digest
                target = max(self.generations) + 1
                self.generations[target] = (pins, self.current)
            else:
                grace, health = struct.unpack(">II", body)
                service, product, digest = "acpi.svc", "", "00" * 32
                if self.current == 0:
                    return 0, struct.pack(">BBII", 6, 0, 0, 0) + bytes(32) + struct.pack(">Q", 0)
                target = self.generations[self.current][1]
            self.activations.append((service, product, grace, health))
            result, detail = self.generation_result
            if result == 0:
                self.current = target
            return 0, (struct.pack(">BBII", result, detail, self.current, target) + bytes.fromhex(digest)
                       + struct.pack(">Q", 0x1000005 if result in (0, 1) else 0))
        if op == 18:
            text = "".join('(System_Generation number => %d previous => %d pins => [%s])\n' % (
                n, prev, " ".join('(Service_Pin service => "%s" executable => "%s")' % kv for kv in pins.items()))
                for n, (pins, prev) in sorted(self.generations.items()) if n)
            return 0, (text + "current %d\n" % self.current).encode()
        return 1, b""


    def evaluate(self, source):
        self.evaluations.append(source)
        sliced = re.fullmatch(r"\(last (\d+) \(first (\d+) (.*)\)\)", source)
        counted = re.fullmatch(r"\(length (.*)\)", source)
        if counted and counted.group(1) in self.lists:
            return b"\x01Integer: %d" % len(self.lists[counted.group(1)][1])
        if sliced and sliced.group(3) in self.lists:
            kind, elements, scalar = self.lists[sliced.group(3)]
            first = elements[:int(sliced.group(2))]
            return self.list_result(kind, first[max(0, len(first) - int(sliced.group(1))):], scalar)
        if source in self.lists:
            return self.list_result(*self.lists[source])
        if source in self.values:
            return b"\x01" + self.values[source].encode()
        return b"\x01Integer: 2"

    def list_result(self, kind, elements, scalar):
        if scalar:
            shown = ", ".join(elements[:self.scalar_carry])
            more = len(elements) - self.scalar_carry
            if more > 0:
                shown += ", ... %d more" % more
            return ("\x01%s: [%s]" % (kind, shown)).encode()
        literal = "[%s]" % " ".join(elements) if elements else "(list-of %s)" % kind[5:-1]
        if len(literal) > self.literal_limit:
            return b"\x00" + (self.too_large_text % len(elements)).encode()
        return ("\x01%s: %s" % (kind, literal)).encode()


class Commands(unittest.TestCase):
    def setUp(self):
        self.home = tempfile.TemporaryDirectory()
        os.environ["XDG_CONFIG_HOME"] = self.home.name
        self.device = StandIn(COUNTING)
        self.cwd = os.getcwd()
        os.chdir(self.home.name)
        self.pair()

    def tearDown(self):
        os.chdir(self.cwd)
        self.device.server.close()
        self.home.cleanup()

    def run_cli(self, *argv, stdin=""):
        out = io.StringIO()
        old = sys.stdin
        sys.stdin = io.StringIO(stdin)
        try:
            with redirect_stdout(out):
                code = cr.main(list(argv))
        finally:
            sys.stdin = old
        return code, out.getvalue()

    def pair(self):
        code, out = self.run_cli("pair", "127.0.0.1:%d" % self.device.port, "--name", "dev", "--code-stdin",
                                 stdin=cr.token_image(COUNTING) + "\n")
        self.assertEqual(code, 0)
        self.assertIn("diagnose, files, builds, mail", out)
        self.assertNotIn(cr.token_image(COUNTING), out)

    def test_store_is_private(self):
        path = cr.config_path()
        self.assertEqual(stat.S_IMODE(os.stat(path).st_mode), 0o600)
        self.assertEqual(stat.S_IMODE(os.stat(os.path.dirname(path)).st_mode), 0o700)

    def test_wrong_code(self):
        code, _ = self.run_cli("pair", "127.0.0.1:%d" % self.device.port, "--name", "bad", "--code-stdin",
                               stdin=cr.token_image(bytes(16)) + "\n")
        self.assertEqual(code, 1)

    def test_eval(self):
        code, out = self.run_cli("eval", "(+ 1 1)")
        self.assertEqual((code, out), (0, "Integer: 2\n"))

    def processes(self, count):
        return ['(Process %d "proc-%d.svc" "com.cubit.proc-%d" Run_State.Receiving %d 0 %d)'
                % (i, i, i, 4096 * i, 1000 * i) for i in range(1, count + 1)]

    def test_ps_whole_list(self):
        """A busy machine's proc.list is one result, read whole."""
        elements = self.processes(70)
        self.device.lists["(proc.list)"] = ("List<Process>", elements, False)
        code, out = self.run_cli("ps")
        self.assertEqual(code, 0)
        kind, shown, more = cr.parse_list(out.strip())
        self.assertEqual((kind, shown, more), ("List<Process>", elements, 0))
        self.assertEqual(self.device.evaluations, ["(proc.list)"])

    def test_long_result_read_in_chunks(self):
        """Regression (logs.recent showed 13 of 151 records): a result longer
        than one reply, a whole log history, arrives whole from one
        evaluation, its chunks read with read_result."""
        elements = ['(LogItem.Logged (LogEntry %d Severity.Information 7 "%s"))' % (i, "m" * 300)
                    for i in range(4096)]
        self.device.lists['(logs.recent "acpi")'] = ("List<LogItem>", elements, False)
        code, out = self.run_cli("logs", "acpi", "--literal")
        self.assertEqual(code, 0)
        self.assertEqual(cr.parse_list(out.strip())[1], elements)
        self.assertEqual(self.device.evaluations, ['(logs.recent "acpi")'])
        self.assertGreater(self.device.result_reads, 30)

    def test_slices_shrink_until_they_fit(self):
        """Past the device's literal ceiling, a list is still read whole, in
        slices that halve until they fit."""
        self.device.literal_limit = 4096
        elements = ['(LogItem.Logged (LogEntry %d Severity.Information 7 "%s"))' % (i, "m" * 300)
                    for i in range(40)]
        self.device.lists['(logs.recent "desktop")'] = ("List<LogItem>", elements, False)
        code, out = self.run_cli("logs", "desktop", "--literal")
        self.assertEqual(code, 0)
        self.assertEqual(cr.parse_list(out.strip())[1], elements)

    def test_logs_show_gaps_and_tallies(self):
        """LOG-005: a typed gap reads as "… N records not retained …", a
        tally as its counts; records one per line."""
        elements = [
            '(LogItem.Logged (LogEntry 1500 Severity.Information 7 "acpi: phase \\"tables\\""))',
            '(LogItem.Gap (LogGap 2250 7 300 GapReason.Not_Retained "... 300 records not retained ..."))',
            '(LogItem.Gap (LogGap 2250 7 12 GapReason.Not_Shown "... 12 records not shown (this result holds no more) ..."))',
            '(LogItem.Logged (LogEntry 2250 Severity.Error 7 "acpi: node failed"))',
            '(LogItem.Tally (LogTally 7 1336 1024 312 5 2 "1336 published, 1024 retained, 312 not retained; '
            'shed 5 over budget, 2 by the publisher"))']
        self.device.lists['(logs.recent "acpi")'] = ("List<LogItem>", elements, False)
        code, out = self.run_cli("logs", "acpi")
        self.assertEqual(code, 0)
        self.assertEqual(out.splitlines(), [
            '       1.500  Information  7         acpi: phase "tables"',
            "\u2026 300 records of source 7 not retained (logstore's history for it was at its ceiling, "
            "logs.retention) \u2026",
            "\u2026 12 records of source 7 retained but not shown (past the items one result holds; "
            "narrow it with --from/--to) \u2026",
            "       2.250  Error        7         acpi: node failed",
            "source 7: 1336 published, 1024 retained, 312 not retained; shed before publication: "
            "5 over logstore's budget, 2 by the publisher (ring full)"])

    def test_endpoint_before_result_too_large(self):
        """The endpoint's older words for the same refusal are understood."""
        self.device.literal_limit = 4096
        self.device.too_large_text = ("This interpreter host does not support the operation's value or "
                                      "lifecycle contract%.0d")
        elements = self.processes(60)
        self.device.lists["(proc.list)"] = ("List<Process>", elements, False)
        code, out = self.run_cli("ps")
        self.assertEqual((code, cr.parse_list(out.strip())[1]), (0, elements))

    def test_scalar_list_ellipsis_filled(self):
        keys = ['"key-%d"' % i for i in range(10)]
        self.device.lists["(config.keys)"] = ("List<String>", keys, True)
        code, out = self.run_cli("eval", "(config.keys)")
        self.assertEqual(code, 0)
        self.assertNotIn("more", out)
        self.assertEqual(cr.parse_list(out.strip())[1], keys)

    def test_small_and_empty_lists_in_one_request(self):
        self.device.lists['(logs.recent "quiet")'] = ("List<LogItem>", [], False)
        code, out = self.run_cli("logs", "quiet")
        self.assertEqual((code, out), (0, "(no records)\n"))
        self.assertEqual(self.device.evaluations, ['(logs.recent "quiet")'])

    def test_logs_window(self):
        """--from/--to reads one window of the history with logs.between;
        plain logs stays logs.recent."""
        window = '(logs.between (LogWindow "acpi" 1000 2000))'
        elements = ['(LogItem.Logged (LogEntry 1500 Severity.Information 7 "acpi: step"))',
                    '(LogItem.Tally (LogTally 7 1 1 0 0 0 "1 published, 1 retained, 0 not retained; '
                    'shed 0 over budget, 0 by the publisher"))']
        self.device.lists[window] = ("List<LogItem>", elements, False)
        code, out = self.run_cli("logs", "acpi", "--from", "1000", "--to", "2000")
        self.assertEqual(code, 0)
        self.assertEqual(out.splitlines()[0], "       1.500  Information  7         acpi: step")
        self.assertEqual(self.device.evaluations, [window])
        code, out = self.run_cli("logs", "acpi", "--from", "1000", "--to", "2000", "--literal")
        self.assertEqual((code, cr.parse_list(out.strip())[1]), (0, elements))
        self.device.lists['(logs.recent "acpi")'] = ("List<LogItem>", elements, False)
        self.device.evaluations.clear()
        self.assertEqual(self.run_cli("logs", "acpi")[0], 0)
        self.assertEqual(self.device.evaluations, ['(logs.recent "acpi")'])

    def test_logs_window_needs_both_ends(self):
        with self.assertRaises(SystemExit):
            self.run_cli("logs", "acpi", "--from", "1000")
        self.assertEqual(self.device.evaluations, [])

    def test_put_get_round_trip(self):
        data = os.urandom(3 * cr.MAXIMUM_CHUNK_BYTES + 5)
        with open("blob", "wb") as f:
            f.write(data)
        self.assertEqual(self.run_cli("put", "blob", "dir/blob.bin")[0], 0)
        self.assertEqual(self.run_cli("get", "dir/blob.bin", "back")[0], 0)
        with open("back", "rb") as f:
            self.assertEqual(f.read(), data)
        empty = open("empty", "wb")
        empty.close()
        self.assertEqual(self.run_cli("put", "empty")[0], 0)
        self.assertEqual(self.device.files[(1, "empty")], b"")

    def test_damaged_download_refused(self):
        self.device.files[(1, "x")] = b"hello"
        original = self.device.answer

        def damaging(op, place, token, body):
            status, payload = original(op, place, token, body)
            return status, (b"jello" if op == 5 else payload)
        self.device.answer = damaging
        code, _ = self.run_cli("get", "x", "x.out")
        self.assertEqual(code, 1)
        self.assertFalse(os.path.exists("x.out"))

    def test_build_follow(self):
        code, out = self.run_cli("build", "userspace/services/ps2/component.ccl")
        self.assertEqual(code, 0)
        self.assertIn("1 ran, 0 answered", out)

    def test_mail_round_trip(self):
        self.assertEqual(self.run_cli("mail", "send", "--text", "hello device", "--name", "m1.txt")[0], 0)
        self.assertEqual(self.device.files[(2, "m1.txt")], b"hello device")
        self.device.files[(3, "reply.txt")] = b"hello host"
        code, out = self.run_cli("mail", "recv", "--dir", "inbox")
        self.assertEqual(code, 0)
        with open(os.path.join("inbox", "reply.txt"), "rb") as f:
            self.assertEqual(f.read(), b"hello host")
        self.assertNotIn((3, "reply.txt"), self.device.files)


class MetricsCommands(Commands):
    """MET-002: cubit-remote metrics series/range/window/latency send the
    typed CCL operations and render their typed results."""
    SERIES = ('(Metric_Series 7 "desktop.latency.input_to_ready" "com.cubit.desktop" "desktop" "12" '
              'Metric_Kind.Distribution Metric_Unit.Microseconds 96 96 0 0 1000 2000 300)')
    STATS = 'Metric_Stats: (Metric_Stats 1 96 28800 120 900 300 280 410 880 900 1500 2 0 32)'

    def test_series(self):
        self.device.lists[cr.series_expression("desktop.latency.*", "")] = ("List<Metric_Series>", [self.SERIES], False)
        code, out = self.run_cli("metrics", "series", "desktop.latency.*")
        self.assertEqual(code, 0)
        self.assertIn("desktop.latency.input_to_ready", out)
        self.assertRegex(out, r"com\.cubit\.desktop +12 +Distribution +Microseconds +96 +0 +0")

    def test_window_and_relative_times(self):
        expression = cr.window_expression("desktop.latency.input_to_ready", "", -30000000, 0)
        self.assertEqual(expression, '(metrics.window (Metric_Window series => "desktop.latency.input_to_ready" '
                                     'source => "" from => -30000000 to => 0))')
        self.device.values[expression] = self.STATS
        code, out = self.run_cli("metrics", "window", "desktop.latency.input_to_ready", "--last", "30")
        self.assertEqual(code, 0)
        self.assertIn("n=96", out)
        self.assertIn("p99=880 p999=900 rate=1.500/s missing=2", out)
        self.assertIn("error<=3.2%", out)

    def test_range_shows_points_rollups_and_gaps(self):
        expression = cr.range_expression("desktop.latency.input_to_ready", "", 1, 2, 0)
        self.device.lists[expression] = ("List<Metric_Item>", [
            "(Metric_Item.Gap (Metric_Gap 1000000 1500000 4 Metric_Gap_Cause.Producer_Shed))",
            "(Metric_Item.Point (Metric_Point 2000000 250))",
            "(Metric_Item.Rollup (Metric_Rollup 3000000 5 1000 100 400 200 190 400))"], False)
        code, out = self.run_cli("metrics", "range", "desktop.latency.input_to_ready", "--from", "1", "--to", "2")
        self.assertEqual(code, 0)
        lines = out.splitlines()
        self.assertIn("4 samples shed by the producer (its ring was full) between 1.000000 and 1.500000", lines[0])
        self.assertEqual(lines[1].split(), ["2.000000", "250"])
        self.assertIn("n=5 sum=1000 min=100 max=400 last=200 p50=190 p99=400", lines[2])

    def test_names_are_quoted_as_ccl_text(self):
        self.assertEqual(cr.metric_text('a"b\\c'), '"a\\"b\\\\c"')

    def test_latency_reads_every_stage_window(self):
        self.device.lists[cr.series_expression("desktop.latency.*", "")] = ("List<Metric_Series>", [self.SERIES], False)
        self.device.lists[cr.series_expression("desktop.frames.*", "")] = ("List<Metric_Series>", [], False)
        self.device.values[cr.window_expression("desktop.latency.input_to_ready", "", -60000000, 0)] = self.STATS
        code, out = self.run_cli("metrics", "latency")
        self.assertEqual(code, 0)
        self.assertRegex(out, r"desktop\.latency\.input_to_ready +series=1 n=96")


class AppCommands(Commands):
    def err(self, *argv):
        err = io.StringIO()
        old = sys.stderr
        sys.stderr = err
        try:
            code, out = self.run_cli(*argv)
        finally:
            sys.stderr = old
        return code, out, err.getvalue()

    def test_replace_and_rollback(self):
        code, out = self.run_cli("app", "replace", "out/a/build/swap-probe.app")
        self.assertEqual(code, 0, out)
        self.assertIn("swap-probe: generation 1 running", out)
        code, out = self.run_cli("app", "replace", "out/a/build/swap-probe.app")
        self.assertIn("generation 2 running", out)
        self.assertIn("previous generation 1 stopped", out)
        code, out = self.run_cli("app", "rollback", "swap-probe")
        self.assertEqual(code, 0)
        self.assertIn("swap-probe: generation 1 running", out)
        code, out = self.run_cli("app", "status", "swap-probe")
        self.assertEqual(code, 0)
        self.assertIn("generation 1", out)

    def test_switch_off_refused_with_reason(self):
        self.device.app_result = 3
        code, _, err = self.err("app", "replace", "out/a/build/swap-probe.app")
        self.assertEqual(code, 2)
        self.assertIn("Switch_Off", err)
        self.assertIn("Allow app replacement", err)

    def test_start_failed_names_procmgrs_reason(self):
        self.device.app_result = 4
        self.device.app_failure = 9
        code, _, err = self.err("app", "replace", "out/a/build/swap-probe.app")
        self.assertEqual(code, 2)
        self.assertIn("Start_Failed", err)
        self.assertIn("procmgr: Manifest_Refused, its manifest was refused", err)

    def test_rollback_without_previous(self):
        self.run_cli("app", "replace", "out/a/build/swap-probe.app")
        code, _, err = self.err("app", "rollback", "swap-probe")
        self.assertEqual(code, 2)
        self.assertIn("No_Previous", err)


class Generations(unittest.TestCase):
    def test_request_frame(self):
        fields, timeout = cr.deadlines(3000, 0)
        body = cr.name_field("switch/acpi-good.svc") + fields + b"acpi.svc"
        frame = cr.request_bytes("system_activate", "workspace", COUNTING, body)
        self.assertEqual(frame[4:8], bytes([1, 16, 1, 0]))
        self.assertEqual(frame[28:], b"\x00\x14switch/acpi-good.svc" + struct.pack(">II", 0, 3000) + b"acpi.svc")
        self.assertEqual(cr.OPERATIONS["system_rollback"], 17)
        self.assertEqual(cr.OPERATIONS["system_generations"], 18)
        # The exchange outlasts everything the device may take.
        self.assertGreater(timeout, (3000 + cr.SWITCH_OVERHEAD_MS) / 1000)

    def test_response_vector(self):
        body = struct.pack(">BBII", 1, 2, 3, 4) + b"\xab" * 32 + struct.pack(">Q", 0x1000005)
        report = cr.parse_generation(body)
        self.assertEqual((report["result_name"], report["detail"], report["current"], report["target"],
                          report["running"]), ("Rolled_Back", 2, 3, 4, 0x1000005))
        self.assertEqual(report["pin_sha256"], "ab" * 32)
        self.assertIn("ended inside its health window", cr.generation_image(report))
        with self.assertRaises(cr.RemoteError):
            cr.parse_generation(body[:-1])

    def test_deadlines_are_bounded(self):
        for health, grace in ((99, 0), (30_001, 0), (3000, -1), (3000, 10_001)):
            with self.assertRaises(SystemExit):
                cr.deadlines(health, grace)
        self.assertTrue(cr.valid_service_name("acpi.svc"))
        for bad in ("", "a/b", "a b", "x" * 65):
            self.assertFalse(cr.valid_service_name(bad), bad)


class SystemCommands(AppCommands):
    def test_activate_rollback_generations(self):
        self.device.files[(1, "switch/acpi-good.svc")] = b"good program"
        code, out = self.run_cli("system", "activate", "acpi.svc", "switch/acpi-good.svc",
                                 "--health-ms", "5000", "--stop-grace-ms", "0")
        self.assertEqual(code, 0, out)
        self.assertIn("generation 1 active", out)
        self.assertEqual(self.device.activations[-1], ("acpi.svc", "switch/acpi-good.svc", 0, 5000))
        code, out = self.run_cli("system", "generations")
        self.assertEqual(code, 0)
        self.assertIn(hashlib.sha256(b"good program").hexdigest(), out)
        self.assertIn("current 1", out)
        code, out = self.run_cli("system", "rollback", "--health-ms", "5000", "--stop-grace-ms", "0")
        self.assertEqual(code, 0, out)
        self.assertIn("generation 0 active", out)
        code, _, err = self.err("system", "rollback", "--health-ms", "5000", "--stop-grace-ms", "0")
        self.assertEqual(code, 2)
        self.assertIn("has none before it", err)

    def test_refusals_say_why(self):
        self.device.files[(1, "p.svc")] = b"x"
        for detail, words in ((6, "Allow app replacement"), (4, "restart contract is reboot"), (11, "manifest differs")):
            self.device.generation_result = (2, detail)
            code, _, err = self.err("system", "activate", "acpi.svc", "p.svc", "--health-ms", "5000",
                                    "--stop-grace-ms", "0")
            self.assertEqual(code, 2)
            self.assertIn(words, err)
        self.device.generation_result = (1, 2)
        code, _, err = self.err("system", "activate", "acpi.svc", "p.svc", "--health-ms", "5000",
                                "--stop-grace-ms", "0")
        self.assertEqual(code, 2)
        self.assertIn("rolled back", err)

    def test_deadlines_required(self):
        with self.assertRaises(SystemExit):
            cr.parser().parse_args(["system", "activate", "acpi.svc", "p.svc"])
        with self.assertRaises(SystemExit):
            cr.parser().parse_args(["system", "replace", "acpi.svc", "acpi.svc", "--health-ms", "5000"])

    def local_program(self, data):
        path = os.path.join(self.home.name, "acpi-built.svc")
        with open(path, "wb") as f:
            f.write(data)
        return path

    def test_replace_uploads_and_activates(self):
        local = self.local_program(b"rebuilt acpi")
        code, out = self.run_cli("system", "replace", "acpi.svc", local, "--health-ms", "20000",
                                 "--stop-grace-ms", "10000")
        self.assertEqual(code, 0, out)
        self.assertEqual(self.device.files[(1, "system/acpi.svc")], b"rebuilt acpi")
        self.assertIn("sha256 " + hashlib.sha256(b"rebuilt acpi").hexdigest(), out)
        self.assertEqual(self.device.activations[-1], ("acpi.svc", "system/acpi.svc", 10000, 20000))
        self.assertIn("generation 1 active", out)

    def test_replace_rolled_back_says_why(self):
        self.device.generation_result = (1, 2)
        code, _, err = self.err("system", "replace", "acpi.svc", self.local_program(b"broken"),
                                "--health-ms", "20000", "--stop-grace-ms", "10000")
        self.assertEqual(code, 2)
        self.assertIn("rolled back", err)

    def test_replace_checks_before_uploading(self):
        local = self.local_program(b"x")
        for argv in (("acpi.svc", local, "--health-ms", "99", "--stop-grace-ms", "0"),
                     ("a/b", local, "--health-ms", "5000", "--stop-grace-ms", "0"),
                     ("acpi.svc", local + ".missing", "--health-ms", "5000", "--stop-grace-ms", "0")):
            with self.assertRaises(SystemExit):
                self.run_cli("system", "replace", *argv)
        self.assertEqual(self.device.files, {})
        self.assertEqual(self.device.activations, [])


class Screenshots(Commands):
    def pair(self):
        self.device.grants = 0b11111
        super().pair()

    def test_screenshot_saved(self):
        code, out = self.run_cli("screenshot", "-o", "shot.png", "--output", "1")
        self.assertEqual(code, 0, out)
        with open("shot.png", "rb") as f:
            self.assertEqual(f.read(), self.device.capture_png)
        self.assertIn("1024x768", out)
        self.assertEqual(self.device.captures, [(1, 0, 0, 0, 0)])

    def test_screenshot_area(self):
        code, out = self.run_cli("screenshot", "-o", "part.png", "--area", "10,20,300,200")
        self.assertEqual(code, 0, out)
        self.assertIn("300x200", out)
        self.assertEqual(self.device.captures, [(0, 10, 20, 300, 200)])

    def test_screenshot_refused_with_reason(self):
        """The switch off: the device's typed reason, no file, exit 2."""
        self.device.capture_result = 2
        err = io.StringIO()
        old = sys.stderr
        sys.stderr = err
        try:
            code, _ = self.run_cli("screenshot", "-o", "off.png")
        finally:
            sys.stderr = old
        self.assertEqual(code, 2)
        self.assertIn("Capture_Off", err.getvalue())
        self.assertIn("Allow screen capture", err.getvalue())
        self.assertFalse(os.path.exists("off.png"))

    def test_capture_not_granted(self):
        self.device.grants = 0b1111
        code, _ = self.run_cli("screenshot", "-o", "no.png")
        self.assertEqual(code, 1)
        self.assertFalse(os.path.exists("no.png"))

    def test_changed_capture_refused(self):
        """A PNG that is not the frame the capture reported is not kept."""
        original = self.device.answer

        def swapping(op, place, token, body):
            status, payload = original(op, place, token, body)
            if op == 12:
                self.device.files[(4, "output-0.png")] = b"another frame"
            return status, payload
        self.device.answer = swapping
        code, _ = self.run_cli("screenshot", "-o", "swapped.png")
        self.assertEqual(code, 1)
        self.assertFalse(os.path.exists("swapped.png"))

    def test_grant_names(self):
        self.assertEqual(cr.grant_names(0b10001), "diagnose, capture")
        self.assertEqual(cr.capture_request(2, (1, 2, 3, 4)), bytes([2, 0, 1, 0, 2, 0, 3, 0, 4]))


if __name__ == "__main__":
    unittest.main()
