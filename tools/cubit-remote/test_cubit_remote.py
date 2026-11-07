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
    VECTOR = bytes([0, 0, 0, 0, 2, 0, 0, 0, 1, 1, 1]) + b"\xab" * 32 + b"\x01" * 32

    def test_response_vector(self):
        report = cr.parse_app(self.VECTOR)
        self.assertEqual((report["result"], report["current"], report["previous"], report["instance"],
                          report["stop"]), (0, 2, 1, 1, 1))
        self.assertEqual(report["current_sha256"], "ab" * 32)
        self.assertEqual(report["previous_sha256"], "01" * 32)
        with self.assertRaises(cr.RemoteError):
            cr.parse_app(self.VECTOR[:-1])

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
        # scalar). A result's literal over literal_limit is refused as the
        # device refuses it (too_large_text); a scalar list carries at most
        # scalar_carry elements and says "... N more".
        self.lists = {}
        self.literal_limit = 4096
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
        self.apps = {}
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
            return 0, self.evaluate(body.decode())
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
                       + b"\x11" * 32 + b"\x22" * 32)
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
        """Regression: a list too large for one result (a busy machine's
        proc.list) is fetched whole, in slices, not reported as a failure."""
        elements = self.processes(70)
        self.device.lists["(proc.list)"] = ("List<Process>", elements, False)
        code, out = self.run_cli("ps")
        self.assertEqual(code, 0)
        kind, shown, more = cr.parse_list(out.strip())
        self.assertEqual((kind, shown, more), ("List<Process>", elements, 0))
        self.assertIn("(length (proc.list))", self.device.evaluations)

    def test_slices_shrink_until_they_fit(self):
        elements = ['(LogEntry %d Severity.Information 7 "%s")' % (i, "m" * 300) for i in range(40)]
        self.device.lists['(logs.recent "desktop")'] = ("List<LogEntry>", elements, False)
        code, out = self.run_cli("logs", "desktop")
        self.assertEqual(code, 0)
        self.assertEqual(cr.parse_list(out.strip())[1], elements)

    def test_endpoint_before_result_too_large(self):
        """The endpoint's older words for the same refusal are understood."""
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
        self.device.lists['(logs.recent "quiet")'] = ("List<LogEntry>", [], False)
        code, out = self.run_cli("logs", "quiet")
        self.assertEqual((code, out), (0, "List<LogEntry>: (list-of LogEntry)\n"))
        self.assertEqual(self.device.evaluations, ['(logs.recent "quiet")'])

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

    def test_rollback_without_previous(self):
        self.run_cli("app", "replace", "out/a/build/swap-probe.app")
        code, _, err = self.err("app", "rollback", "swap-probe")
        self.assertEqual(code, 2)
        self.assertIn("No_Previous", err)


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
