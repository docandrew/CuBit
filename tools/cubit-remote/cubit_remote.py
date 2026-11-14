#!/usr/bin/env python3
"""cubit-remote: the host client of a CuBit machine's ccl-remote endpoint.

The device side is userspace/ccl/apps/ccl-remote; the wire profile is
userspace/ccl/remote/remote_wire.ads (proved). One request per TCP
connection: a 28-byte header carrying the pairing token, then a body.

  cubit-remote pair HOST[:PORT] [--name NAME]   type the code the device shows
  cubit-remote hello                            check the pairing, show grants
  cubit-remote eval '(proc.list)'               evaluate CCL on the device
  cubit-remote logs SERVICE [--literal]         (logs.recent "SERVICE"), whole: records,
                                                gaps ("… N records not retained …") and
                                                tallies, one per line
  cubit-remote logs SERVICE --from MS --to MS   only the records whose time is in MS..MS
                                                (milliseconds since boot): logs.between,
                                                to read a long history in slices
  cubit-remote ps                               (proc.list), whole
  cubit-remote latency [--from MS --to MS]      input-to-photon (LAT-002): Desktop's
                                                "desktop: latency" report lines, and the
                                                display path's per-second lines
                                                (intel-gpu flips, display.svc stats)
  cubit-remote metrics series [NAME] [--source S]
                                                metrics.svc's series (MET-002): NAME exact, a
                                                trailing * a prefix, none every series
  cubit-remote metrics range SERIES [--last S | --from US --to US] [--step US] [--source S]
                                                one series' samples (or, with --step, its
                                                windows) and its gaps: metrics.range
  cubit-remote metrics window SERIES [--last S | --from US --to US] [--source S]
                                                count, rate, p50/p90/p99/p999 over a window,
                                                merged over every series SERIES selects
  cubit-remote metrics latency [--last S]       LAT-002 from the store: every desktop.latency
                                                stage and the frame series over the window
  cubit-remote ls [PATH] [--place P]            list the workspace (or mail)
  cubit-remote put LOCAL [REMOTE] [-r]          upload into the workspace
  cubit-remote get REMOTE [LOCAL]               download (SHA-256 checked)
  cubit-remote stat REMOTE | rm REMOTE
  cubit-remote build COMPONENT [--detach]       ccl-build on a workspace component
  cubit-remote build --attach JOB | --cancel JOB
  cubit-remote mail send FILE [--name NAME] | mail send --text TEXT --name NAME
  cubit-remote mail recv [--dir DIR] [--keep] | mail ls
  cubit-remote screenshot [-o FILE.png] [--output N] [--area X,Y,W,H]
                                                one frame of the device's screen as a PNG
  cubit-remote app replace PRODUCT              a workspace build's product becomes the
                                                app's next generation and replaces it
  cubit-remote app rollback APP | app status APP
  cubit-remote system activate SERVICE PRODUCT --health-ms N --stop-grace-ms N
                                                a workspace product becomes SERVICE's pin in a
                                                new system generation, which is activated
  cubit-remote system replace SERVICE LOCAL --health-ms N --stop-grace-ms N
                                                upload a program built here (to system/SERVICE)
                                                and activate it as above
  cubit-remote system rollback --health-ms N --stop-grace-ms N
  cubit-remote system generations               the generation records and the current one

Screenshots need the device's display-capture grant (its startup entry says
(display-capture approve-declared)) and the person at the device turning on
"Allow screen capture" (Deskbar, Remote access); otherwise the device says
why it refused. Each capture shows on the device (a mark in the Deskbar,
the remote window's activity list, its log) and is rate-limited.

App replacement needs the builds grant, a work root on the device's
system volume, and the person at the device turning on "Allow app
replacement" (Deskbar, Remote access); the device keeps each generation
(and its SHA-256) under its work root, outside the workspace.

System generations (like Nix): a product enters the device's storehouse
by its SHA-256, a new generation pins the service (its startup-plan name,
"acpi.svc") to it, and activating it makes procmgr stop the service and
start the pinned program as the same startup entry; a program that ends
or has not registered its service by the end of the health window is
rolled back to the one before, and the current generation stays. The
service's startup entry must declare a live restart contract, (replace
transparent) or (replace graceful), and the person at
the device must turn on "Allow app replacement". Both deadlines are
explicit (milliseconds): --health-ms and --stop-grace-ms (0: the service
reads no Stop, so procmgr ends it at once). The generations live in the
device's storehouse; on the live USB that is RAM, so a reboot returns to
the image's programs.

system replace is the upload and the activation in one command: it puts
LOCAL into the workspace as system/SERVICE and runs system activate on it.
For ACPI, from the repository root (the log says why a program failed its
window, so it is shown whatever the outcome):
  nix develop -c make -C kernel acpi-native && { tools/cubit-remote/cubit-remote \
    system replace acpi.svc userspace/services/acpi/native/build/acpi.svc \
    --health-ms 20000 --stop-grace-ms 10000; tools/cubit-remote/cubit-remote logs acpi; }

Pairing tokens are stored in ~/.config/cubit-remote/hosts.json (0600, the
directory 0700) and never printed. Python 3 standard library only.

A result is as long as its value (a whole log history, for example): the
evaluate reply carries its length and first chunk and read_result the rest,
so one evaluation is read whole. A list of scalars the device still shows
in part (it ends with "... N more") is fetched in slices: (length E), then
(last k (first i E)) for consecutive ranges; slices are separate
evaluations, so a list that changes meanwhile may shift between them.
"""

import argparse
import getpass
import hashlib
import json
import os
import re
import socket
import stat as stat_module
import struct
import sys
import time

# --- Wire profile (remote_wire.ads) -----------------------------------------
MAGIC = b"CBRM"
PROTOCOL_VERSION = 1
TOKEN_BYTES = 16
TOKEN_TEXT_LENGTH = 26
REQUEST_HEADER_BYTES = 28
RESPONSE_HEADER_BYTES = 12
MAXIMUM_CHUNK_BYTES = 32_768
MAXIMUM_NAME_BYTES = 240
MAXIMUM_SOURCE_BYTES = 4_096
MAXIMUM_FILE_BYTES = 268_435_456
MAXIMUM_RESPONSE_BODY = 65_536
# An Evaluate reply's fixed fields: ok (1), the whole text's length (8).
EVALUATE_FIXED_BYTES = 9
STAGING_SUFFIX = ".cubit-remote-part"
DEFAULT_PORT = 8445

OPERATIONS = {
    "hello": 1, "evaluate": 2, "list": 3, "stat": 4, "get": 5, "put": 6,
    "commit": 7, "remove": 8, "build_start": 9, "build_poll": 10, "build_cancel": 11,
    "capture": 12, "app_replace": 13, "app_rollback": 14, "app_status": 15,
    "system_activate": 16, "system_rollback": 17, "system_generations": 18,
    "read_result": 19,
}
PLACES = {"none": 0, "workspace": 1, "to-device": 2, "to-host": 3, "captures": 4}
STATUSES = [
    "OK", "Malformed", "Unauthorized", "Not_Granted", "Not_Found", "Too_Large",
    "Conflict", "Busy", "Failed", "Integrity_Mismatch", "Unavailable",
    "Unsupported_Version", "Locked", "Capacity",
]
JOB_STATES = {1: "running", 2: "exited", 3: "failed-to-start", 4: "cancelled", 5: "unknown"}
GRANTS = [(1, "diagnose"), (2, "files"), (4, "builds"), (8, "mail"), (16, "capture")]
# Remote_Wire.Capture_Result, with what the person reads for each refusal.
CAPTURE_RESULTS = [
    ("Captured", "captured"),
    ("No_Capability", "the device grants ccl-remote no display-capture capability "
                      "(its startup entry needs (display-capture approve-declared))"),
    ("Capture_Off", "screen capture is off on the device: the person there turns on "
                    "Allow screen capture (Deskbar, Remote access)"),
    ("Rate_Limited", "too soon after the last capture; try again in a second"),
    ("No_Session", "the device's Desktop has no remote session announced"),
    ("No_Frame", "that output shows no frame yet"),
    ("Bad_Output", "no such output"),
    ("Bad_Area", "the area is empty or leaves the output"),
    ("Capture_Failed", "the capture failed on the device (see its log)"),
]
CAPTURE_FIXED_BYTES = 1 + 4 + 4 + 8 + 32
# Remote_Wire.App_Result, with what the person reads for each refusal.
APP_RESULTS = [
    ("Replaced", "replaced"),
    ("Rolled_Back", "rolled back"),
    ("Reported", "reported"),
    ("Switch_Off", "app replacement is off on the device: the person there turns on "
                   "Allow app replacement (Deskbar, Remote access)"),
    ("Start_Failed", "the generation did not start (see the device's log); "
                     "the one stopped for it runs again"),
    ("Stop_Failed", "the running instance did not end, even when ended; nothing was started"),
    ("No_Previous", "the app has no previous generation"),
]
APP_RESULT_DONE = (0, 1, 2)
INSTANCE_STATES = {0: "not started", 1: "running", 2: "exited"}
STOP_KINDS = {0: "", 1: "stopped", 2: "ended after the stop deadline"}
APP_RESPONSE_BYTES = 1 + 4 + 4 + 1 + 1 + 32 + 32
MAXIMUM_APP_NAME_BYTES = 64
PRODUCT_SUFFIX = ".app"
# Remote_Wire.Generation_Result, and CuBit.Service_Replacement's Refusal
# and Health_Failure (the detail byte).
GENERATION_RESULTS = ["Activated", "Rolled_Back", "Refused", "Stop_Failed", "Rollback_Failed",
                      "Unchanged", "No_Previous", "Too_Many_Changes", "Pending_Boot"]
# Pending_Boot: a (replace reboot) service's pin, admitted by procmgr and
# made current; it runs from the next boot.
GENERATION_DONE = (0, 5, 8)
SWITCH_REFUSALS = {
    1: ("Malformed", "procmgr found the request malformed"),
    2: ("Grant_Unavailable", "procmgr could not read the request"),
    3: ("Unknown_Service", "no startup-plan entry runs that service"),
    4: ("Not_Replaceable", "the service's restart contract is reboot (its startup entry says "
                           "(replace reboot) or nothing): it is not switched live"),
    5: ("Not_Granted", "ccl-remote may not stage that program"),
    6: ("Switch_Off", "app replacement is off on the device: the person there turns on "
                      "Allow app replacement (Deskbar, Remote access)"),
    7: ("Unsigned_Refused", "signature policy refused the program"),
    8: ("Busy", "another switch, or startup, is in progress"),
    9: ("Image_Unreadable", "procmgr could not read the image's program"),
    10: ("Program_Unreadable", "procmgr could not read the program's manifest sections"),
    11: ("Manifest_Changed", "the program's manifest differs from the image's: a switch never "
                             "widens what the service holds (build a new image instead)"),
}
HEALTH_FAILURES = {0: "healthy", 1: "the program did not start", 2: "it ended inside its health window",
                   3: "it did not register its service by the end of the health window"}
GENERATION_RESPONSE_BYTES = 1 + 1 + 4 + 4 + 32 + 8
MAXIMUM_SERVICE_NAME_BYTES = 64
# CuBit.Service_Replacement's ceilings (milliseconds) and the time procmgr
# may take beyond the two deadlines (two ends and two starts).
MAXIMUM_STOP_GRACE_MS = 10_000
MINIMUM_HEALTH_MS, MAXIMUM_HEALTH_MS = 100, 30_000
SWITCH_OVERHEAD_MS = 2 * 5_000 + 10_000 + 2_000

# Whole lists: elements per slice at first (halved while a slice is still
# too large), and the device's own words for a result it could not carry.
LIST_SLICE_START = 32
TOO_LARGE_TEXT = "The result is too large to show whole"
# What an endpoint built before Result_Too_Large says for the same case.
OLD_TOO_LARGE_TEXT = "does not support the operation's value or lifecycle contract"
LIST_RESULT = re.compile(r"\A(List<[^>]*>): (?:\[(.*)\]|\(list-of [^)]*\))\Z", re.S)
MORE_ELEMENTS = re.compile(r"(?:, )?\.\.\. (\d+) more\Z")
INTEGER_RESULT = re.compile(r"\AInteger: (\d+)\Z")

# Seconds: connect, and the whole exchange of one request.
CONNECT_TIMEOUT_S = 10
EXCHANGE_TIMEOUT_S = 60
BUILD_POLL_INTERVAL_S = 0.5

SYMBOLS = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"


class RemoteError(Exception):
    """A response with a status other than OK."""

    def __init__(self, status, detail=""):
        self.status = status
        name = STATUSES[status] if status < len(STATUSES) else "status %d" % status
        super().__init__(name + (": " + detail if detail else ""))


def token_image(token):
    """The 26 Crockford base32 symbols of a 16-byte token."""
    assert len(token) == TOKEN_BYTES
    value = int.from_bytes(token, "big") << (TOKEN_TEXT_LENGTH * 5 - TOKEN_BYTES * 8)
    return "".join(SYMBOLS[(value >> (5 * (TOKEN_TEXT_LENGTH - 1 - i))) & 31]
                   for i in range(TOKEN_TEXT_LENGTH))


def decode_token_text(text):
    """What a person typed, as Remote_Wire.Decode_Token_Text reads it."""
    aliases = {"O": "0", "I": "1", "L": "1"}
    value = 0
    count = 0
    for c in text.upper():
        if c in "- ":
            continue
        c = aliases.get(c, c)
        index = SYMBOLS.find(c)
        if index < 0 or count == TOKEN_TEXT_LENGTH:
            raise ValueError("not a pairing code")
        value = value * 32 + index
        count += 1
    padding = TOKEN_TEXT_LENGTH * 5 - TOKEN_BYTES * 8
    if count != TOKEN_TEXT_LENGTH or value & ((1 << padding) - 1):
        raise ValueError("not a pairing code (26 symbols)")
    return (value >> padding).to_bytes(TOKEN_BYTES, "big")


def valid_name(name):
    """Remote_Wire.Valid_Name: a relative name inside a place."""
    if not 1 <= len(name) <= MAXIMUM_NAME_BYTES or name.endswith(STAGING_SUFFIX):
        return False
    for part in name.split("/"):
        if part in ("", ".", ".."):
            return False
        if any(not (c.isascii() and (c.isalnum() or c in "._-+")) for c in part):
            return False
    return True


def valid_app_name(name):
    """Remote_Wire.Valid_App_Name: one component, at most 64 bytes."""
    return 1 <= len(name) <= MAXIMUM_APP_NAME_BYTES and "/" not in name and valid_name(name)


def app_of_product(product):
    """Remote_Wire.App_Of_Product: the app a product is, or ""."""
    if len(product) <= len(PRODUCT_SUFFIX) or not product.endswith(PRODUCT_SUFFIX):
        return ""
    name = product.rsplit("/", 1)[-1][:-len(PRODUCT_SUFFIX)]
    return name if valid_app_name(name) else ""


def name_field(name):
    raw = name.encode("ascii")
    return struct.pack(">H", len(raw)) + raw


def request_bytes(operation, place, token, body=b""):
    return (MAGIC + bytes([PROTOCOL_VERSION, OPERATIONS[operation], PLACES[place], 0])
            + struct.pack(">I", len(body)) + token + body)


def exchange(address, port, token, operation, place="none", body=b"", timeout=EXCHANGE_TIMEOUT_S):
    """One request on its own connection; the response body, or RemoteError."""
    frame = request_bytes(operation, place, token, body)
    with socket.create_connection((address, port), timeout=CONNECT_TIMEOUT_S) as s:
        s.settimeout(timeout)
        s.sendall(frame)
        header = _read_exact(s, RESPONSE_HEADER_BYTES)
        if header[:4] != MAGIC or header[4] != PROTOCOL_VERSION:
            raise RemoteError(1, "not a ccl-remote response")
        length = struct.unpack(">I", header[8:12])[0]
        if length > MAXIMUM_RESPONSE_BODY:
            raise RemoteError(1, "response too long")
        payload = _read_exact(s, length)
    if header[5] != 0:
        raise RemoteError(header[5], payload.decode("ascii", "replace") if operation == "evaluate" else "")
    return payload


def _read_exact(s, count):
    data = bytearray()
    while len(data) < count:
        part = s.recv(count - len(data))
        if not part:
            raise RemoteError(8, "the device closed the connection")
        data += part
    return bytes(data)


# --- Host store --------------------------------------------------------------
def config_path():
    base = os.environ.get("XDG_CONFIG_HOME") or os.path.join(os.path.expanduser("~"), ".config")
    return os.path.join(base, "cubit-remote", "hosts.json")


def load_hosts():
    path = config_path()
    if not os.path.exists(path):
        return {"default": None, "hosts": {}}
    mode = stat_module.S_IMODE(os.stat(path).st_mode)
    if mode & 0o077:
        raise SystemExit("cubit-remote: %s is readable by others (mode %o); chmod 600 it" % (path, mode))
    with open(path) as f:
        return json.load(f)


def save_hosts(store):
    path = config_path()
    os.makedirs(os.path.dirname(path), mode=0o700, exist_ok=True)
    os.chmod(os.path.dirname(path), 0o700)
    temporary = path + ".new"
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, "w") as f:
        json.dump(store, f, indent=2)
    os.chmod(temporary, 0o600)
    os.replace(temporary, path)


class Host:
    def __init__(self, name, address, port, token):
        self.name, self.address, self.port, self.token = name, address, port, token

    def call(self, operation, place="none", body=b"", timeout=EXCHANGE_TIMEOUT_S):
        return exchange(self.address, self.port, self.token, operation, place, body, timeout)


def selected_host(args):
    store = load_hosts()
    name = args.host or store.get("default")
    if not name or name not in store["hosts"]:
        raise SystemExit("cubit-remote: no paired host%s; run cubit-remote pair HOST"
                         % (" named " + name if name else ""))
    entry = store["hosts"][name]
    return Host(name, entry["address"], entry["port"], bytes.fromhex(entry["token"]))


def grant_names(bits):
    return ", ".join(n for b, n in GRANTS if bits & b) or "none"


# --- Operations ---------------------------------------------------------------
def hello(host):
    body = host.call("hello")
    bits = body[0]
    chunk = struct.unpack(">I", body[1:5])[0]
    return bits, chunk, body[5:].decode("ascii", "replace")


def evaluate(host, source):
    """(ok, text): the whole result. The reply carries its length and first
    chunk; the device keeps the rest for read_result, chunk by chunk."""
    raw = source.encode("ascii")
    if not 1 <= len(raw) <= MAXIMUM_SOURCE_BYTES:
        raise SystemExit("cubit-remote: CCL source must be 1..%d ASCII bytes" % MAXIMUM_SOURCE_BYTES)
    body = host.call("evaluate", "none", raw)
    if len(body) < EVALUATE_FIXED_BYTES:
        raise RemoteError(1, "an evaluate reply shorter than its fixed fields")
    total = struct.unpack(">Q", body[1:EVALUATE_FIXED_BYTES])[0]
    text = bytearray(body[EVALUATE_FIXED_BYTES:])
    while len(text) < total:
        part = host.call("read_result", "none", struct.pack(">Q", len(text)))
        if len(part) < 8 or struct.unpack(">Q", part[:8])[0] != total or len(part) == 8:
            raise RemoteError(1, "the device's result changed or ended while it was read")
        text += part[8:]
    return body[0] == 1, bytes(text[:total]).decode("ascii", "replace")


def split_elements(body):
    """The top-level elements of a list's text: separated by spaces (record
    literals) or ", " (scalars), outside parentheses, brackets and quotes."""
    items, current, depth, quoted, escaped = [], [], 0, False, False
    for c in body:
        if quoted:
            current.append(c)
            if escaped:
                escaped = False
            elif c == "\\":
                escaped = True
            elif c == '"':
                quoted = False
            continue
        if c == '"':
            quoted = True
        elif c in "([":
            depth += 1
        elif c in ")]":
            depth -= 1
        elif depth == 0 and c in " ,":
            if current:
                items.append("".join(current))
                current = []
            continue
        current.append(c)
    if current:
        items.append("".join(current))
    return items


def parse_list(text):
    """A list result as (type, elements, more): more counts the elements the
    device left out ("... N more"). None when text is not a list result."""
    found = LIST_RESULT.match(text)
    if not found:
        return None
    body = found.group(2) or ""
    more = MORE_ELEMENTS.search(body)
    if more:
        body = body[:more.start()]
    return found.group(1), split_elements(body), int(more.group(1)) if more else 0


def single_form(source):
    """Whether source is one parenthesized expression (it can be sliced)."""
    source = source.strip()
    if not source.startswith("("):
        return False
    depth, quoted, escaped = 0, False, False
    for i, c in enumerate(source):
        if quoted:
            if escaped:
                escaped = False
            elif c == "\\":
                escaped = True
            elif c == '"':
                quoted = False
        elif c == '"':
            quoted = True
        elif c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return i == len(source) - 1
    return False


def too_large(text):
    return text.startswith(TOO_LARGE_TEXT) or OLD_TOO_LARGE_TEXT in text


def evaluate_whole(host, expression):
    """(ok, text) of expression with every element of a list result: when the
    device could not carry it whole, it is fetched in slices (module doc)."""
    ok, text = evaluate(host, expression)
    shown = parse_list(text) if ok else None
    if ok and (shown is None or shown[2] == 0):
        return ok, text
    if not ok and not (too_large(text) and single_form(expression)):
        return ok, text
    counted_ok, counted = evaluate(host, "(length %s)" % expression)
    count = INTEGER_RESULT.match(counted) if counted_ok else None
    if count is None:
        return ok, text
    total = int(count.group(1))
    kind, elements, step = shown[0] if shown else None, [], LIST_SLICE_START
    while len(elements) < total:
        size = min(step, total - len(elements))
        part_ok, part = evaluate(host, "(last %d (first %d %s))" % (size, len(elements) + size, expression))
        if not part_ok and too_large(part) and size > 1:
            step = max(1, size // 2)
            continue
        sliced = parse_list(part) if part_ok else None
        if sliced is None or not sliced[1]:
            return False, part if not part_ok else "cubit-remote: the list changed while it was read: " + part
        kind = sliced[0]
        elements += sliced[1]
    scalars = elements and not elements[0].startswith("(")
    return True, "%s: [%s]" % (kind or "List", (", " if scalars else " ").join(elements))


def remote_stat(host, place, name):
    body = host.call("stat", place, name_field(name))
    return struct.unpack(">Q", body[:8])[0], body[8:40]


def put_file(host, place, local, name, progress=None):
    if not valid_name(name):
        raise SystemExit("cubit-remote: %r is not a name inside the place" % name)
    size = os.path.getsize(local)
    if size > MAXIMUM_FILE_BYTES:
        raise SystemExit("cubit-remote: %s is larger than %d bytes" % (local, MAXIMUM_FILE_BYTES))
    digest = hashlib.sha256()
    offset = 0
    with open(local, "rb") as f:
        while True:
            chunk = f.read(MAXIMUM_CHUNK_BYTES)
            if offset > 0 and not chunk:
                break
            digest.update(chunk)
            host.call("put", place, name_field(name) + struct.pack(">Q", offset) + chunk)
            offset += len(chunk)
            if progress:
                progress(offset, size)
            if len(chunk) < MAXIMUM_CHUNK_BYTES:
                break
    host.call("commit", place, name_field(name) + struct.pack(">Q", offset) + digest.digest())
    return offset, digest.hexdigest()


def get_file(host, place, name, local):
    size, expected = remote_stat(host, place, name)
    digest = hashlib.sha256()
    offset = 0
    temporary = local + ".part"
    with open(temporary, "wb") as f:
        while offset < size:
            chunk = host.call("get", place, name_field(name) + struct.pack(">Q", offset))
            if not chunk:
                break
            f.write(chunk)
            digest.update(chunk)
            offset += len(chunk)
    if offset != size or digest.digest() != expected:
        os.unlink(temporary)
        raise RemoteError(9, "%s changed or arrived damaged (%d of %d bytes)" % (name, offset, size))
    os.replace(temporary, local)
    return size, digest.hexdigest()


class CaptureRefused(Exception):
    """The device answered a capture without a frame (Capture_Result)."""

    def __init__(self, code):
        self.code = code
        name, why = CAPTURE_RESULTS[code] if code < len(CAPTURE_RESULTS) else ("result %d" % code, "")
        self.name = name
        super().__init__("%s: %s" % (name, why) if why else name)


def capture_request(output=0, area=None):
    """Capture's body: output (1), then x, y, width, height (2 each)."""
    x, y, w, h = area or (0, 0, 0, 0)
    if not 0 <= output <= 255 or any(not 0 <= v <= 65535 for v in (x, y, w, h)):
        raise SystemExit("cubit-remote: an output is 0..255 and an area's fields 0..65535")
    return struct.pack(">BHHHH", output, x, y, w, h)


def parse_capture(body):
    """(width, height, size, sha256, name) of a Captured response, or
    CaptureRefused."""
    if not body:
        raise RemoteError(1, "an empty capture response")
    if body[0] != 0:
        raise CaptureRefused(body[0])
    if len(body) <= CAPTURE_FIXED_BYTES:
        raise RemoteError(1, "a short capture response")
    width, height, size = struct.unpack(">IIQ", body[1:17])
    return width, height, size, body[17:49], body[49:].decode("ascii", "replace")


def screenshot(host, local, output=0, area=None):
    """Take one frame and download its PNG to local (SHA-256 checked twice:
    against the stat and against what the capture reported)."""
    width, height, size, digest, name = parse_capture(host.call("capture", "none", capture_request(output, area)))
    if not valid_name(name):
        raise RemoteError(1, "the device named its capture %r" % name)
    got_size, got_digest = get_file(host, "captures", name, local)
    if got_size != size or got_digest != digest.hex():
        os.unlink(local)
        raise RemoteError(9, "%s is not the frame just captured (it changed)" % name)
    return width, height, size, got_digest


class AppRefused(Exception):
    """An app operation the device answered without doing (App_Result)."""

    def __init__(self, report):
        self.report = report
        code = report["result"]
        name, why = APP_RESULTS[code] if code < len(APP_RESULTS) else ("result %d" % code, "")
        self.name = name
        super().__init__("%s: %s" % (name, why))


def parse_app(body):
    """App_Response as a dict: result, current, previous, instance, stop,
    current_sha256, previous_sha256 (hex)."""
    if len(body) != APP_RESPONSE_BYTES:
        raise RemoteError(1, "an app response of %d bytes" % len(body))
    result, current, previous, instance, stop = struct.unpack(">BIIBB", body[:11])
    return {"result": result, "current": current, "previous": previous, "instance": instance,
            "stop": stop, "current_sha256": body[11:43].hex(), "previous_sha256": body[43:75].hex()}


def app_request(operation, name):
    """(place, body) of an app operation on name (a product or an app)."""
    if operation == "app_replace":
        if not valid_name(name) or not app_of_product(name):
            raise SystemExit("cubit-remote: %r is not a product (.../APP.app) in the workspace" % name)
        return "workspace", name_field(name)
    if not valid_app_name(name):
        raise SystemExit("cubit-remote: %r is not an app's name" % name)
    return "none", name_field(name)


def app_call(host, operation, name):
    """One app operation; its report, or AppRefused."""
    place, body = app_request(operation, name)
    report = parse_app(host.call(operation, place, body))
    if report["result"] not in APP_RESULT_DONE:
        raise AppRefused(report)
    return report


def app_image(app, report):
    def gen(n):
        return "generation %d" % n
    if not report["current"]:
        return "%s: no generation" % app
    text = "%s: %s %s (sha256 %s)" % (app, gen(report["current"]), INSTANCE_STATES.get(report["instance"], "?"),
                                      report["current_sha256"])
    if report["previous"]:
        stop = STOP_KINDS.get(report["stop"], "")
        text += "; previous %s%s" % (gen(report["previous"]), " " + stop if stop else "")
    return text


class GenerationRefused(Exception):
    """An activation that did not make the target current (Generation_Result)."""

    def __init__(self, report):
        self.report = report
        self.name = report["result_name"]
        super().__init__(generation_image(report))


def valid_service_name(name):
    """Remote_Wire.Valid_Service_Name: a startup-plan entry's name."""
    return (1 <= len(name) <= MAXIMUM_SERVICE_NAME_BYTES
            and all(c.isascii() and (c.isalnum() or c in "._-+") for c in name))


def parse_generation(body):
    """Generation_Response as a dict."""
    if len(body) != GENERATION_RESPONSE_BYTES:
        raise RemoteError(1, "a generation response of %d bytes" % len(body))
    result, detail, current, target = struct.unpack(">BBII", body[:10])
    running = struct.unpack(">Q", body[42:50])[0]
    return {"result": result,
            "result_name": GENERATION_RESULTS[result] if result < len(GENERATION_RESULTS) else str(result),
            "detail": detail, "current": current, "target": target,
            "pin_sha256": body[10:42].hex(), "running": running}


def deadlines(health_ms, stop_grace_ms):
    if not MINIMUM_HEALTH_MS <= health_ms <= MAXIMUM_HEALTH_MS:
        raise SystemExit("cubit-remote: --health-ms is %d .. %d" % (MINIMUM_HEALTH_MS, MAXIMUM_HEALTH_MS))
    if not 0 <= stop_grace_ms <= MAXIMUM_STOP_GRACE_MS:
        raise SystemExit("cubit-remote: --stop-grace-ms is 0 .. %d" % MAXIMUM_STOP_GRACE_MS)
    # The device answers within both deadlines plus procmgr's own bound.
    timeout = (health_ms + stop_grace_ms + SWITCH_OVERHEAD_MS) / 1000 + CONNECT_TIMEOUT_S
    return struct.pack(">II", stop_grace_ms, health_ms), timeout


def system_activate(host, service, product, health_ms, stop_grace_ms):
    """Pin SERVICE to PRODUCT in a new generation and activate it; the report, or GenerationRefused."""
    if not valid_service_name(service):
        raise SystemExit("cubit-remote: %r is not a startup-plan service name" % service)
    if not valid_name(product):
        raise SystemExit("cubit-remote: %r is not a name in the workspace" % product)
    fields, timeout = deadlines(health_ms, stop_grace_ms)
    report = parse_generation(host.call("system_activate", "workspace",
                                        name_field(product) + fields + service.encode("ascii"), timeout))
    if report["result"] not in GENERATION_DONE:
        raise GenerationRefused(report)
    return report


def replace_product_name(service):
    """Where system replace puts the program in the workspace."""
    name = "system/" + service
    if not valid_name(name):
        raise SystemExit("cubit-remote: %r is not a name in the workspace" % name)
    return name


def system_rollback(host, health_ms, stop_grace_ms):
    fields, timeout = deadlines(health_ms, stop_grace_ms)
    report = parse_generation(host.call("system_rollback", "none", fields, timeout))
    if report["result"] not in GENERATION_DONE:
        raise GenerationRefused(report)
    return report


def system_generations(host):
    """The records (typed CCL values) and the current number."""
    text = host.call("system_generations").decode("utf-8", "replace")
    lines = text.splitlines()
    current = None
    if lines and lines[-1].startswith("current "):
        current = int(lines.pop().split()[1])
    return lines, current


def generation_image(report):
    name = report["result_name"]
    if name == "Activated":
        text = "generation %d active" % report["current"]
    elif name == "Unchanged":
        text = "generation %d current; nothing to switch" % report["current"]
    elif name == "Pending_Boot":
        text = ("generation %d current; its restart contract is reboot, so it runs from the next boot"
                % report["current"])
    elif name == "Rolled_Back":
        text = ("generation %d failed: %s; rolled back, generation %d stays"
                % (report["target"], HEALTH_FAILURES.get(report["detail"], "?"), report["current"]))
    elif name == "Refused":
        refusal = SWITCH_REFUSALS.get(report["detail"], ("?", "refusal %d" % report["detail"]))
        text = "refused (%s): %s" % refusal
    elif name == "Stop_Failed":
        text = "the running instance did not end; nothing changed"
    elif name == "Rollback_Failed":
        text = "FAILED: the new program failed and the one before did not start again"
    elif name == "No_Previous":
        text = "the image's generation (0) has none before it"
    else:
        text = "the generations differ in more pins than one activation switches"
    if report["pin_sha256"].strip("0") and name in ("Activated", "Rolled_Back"):
        text += "; pin sha256 %s" % report["pin_sha256"]
    if report["running"]:
        text += "; pid %d" % report["running"]
    return text


def build_poll(host, job, offset):
    body = host.call("build_poll", "none", struct.pack(">IQ", job, offset))
    state, code, total, first_kept = struct.unpack(">BIQQ", body[:21])
    return state, code, total, first_kept, body[21:]


def follow_build(host, job, out):
    """Stream a job's output until it ends; its exit code (or 1)."""
    offset = 0
    while True:
        state, code, total, first_kept, output = build_poll(host, job, offset)
        if first_kept > offset:
            out.write("[cubit-remote: %d bytes of output were not kept]\n" % (first_kept - offset))
            offset = first_kept
        out.write(output.decode("utf-8", "replace"))
        out.flush()
        offset += len(output)
        if state != 1 and offset >= total:
            sys.stderr.write("cubit-remote: job %d %s%s\n" % (
                job, JOB_STATES.get(state, "?"), " (exit code %d)" % code if state == 2 else ""))
            return code if state == 2 else 1
        if not output:
            time.sleep(BUILD_POLL_INTERVAL_S)


# --- Commands -------------------------------------------------------------------
def command_pair(args):
    target = args.target
    address, _, port_text = target.partition(":")
    port = int(port_text) if port_text else DEFAULT_PORT
    if args.code_stdin:
        code = sys.stdin.readline().strip()
    else:
        code = getpass.getpass("Pairing code shown on the device: ")
    try:
        token = decode_token_text(code)
    except ValueError as e:
        raise SystemExit("cubit-remote: %s" % e)
    host = Host(args.name or address, address, port, token)
    bits, _, server = hello(host)
    store = load_hosts()
    store["hosts"][host.name] = {"address": address, "port": port, "token": token.hex()}
    store["default"] = host.name
    save_hosts(store)
    print("paired with %s (%s:%d, %s); the host may: %s" % (host.name, address, port, server, grant_names(bits)))


def command_hosts(args):
    store = load_hosts()
    for name, entry in sorted(store["hosts"].items()):
        mark = "*" if name == store.get("default") else " "
        print("%s %s %s:%d" % (mark, name, entry["address"], entry["port"]))


def command_unpair(args):
    store = load_hosts()
    store["hosts"].pop(args.name, None)
    if store.get("default") == args.name:
        store["default"] = next(iter(store["hosts"]), None)
    save_hosts(store)


def command_hello(args):
    bits, chunk, server = hello(selected_host(args))
    print("%s; the host may: %s; chunk %d bytes" % (server, grant_names(bits), chunk))


def command_eval(args):
    source = open(args.file).read() if args.file else args.source
    if source is None:
        raise SystemExit("cubit-remote: give CCL source or --file")
    ok, text = evaluate_whole(selected_host(args), source)
    print(text)
    return 0 if ok else 1


# (logs.recent "service") is a List<LogItem> (LOG-005): records, typed gaps
# where records are missing, and each instance's tally.
LOG_ITEM = re.compile(r'\A\(LogItem\.(Logged|Gap|Tally) \((LogEntry|LogGap|LogTally) (.*)\)\)\Z', re.S)
LOG_ENTRY_FIELDS = re.compile(r'\A(-?\d+) Severity\.(\w+) (-?\d+) "((?:[^"\\]|\\.)*)"\Z', re.S)
LOG_GAP_FIELDS = re.compile(r'\A(-?\d+) (-?\d+) (-?\d+) GapReason\.(\w+) "(?:[^"\\]|\\.)*"\Z', re.S)
LOG_TALLY_FIELDS = re.compile(r'\A(-?\d+) (-?\d+) (-?\d+) (-?\d+) (-?\d+) (-?\d+) "(?:[^"\\]|\\.)*"\Z', re.S)
GAP_WORDS = {
    "Not_Retained": "not retained (logstore's history for it was at its ceiling, logs.retention)",
    "Lost": "lost before delivery",
    "Not_Shown": "retained but not shown (past the items one result holds; narrow it with --from/--to)",
}


def unescape(text):
    return re.sub(r'\\(.)', r'\1', text)


def render_log_item(element):
    """One LogItem as a line: a record, "… N records not retained …", or a
    tally. None when element is not a LogItem."""
    found = LOG_ITEM.match(element)
    if not found:
        return None
    member, _, fields = found.groups()
    if member == "Logged":
        entry = LOG_ENTRY_FIELDS.match(fields)
        if not entry:
            return None
        time_ms, level, source, message = entry.groups()
        return "%12.3f  %-11s  %-8s  %s" % (int(time_ms) / 1000.0, level, source, unescape(message))
    if member == "Gap":
        gap = LOG_GAP_FIELDS.match(fields)
        if not gap or gap.group(4) not in GAP_WORDS:
            return None
        _, source, count, reason = gap.groups()
        whose = "" if source == "0" else " of source %s" % source
        return "\u2026 %s records%s %s \u2026" % (count, whose, GAP_WORDS[reason])
    tally = LOG_TALLY_FIELDS.match(fields)
    if not tally:
        return None
    source, published, retained, not_retained, over_budget, publisher_full = tally.groups()
    return ("source %s: %s published, %s retained, %s not retained; shed before publication: "
            "%s over logstore's budget, %s by the publisher (ring full)"
            % (source, published, retained, not_retained, over_budget, publisher_full))


def render_logs(text):
    """A logs.recent result, one line per item; text itself when it is not
    a List<LogItem> (a failure, or a device that predates LOG-005)."""
    shown = parse_list(text)
    if shown is None or shown[0] != "List<LogItem>":
        return text
    if not shown[1]:
        return "(no records)"
    lines = [render_log_item(element) for element in shown[1]]
    if any(line is None for line in lines):
        return text
    return "\n".join(lines)


def logs_expression(service, start, end):
    """(logs.recent "service"), or, given a window, (logs.between (LogWindow
    "service" from to)): the records whose time (milliseconds since boot)
    is in from..to. Both ends are given; the device refuses a window that
    ends before it starts."""
    if start is None and end is None:
        return '(logs.recent "%s")' % service
    if start is None or end is None:
        raise SystemExit("cubit-remote: a window needs both --from and --to (milliseconds since boot)")
    return '(logs.between (LogWindow "%s" %d %d))' % (service, start, end)


def command_logs(args):
    expression = logs_expression(args.service, args.start, args.end)
    ok, text = evaluate_whole(selected_host(args), expression)
    print(text if args.literal or not ok else render_logs(text))
    return 0 if ok else 1


# LAT-002: the report lines each service writes (Desktop_Latency_Report,
# intel_gpu_display_executor Report, display.svc maybePrintStats).
LATENCY_SOURCES = (
    ("desktop", "desktop: latency "),
    ("intel-gpu", "intel-gpu: flips per-second "),
    ("display", "display: stats "),
)


def latency_lines(rendered, prefix):
    return [line for line in rendered.splitlines() if prefix in line]


def command_latency(args):
    host = selected_host(args)
    status = 0
    for service, prefix in LATENCY_SOURCES:
        ok, text = evaluate_whole(host, logs_expression(service, args.start, args.end))
        if not ok:
            print("%s: %s" % (service, text))
            status = 1
            continue
        lines = latency_lines(render_logs(text), prefix)
        print("\n".join(lines) if lines else "%s: (no %s records)" % (service, prefix.strip()))
    return status


# MET-002: metrics.svc's series store through metrics.series, metrics.range
# and metrics.window (CCL.Interfaces.Metrics). Times are microseconds of the
# device's monotonic clock; 0 or less is that long before now.
MICROSECONDS_PER_SECOND = 1000000


def parse_fields(text):
    """A record or variant literal as (name, fields): fields are ints, str,
    enum member names (Type.Member), or nested (name, fields)."""
    text = text.strip()
    if not (text.startswith("(") and text.endswith(")")):
        raise ValueError("not a record literal: %r" % text[:60])
    inner = text[1:-1]
    parts = split_elements(inner)
    if not parts:
        raise ValueError("an empty literal")
    head, fields = parts[0], []
    for part in parts[1:]:
        if part.startswith("("):
            fields.append(parse_fields(part))
        elif part.startswith('"'):
            fields.append(unescape(part[1:-1]))
        elif re.fullmatch(r"-?\d+", part):
            fields.append(int(part))
        else:
            fields.append(part)
    return head, fields


def metric_text(value):
    return '"%s"' % value.replace("\\", "\\\\").replace('"', '\\"')


def metric_times(args):
    if args.start is not None or args.end is not None:
        if args.start is None or args.end is None:
            raise SystemExit("cubit-remote: give both --from and --to (microseconds since boot), or --last")
        return args.start, args.end
    return -int(args.last * MICROSECONDS_PER_SECOND), 0


def series_expression(name, source):
    return "(metrics.series (Metric_Selector name => %s source => %s))" % (metric_text(name), metric_text(source))


def range_expression(series, source, start, end, step):
    return ("(metrics.range (Metric_Range series => %s source => %s from => %d to => %d step => %d))"
            % (metric_text(series), metric_text(source), start, end, step))


def window_expression(series, source, start, end):
    return ("(metrics.window (Metric_Window series => %s source => %s from => %d to => %d))"
            % (metric_text(series), metric_text(source), start, end))


SERIES_FIELDS = ("id", "name", "source", "program", "instance", "kind", "unit", "published",
                 "retained", "shed", "evicted", "first", "last", "last_value")
STATS_FIELDS = ("series", "count", "sum", "min", "max", "last", "p50", "p90", "p99", "p999",
                "rate_milli", "missing", "resolution", "error_permille")


def member(value):
    return value.split(".", 1)[1] if isinstance(value, str) and "." in value else value


def render_series(text):
    shown = parse_list(text)
    if shown is None or shown[0] != "List<Metric_Series>":
        return text
    if not shown[1]:
        return "(no series)"
    lines = ["%-44s %-26s %-8s %-12s %-12s %10s %8s %8s"
             % ("name", "source", "instance", "kind", "unit", "samples", "shed", "evicted")]
    for element in shown[1]:
        _, fields = parse_fields(element)
        row = dict(zip(SERIES_FIELDS, fields))
        lines.append("%-44s %-26s %-8s %-12s %-12s %10d %8d %8d"
                     % (row["name"], row["source"], row["instance"], member(row["kind"]),
                        member(row["unit"]), row["published"], row["shed"], row["evicted"]))
    return "\n".join(lines)


def render_range(text):
    shown = parse_list(text)
    if shown is None or shown[0] != "List<Metric_Item>":
        return text
    lines = []
    for element in shown[1]:
        name, fields = parse_fields(element)
        _, payload = fields[0]
        if name.endswith(".Point"):
            lines.append("%14.6f  %d" % (payload[0] / MICROSECONDS_PER_SECOND, payload[1]))
        elif name.endswith(".Rollup"):
            start, count, total, low, high, last, p50, p99 = payload
            lines.append("%14.6f  n=%d sum=%d min=%d max=%d last=%d p50=%d p99=%d"
                         % (start / MICROSECONDS_PER_SECOND, count, total, low, high, last, p50, p99))
        else:
            first, last, count, cause = payload
            lines.append("\u2026 %d samples %s between %.6f and %.6f \u2026"
                         % (count, member(cause).replace("_", " ").lower(),
                            first / MICROSECONDS_PER_SECOND, last / MICROSECONDS_PER_SECOND))
    return "\n".join(lines) if lines else "(no samples in the range)"


def window_stats(text):
    """A metrics.window result ("Metric_Stats: (Metric_Stats ...)") as a
    dict, or None."""
    typed = re.match(r"\AMetric_Stats: (\(.*\))\Z", text, re.S)
    try:
        name, fields = parse_fields(typed.group(1) if typed else text)
    except ValueError:
        return None
    if name != "Metric_Stats" or len(fields) != len(STATS_FIELDS):
        return None
    return dict(zip(STATS_FIELDS, fields))


def render_window(text):
    stats = window_stats(text)
    if stats is None:
        return text
    return ("series=%(series)d n=%(count)d sum=%(sum)d min=%(min)d max=%(max)d last=%(last)d "
            "p50=%(p50)d p90=%(p90)d p99=%(p99)d p999=%(p999)d rate=%(rate)s/s missing=%(missing)d "
            "resolution_us=%(resolution)d error<=%(error)s%%"
            % dict(stats, rate="%.3f" % (stats["rate_milli"] / 1000.0),
                   error="%.1f" % (stats["error_permille"] / 10.0)))


def command_metrics(args):
    host = selected_host(args)
    if args.what == "series":
        ok, text = evaluate_whole(host, series_expression(args.name or "", args.source))
        print(text if args.literal or not ok else render_series(text))
        return 0 if ok else 1
    if args.what in ("range", "window") and not args.name:
        raise SystemExit("cubit-remote: name the series")
    start, end = metric_times(args)
    if args.what == "range":
        ok, text = evaluate(host, range_expression(args.name, args.source, start, end, args.step))
        print(text if args.literal or not ok else render_range(text))
        return 0 if ok else 1
    if args.what == "window":
        ok, text = evaluate(host, window_expression(args.name, args.source, start, end))
        print(text if args.literal or not ok else render_window(text))
        return 0 if ok else 1
    # latency: every LAT-002 series over the window, from the store.
    ok, text = evaluate_whole(host, series_expression("desktop.latency.*", ""))
    shown = parse_list(text) if ok else None
    if shown is None:
        print(text)
        return 1
    names = sorted({dict(zip(SERIES_FIELDS, parse_fields(e)[1]))["name"] for e in shown[1]})
    frames_ok, frames = evaluate_whole(host, series_expression("desktop.frames.*", ""))
    frame_list = parse_list(frames) if frames_ok else None
    if frame_list:
        names += sorted({dict(zip(SERIES_FIELDS, parse_fields(e)[1]))["name"] for e in frame_list[1]})
    status = 0
    for name in names:
        ok, text = evaluate(host, window_expression(name, "", start, end))
        if not ok:
            status = 1
        print("%-44s %s" % (name, render_window(text) if ok else text))
    return status


def command_ps(args):
    ok, text = evaluate_whole(selected_host(args), "(proc.list)")
    print(text)
    return 0 if ok else 1


def command_ls(args):
    path = args.path or ""
    if path and not valid_name(path):
        raise SystemExit("cubit-remote: %r is not a name inside the place" % path)
    sys.stdout.write(selected_host(args).call("list", args.place, name_field(path)).decode("ascii", "replace"))


def command_put(args):
    host = selected_host(args)
    if os.path.isdir(args.local):
        if not args.recursive:
            raise SystemExit("cubit-remote: %s is a directory (use -r)" % args.local)
        base = args.remote or os.path.basename(os.path.normpath(args.local))
        count = 0
        for root, dirs, files in os.walk(args.local):
            dirs.sort()
            for f in sorted(files):
                local = os.path.join(root, f)
                relative = os.path.relpath(local, args.local).replace(os.sep, "/")
                put_file(host, args.place, local, base + "/" + relative)
                count += 1
        print("put %d files under %s" % (count, base))
    else:
        name = args.remote or os.path.basename(args.local)
        size, digest = put_file(host, args.place, args.local, name)
        print("put %s (%d bytes, sha256 %s)" % (name, size, digest))


def command_get(args):
    local = args.local or os.path.basename(args.remote)
    size, digest = get_file(selected_host(args), args.place, args.remote, local)
    print("got %s (%d bytes, sha256 %s)" % (local, size, digest))


def command_stat(args):
    size, digest = remote_stat(selected_host(args), args.place, args.remote)
    print("%s %d sha256 %s" % (args.remote, size, digest.hex()))


def command_rm(args):
    selected_host(args).call("remove", args.place, name_field(args.remote))


def command_build(args):
    host = selected_host(args)
    if args.cancel:
        body = host.call("build_cancel", "none", struct.pack(">I", args.cancel))
        print("job %d %s" % (args.cancel, JOB_STATES.get(body[0], "?")))
        return 0
    if args.attach:
        return follow_build(host, args.attach, sys.stdout)
    if not args.component:
        raise SystemExit("cubit-remote: give a component (its component.ccl, relative to the workspace)")
    job = struct.unpack(">I", host.call("build_start", "workspace", name_field(args.component)))[0]
    sys.stderr.write("cubit-remote: job %d started (cubit-remote build --attach %d)\n" % (job, job))
    if args.detach:
        print(job)
        return 0
    return follow_build(host, job, sys.stdout)


def command_mail(args):
    host = selected_host(args)
    if args.mail_command == "send":
        if args.text is not None:
            if not args.name:
                raise SystemExit("cubit-remote: --text needs --name")
            temporary = os.path.join(os.environ.get("TMPDIR", "/tmp"), "cubit-remote-mail-%d" % os.getpid())
            with open(temporary, "w") as f:
                f.write(args.text)
            try:
                put_file(host, "to-device", temporary, args.name)
            finally:
                os.unlink(temporary)
            print("sent %s" % args.name)
        else:
            name = args.name or os.path.basename(args.file)
            put_file(host, "to-device", args.file, name)
            print("sent %s" % name)
    elif args.mail_command == "ls":
        sys.stdout.write(host.call("list", "to-host", name_field("")).decode("ascii", "replace"))
    else:
        listing = host.call("list", "to-host", name_field("")).decode("ascii", "replace")
        os.makedirs(args.dir, exist_ok=True)
        for line in listing.splitlines():
            kind, _, rest = line.partition(" ")
            _, _, name = rest.partition(" ")
            if kind != "file" or name.endswith(STAGING_SUFFIX) or not valid_name(name):
                continue
            get_file(host, "to-host", name, os.path.join(args.dir, name))
            if not args.keep:
                host.call("remove", "to-host", name_field(name))
            print(os.path.join(args.dir, name))


def command_screenshot(args):
    host = selected_host(args)
    area = None
    if args.area:
        try:
            area = tuple(int(v) for v in args.area.split(","))
        except ValueError:
            area = ()
        if len(area) != 4:
            raise SystemExit("cubit-remote: --area is X,Y,WIDTH,HEIGHT")
    local = args.o or "screenshot-%s-%s.png" % (host.name.replace(":", "_"), time.strftime("%Y%m%d-%H%M%S"))
    try:
        width, height, size, digest = screenshot(host, local, args.output, area)
    except CaptureRefused as e:
        sys.stderr.write("cubit-remote: no screenshot: %s\n" % e)
        return 2
    print("saved %s (%dx%d, %d bytes, sha256 %s)" % (local, width, height, size, digest))
    return 0


APP_COMMANDS = {"replace": ("app_replace", "replaced"), "rollback": ("app_rollback", "rolled back"),
                "status": ("app_status", "reported")}


def command_app(args):
    host = selected_host(args)
    operation, done = APP_COMMANDS[args.app_command]
    app = app_of_product(args.name) if operation == "app_replace" else args.name
    try:
        report = app_call(host, operation, args.name)
    except AppRefused as e:
        sys.stderr.write("cubit-remote: %s not %s: %s\n" % (app or args.name, done, e))
        return 2
    print(app_image(app, report))
    return 0


def command_system(args):
    host = selected_host(args)
    if args.system_command == "generations":
        lines, current = system_generations(host)
        for line in lines:
            print(line)
        print("current %s" % current)
        return 0
    product = getattr(args, "product", None)
    if args.system_command == "replace":
        # Checked before anything moves: the deadlines and both names.
        deadlines(args.health_ms, args.stop_grace_ms)
        if not valid_service_name(args.service):
            raise SystemExit("cubit-remote: %r is not a startup-plan service name" % args.service)
        product = replace_product_name(args.service)
        if not os.path.isfile(args.local):
            raise SystemExit("cubit-remote: %s is not a file" % args.local)
        size, digest = put_file(host, "workspace", args.local, product)
        print("put %s (%d bytes, sha256 %s)" % (product, size, digest))
    try:
        if args.system_command in ("activate", "replace"):
            report = system_activate(host, args.service, product, args.health_ms, args.stop_grace_ms)
        else:
            report = system_rollback(host, args.health_ms, args.stop_grace_ms)
    except GenerationRefused as e:
        sys.stderr.write("cubit-remote: %s\n" % e)
        return 2
    print(generation_image(report))
    return 0


def parser():
    p = argparse.ArgumentParser(prog="cubit-remote", description=__doc__.split("\n")[0])
    p.add_argument("--host", help="a paired host's name (default: the last paired)")
    sub = p.add_subparsers(dest="command", required=True)

    s = sub.add_parser("pair", help="pair with a device: type the code it shows")
    s.add_argument("target", help="HOST[:PORT] (port %d by default)" % DEFAULT_PORT)
    s.add_argument("--name", help="what to call it here (default: the host)")
    s.add_argument("--code-stdin", action="store_true", help="read the code from standard input")
    s.set_defaults(run=command_pair)

    s = sub.add_parser("hosts", help="the paired hosts")
    s.set_defaults(run=command_hosts)
    s = sub.add_parser("unpair", help="forget a host's token")
    s.add_argument("name")
    s.set_defaults(run=command_unpair)
    s = sub.add_parser("hello", help="check the pairing; show what the host may do")
    s.set_defaults(run=command_hello)

    s = sub.add_parser("eval", help="evaluate CCL on the device")
    s.add_argument("source", nargs="?")
    s.add_argument("--file")
    s.set_defaults(run=command_eval)
    s = sub.add_parser("logs", help="a service's recent log records")
    s.add_argument("service")
    s.add_argument("--literal", action="store_true", help="print the typed List<LogItem> as CCL")
    s.add_argument("--from", dest="start", type=int, metavar="MS",
                   help="the window's first millisecond since boot (with --to): logs.between")
    s.add_argument("--to", dest="end", type=int, metavar="MS",
                   help="the window's last millisecond since boot (with --from)")
    s.set_defaults(run=command_logs)
    s = sub.add_parser("ps", help="the processes")
    s.set_defaults(run=command_ps)
    s = sub.add_parser("metrics", help="metrics.svc's series (MET-002)")
    s.add_argument("what", choices=("series", "range", "window", "latency"))
    s.add_argument("name", nargs="?", help="a series name (series: a trailing * is a prefix)")
    s.add_argument("--source", default="", help="only this source (a program identity)")
    s.add_argument("--last", type=float, default=60.0, metavar="S",
                   help="the window: the last S seconds (default 60)")
    s.add_argument("--from", dest="start", type=int, metavar="US",
                   help="the window's first microsecond since boot (with --to; 0 or less: before now)")
    s.add_argument("--to", dest="end", type=int, metavar="US", help="the window's last microsecond (with --from)")
    s.add_argument("--step", type=int, default=0, metavar="US",
                   help="range: windows no wider than US instead of raw samples")
    s.add_argument("--literal", action="store_true", help="print the typed CCL result")
    s.set_defaults(run=command_metrics)
    s = sub.add_parser("latency", help="input-to-photon report lines (LAT-002)")
    s.add_argument("--from", dest="start", type=int, metavar="MS",
                   help="the window's first millisecond since boot (with --to)")
    s.add_argument("--to", dest="end", type=int, metavar="MS",
                   help="the window's last millisecond since boot (with --from)")
    s.set_defaults(run=command_latency)

    for name, function, helptext in (
            ("ls", command_ls, "list a place"), ("stat", command_stat, "size and SHA-256"),
            ("rm", command_rm, "remove a file"), ("get", command_get, "download a file"),
            ("put", command_put, "upload a file or (-r) a directory")):
        s = sub.add_parser(name, help=helptext)
        s.add_argument("--place", choices=["workspace", "to-device", "to-host", "captures"], default="workspace")
        if name == "ls":
            s.add_argument("path", nargs="?")
        elif name == "put":
            s.add_argument("local")
            s.add_argument("remote", nargs="?")
            s.add_argument("-r", "--recursive", action="store_true")
        elif name == "get":
            s.add_argument("remote")
            s.add_argument("local", nargs="?")
        else:
            s.add_argument("remote")
        s.set_defaults(run=function)

    s = sub.add_parser("build", help="ccl-build a workspace component; follow its output")
    s.add_argument("component", nargs="?")
    s.add_argument("--detach", action="store_true", help="print the job and return")
    s.add_argument("--attach", type=int, metavar="JOB", help="follow a job started earlier")
    s.add_argument("--cancel", type=int, metavar="JOB", help="interrupt a running job")
    s.set_defaults(run=command_build)

    s = sub.add_parser("mail", help="the mailbox with the device's agent")
    mail = s.add_subparsers(dest="mail_command", required=True)
    m = mail.add_parser("send", help="a file (or --text) into mail/to-device")
    m.add_argument("file", nargs="?")
    m.add_argument("--text")
    m.add_argument("--name")
    m = mail.add_parser("recv", help="take what mail/to-host holds")
    m.add_argument("--dir", default=".")
    m.add_argument("--keep", action="store_true", help="leave the messages on the device")
    mail.add_parser("ls", help="list mail/to-host")
    s.set_defaults(run=command_mail)

    s = sub.add_parser("screenshot", help="one frame of the device's screen, saved as a PNG")
    s.add_argument("-o", metavar="FILE.png", help="where to save it (default: screenshot-HOST-TIME.png)")
    s.add_argument("--output", type=int, default=0, help="the output (monitor) number, from 0")
    s.add_argument("--area", metavar="X,Y,W,H", help="only this rectangle, in the output's pixels")
    s.set_defaults(run=command_screenshot)

    s = sub.add_parser("app", help="apps built on the device: swap in a product, roll back")
    app = s.add_subparsers(dest="app_command", required=True)
    a = app.add_parser("replace", help="a product in the workspace (.../APP.app) becomes APP's next generation")
    a.add_argument("name", metavar="PRODUCT")
    a = app.add_parser("rollback", help="start APP's previous generation again")
    a.add_argument("name", metavar="APP")
    a = app.add_parser("status", help="APP's generations and whether it runs")
    a.add_argument("name", metavar="APP")
    s.set_defaults(run=command_app)

    s = sub.add_parser("system", help="system generations: pin a service to a build, activate, roll back")
    system = s.add_subparsers(dest="system_command", required=True)
    for name, helptext in (("activate", "PRODUCT becomes SERVICE's pin in a new generation, activated"),
                           ("replace", "upload LOCAL as system/SERVICE and activate it as SERVICE's pin"),
                           ("rollback", "activate the current generation's previous one")):
        a = system.add_parser(name, help=helptext)
        if name in ("activate", "replace"):
            a.add_argument("service", metavar="SERVICE", help='its startup-plan name, "acpi.svc"')
        if name == "activate":
            a.add_argument("product", metavar="PRODUCT", help="the program in the workspace")
        if name == "replace":
            a.add_argument("local", metavar="LOCAL", help="the program built on this host")
        # Explicit, never defaulted: they are part of what the caller asks.
        a.add_argument("--health-ms", type=int, required=True,
                       help="the health window, %d .. %d" % (MINIMUM_HEALTH_MS, MAXIMUM_HEALTH_MS))
        a.add_argument("--stop-grace-ms", type=int, required=True,
                       help="how long the running instance has to obey Stop, 0 .. %d "
                            "(0: a service that reads no Stop is ended at once)" % MAXIMUM_STOP_GRACE_MS)
    system.add_parser("generations", help="the generation records and the current one")
    s.set_defaults(run=command_system)
    return p


def main(argv=None):
    args = parser().parse_args(argv)
    try:
        result = args.run(args)
    except RemoteError as e:
        sys.stderr.write("cubit-remote: %s\n" % e)
        return 1
    except OSError as e:
        sys.stderr.write("cubit-remote: %s\n" % e)
        return 1
    return result or 0


if __name__ == "__main__":
    sys.exit(main())
