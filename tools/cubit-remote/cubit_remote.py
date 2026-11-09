#!/usr/bin/env python3
"""cubit-remote: the host client of a CuBit machine's ccl-remote endpoint.

The device side is userspace/ccl/apps/ccl-remote; the wire profile is
userspace/ccl/remote/remote_wire.ads (proved). One request per TCP
connection: a 28-byte header carrying the pairing token, then a body.

  cubit-remote pair HOST[:PORT] [--name NAME]   type the code the device shows
  cubit-remote hello                            check the pairing, show grants
  cubit-remote eval '(proc.list)'               evaluate CCL on the device
  cubit-remote logs SERVICE                     (logs.recent "SERVICE"), whole
  cubit-remote ps                               (proc.list), whole
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

Pairing tokens are stored in ~/.config/cubit-remote/hosts.json (0600, the
directory 0700) and never printed. Python 3 standard library only.

A list result longer than one response carries (the device says "too large
to show whole", or ends a list with "... N more") is fetched whole, in
slices: (length E), then (last k (first i E)) for consecutive ranges, k
halved while a slice is still too large. Slices are separate evaluations,
so a list that changes meanwhile (new log records) may shift between them.
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
STAGING_SUFFIX = ".cubit-remote-part"
DEFAULT_PORT = 8445

OPERATIONS = {
    "hello": 1, "evaluate": 2, "list": 3, "stat": 4, "get": 5, "put": 6,
    "commit": 7, "remove": 8, "build_start": 9, "build_poll": 10, "build_cancel": 11,
    "capture": 12, "app_replace": 13, "app_rollback": 14, "app_status": 15,
    "system_activate": 16, "system_rollback": 17, "system_generations": 18,
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
    raw = source.encode("ascii")
    if not 1 <= len(raw) <= MAXIMUM_SOURCE_BYTES:
        raise SystemExit("cubit-remote: CCL source must be 1..%d ASCII bytes" % MAXIMUM_SOURCE_BYTES)
    body = host.call("evaluate", "none", raw)
    return body[0] == 1, body[1:].decode("ascii", "replace")


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


def command_logs(args):
    ok, text = evaluate_whole(selected_host(args), '(logs.recent "%s")' % args.service)
    print(text)
    return 0 if ok else 1


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
    try:
        if args.system_command == "activate":
            report = system_activate(host, args.service, args.product, args.health_ms, args.stop_grace_ms)
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
    s.set_defaults(run=command_logs)
    s = sub.add_parser("ps", help="the processes")
    s.set_defaults(run=command_ps)

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
                           ("rollback", "activate the current generation's previous one")):
        a = system.add_parser(name, help=helptext)
        if name == "activate":
            a.add_argument("service", metavar="SERVICE", help='its startup-plan name, "acpi.svc"')
            a.add_argument("product", metavar="PRODUCT", help="the program in the workspace")
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
