#!/usr/bin/env python3
"""The headless remote-screenshot test's host side (tests/headless/run.sh).

Usage: remote_screenshot.py SERIAL_LOG MONITOR_SOCKET TIMEOUT_SECONDS PORT [OUT_DIR]

A desktop and ccl-remote, which the startup profile grants Desktop's
display-capture endpoint (tests/headless/init-remote-screenshot.ccl). Pairs
like a LAN host (QEMU's user-net forward), then checks REMOTE-001's
remote screenshots end to end:
  - hello reports the capture grant; Desktop shows the remote session;
  - with "Allow screen capture" off (the default) a capture, over the wire
    and through CCL's (display.capture), is refused with the typed reason;
  - by keyboard only (Super+N opens the Deskbar panel, whose Remote access
    section is first: Tab focuses the switch, Space turns it on) the person
    allows it; then `cubit-remote screenshot` saves a PNG of the whole
    output at its native size, not blank; an area capture saves just that
    rectangle; (display.capture) returns an Image of the output's size;
  - a capture sooner than Desktop's interval is refused (rate limit);
  - turning the switch off refuses captures again;
  - every capture and refusal is a "ccl-remote: host capture ..." line and
    a "desktop: screen captured/refused ..." line.
The PNGs are kept in OUT_DIR (default: a temporary directory). Quits QEMU.
Prints PASS/FAIL lines; exits 1 on any failure.
"""
import os
import re
import socket
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parents[1] / "tools" / "cubit-remote"))
sys.path.insert(0, str(HERE))
import cubit_remote as cr  # noqa: E402
import check_png  # noqa: E402

serial, monitor, timeout, port = sys.argv[1], sys.argv[2], float(sys.argv[3]), int(sys.argv[4])
out_dir = Path(sys.argv[5]) if len(sys.argv) > 5 else Path(tempfile.mkdtemp(prefix="remote-screenshot-"))
out_dir.mkdir(parents=True, exist_ok=True)
deadline = time.monotonic() + timeout
CODE = re.compile(rb"ccl-remote: pairing code ([0-9A-Z-]{31})")
LISTENING = b"ccl-remote: listening on "
SESSION_SHOWN = b"desktop: remote session (process "
CAPTURED = re.compile(rb"desktop: screen captured for process \d+: output 0 (\d+)x(\d+) \(capture")
# Desktop_Capture_Policy.Minimum_Interval_Ms, with room.
RATE_INTERVAL_S = 1.0
INTERVAL_PAUSE_S = RATE_INTERVAL_S + 0.3
# A real desktop frame has far more colours than a blank or one-colour one.
MINIMUM_COLOURS = 16
failures = 0


def check(condition, what):
    global failures
    print(("PASS: " if condition else "FAIL: ") + what, flush=True)
    if not condition:
        failures += 1


def hmp(command):
    with socket.socket(socket.AF_UNIX) as s:
        s.settimeout(3)
        s.connect(monitor)
        s.sendall((command + "\n").encode())
        time.sleep(0.05)


def keys(*names, pause=0.3):
    for name in names:
        hmp("sendkey " + name)
        time.sleep(pause)


def serial_bytes():
    try:
        return Path(serial).read_bytes()
    except OSError:
        return b""


def wait_for_serial(marker, seconds=10, count=1):
    end = time.monotonic() + seconds
    while serial_bytes().count(marker) < count and time.monotonic() < end:
        time.sleep(0.2)
    return serial_bytes().count(marker) >= count


def capture_code(host, area=None):
    """The Capture_Result of one wire capture (0: captured), its fields."""
    body = host.call("capture", "none", cr.capture_request(0, area))
    try:
        return 0, cr.parse_capture(body)
    except cr.CaptureRefused as refused:
        return refused.code, None


def colours(rgb):
    return len({rgb[i:i + 3] for i in range(0, len(rgb), 3)})


def main():
    code = None
    while time.monotonic() < deadline:
        data = serial_bytes()
        found = CODE.search(data)
        if found and LISTENING in data and SESSION_SHOWN in data:
            code = found.group(1).decode()
            break
        if b"CUBIT KERNEL PANIC" in data:
            break
        time.sleep(0.5)
    check(code is not None, "the guest listens, shows a pairing code, and Desktop shows the remote session")
    if code is None:
        return
    host = cr.Host("guest", "127.0.0.1", port, cr.decode_token_text(code))
    for _ in range(20):
        try:
            bits, _, _ = cr.hello(host)
            break
        except (OSError, cr.RemoteError):
            time.sleep(0.5)
    else:
        check(False, "hello with the pairing token")
        return
    check(bits == 0b10001, "hello: grants %s (diagnose and capture only)" % cr.grant_names(bits))
    check(wait_for_serial(b"desktop: remote session (process ") and
          wait_for_serial(b") paired with "), "Desktop shows who paired")
    time.sleep(2)   # Desktop's first frames are on screen

    # Off by default: refused, with the typed reason, both ways.
    result, _ = capture_code(host)
    check(result == 2, "with the switch off a capture is refused: Capture_Off (got %d)" % result)
    ok, text = cr.evaluate(host, "(display.capture)")
    check(not ok and "screen capture is off" in text, "(display.capture) is refused while off: %r" % text[:120])
    check(wait_for_serial(b"desktop: screen capture refused for process ") and
          wait_for_serial(b"ccl-remote: host capture output 0 (screen capture is off"),
          "the refusals are logged on the device")

    # The person allows it: Super+N, Tab to the switch, Space.
    keys("meta_l-n")
    time.sleep(0.8)
    keys("tab", "spc")
    check(wait_for_serial(b"desktop: screen capture allowed for the remote session"),
          "the person turns on Allow screen capture (keyboard, the status panel)")
    time.sleep(0.8)   # the panel redrawn with the switch on

    # The whole output, as a PNG on the host.
    whole = out_dir / "remote-screenshot.png"
    try:
        width, height, size, digest = cr.screenshot(host, str(whole))
        made = True
    except (cr.CaptureRefused, cr.RemoteError) as error:
        made, width, height = False, 0, 0
        print("  screenshot: %s" % error)
    check(made, "cubit-remote screenshot saved %s" % whole)
    shown = CAPTURED.search(serial_bytes())
    if made:
        w, h, rgb = check_png.decode(whole.read_bytes())
        check((w, h) == (width, height) and shown is not None and (w, h) == (int(shown.group(1)), int(shown.group(2))),
              "the PNG is the output's native size (%dx%d; Desktop says %s)"
              % (w, h, shown.group(0).decode() if shown else "nothing"))
        check(colours(rgb) >= MINIMUM_COLOURS, "the PNG is not blank (%d colours)" % colours(rgb))
    check(wait_for_serial(b"ccl-remote: host capture output 0 ") and
          wait_for_serial(b"-> captures/output-0.png ("), "the capture is an activity line on the device")

    # Through CCL: an Image of the output's size.
    time.sleep(INTERVAL_PAUSE_S)
    ok, text = cr.evaluate(host, "(display.capture)")
    check(ok and text.startswith("Image: (Image %d %d " % (width, height)),
          "(display.capture) returns the output as an Image: %r" % text[:80])
    # The rate limit: two captures in one evaluation, the second refused.
    ok, text = cr.evaluate(host, "(list (display.capture) (display.capture))")
    check(not ok and "too soon after the last capture" in text,
          "a capture sooner than the interval is refused: %r" % text[:120])
    check(wait_for_serial(b"desktop: screen capture refused for process ") and
          wait_for_serial(b"(too soon after the last capture)"), "the rate-limited capture is logged")

    # An area of the output.
    time.sleep(INTERVAL_PAUSE_S)
    part = out_dir / "remote-screenshot-area.png"
    try:
        aw, ah, _, _ = cr.screenshot(host, str(part), 0, (16, 24, 200, 100))
        w, h, rgb = check_png.decode(part.read_bytes())
        check((aw, ah, w, h) == (200, 100, 200, 100), "an area capture is just that rectangle (%dx%d)" % (w, h))
    except (cr.CaptureRefused, cr.RemoteError) as error:
        check(False, "an area capture: %s" % error)
    time.sleep(INTERVAL_PAUSE_S)
    result, _ = capture_code(host, (width - 10, 0, 20, 20))
    check(result == 7, "an area leaving the output is refused: Bad_Area (got %d)" % result)

    # A full screenshot while the last capture's Deskbar mark still shows.
    marked = out_dir / "remote-screenshot-marked.png"
    time.sleep(INTERVAL_PAUSE_S)
    keys("esc")
    time.sleep(0.6)
    try:
        cr.screenshot(host, str(marked))
    except (cr.CaptureRefused, cr.RemoteError) as error:
        print("  marked screenshot: %s" % error)

    # Off again: refused again.
    keys("meta_l-n")
    time.sleep(0.8)
    keys("tab", "spc", "esc")
    check(wait_for_serial(b"desktop: screen capture not allowed for the remote session"),
          "the person turns screen capture off")
    time.sleep(INTERVAL_PAUSE_S)
    result, _ = capture_code(host)
    check(result == 2, "after turning it off a capture is refused again (got %d)" % result)
    print("remote-screenshot: PNGs in %s" % out_dir, flush=True)


try:
    main()
finally:
    try:
        hmp("quit")
    except OSError as error:
        print("remote-screenshot: could not quit QEMU: %s" % error, file=sys.stderr)
print("remote-screenshot: %s" % ("PASS" if not failures else "FAIL (%d)" % failures), flush=True)
sys.exit(1 if failures else 0)
