#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""BUG-007: after one input and no further input, the change reaches the
screen within a named bound (tests/headless/run.sh --test screen-settle).

Runs inside the headless files test's injector once Files has listed its
first folder. Each step sends one key through QEMU's monitor (the real
PS/2 -> ps2.drv -> Desktop path), sends nothing else, and screendumps the
whole display SETTLE_BOUND_S after the key was taken. The screendump is
what the scanout holds, so a frame that was drawn but not yet presented
(a lost wake or damage, a late fence) fails here even though the serial
log shows the action on time.

Steps, each against the shot taken before the first key:
  Super held     released, the Apps menu opens  (screen differs)
  Esc            it closes                      (its pixels as before)
  Super tap      the Apps menu opens            (screen differs)
  Esc            it closes                      (its pixels as before)
  Shift+F10      Files' context menu opens      (screen differs)
  Esc            it closes                      (its pixels as before)
  Down           Files' cursor moves one row    (screen differs)
  Up             it moves back                  (its pixels as before)

A step that fails at the bound is looked at once more after LATE_LOOK_S
of quiet, to tell a late frame from one that waits for the next input.

The held key is pressed and released by QMP input-send-event, two events
with the hold between them. QEMU's own hold (HMP "sendkey KEY MS", QMP
send-key "hold-time") queues the release after MS and then a second MS of
delay behind it, so every key sent in the 2 x MS after the press reaches
the guest only when that queue drains: the screen then looks one step
behind while CuBit shows each key within a millisecond of its arrival
(BUG-007). SCREEN_SETTLE_HOLD=sendkey holds that way instead, to show it.

Usage: settle.py MONITOR_SOCKET QMP_SOCKET SHOT_PREFIX
Prints SCREEN-SETTLE: lines and exits 0 when every step met the bound.
"""
import json
import os
import socket
import sys
import time

monitor, qmp_path, prefix = sys.argv[1:4]
hold_mode = os.environ.get('SCREEN_SETTLE_HOLD', 'qmp')

# The bound from the key's acceptance to the screen showing its result.
SETTLE_BOUND_S = 0.5
# A failed step's second look, with no input in between.
LATE_LOOK_S = 3.0
# Quiet before the base shot, so the first frames have settled.
BASE_QUIET_S = 1.0
# The Deskbar's bottom rows (its clock, CPU and memory bars change on
# their own) are left out of every comparison.
TASKBAR_ROWS = 32
# A menu or a moved cursor row changes at least this many pixels; once
# closed (moved back), at least this share of them is as before.
CHANGED_MIN_PIXELS = 500
RESTORED_MIN = 0.97
# The kernel's stuck-call panel: its background colour (Boot_Diagnostics.
# Stuck_Background, 16#202020#), from this column, on this share of a row.
PANEL_BACKGROUND = bytes([0x20, 0x20, 0x20])
PANEL_LEFT = 8
PANEL_SHARE = 0.25
MONITOR_TIMEOUT_S = 10.0
# The first step holds Super this long: a press, then its release.
HOLD_MS = 3000
PROMPT = b'(qemu)'


class Monitor:
    def __init__(self, path):
        self.s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.s.connect(path)
        self.s.settimeout(MONITOR_TIMEOUT_S)
        self.wait_prompt()

    def wait_prompt(self):
        buffer = b''
        while PROMPT not in buffer:
            chunk = self.s.recv(4096)
            if not chunk:
                raise RuntimeError('monitor closed')
            buffer += chunk

    def command(self, text):
        """Sends one command; returns once QEMU has carried it out."""
        self.s.sendall((text + '\n').encode())
        self.wait_prompt()


class QMP:
    def __init__(self, path):
        self.s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.s.connect(path)
        self.s.settimeout(MONITOR_TIMEOUT_S)
        self.f = self.s.makefile('rb')
        self.reply()                      # the greeting
        self.execute('qmp_capabilities')

    def reply(self):
        while True:
            line = self.f.readline()
            if not line:
                raise RuntimeError('QMP closed')
            message = json.loads(line)
            if 'event' in message:
                continue
            return message

    def execute(self, command, arguments=None):
        request = {'execute': command}
        if arguments is not None:
            request['arguments'] = arguments
        self.s.sendall((json.dumps(request) + '\n').encode())
        answer = self.reply()
        if 'error' in answer:
            raise RuntimeError('QMP %s: %s' % (command, answer['error']))

    def key(self, qcode, down):
        """One key edge, sent to the guest at once (no QEMU key queue)."""
        self.execute('input-send-event', {'events': [
            {'type': 'key', 'data': {'down': down, 'key': {'type': 'qcode', 'data': qcode}}}]})


def load(path):
    with open(path, 'rb') as f:
        data = f.read()
    parts = data.split(b'\n', 3)
    if parts[0] != b'P6':
        raise RuntimeError('%s: not a binary PPM' % path)
    width, height = map(int, parts[1].split())
    return width, height, parts[3]


def panel_rows(image):
    """Rows the kernel's diagnostic panel covers (Boot_Diagnostics.
    Stuck_Calls: drawn straight into the scanout on its own schedule, over
    whatever Desktop presented, so its rows say nothing about Desktop)."""
    width, height, data = image
    stride = width * 3
    rows = set()
    for y in range(height):
        row = data[y * stride:(y + 1) * stride]
        dark = sum(1 for x in range(PANEL_LEFT, width // 2)
                   if row[x * 3:x * 3 + 3] == PANEL_BACKGROUND)
        if dark >= (width // 2 - PANEL_LEFT) * PANEL_SHARE:
            rows.add(y)
    return rows


def differing(base, image):
    """The pixel offsets where image differs from base (Deskbar and the
    kernel's panel left out)."""
    width, height, a = base
    w2, h2, b = image
    if (width, height) != (w2, h2):
        raise RuntimeError('the screen changed size')
    found = set()
    stride = width * 3
    skipped = panel_rows(base) | panel_rows(image)
    for y in range(max(0, height - TASKBAR_ROWS)):
        if y in skipped:
            continue
        row = y * stride
        if a[row:row + stride] == b[row:row + stride]:
            continue
        for x in range(width):
            i = row + x * 3
            if a[i:i + 3] != b[i:i + 3]:
                found.add(i)
    return found


def judge(base, image, differs, opened):
    """An opening step changes at least CHANGED_MIN_PIXELS; a closing step
    puts back the pixels its opening step changed (at least RESTORED_MIN of
    them: a kernel diagnostic overlay may be drawn over a few meanwhile)."""
    now = differing(base, image)
    if differs:
        return len(now) >= CHANGED_MIN_PIXELS, now, '%d pixels changed' % len(now)
    if not opened:
        return False, now, 'its opening step changed nothing'
    restored = 1.0 - len(now & opened) / len(opened)
    return restored >= RESTORED_MIN, now, '%.1f%% of %d pixels back' % (100.0 * restored, len(opened))


def main():
    mon = Monitor(monitor)
    qmp = QMP(qmp_path)
    shots = 0

    def shot(name):
        nonlocal shots
        shots += 1
        path = '%s-settle-%02d-%s.ppm' % (prefix, shots, name)
        mon.command('screendump "%s"' % path)
        return load(path)

    time.sleep(BASE_QUIET_S)
    base = shot('base')
    # A held Super first (its release opens the Apps menu), then the
    # next key at once after the release.
    steps = [('hold meta_l', 'held-super-apps-open', True),
             ('esc', 'held-apps-closed', False),
             ('meta_l', 'apps-open', True),
             ('esc', 'apps-closed', False),
             ('shift-f10', 'context-open', True),
             ('esc', 'context-closed', False),
             ('down', 'cursor-moved', True),
             ('up', 'cursor-back', False)]
    failures = 0
    opened = set()
    for key, name, differs in steps:
        if key.startswith('hold '):
            held = key.split()[1]
            if hold_mode == 'sendkey':
                mon.command('sendkey %s %d' % (held, HOLD_MS))
                time.sleep(HOLD_MS / 1000.0)
            else:
                qmp.key(held, True)
                time.sleep(HOLD_MS / 1000.0)
                qmp.key(held, False)
        else:
            mon.command('sendkey %s' % key)
        # A held key's result follows its release.
        taken = time.monotonic()
        time.sleep(SETTLE_BOUND_S)
        image = shot(name)
        elapsed = time.monotonic() - taken
        ok, now, why = judge(base, image, differs, opened)
        print('SCREEN-SETTLE: %s %s at %.2f s: %s: %s' %
              (key, name, elapsed, why, 'ok' if ok else 'FAIL'), flush=True)
        if not ok:
            failures += 1
            time.sleep(LATE_LOOK_S)
            late_image = shot(name + '-late')
            late_ok, late_now, late_why = judge(base, late_image, differs, opened)
            print('SCREEN-SETTLE: %s %s after %.1f s more of quiet: %s: %s' %
                  (key, name, LATE_LOOK_S, late_why,
                   'late' if late_ok else 'still not shown (waits for the next input)'),
                  flush=True)
            if late_ok:
                now = late_now
        if differs:
            opened = now
    print('SCREEN-SETTLE: %s (%d of %d steps late; bound %.1f s; held by %s)' %
          ('PASS' if failures == 0 else 'FAIL', failures, len(steps), SETTLE_BOUND_S, hold_mode),
          flush=True)
    return 0 if failures == 0 else 1


if __name__ == '__main__':
    sys.exit(main())
