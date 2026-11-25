#!/usr/bin/env python3
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
"""Drives the desktop-launch headless test over QMP (tests/headless/run.sh).

BUG-008: on run-desktop's session, apps stopped launching from the Desktop
after the first one, with no message. This launches apps through the Apps
menu the way a person does, one after another, and checks each one:

  1. Bible by keyboard (Super, arrows, Enter), then closed by a tablet
     click on its close button
  2. Dictionary by keyboard, closed the same way
  3. Files: the menu opened by Super, the entry clicked with the tablet;
     closed
  4. Calculator the same way, left open
  5. Text by keyboard, and at once Dictionary again: each launch must start
     or be refused with a visible notice ("desktop: launch failed:"),
     never dropped in silence

For every launch: Desktop's "launch submitted token=T" and "launch complete
token=T", the app's own "<app>: starting" and window marker, in that order
and after the menu was used. Desktop's lines reach serial through the test
logstore (the session profile approves the test console).

The Apps menu's rows and entries, and each window's close button, are
placed from the geometry Desktop logs ("desktop: apps menu row/entry ...",
"desktop: surface N close button at ...").

Writes one line per step to RESULT_FILE ("done" last), then quits QEMU.
Usage: desktop-launch.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS RESULT_FILE
Environment: DESKTOP_LAUNCH_SIZE=WxH, the output (default 1024x768).
"""
import json
import os
import re
import socket
import sys
import time

PANIC = 'CUBIT KERNEL PANIC'
# QEMU's absolute axes (INPUT_EVENT_ABS_MAX).
ABS_MAX = 0x7FFF
KEY_HOLD_S = 0.06
KEY_GAP_S = 0.15
LAUNCH_WAIT_S = 60
SETTLE_S = 3.0

PLACE = r' at\s+(?P<x>\d+),\s*(?P<y>\d+) size\s+(?P<w>\d+) x\s+(?P<h>\d+)'
ROW = re.compile(r'desktop: apps menu row\s+(?P<index>\d+) (?P<name>\S+)' + PLACE)
# The menu's order (system.ccl's categories and keys), for a Desktop that
# does not log its menu: the keyboard path only.
KNOWN_ROWS = ['System', 'Development', 'Web', 'Games', 'Tools', 'Reference']
KNOWN_ENTRIES = {'Reference': ['Dictionary', 'Bible'], 'Tools': ['Files', 'Text', 'Calculator']}
SUBMITTED = re.compile(r'desktop: launch submitted token=(\d+)')
SURFACE = re.compile(r'desktop: surface\s+(\d+) at\s+(\d+),\s*(\d+) size')

# label, category, the app's start and window markers
APPS = {
    'Bible': ('Reference', 'bible: starting', 'bible: window ready'),
    'Dictionary': ('Reference', 'dictionary: starting', 'dictionary: window ready'),
    'Files': ('Tools', 'files: starting', 'files: native window ready'),
    'Calculator': ('Tools', 'calculator: starting', 'calculator: window ready'),
    'Text': ('Tools', 'text: starting', 'text: native window ready'),
}


class Failure(Exception):
    pass


def main(serial_path, socket_path, timeout, result_path):
    deadline = time.monotonic() + timeout - 5
    width, height = (int(v) for v in os.environ.get('DESKTOP_LAUNCH_SIZE', '1024x768').split('x'))
    results = []

    def note(line):
        results.append(line)
        print('desktop-launch: ' + line, flush=True)
        with open(result_path, 'w') as f:
            f.write('\n'.join(results) + '\n')

    def text():
        try:
            with open(serial_path, errors='replace') as f:
                return f.read()
        except FileNotFoundError:
            return ''

    def wait(pattern, what, since=0, seconds=LAUNCH_WAIT_S):
        """The first match of pattern after offset since."""
        until = min(deadline, time.monotonic() + seconds)
        regex = re.compile(pattern)
        while time.monotonic() < until:
            data = text()
            if PANIC in data:
                raise Failure('kernel panic')
            found = regex.search(data, since)
            if found:
                return found
            time.sleep(0.1)
        raise Failure(f'timed out waiting for {what}')

    qmp = None
    while time.monotonic() < deadline and qmp is None:
        try:
            qmp = socket.socket(socket.AF_UNIX)
            qmp.connect(socket_path)
        except OSError:
            qmp = None
            time.sleep(0.2)
    if qmp is None:
        raise Failure('QMP socket never opened')
    stream = qmp.makefile('rwb')
    json.loads(stream.readline())

    def command(name, arguments=None):
        message = {'execute': name}
        if arguments:
            message['arguments'] = arguments
        stream.write(json.dumps(message).encode() + b'\n')
        stream.flush()
        while True:
            reply = json.loads(stream.readline())
            if 'event' in reply:
                continue
            if 'error' in reply:
                raise Failure(f'QMP {name}: {reply["error"]}')
            return reply

    command('qmp_capabilities')

    def key(qcode):
        for down in (True, False):
            command('input-send-event', {'events': [
                {'type': 'key', 'data': {'down': down, 'key': {'type': 'qcode', 'data': qcode}}}]})
            time.sleep(KEY_HOLD_S if down else KEY_GAP_S)

    def point(x, y):
        command('input-send-event', {'events': [
            {'type': 'abs', 'data': {'axis': 'x', 'value': x * ABS_MAX // max(1, width - 1)}},
            {'type': 'abs', 'data': {'axis': 'y', 'value': y * ABS_MAX // max(1, height - 1)}}]})
        time.sleep(0.15)

    def click(x, y):
        point(x, y)
        for down in (True, False):
            command('input-send-event', {'events': [
                {'type': 'btn', 'data': {'down': down, 'button': 'left'}}]})
            time.sleep(0.08)
        time.sleep(0.3)

    def centre(found):
        x, y, w, h = (int(found.group(k)) for k in 'xywh')
        return x + w // 2, y + h // 2

    def open_menu():
        """Super opens the Apps menu: its rows as Desktop drew them (None
        when Desktop does not log them)."""
        since = len(text())
        key('meta_l')
        try:
            wait(ROW.pattern, 'the Apps menu rows', since, 5)
        except Failure:
            return None, since
        time.sleep(0.3)
        return {m.group('name'): m for m in ROW.finditer(text(), since)}, since

    def entry_after(since, label):
        pattern = r'desktop: apps menu entry\s+(?P<index>\d+) ' + re.escape(label) + PLACE
        found = wait(pattern, f'the {label} entry', since, 10)
        time.sleep(0.2)
        return found

    def choose(label, by):
        category = APPS[label][0]
        rows, since = open_menu()
        if rows is None:
            if by != 'keyboard':
                raise Failure('Desktop logs no Apps menu geometry for the tablet')
            if not geometry_missing:
                geometry_missing.append(True)
                note('Desktop logs no Apps menu geometry: keys follow system.ccl order')
            row = KNOWN_ROWS.index(category) + 1
        elif category not in rows:
            raise Failure(f'no {category} row in the Apps menu ({", ".join(rows)})')
        else:
            row = int(rows[category].group('index'))
        if by == 'keyboard':
            for _ in range(row - 1):
                key('down')
            key('right')
            if rows is None:
                time.sleep(0.3)
                index = KNOWN_ENTRIES[category].index(label) + 1
            else:
                index = int(entry_after(since, label).group('index'))
            for _ in range(index - 1):
                key('down')
            mark = len(text())
            key('ret')
        else:
            point(*centre(rows[category]))     # a category opens on hover
            found = entry_after(since, label)
            mark = len(text())
            click(*centre(found))
        return mark

    def started(label, mark, how):
        """The launch after mark: submitted, completed, the app running."""
        _, starting, window = APPS[label]
        submitted = wait(SUBMITTED.pattern + '|desktop: launch (busy or quarantined)',
                         f'{label}: Desktop submitting its launch', mark, 15)
        if submitted.group(1) is None:
            raise Failure(f'{label}: Desktop refused the launch in silence ("{submitted.group(0)}")')
        token = submitted.group(1)
        completed = wait(rf'desktop: launch complete token={token} pid=|desktop: launch completion uncertain',
                         f'{label}: the launch completing (token {token})', submitted.end(), 30)
        if 'uncertain' in completed.group(0):
            # The app may still start; the next launch shows the cost.
            misread.append(label)
            note(f'{label}: Desktop could not read the launch reply (token {token}): "{completed.group(0)}"')
        wait(re.escape(starting), f'{label}: "{starting}"', mark)
        wait(re.escape(window), f'{label}: "{window}"', mark)
        surface = wait(SURFACE.pattern, f'{label}: its window', mark, 30)
        note(f'launched {label} by {how} (token {token}, surface {surface.group(1)})')
        return surface.group(1)

    def close(label, surface):
        try:
            found = wait(rf'desktop: surface\s+{surface} close button at\s+(\d+),\s*(\d+) size\s+(\d+) x\s+(\d+)',
                         f'{label}: its close button', 0, 15)
        except Failure:
            # A Desktop that does not log it: the window stays open (the
            # next launch is what this test is about).
            note(f'{label}: Desktop logs no close button; left open')
            return
        x, y, w, h = (int(v) for v in found.groups())
        mark = len(text())
        time.sleep(0.5)
        click(x + w // 2, y + h // 2)
        wait(rf'desktop: surface\s*{surface}\b[^\n]* ended: ', f'{label}: its window closing', mark, 20)
        note(f'closed {label}')
        time.sleep(1.0)

    geometry_missing = []
    misread = []
    wait(r'desktop: asynchronous frame released', 'the first frame', 0, timeout)
    time.sleep(SETTLE_S)

    for label, how, keep in (('Bible', 'keyboard', False), ('Dictionary', 'keyboard', False),
                             ('Files', 'tablet', False), ('Calculator', 'tablet', True)):
        surface = started(label, choose(label, how), how)
        if not keep:
            close(label, surface)

    # Two launches at once: each starts, or is refused out loud.
    first = choose('Text', 'keyboard')
    second = choose('Dictionary', 'keyboard')
    for label, mark, until in (('Text', first, second), ('Dictionary', second, None)):
        data = text()
        window = data[mark:until] if until else data[mark:]
        if not SUBMITTED.search(window) and 'desktop: launch failed:' not in window:
            wait(r'desktop: launch submitted token=|desktop: launch failed:',
                 f'{label}: a launch or a notice', mark, 15)
    wait(re.escape(APPS['Text'][2]), 'Text: its window', first)
    data = text()[second:]
    if 'desktop: launch failed:' in data:
        note('second quick launch refused with a notice: ' +
             data[data.index('desktop: launch failed:'):].splitlines()[0])
    else:
        wait(re.escape(APPS['Dictionary'][2]), 'Dictionary (second time): its window', second)
        note('launched Text and Dictionary back to back')

    if misread:
        raise Failure(f'Desktop misread the launch replies of {", ".join(misread)}')
    silent = re.findall(r'desktop: launch (busy or quarantined|completion uncertain)', text())
    if silent:
        raise Failure(f'Desktop dropped a launch in silence ({silent[0]})')
    note('done')


if __name__ == '__main__':
    serial_path, socket_path, timeout, result_path = sys.argv[1:5]
    try:
        main(serial_path, socket_path, float(timeout), result_path)
    except Exception as error:  # the result file says which step failed
        with open(result_path, 'a') as f:
            f.write(f'FAIL: {error}\n')
        print(f'desktop-launch: FAIL: {error}', file=sys.stderr, flush=True)
    finally:
        try:
            with socket.socket(socket.AF_UNIX) as s:
                s.connect(socket_path)
                f = s.makefile('rwb')
                f.readline()
                for name in ('qmp_capabilities', 'quit'):
                    f.write(json.dumps({'execute': name}).encode() + b'\n')
                    f.flush()
                    f.readline()
        except OSError:
            pass
