#!/usr/bin/env python3
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
"""Drives the usb-eject headless test over QMP (tests/headless/run.sh).

One qemu-xhci controller (xhcia), empty at boot. On usb-eject.app's serial
markers it plugs stick A (port 1) and B (port 2) and pulls them:

  "plug A"         blockdev-add + device_add A
  "pull A"         device_del A (after Eject), waits for DEVICE_DELETED
  "plug A again"   A again (a new block node on the same image)
  "plug B"         B beside A
  "surprise A"     device_del A while it is written; then fsck.fat -n on
                   the image must report the dirty bit (written: "dirty")
  "plug A dirty"   A again
  "end"            quit

Writes the steps to RESULT_FILE ("done" last). Usage:
  drive.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS RESULT_FILE STICK_DIR
"""
import json
import os
import re
import socket
import subprocess
import sys
import time

PANIC = 'CUBIT KERNEL PANIC'


def main(serial_path, socket_path, timeout, result_path, stick_dir):
    deadline = time.monotonic() + timeout - 5
    results = []
    plugs = {'A': 0, 'B': 0}

    def text():
        try:
            with open(serial_path, errors='replace') as f:
                return f.read()
        except FileNotFoundError:
            return ''

    def wait(pattern, what):
        while time.monotonic() < deadline:
            data = text()
            if PANIC in data:
                raise RuntimeError('kernel panic')
            if 'usb-eject: FAIL' in data:
                raise RuntimeError('the guest failed')
            if re.search(pattern, data):
                return
            time.sleep(0.1)
        raise RuntimeError(f'timed out waiting for {what}')

    qmp = None
    while time.monotonic() < deadline and qmp is None:
        try:
            qmp = socket.socket(socket.AF_UNIX)
            qmp.connect(socket_path)
        except OSError:
            qmp = None
            time.sleep(0.2)
    if qmp is None:
        raise RuntimeError('QMP socket never opened')
    stream = qmp.makefile('rwb')
    json.loads(stream.readline())
    events = []

    def command(name, arguments=None):
        message = {'execute': name}
        if arguments:
            message['arguments'] = arguments
        stream.write(json.dumps(message).encode() + b'\n')
        stream.flush()
        while True:
            reply = json.loads(stream.readline())
            if 'event' in reply:
                events.append(reply)
                continue
            if 'error' in reply:
                raise RuntimeError(f'{name}: {reply["error"]}')
            return reply['return']

    def plug(stick, port):
        plugs[stick] += 1
        node = f'stick{stick.lower()}{plugs[stick]}'
        command('blockdev-add', {'driver': 'raw', 'node-name': node,
                                 'file': {'driver': 'file',
                                          'filename': os.path.join(stick_dir, f'stick{stick.lower()}.img')}})
        command('device_add', {'driver': 'usb-storage', 'id': f'usb{stick.lower()}',
                               'bus': 'xhcia.0', 'port': str(port), 'drive': node})
        print(f'usb-eject-drive: plugged {stick} ({node})', flush=True)

    def pull(stick):
        device = f'usb{stick.lower()}'
        command('device_del', {'id': device})
        # Unplug completes asynchronously: DEVICE_DELETED arrives between
        # replies (command() keeps events), so ask something cheap meanwhile.
        while time.monotonic() < deadline:
            if any(e.get('event') == 'DEVICE_DELETED' and e.get('data', {}).get('device') == device
                   for e in events):
                break
            time.sleep(0.2)
            command('query-status')
        else:
            raise RuntimeError(f'{device} never deleted')
        events.clear()
        # The block node goes with the device only for -drive; delete ours.
        node = f'stick{stick.lower()}{plugs[stick]}'
        try:
            command('blockdev-del', {'node-name': node})
        except RuntimeError:
            pass
        print(f'usb-eject-drive: pulled {stick}', flush=True)

    command('qmp_capabilities')
    try:
        wait(r'usb-eject: plug A\n', 'plug A')
        plug('A', 1)
        wait(r'usb-eject: pull A', 'pull A')
        pull('A')
        results.append('ejected A pulled')
        wait(r'usb-eject: plug A again', 'plug A again')
        plug('A', 1)
        wait(r'usb-eject: plug B', 'plug B')
        plug('B', 2)
        wait(r'usb-eject: surprise A', 'surprise A')
        pull('A')
        out = subprocess.run(['fsck.fat', '-n', os.path.join(stick_dir, 'sticka.img')],
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                             env=dict(os.environ, PATH=os.environ.get('PATH', '') + ':/usr/sbin')
                             ).stdout.decode('utf-8', 'replace')
        if 'Dirty bit is set' not in out:
            raise RuntimeError(f'stick A not dirty after the surprise removal: {out.strip()}')
        results.append('dirty')
        print('usb-eject-drive: stick A dirty after the surprise removal ok', flush=True)
        wait(r'usb-eject: plug A dirty', 'plug A dirty')
        plug('A', 1)
        wait(r'usb-eject: end', 'the end')
        results.append('done')
    except RuntimeError as error:
        results.append(f'FAIL {error}')
        print(f'usb-eject-drive: FAIL {error}', flush=True)
    time.sleep(0.5)
    try:
        command('quit')
    except (OSError, RuntimeError, ValueError):
        pass
    with open(result_path, 'w') as f:
        f.write('\n'.join(results) + '\n')


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2], float(sys.argv[3]), sys.argv[4], sys.argv[5])
