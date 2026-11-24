#!/usr/bin/env python3
# CuBit
# Copyright (C) 2026 Jon Andrew
# SPDX-License-Identifier: GPL-3.0-or-later
"""Drives the usb-disk headless test over QMP (tests/headless/run.sh).

QEMU has two qemu-xhci controllers: xhcia with stick 1 (@usb:0, the FAT32
suite) and xhcib, empty at boot. Steps, each after the guest's serial
evidence:

  1. "fat-volume: plug removal stick": hot-plug stick 2 on xhcib (@usb:1);
  2. "fat-volume: unplug": device_del stick 2 while usb-disk.app writes it;
  3. "fat-volume: end": hot-plug stick 3 on xhcib (port 2): once devmgr
     detached @usb:1, stick 3 must attach as @usb:1
     (FS-034: removal is no longer final);
  4. quit.

Prints "usb-disk-drive: <step> ok" lines (stdout, the run's log) and
exits non-zero on a timeout.

Usage: drive.py SERIAL_LOG QMP_SOCKET TIMEOUT_SECONDS RESULT_FILE
"""
import json
import re
import socket
import sys
import time

PANIC = 'CUBIT KERNEL PANIC'


def main(serial_path, socket_path, timeout, result_path):
    deadline = time.monotonic() + timeout - 5
    results = []

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

    def command(name, arguments=None):
        message = {'execute': name}
        if arguments:
            message['arguments'] = arguments
        stream.write(json.dumps(message).encode() + b'\n')
        stream.flush()
        while True:
            reply = json.loads(stream.readline())
            if 'return' in reply or 'error' in reply:
                if 'error' in reply:
                    raise RuntimeError(f'{name}: {reply["error"]}')
                return reply['return']

    command('qmp_capabilities')
    try:
        wait(r'fat-volume: (plug removal stick|FAIL)', 'the plug request')
        if 'fat-volume: plug removal stick' in text():
            command('device_add', {'driver': 'usb-storage', 'id': 'usbstick2',
                                   'bus': 'xhcib.0', 'port': '1', 'drive': 'stick2'})
            print('usb-disk-drive: stick 2 plugged', flush=True)
        wait(r'fat-volume: (unplug|FAIL|end)', 'the unplug request')
        if 'fat-volume: unplug' in text():
            command('device_del', {'id': 'usbstick2'})
            results.append('unplug sent')
            print('usb-disk-drive: unplug sent', flush=True)
        wait(r'fat-volume: end', 'the end of usb-disk.app')
        # Port 2: port 1's unplug completes asynchronously in QEMU.
        command('device_add', {'driver': 'usb-storage', 'id': 'usbstick3',
                               'bus': 'xhcib.0', 'port': '2', 'drive': 'stick3'})
        print('usb-disk-drive: stick 3 plugged', flush=True)
        while time.monotonic() < deadline and text().count('devmgr: @usb:1 attached') < 2:
            if PANIC in text():
                raise RuntimeError('kernel panic')
            time.sleep(0.1)
        if text().count('devmgr: @usb:1 attached') < 2:
            raise RuntimeError('timed out waiting for stick 3 as @usb:1')
        results.append('replacement attached')
        print('usb-disk-drive: replacement attached ok', flush=True)
    except RuntimeError as error:
        results.append(f'FAIL {error}')
        print(f'usb-disk-drive: FAIL {error}', flush=True)
    time.sleep(0.5)
    try:
        command('quit')
    except (OSError, RuntimeError, ValueError):
        pass
    with open(result_path, 'w') as f:
        f.write('\n'.join(results) + '\n')


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2], float(sys.argv[3]), sys.argv[4])
