#!/usr/bin/env python3
"""Drives the usb-hotplug headless test through QEMU's HMP monitor.

QEMU has two qemu-xhci controllers, xhcia (empty at boot) and xhcib with a
usb-mouse (the IdeaPad's layout: TCSS controller first and empty, the PCH
controller with the mouse second). Each step waits for the guest's serial
evidence before the next:

  1. the boot mouse on the SECOND controller delivers reports to Desktop;
  2. unplug it: that instance detaches and disables the slot;
  3. hot-plug a mouse on the FIRST controller: enumerated, delivers;
  4. unplug and replug it: the slot is reused, delivers again;
  5. hot-plug a keyboard on the second controller: keys reach Desktop.

Usage: drive.py SERIAL_LOG MONITOR_SOCKET TIMEOUT_SECONDS
"""
import re
import socket
import sys
import time

LOCATION = r'[0-9A-F]{2}:[0-9A-F]{2}\.[0-7]'


class Fail(Exception):
    pass


def main(serial_path, monitor_path, timeout):
    deadline = time.monotonic() + timeout - 5

    def text():
        try:
            with open(serial_path, errors='replace') as f:
                return f.read()
        except FileNotFoundError:
            return ''

    def wait(pattern, what, count=1, start=0):
        while time.monotonic() < deadline:
            found = re.findall(pattern, text()[start:])
            if len(found) >= count:
                return found
            time.sleep(0.1)
        raise Fail(f'timed out waiting for {what} ({pattern})')

    monitor = None
    while time.monotonic() < deadline and monitor is None:
        try:
            monitor = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            monitor.connect(monitor_path)
        except OSError:
            monitor = None
            time.sleep(0.2)
    if monitor is None:
        raise Fail('monitor socket never opened')
    monitor.settimeout(0.005)

    def hmp(command):
        monitor.sendall((command + '\n').encode())
        try:
            while monitor.recv(4096):
                pass
        except (socket.timeout, BlockingIOError):
            pass

    def published(location, key='published'):
        values = re.findall(rf'xhci: {location} stats [^\n]* {key}=(\d+)', text())
        return int(values[-1]) if values else 0

    def move(count):
        for index in range(count):
            hmp('mouse_move 3 2' if index % 2 else 'mouse_move -3 -2')
            if index % 16 == 0:
                hmp('mouse_button 1')
            if index % 16 == 8:
                hmp('mouse_button 0')
            time.sleep(0.015)
        hmp('mouse_button 0')

    def deliver(location, what, minimum=24):
        before = published(location)
        for attempt in range(6):
            move(48)
            end = time.monotonic() + 3
            while time.monotonic() < end:
                if published(location) >= before + minimum:
                    print(f'usb-hotplug: {what}: {location} published '
                          f'{published(location) - before} reports', flush=True)
                    return
                time.sleep(0.1)
        raise Fail(f'{what}: {location} published only {published(location) - before}')

    wait(r'boot-logs: window ready', 'the desktop and boot-logs')
    started = wait(rf'devmgr: xhci\.drv ({LOCATION}) started;[^\n]*mouse=(\d)',
                   'both xHCI instances', count=2)
    instances = {loc: mouse for loc, mouse in started}
    with_mouse = [loc for loc, mouse in instances.items() if mouse == '1']
    without = [loc for loc, mouse in instances.items() if mouse == '0']
    if len(with_mouse) != 1 or len(without) != 1:
        raise Fail(f'expected one controller with the boot mouse: {instances}')
    second, first = with_mouse[0], without[0]
    if first > second:
        raise Fail(f'the mouse must be on the second controller: {instances}')
    wait(rf'xhci: {first} bring-up complete', 'the empty controller staying up for hotplug')
    wait(rf'xhci: {second} bring-up complete', 'the mouse controller')

    deliver(second, 'boot mouse on the second controller')

    mark = len(text())
    hmp('device_del bootmouse')
    wait(rf'xhci: {second} port \d+ detach slots=1', 'detach on the second controller', start=mark)
    wait(rf'xhci: {second} slot \d+ disable ok', 'slot disabled', start=mark)
    print('usb-hotplug: unplug detached and disabled the slot', flush=True)

    mark = len(text())
    hmp('device_add usb-mouse,bus=xhcia.0,port=1,id=hpmouse')
    wait(rf'xhci: {first} port \d+ attached slot=(\d+)', 'hotplug attach on the first controller',
         start=mark)
    deliver(first, 'hot-plugged mouse on the first controller')

    mark = len(text())
    hmp('device_del hpmouse')
    wait(rf'xhci: {first} slot \d+ disable ok', 'unplug of the hot-plugged mouse', start=mark)
    mark = len(text())
    hmp('device_add usb-mouse,bus=xhcia.0,port=1,id=hpmouse2')
    wait(rf'xhci: {first} port \d+ attached slot=(\d+)', 'replug attach', start=mark)
    deliver(first, 'replugged mouse')

    mark = len(text())
    hmp('device_add usb-kbd,bus=xhcib.0,port=2,id=hpkbd')
    wait(rf'xhci: {second} port \d+ attached slot=\d+[^\n]*roles=keyboard',
         'hot-plugged keyboard', start=mark)
    before = published(second, 'keys-published')
    for attempt in range(10):
        for key in ('a', 'b', 'c'):
            hmp(f'sendkey {key}')
            time.sleep(0.1)
        end = time.monotonic() + 2
        while time.monotonic() < end and published(second, 'keys-published') <= before:
            time.sleep(0.1)
        if published(second, 'keys-published') > before:
            break
    else:
        raise Fail('the hot-plugged keyboard published no keys')
    print(f'usb-hotplug: hot-plugged keyboard published '
          f'{published(second, "keys-published") - before} key bytes', flush=True)
    # The periodic health line (every 5 s while counters move) from both.
    wait(rf'xhci: {first} health ', 'a health line from the first controller')
    wait(rf'xhci: {second} health ', 'a health line from the second controller')
    print('usb-hotplug: DRIVE PASS', flush=True)
    time.sleep(1)
    hmp('quit')


if __name__ == '__main__':
    try:
        main(sys.argv[1], sys.argv[2], int(sys.argv[3]))
    except Fail as error:
        print(f'usb-hotplug: FAIL {error}', file=sys.stderr, flush=True)
        sys.exit(1)
