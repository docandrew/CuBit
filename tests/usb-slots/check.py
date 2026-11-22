#!/usr/bin/env python3
"""Checks a usb-slots headless serial log (tests/headless/run.sh).

LIM-001 regression: xhci.drv once had a fixed 8 device slots, so the ninth
USB device never enumerated. QEMU's qemu-xhci reports MaxSlots=64; run.sh
attaches eight USB hubs (one on the root port, seven on its ports 1-7) and
a boot mouse on the first hub's port 8, so the mouse needs slot 9.

Passes when the controller enables every slot it reports, all eight hubs
hold slots, the mouse is configured on a slot past 8, devmgr hears that a
mouse came up, and nothing faulted.
"""
import re
import sys

LOCATION = r'[0-9A-F]{2}:[0-9A-F]{2}\.[0-7]'
OLD_FIXED_SLOTS = 8


def fail(message):
    print(f'usb-slots: FAIL {message}', file=sys.stderr)
    sys.exit(1)


def main(path):
    text = open(path, errors='replace').read()
    identify = re.search(rf'xhci: ({LOCATION}) stage identify ok .* slots=(\d+) ', text)
    if not identify:
        fail('no "stage identify ok" line with the controller\'s slot count')
    location, reported = identify.group(1), int(identify.group(2))
    if reported <= OLD_FIXED_SLOTS:
        fail(f'the controller reports {reported} slots; the test needs more than {OLD_FIXED_SLOTS}')
    enabled = re.search(rf'xhci: {location} stage interrupter ok .* slots-enabled=(\d+)', text)
    if not enabled or int(enabled.group(1)) != reported:
        fail(f'slots enabled {enabled.group(1) if enabled else None}, controller reports {reported}')
    hubs = {int(s) for s in re.findall(rf'xhci: {location} slot (\d+) hub ok ports=', text)}
    if len(hubs) < OLD_FIXED_SLOTS:
        fail(f'{len(hubs)} hubs configured (slots {sorted(hubs)}), expected {OLD_FIXED_SLOTS}')
    mouse = re.findall(rf'xhci: {location} slot (\d+) configured roles=mouse', text)
    if not mouse:
        refused = re.search(rf'xhci: {location} stage enable-slot fail [^\n]*', text)
        fail('the mouse was never configured' + (f' ({refused.group(0)})' if refused else ''))
    if max(int(s) for s in mouse) <= OLD_FIXED_SLOTS:
        fail(f'the mouse took slot {mouse}, not one past the old fixed {OLD_FIXED_SLOTS}')
    if not re.search(rf'devmgr: xhci\.drv[^\n]* started; [^\n]* mouse=1', text):
        fail('devmgr did not hear that the controller brought up a mouse')
    for marker in ('EXCEPTION', 'PANIC', 'User-space exception', 'completion routing fault'):
        if marker in text:
            fail(f'fault marker {marker!r} in the log')
    print(f'usb-slots: PASS {location} slots={reported} hubs={sorted(hubs)} mouse-slot={mouse[-1]}')


if __name__ == '__main__':
    main(sys.argv[1])
