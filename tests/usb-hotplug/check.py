#!/usr/bin/env python3
"""Checks a usb-hotplug headless serial log (tests/headless/run.sh).

drive.py already required each step's evidence; this checks what holds for
the whole run: both instances' bring-up stage lines, the evidence that
input from BOTH instances reached Desktop (one new source per publisher),
the stage lines reaching logstore (boot-logs replays them), and no fault.
"""
import re
import sys

LOCATION = r'[0-9A-F]{2}:[0-9A-F]{2}\.[0-7]'


def fail(message):
    print(f'usb-hotplug: FAIL {message}', file=sys.stderr)
    sys.exit(1)


def main(path):
    text = open(path, errors='replace').read()
    locations = sorted(set(re.findall(rf'xhci: ({LOCATION}) bring-up complete', text)))
    if len(locations) != 2:
        fail(f'expected two xHCI instances to finish bring-up: {locations}')
    for location in locations:
        for stage in ('identify', 'quirks', 'reset', 'interrupter', 'settle', 'irq'):
            if not re.search(rf'xhci: {location} stage {stage} ok', text):
                fail(f'{location} has no "stage {stage} ok" line')
        if not re.search(rf'xhci: {location} health ', text):
            fail(f'{location} printed no health line')
    # The first transfer events are logged, with completion codes.
    if not re.search(rf'xhci: {LOCATION} event 1 xfer slot=\d+ dci=\d+ cc=success', text):
        fail('no first-transfer-event line')
    # Desktop saw relative-pointer reports from two publishers: the second
    # controller's boot mouse and the first controller's hot-plugged one.
    publishers = set(re.findall(r'desktop: input source relative-pointer publisher (\d+)', text))
    if len(publishers) < 2:
        fail(f'Desktop accepted pointer reports from {len(publishers)} instance(s), expected 2')
    if not re.search(r'desktop: input source keyboard publisher \d+', text):
        fail('Desktop accepted no keyboard reports from the hot-plugged keyboard')
    # The stage lines went through logstore (boot-logs prints what it keeps).
    for location in locations:
        if not re.search(rf'boot-logs: xhci: {location} stage identify ok', text):
            fail(f'{location} stage lines did not reach logstore')
    for marker in ('EXCEPTION', 'PANIC', 'User-space exception', 'completion routing fault'):
        if marker in text:
            fail(f'fault marker {marker!r} in the log')
    print(f'usb-hotplug: PASS instances {locations}, pointer publishers {sorted(publishers)}')


if __name__ == '__main__':
    main(sys.argv[1])
