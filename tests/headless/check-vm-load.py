#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check a headless vm-desktop run with CUBIT_VM_LOAD=1: while both guest
vCPUs spin, audio keeps every period, the game keeps its frames, the
desktop presents every refresh on time and keys still reach the guest.

The driver (tests/headless/vm-desktop-shots.py) appends to the serial log,
once QEMU has exited, the offsets bounding the window the guest spun
("vm-load: spin window starts offset=N" / "ends offset=N"). Within that
window of the serial log:
- mixer.svc's missed_periods sum to at most MAXIMUM_MISSED_PERIODS, none
  more than MAXIMUM_MISSED_BURST in one interval;
- Neon Blast's frame_ms_max stays at most MAXIMUM_FRAME_MS;
- the desktop reports no refresh late by LATE_REFRESHES_COLUMN or more;
- vmm's key-to-echo (key received to the guest's echo, the guest's own
  scheduling included) stays at most MAXIMUM_ECHO_MS.

Usage: check-vm-load.py SERIAL_LOG [--driver-log LOG]
Prints VM-LOAD: lines; exits non-zero on failure.
"""
import argparse
import re
import statistics
import sys

# A 90 s window is about 17,000 periods. QEMU's own main loop now and then
# runs its HDA timer about 10 ms late (measured: the period interrupt
# arrives late, hda.drv then takes it within 40 us), an isolated miss the
# guest cannot prevent; a guest-side stall shows as a burst or many.
MAXIMUM_MISSED_PERIODS = 5
MAXIMUM_MISSED_BURST = 2
# Neon Blast at 60 Hz: one frame may slip to the next refresh, not three.
MAXIMUM_FRAME_MS = 50.0
LATE_REFRESHES_COLUMN = '4+'
# The guest shell competes with two spinning `yes` inside the guest.
MAXIMUM_ECHO_MS = 100.0


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('serial')
    parser.add_argument('--driver-log', help='the driver output, when it has the offsets instead')
    args = parser.parse_args()
    driver = open(args.driver_log if args.driver_log else args.serial, errors='replace').read()
    start = re.search(r'vm-load: spin window starts offset=(\d+)', driver)
    end = re.search(r'vm-load: spin window ends offset=(\d+)', driver)
    if not start or not end:
        print('VM-LOAD: FAIL the driver reported no spin window')
        return 1
    window = open(args.serial, 'rb').read()[int(start.group(1)):int(end.group(1))]
    text = window.decode(errors='replace')
    missed = [int(m) for m in re.findall(r'mixer: stats .*?missed_periods=(\d+)', text)]
    frames = [float(m) for m in re.findall(r'neon-blast: stats .*?frame_ms_max=([\d.]+)', text)]
    late = [int(m) for m in re.findall(r'refreshes_late .*?' + re.escape(LATE_REFRESHES_COLUMN) + r'=(\d+)', text)]
    echoes = [int(m) / 1000 for m in re.findall(r'vmm: key to echo (\d+) us', text)]
    print('VM-LOAD: mixer_intervals=%d missed_periods=%d frames_reports=%d frame_ms_max=%s '
          'late_refreshes=%d echoes=%d echo_median_ms=%s echo_max_ms=%s' %
          (len(missed), sum(missed), len(frames), max(frames) if frames else '-',
           sum(late), len(echoes),
           '%.1f' % statistics.median(echoes) if echoes else '-',
           '%.1f' % max(echoes) if echoes else '-'))
    failures = []
    if not missed:
        failures.append('no mixer statistics in the window')
    if sum(missed) > MAXIMUM_MISSED_PERIODS or (missed and max(missed) > MAXIMUM_MISSED_BURST):
        failures.append('audio missed periods')
    if not frames or max(frames) > MAXIMUM_FRAME_MS:
        failures.append('the game lost frames')
    if sum(late) > 0:
        failures.append('the desktop presented late')
    if not echoes or max(echoes) > MAXIMUM_ECHO_MS:
        failures.append('keys did not reach the guest in time')
    if failures:
        print('VM-LOAD: FAIL ' + '; '.join(failures))
        return 1
    print('VM-LOAD: PASS')
    return 0


if __name__ == '__main__':
    sys.exit(main())
