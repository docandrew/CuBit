#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check that a growing heap does not stall everyone else (bench-latency).

sched-latency's commit load maps, touches and unmaps COMMIT_BYTES of
anonymous memory in a loop. On CuBit every map commits the region's pages
(CHANGE_OWNED_RANGE) and every unmap retires them (RELEASE_OWNED_REGION),
both under the kernel's grantLock with interrupts off, and every CPU's wait
and grant paths take grantLock. Until 2026-11 one commit could back 16 MiB
and one release retired a whole region, so a single process's heap growth
held every other CPU up for tens of milliseconds (audio periods missed,
input stalled: the ~18 ms window under Neon Blast). Commits and releases
now go in steps of Owned_Reservation_Policy.Pages_Per_Locked_Step pages.

The check: every workload measured against the commit load stays within
MAXIMUM_LATENCY_US at its worst sample.

Usage: check-commit-load.py SERIAL_LOG
Prints COMMIT-LOAD: lines; exits non-zero on failure.
"""
import re
import sys

# sched-latency's MISS_LATENCY_US, the 1 ms input target: a stall longer
# than that under heap growth is a lock held too long, not scheduling.
MAXIMUM_LATENCY_US = 1000.0

LINE = re.compile(r"sched-latency: workload=(\S+) load=commit samples=(\d+) p50_us=(\S+)"
                  r" p99_us=(\S+) max_us=(\S+) missed=(\d+) bg_rate=(\S+) bg_unit=(\S+)")


def main():
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    with open(sys.argv[1], errors='replace') as log:
        rows = LINE.findall(log.read())
    if not rows:
        print('COMMIT-LOAD: FAIL no workload ran against the commit load')
        return 1
    failed = False
    for workload, samples, p50, p99, worst, missed, rate, unit in rows:
        over = float(worst) > MAXIMUM_LATENCY_US
        failed = failed or over or float(rate) <= 0
        print('COMMIT-LOAD: workload=%s samples=%s p50_us=%s p99_us=%s max_us=%s '
              'missed=%s maps_per_s=%s bound_us=%.0f%s'
              % (workload, samples, p50, p99, worst, missed, rate, MAXIMUM_LATENCY_US,
                 ' OVER' if over else ''))
    if failed:
        print('COMMIT-LOAD: FAIL heap growth held other threads up (or made no progress)')
        return 1
    print('COMMIT-LOAD: PASS')
    return 0


if __name__ == '__main__':
    sys.exit(main())
