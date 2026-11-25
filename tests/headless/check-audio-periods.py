#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check that mixer.svc met its audio period deadlines in a headless run.

mixer.svc prints one stats line a second (userspace/services/mixer/main.adb):
  mixer: stats periods=N active=N silent=N missed_periods=N underruns=N
missed_periods counts the HDA period completions that reached the mixer only
together with a later one (hda.drv saw the DMA position advance by more than
one period), so each is a period whose deadline passed unobserved.

The check sums missed_periods over the stream's steady state (from the first
"mixer: HDA period IRQ active" on, leaving out each interval in which the
output (re)started, "hda: stream started": the mixer primes the whole
cyclic buffer then and the stream's first completions are not deadlines
it could miss) and fails when the total exceeds
--max-missed (default MAXIMUM_MISSED_PERIODS) or one interval exceeds
--max-burst (default MAXIMUM_MISSED_BURST): a burst that long empties a
client ring and is heard as a dropout.

Usage: check-audio-periods.py SERIAL_LOG [--max-missed N] [--max-burst N]
Prints AUDIO-PERIODS: lines; exits non-zero on failure.
"""
import argparse
import re
import sys

# A 10-minute Neon Blast run is about 112,000 periods. Before QEMU's WAV
# and serial writes were kept off the host file system (run.sh), such a run
# missed 79-155 periods in bursts of up to 4 (21 ms); after, 0-3. The 3 is
# one remaining stall of about 18 ms, rarer than one per 10 minutes, that
# holds hda.drv inside the kernel while another CPU runs with its timer
# interrupt held off (kernel follow-up; MAXIMUM_MISSED_BURST goes to 1 once
# it is fixed). A burst of 4 or more empties Neon Blast's audio queue and
# is heard as a dropout.
MAXIMUM_MISSED_PERIODS = 5
MAXIMUM_MISSED_BURST = 3

STATS = re.compile(r'mixer: stats periods=(\d+) active=(\d+) silent=(\d+) '
                   r'missed_periods=(\d+) underruns=(\d+)')
STARTED = 'mixer: HDA period IRQ active'
RESTARTED = 'hda: stream started'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('serial')
    parser.add_argument('--max-missed', type=int, default=MAXIMUM_MISSED_PERIODS)
    parser.add_argument('--max-burst', type=int, default=MAXIMUM_MISSED_BURST)
    args = parser.parse_args()
    started = False
    restart_pending = False
    intervals = periods = missed = worst = skipped = 0
    with open(args.serial, errors='replace') as log:
        for line in log:
            if STARTED in line:
                started = True
                continue
            if RESTARTED in line:
                restart_pending = True
                continue
            match = STATS.search(line)
            if not started or not match:
                continue
            if restart_pending:
                restart_pending = False
                skipped += 1
                continue
            intervals += 1
            periods += int(match.group(1))
            count = int(match.group(4))
            missed += count
            worst = max(worst, count)
    print('AUDIO-PERIODS: intervals=%d periods=%d missed_periods=%d worst_interval=%d '
          'start_intervals_left_out=%d bound=%d burst_bound=%d'
          % (intervals, periods, missed, worst, skipped, args.max_missed, args.max_burst))
    if intervals == 0:
        print('AUDIO-PERIODS: FAIL no mixer stats after the period interrupt started')
        return 1
    if missed > args.max_missed or worst > args.max_burst:
        print('AUDIO-PERIODS: FAIL the mixer missed period deadlines')
        return 1
    print('AUDIO-PERIODS: PASS')
    return 0


if __name__ == '__main__':
    sys.exit(main())
