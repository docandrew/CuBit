#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Neon Blast beside CPU-hungry programs: frames drop, audio does not.

run.sh --test neon-blast with CUBIT_NEON_BURN=1 (usually --cpus 2) starts
two bench-burn.app beside the game (init-neon-blast-burn.ccl). The game's
soundtrack is synthesized on its own thread in a real-time share its
manifest declares (SCHED-001, app audio threads), so however long a frame
takes, the mixer's ring must never run dry.

The game's stats line (every five seconds) carries its stream's underruns
since the stream opened and the frame time. The check, from the first
stats line on (the stream's start-up underruns come before it):
  * no new underrun, in any line;
  * the load was real: the median line's average frame took at least
    SLOW_FRAME_MS (the software path paces at 60 Hz, 16.7 ms);
  * at least MINIMUM_LINES lines (half a minute and more of play).

Usage: check_underruns.py SERIAL_LOG
"""
import re
import sys

SLOW_FRAME_MS = 20.0
MINIMUM_LINES = 6

LINE = re.compile(r"neon-blast: stats path=(\S+) frames=(\d+) frame_ms_avg=(\S+) "
                  r"frame_ms_max=(\S+) .*? underruns=(\d+) queue=(\S+) ")


def main():
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    with open(sys.argv[1], errors='replace') as log:
        rows = LINE.findall(log.read())
    if len(rows) < MINIMUM_LINES:
        print('NEON-UNDERRUNS: FAIL %d stats lines, at least %d wanted' % (len(rows), MINIMUM_LINES))
        return 1
    first = int(rows[0][4])
    averages = sorted(float(row[2]) for row in rows)
    median_frame = averages[len(averages) // 2]
    new = max(int(row[4]) for row in rows) - first
    frames = sum(int(row[1]) for row in rows)
    print('NEON-UNDERRUNS: lines=%d frames=%d frame_ms_median=%.1f frame_ms_max=%.1f '
          'underruns_at_start=%d new_underruns=%d slow_frame_ms=%.1f path=%s'
          % (len(rows), frames, median_frame, averages[-1], first, new, SLOW_FRAME_MS, rows[0][0]))
    if median_frame < SLOW_FRAME_MS:
        print('NEON-UNDERRUNS: FAIL the load never slowed the frames (not a constrained run)')
        return 1
    if new != 0:
        print('NEON-UNDERRUNS: FAIL the soundtrack underran while frames dropped')
        return 1
    print('NEON-UNDERRUNS: PASS')
    return 0


if __name__ == '__main__':
    sys.exit(main())
