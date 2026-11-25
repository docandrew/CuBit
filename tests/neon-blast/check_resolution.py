#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Neon Blast's dynamic resolution in a headless run (APP-008,
tests/headless/run.sh --test neon-blast): read the game's stats lines.

Usage: check_resolution.py SERIAL_LOG [plain|constrained]

plain: every software stats line names its window (output=WxH) and its
internal size (internal=WxH, no larger than the window, at a scale_pct
from 25 to 100).

constrained (a window too big for the CPU, e.g. CUBIT_TEST_VIDEO_SIZE=
3840x2160): also
- the resolution dropped (res_drops >= 1, some line below 100%);
- once dropped, the frames came back inside the budget: from the second
  line after the first drop, the median frame_ms_avg is at most
  FRAME_LIMIT_MS, and the median render+scale at most BUDGET_MS;
- the resolution rose again at least once (res_raises >= 1): the drive's
  lighter stretches (the title, the attract demo between stages) or a
  smaller window give it back.
Prints NEON-RES: PASS/FAIL and one line per stats line.
"""
import re
import statistics
import sys

# The 60 Hz frame is 16.7 ms; the controller gives render + scale 10 ms of
# it (Neon_Resolution.Budget_Us). The frame interval also holds the scene,
# the overlay and the desktop's composition, so it is held to 25 ms (40
# fps, against 4-5 fps at native on a 4K window).
FRAME_LIMIT_MS = 25.0
BUDGET_MS = 10.0
STATS = re.compile(r'neon-blast: stats path=software (.*)')


def fields(text):
    return dict(item.split('=', 1) for item in text.split() if '=' in item)


def size(value):
    w, h = value.split('x')
    return int(w), int(h)


def main():
    serial = sys.argv[1]
    mode = sys.argv[2] if len(sys.argv) > 2 else 'plain'
    with open(serial, 'rb') as f:
        text = f.read().decode('utf-8', 'replace')
    lines = [fields(m.group(1)) for m in STATS.finditer(text)]
    failures = []
    if not lines:
        failures.append('no software stats lines')
    for n, line in enumerate(lines):
        missing = [k for k in ('output', 'internal', 'scale_pct', 'scale_ms_avg', 'res_drops', 'res_raises')
                   if k not in line]
        if missing:
            failures.append('line %d lacks %s' % (n, ', '.join(missing)))
            continue
        ow, oh = size(line['output'])
        iw, ih = size(line['internal'])
        pct = int(line['scale_pct'])
        print('NEON-RES: %2d output %s internal %s (%d%%) frame_ms_avg %s render_ms_avg %s scale_ms_avg %s '
              'drops %s raises %s phase %s' % (n, line['output'], line['internal'], pct,
                                               line.get('frame_ms_avg'), line.get('render_ms_avg'),
                                               line['scale_ms_avg'], line['res_drops'], line['res_raises'],
                                               line.get('phase')), flush=True)
        if iw > ow or ih > oh or not 25 <= pct <= 100:
            failures.append('line %d: internal %s outside output %s at %d%%' % (n, line['internal'],
                                                                                 line['output'], pct))
    if mode == 'constrained' and lines and not failures:
        dropped = [n for n, line in enumerate(lines) if int(line['res_drops']) >= 1]
        if not dropped or min(int(line['scale_pct']) for line in lines) == 100:
            failures.append('the resolution never dropped')
        else:
            settled = lines[dropped[0] + 2:]
            if len(settled) < 3:
                failures.append('too few stats lines after the drop (%d)' % len(settled))
            else:
                frame = statistics.median(float(line['frame_ms_avg']) for line in settled)
                work = statistics.median(float(line['render_ms_avg']) + float(line['scale_ms_avg'])
                                         for line in settled)
                print('NEON-RES: after the drop: median frame %.1f ms (limit %.1f), render+scale %.1f ms '
                      '(budget %.1f)' % (frame, FRAME_LIMIT_MS, work, BUDGET_MS), flush=True)
                if frame > FRAME_LIMIT_MS:
                    failures.append('frames %.1f ms after the drop, over %.1f' % (frame, FRAME_LIMIT_MS))
                if work > BUDGET_MS:
                    failures.append('render+scale %.1f ms after the drop, over %.1f' % (work, BUDGET_MS))
            if max(int(line['res_raises']) for line in lines) < 1:
                failures.append('the resolution never rose again')
    if failures:
        print('NEON-RES: FAIL ' + '; '.join(failures), flush=True)
        return 1
    print('NEON-RES: PASS (%s)' % mode, flush=True)
    return 0


sys.exit(main())
