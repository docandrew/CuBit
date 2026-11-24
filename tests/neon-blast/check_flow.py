#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Flow check for a long Neon Blast render (tests/neon-blast/run.sh): the
soundtrack is one continuous piece. Reads the WAV neon_audio_host wrote and
its sections file (song sample, stage, section, BPM, style), and fails if:
- any window of WINDOW_MS after the first seconds is near silent (a gap or
  a fade-out to nothing);
- the level right after a section boundary falls more than DIP_DB below the
  level just before it (a fade between sections);
- the stage's form is not all there, in order, and the next stage does not
  follow its outro.

Usage: check_flow.py FILE.wav FILE.wav.sections.txt
"""
import array
import math
import sys
import wave

WINDOW_MS = 250          # judged windows
WARM_UP_S = 1.0          # the very start may be quiet
FLOOR_DB = -45.0         # quieter than this is a gap
DIP_DB = 12.0            # the most a section's first second may fall below the last one's
FORM = ['INTRO', 'GROOVE', 'BUILD', 'DROP', 'BREAKDOWN', 'BUILD', 'PEAK', 'OUTRO']

wav, sections_file = sys.argv[1], sys.argv[2]
with wave.open(wav) as w:
    rate = w.getframerate()
    data = array.array('h')
    data.frombytes(w.readframes(w.getnframes()))
frames = len(data) // 2


def rms_db(first, last):
    first, last = max(0, first), min(frames, last)
    if last <= first:
        return -120.0
    total = 0
    count = 0
    for i in range(2 * first, 2 * last, 6):   # a sample in three, both channels alike
        total += data[i] * data[i]
        count += 1
    return 10 * math.log10(max(total / max(count, 1), 1e-9) / (32768.0 * 32768.0))


window = rate * WINDOW_MS // 1000
quietest = 0.0
gaps = 0
for start in range(int(WARM_UP_S * rate), frames - window, window):
    level = rms_db(start, start + window)
    quietest = min(quietest, level)
    if level < FLOOR_DB:
        gaps += 1

rows = []
for line in open(sections_file):
    parts = line.split()
    if len(parts) >= 4:
        rows.append((int(parts[0]), int(parts[1]), parts[2], int(parts[3])))
dips = []
for sample, stage, section, bpm in rows[1:]:
    if sample + rate > frames:
        continue
    before = rms_db(sample - rate, sample)
    after = rms_db(sample, sample + rate)
    if after < before - DIP_DB:
        dips.append('%s at %.1fs (%.1f -> %.1f dB)' % (section, sample / rate, before, after))

played = [section for _, stage, section, _ in rows if stage > 0]
stages = sorted({stage for _, stage, _, _ in rows if stage > 0})
form_ok = played[:len(FORM)] == FORM
next_stage_ok = len(stages) >= 2 and len(played) > len(FORM) and played[len(FORM)] == 'INTRO'
tempos = sorted({bpm for _, _, _, bpm in rows})

print('NEON-FLOW: seconds=%.0f quietest_window=%.1fdB gaps=%d sections=%d stages=%s tempos=%s' %
      (frames / rate, quietest, gaps, len(rows), stages, tempos))
for dip in dips:
    print('NEON-FLOW: dip ' + dip)
if not form_ok:
    print('NEON-FLOW: form %s, wanted %s' % (played[:len(FORM)], FORM))
ok = gaps == 0 and not dips and form_ok and next_stage_ok
print('NEON-FLOW: ' + ('PASS' if ok else 'FAIL'))
sys.exit(0 if ok else 1)
