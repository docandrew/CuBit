#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Beat-to-sample sync check for a Neon Blast replay (tests/neon-blast):
find each kick's onset in the rendered soundtrack and compare it with the
sample the composer placed the kick on. The kick is a 42-160 Hz sine burst;
its onset is where the low band's envelope first rises past half of its
peak within a window after the composer's time.

Usage: check_sync.py replay.wav kicks.txt EXPECTED_MS TOLERANCE_MS SPREAD_MS
EXPECTED_MS is the baseline: the master chain's fixed delay (the 7 ms tape
stage) plus this detector's own lag (filters, envelope and the kick's
attack). Fails if the median moves by more than TOLERANCE_MS (a block or
a tick of slip), the spread exceeds SPREAD_MS, or kicks are missing.
"""
import array
import math
import statistics
import sys
import wave

wav, kicks_file = sys.argv[1], sys.argv[2]
expected = float(sys.argv[3])
tolerance = float(sys.argv[4])
spread_limit = float(sys.argv[5])
with wave.open(wav) as w:
    rate = w.getframerate()
    data = array.array('h')
    data.frombytes(w.readframes(w.getnframes()))
mono = [(data[2 * i] + data[2 * i + 1]) * 0.5 for i in range(len(data) // 2)]
# Low band: two one-pole low-passes at 160 Hz, then a 3 ms envelope.
k = 1 - math.exp(-2 * math.pi * 160 / rate)
a = b = 0.0
low = []
for v in mono:
    a += k * (v - a)
    b += k * (a - b)
    low.append(abs(b))
e = 0.0
ke = 1 - math.exp(-1 / (0.003 * rate))
env = []
for v in low:
    e += ke * (v - e)
    env.append(e)
kicks = [int(line) for line in open(kicks_file) if line.strip()]
offsets = []
window = int(0.06 * rate)
for t in kicks:
    if t + window >= len(env) or t < window:
        continue
    before = max(env[t - window // 2:t]) if t > 0 else 0.0
    peak = max(env[t:t + window])
    if peak < 2 * before + 200:          # masked by the music: no clean onset
        continue
    half = before + 0.5 * (peak - before)
    onset = next(i for i in range(t, t + window) if env[i] >= half)
    offsets.append((onset - t) * 1000 / rate)
if len(offsets) < max(4, len(kicks) // 3):
    print('NEON-SYNC: FAIL only %d of %d kicks measurable' % (len(offsets), len(kicks)))
    sys.exit(1)
median = statistics.median(offsets)
spread = statistics.pstdev(offsets)
print('NEON-SYNC: kicks=%d measured=%d onset_ms median=%.2f spread=%.2f expected=%.1f' %
      (len(kicks), len(offsets), median, spread, expected))
ok = abs(median - expected) <= tolerance and spread <= spread_limit
print('NEON-SYNC: ' + ('PASS' if ok else 'FAIL'))
sys.exit(0 if ok else 1)
