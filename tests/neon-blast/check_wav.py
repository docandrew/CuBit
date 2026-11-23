#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Check the WAV QEMU's HDA wrote during a headless Neon Blast run: it holds
audio, the music is loud enough, it never clips hard, and once the music
runs (after WARM_UP_S) there is no silent gap longer than MAX_GAP_MS (a
stalled producer) and no more than NEON_WAV_MAX_DROPOUTS (default 0)
dropouts: runs of exact digital silence, which is what the mixer plays for
a period a stream underran.

Usage: check_wav.py FILE.wav [--report]
Prints NEON-WAV: lines; exits non-zero on failure.
"""
import array
import math
import sys
import struct

MAX_GAP_MS = 60           # longest allowed silence once the music runs
WARM_UP_S = 3.0           # the stream's first seconds (start-up) are not judged
DROPOUT_MS = 1.0          # an exact-zero run this long is a dropout (an underrun)
SILENCE = 24              # |sample| at or below this is silence (16-bit)
MIN_RMS_DB = -40.0        # quieter than this overall: the music did not play
CLIP = 32700              # samples at or beyond this clip
MAX_CLIPPED = 0.001       # share of samples


def read_wav(path):
    """(rate, channels, sample bytes, samples). QEMU's wav audiodev writes
    its header's sizes only when it exits cleanly; a run ended by the
    timeout leaves them 0, so the data runs to the end of the file."""
    with open(path, 'rb') as f:
        blob = f.read()
    if blob[:4] != b'RIFF' or blob[8:12] != b'WAVE':
        raise ValueError('not a RIFF WAVE file')
    position = 12
    rate = channels = width = 0
    while position + 8 <= len(blob):
        chunk, size = blob[position:position + 4], struct.unpack('<I', blob[position + 4:position + 8])[0]
        body = position + 8
        if chunk == b'fmt ':
            channels, rate = struct.unpack('<HI', blob[body + 2:body + 8])
            width = struct.unpack('<H', blob[body + 14:body + 16])[0] // 8
        elif chunk == b'data':
            end = len(blob) if size == 0 or body + size > len(blob) else body + size
            end -= (end - body) % max(1, channels * width)
            data = array.array('h')
            data.frombytes(blob[body:end])
            return rate, channels, width, data
        position = body + size + (size & 1)
    raise ValueError('no data chunk')


def main():
    path = sys.argv[1]
    rate, channels, width, data = read_wav(path)
    frames = len(data) // max(1, channels)
    if width != 2 or channels != 2 or frames == 0:
        print('NEON-WAV: FAIL format rate=%d channels=%d width=%d frames=%d' % (rate, channels, width, frames))
        return 1
    loud = [max(abs(data[2 * i]), abs(data[2 * i + 1])) for i in range(frames)]
    warm = int(WARM_UP_S * rate)
    first = next((i for i, v in enumerate(loud) if v > SILENCE and i >= warm), None)
    if first is None:
        print('NEON-WAV: FAIL all silent (%d frames)' % frames)
        return 1
    last = max(i for i, v in enumerate(loud) if v > SILENCE)
    longest = run = 0
    gaps = 0
    for v in loud[first:last + 1]:
        if v <= SILENCE:
            run += 1
        else:
            if run * 1000 / rate > MAX_GAP_MS:
                gaps += 1
            longest = max(longest, run)
            run = 0
    music = data[2 * first:2 * (last + 1)]
    # Dropouts: exact digital silence inside the music (the mixer fills an
    # underrun period with zeros; the synthesizer's tails never reach 0).
    dropouts = []
    zero = 0
    for i in range(first, last + 1):
        if data[2 * i] == 0 and data[2 * i + 1] == 0:
            zero += 1
        else:
            if zero * 1000 / rate >= DROPOUT_MS:
                dropouts.append(i / rate)
            zero = 0
    max_dropouts = int(__import__('os').environ.get('NEON_WAV_MAX_DROPOUTS', '0'))
    rms = math.sqrt(sum(x * x for x in music) / max(1, len(music))) / 32768
    rms_db = 20 * math.log10(rms + 1e-12)
    clipped = sum(1 for x in music if abs(x) >= CLIP) / max(1, len(music))
    print('NEON-WAV: rate=%d seconds=%.1f music_from=%.2fs rms=%.1fdB longest_silence_ms=%.1f '
          'gaps_over_%dms=%d clipped=%.5f dropouts=%d%s' %
          (rate, frames / rate, first / rate, rms_db, longest * 1000 / rate, MAX_GAP_MS, gaps,
           clipped, len(dropouts), (' at ' + ' '.join('%.2fs' % t for t in dropouts[:12])) if dropouts else ''))
    ok = gaps == 0 and rms_db > MIN_RMS_DB and clipped <= MAX_CLIPPED and len(dropouts) <= max_dropouts
    print('NEON-WAV: ' + ('PASS' if ok else 'FAIL'))
    return 0 if ok else 1


sys.exit(main())
