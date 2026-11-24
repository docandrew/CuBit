#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Copy a 16-bit WAV with SECONDS of silence at AT seconds: the mutation
that tests/neon-blast/check_flow.py must catch.

Usage: mute_wav.py IN.wav OUT.wav AT SECONDS
"""
import sys
import wave

source, target, at, seconds = sys.argv[1], sys.argv[2], float(sys.argv[3]), float(sys.argv[4])
with wave.open(source) as w:
    params = w.getparams()
    data = bytearray(w.readframes(w.getnframes()))
frame = params.nchannels * params.sampwidth
first = int(at * params.framerate) * frame
count = int(seconds * params.framerate) * frame
data[first:first + count] = bytes(len(data[first:first + count]))
with wave.open(target, 'wb') as o:
    o.setparams(params)
    o.writeframes(bytes(data))
