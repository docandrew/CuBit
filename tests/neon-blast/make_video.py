#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Mux a filmed headless Neon Blast run into an MP4 (H.264 + AAC): the
QEMU screendumps headless_drive.py took, each timed by the position QEMU's
HDA WAV had reached when it was taken, and that WAV's soundtrack from the
first frame on. A QEMU capture, not laptop hardware.

Usage: make_video.py VIDEO_DIR AUDIO_WAV OUT.mp4 FFMPEG [--crop W:H:X:Y]
       [--height 720] [--bitrate 1300k]
"""
import argparse
import os
import struct
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('video_dir')
parser.add_argument('audio')
parser.add_argument('out')
parser.add_argument('ffmpeg')
parser.add_argument('--crop', default='')
parser.add_argument('--height', type=int, default=720)
parser.add_argument('--bitrate', default='1300k')
args = parser.parse_args()
args.video_dir = os.path.abspath(args.video_dir)

with open(args.audio, 'rb') as f:
    header = f.read(44)
channels, rate = struct.unpack('<HI', header[22:28])
bits = struct.unpack('<H', header[34:36])[0]
frame_bytes = channels * bits // 8
# A WAV QEMU did not finalise (killed at the run's timeout) has zero sizes:
# give ffmpeg a copy with its sizes filled in.
if struct.unpack('<I', header[40:44])[0] == 0:
    fixed = os.path.join(args.video_dir, 'audio-fixed.wav')
    size = os.path.getsize(args.audio) - 44
    with open(args.audio, 'rb') as source, open(fixed, 'wb') as target:
        source.seek(44)
        target.write(header[:4] + struct.pack('<I', 36 + size) + header[8:40] + struct.pack('<I', size))
        target.write(source.read())
    args.audio = fixed
rows = [line.split() for line in open(os.path.join(args.video_dir, 'frames.txt')) if line.strip()]
times = [max(0, int(r[1]) - 44) / (rate * frame_bytes) for r in rows]
walls = [float(r[2]) for r in rows]
# Prefer the audio clock; where the WAV did not grow (buffered writes),
# fall back to the wall clock's spacing.
stamps = []
for i in range(len(rows)):
    if i > 0 and times[i] <= stamps[-1]:
        stamps.append(stamps[-1] + (walls[i] - walls[i - 1]))
    else:
        stamps.append(times[i])
start = stamps[0]
with tempfile.NamedTemporaryFile('w', suffix='.txt', delete=False) as listing:
    for i, row in enumerate(rows):
        duration = (stamps[i + 1] - stamps[i]) if i + 1 < len(rows) else 0.05
        listing.write("file '%s'\nduration %.4f\n" % (os.path.join(args.video_dir, row[0]), max(duration, 0.001)))
    listing.write("file '%s'\n" % os.path.join(args.video_dir, rows[-1][0]))
length = stamps[-1] - start
scale = 'scale=-2:%d:flags=lanczos' % args.height
filters = ('crop=%s,' % args.crop if args.crop else '') + scale + ',fps=30,format=yuv420p'
command = [args.ffmpeg, '-y', '-loglevel', 'error', '-f', 'concat', '-safe', '0', '-i', listing.name,
           '-ss', '%.4f' % start, '-t', '%.4f' % length, '-i', args.audio,
           '-vf', filters, '-c:v', 'libx264', '-preset', 'slow', '-b:v', args.bitrate,
           '-maxrate', args.bitrate, '-bufsize', '2600k', '-c:a', 'aac', '-b:a', '128k',
           '-movflags', '+faststart', '-shortest', args.out]
subprocess.run(command, check=True)
os.unlink(listing.name)
print('video: %s %.1f s, %.2f MB, audio from %.2f s of the WAV' %
      (args.out, length, os.path.getsize(args.out) / 1e6, start))
