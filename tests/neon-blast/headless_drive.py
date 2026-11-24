#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Drive Neon Blast in a headless CuBit run (tests/headless/run.sh --test
neon-blast): wait for its first frame, photograph the title, its seed
picker and the bestiary, then let the attract demo play each stage and each
stage's boss (the title's start choice, stepped with the Down key) and
photograph them, then play by QEMU's PS/2 keyboard and mouse (WASD, aim,
fire, dash), photographing with and without the overlay.

Before that, F9 on the title runs the replay benchmark (seed 7, 15 s,
640 x 400, software path) on CuBit; its frame and audio hashes must equal
the Linux-hosted run of the same build (tests/neon-blast/run.sh writes
build/run/a.txt; run it first, or the comparison is skipped and says so).

With NEON_BLAST_VIDEO_SECONDS set, it also films the attract demo: QEMU
screendumps at a steady rate, each stamped with how far QEMU's HDA WAV had
been written at that moment, so tests/neon-blast/make_video.py can line the
frames up with the soundtrack (SHOT_DIR/video/frames.txt).

Usage: headless_drive.py MONITOR_SOCKET SERIAL_LOG SHOT_DIR [AUDIO_WAV]
Screenshots are QEMU screendumps (PPM) of the whole display. Prints
NEON-DRIVE: PASS/FAIL and the game's stats lines.
"""
import re
import os
import socket
import sys
import time

monitor, serial, shots = sys.argv[1:4]
audio = sys.argv[4] if len(sys.argv) > 4 else ''
video_seconds = float(os.environ.get('NEON_BLAST_VIDEO_SECONDS', '0'))
VIDEO_FPS = 20.0
os.makedirs(shots, exist_ok=True)


def hmp(command):
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.connect(monitor)
        s.settimeout(2.0)
        try:
            s.recv(4096)          # banner
        except socket.timeout:
            pass
        s.sendall((command + '\n').encode())
        time.sleep(0.05)
        try:
            s.recv(4096)
        except socket.timeout:
            pass


def log_text():
    with open(serial, 'rb') as f:
        return f.read().decode('utf-8', 'replace')


def wait_for(marker, seconds):
    deadline = time.time() + seconds
    while time.time() < deadline:
        if marker in log_text():
            return True
        time.sleep(0.2)
    print('NEON-DRIVE: FAIL waiting for %r' % marker, flush=True)
    return False


def shot(name):
    path = os.path.join(shots, name + '.ppm')
    hmp('screendump "%s"' % path)
    print('NEON-DRIVE: screenshot %s' % path, flush=True)


def film(seconds):
    """Screendumps at VIDEO_FPS over one monitor connection, each with the
    WAV's size when it was taken."""
    folder = os.path.join(shots, 'video')
    os.makedirs(folder, exist_ok=True)
    index = []
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        s.connect(monitor)
        s.settimeout(5.0)
        s.recv(4096)
        start = time.time()
        n = 0
        while time.time() - start < seconds:
            due = start + n / VIDEO_FPS
            if time.time() < due:
                time.sleep(due - time.time())
            path = os.path.join(folder, 'f%05d.ppm' % n)
            wav_bytes = os.path.getsize(audio) if audio and os.path.exists(audio) else 0
            s.sendall(('screendump "%s"\n' % path).encode())
            buffer = b''
            while b'(qemu)' not in buffer:
                buffer += s.recv(4096)
            index.append('%s %d %.4f' % (os.path.basename(path), wav_bytes, time.time() - start))
            n += 1
    with open(os.path.join(folder, 'frames.txt'), 'w') as f:
        f.write('\n'.join(index) + '\n')
    print('NEON-DRIVE: filmed %d frames in %.1f s' % (len(index), seconds), flush=True)


def key(name, hold_ms=None):
    hmp('sendkey %s%s' % (name, '' if hold_ms is None else ' %d' % hold_ms))


def move(dx, dy):
    while dx or dy:
        sx = max(-60, min(60, dx))
        sy = max(-60, min(60, dy))
        hmp('mouse_move %d %d' % (sx, sy))
        dx -= sx
        dy -= sy
        time.sleep(0.02)


HERE = os.path.dirname(os.path.abspath(__file__))


def hashes(text, prefix):
    frame = re.search(prefix + r'.*?frame_hash= *(\d+)', text, re.S)
    audio = re.search(prefix + r'.*?audio_hash= *(\d+)', text, re.S)
    return (frame.group(1) if frame else None, audio.group(1) if audio else None)


def benchmark():
    key('f9')
    if not wait_for('neon-blast: replay frames=', 240):
        return False
    line = next(l for l in log_text().splitlines() if 'neon-blast: replay frames=' in l)
    print('NEON-DRIVE: ' + line.strip(), flush=True)
    native = hashes(line, 'replay')
    expected_file = os.path.join(HERE, 'build/run/a.txt')
    if not os.path.exists(expected_file):
        print('NEON-DRIVE: replay hashes not compared (no hosted run: tests/neon-blast/run.sh)', flush=True)
        return True
    hosted = hashes(open(expected_file).read(), 'REPLAY')
    same = native == hosted
    print('NEON-DRIVE: replay hashes %s the Linux-hosted run (CuBit %s, host %s)' %
          ('MATCH' if same else 'DIFFER FROM', native, hosted), flush=True)
    return same


STAGE_SHOTS = (20, 32, 44)   # seconds into a stage's demo: groove, build, drop
BOSS_SHOTS = (14, 24)        # seconds into a boss-practice demo: the totem
DEMO_IDLE = 13.5             # the attract demo starts after 12 s without input
CHOICES = 8                  # the title's starts: stages 1 .. 4, then their bosses
VIDEO_CHOICE = 2             # the film: stage 3


def demo_at(choice, here, shots_at, label):
    """From the title (start choice `here`), step the start to `choice`,
    idle until the attract demo plays it, photograph it, then hand the game
    back. Returns the new current choice."""
    for _ in range((choice - here) % CHOICES):
        key('down')
        time.sleep(0.4)
    here = choice
    time.sleep(DEMO_IDLE)
    began = time.time()
    for n, at in enumerate(shots_at):
        due = began + at
        if time.time() < due:
            time.sleep(due - time.time())
        shot('%s-%c' % (label, ord('a') + n))
    key('shift')              # a person is back: the demo ends, the title returns
    time.sleep(1.5)
    return here


def main():
    if not wait_for('neon-blast: first frame painted', 180):
        return 1
    time.sleep(2.5)
    shot('01-title')
    replay_ok = benchmark()
    time.sleep(2.0)
    # The playlist: step the seed on twice, keep it, step back.
    key('right')
    time.sleep(0.5)
    key('right')
    time.sleep(2.5)
    key('k')
    time.sleep(1.0)
    shot('02-title-seed')
    key('left')
    time.sleep(0.5)
    key('left')
    time.sleep(2.5)
    key('g')
    time.sleep(2.0)
    shot('03-bestiary')
    key('g')
    time.sleep(1.0)
    key('f3')
    time.sleep(1.5)
    shot('04-title-overlay')
    key('f3')
    # Each stage, then each boss, as the attract demo plays them.
    here = 0
    for stage in range(4):
        here = demo_at(stage, here, STAGE_SHOTS, '1%d-stage%d' % (stage, stage + 1))
    for stage in range(4):
        here = demo_at(4 + stage, here, BOSS_SHOTS, '2%d-boss%d' % (stage, stage + 1))
    # Film a demo of stage 3 once it has built up.
    if video_seconds > 0:
        for _ in range((VIDEO_CHOICE - here) % CHOICES):
            key('down')
            time.sleep(0.4)
        here = VIDEO_CHOICE
        time.sleep(DEMO_IDLE + 24)
        film(video_seconds)
        key('shift')
        time.sleep(1.5)
    key('f3')
    time.sleep(1.0)
    shot('30-demo-overlay')
    key('f3')
    # Enter starts a game of our own (from the chosen start).
    move(-400, -300)
    move(300, 200)
    key('ret')
    time.sleep(3.0)
    # Play: strafe with held keys, sweep the aim, fire in bursts, dash.
    pattern = ['d', 'w', 'a', 's']
    for round_number in range(16):
        hmp('mouse_button 1')
        key(pattern[round_number % 4], 450)
        move(80 if round_number % 2 == 0 else -80, 40 if round_number % 3 == 0 else -30)
        time.sleep(0.35)
        hmp('mouse_button 0')
        if round_number % 5 == 2:
            key('spc')
        if round_number == 6:
            shot('31-play-a')
        if round_number == 11:
            key('f3')
            time.sleep(1.2)
            shot('32-play-overlay')
            key('f3')
        time.sleep(0.15)
    shot('33-play-b')
    time.sleep(6)          # one more stats interval
    stats = [line for line in log_text().splitlines() if 'neon-blast: stats' in line]
    for line in stats:
        print('NEON-DRIVE: ' + line.strip(), flush=True)
    ok = bool(stats) and replay_ok
    print('NEON-DRIVE: ' + ('PASS' if ok else 'FAIL' + ('' if stats else ' no stats lines')
                                         + ('' if replay_ok else ' replay')), flush=True)
    return 0 if ok else 1


sys.exit(main())
