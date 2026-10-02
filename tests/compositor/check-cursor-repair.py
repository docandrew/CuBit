"""Native direct-output cursor restoration oracle after desktop-display input.

Pass a private runner TMPDIR, serial log and observer time budget. The normal
headless runner still owns the VM, input regression and final fault scan.
"""
from pathlib import Path
import socket
import sys
import time

work, serial = map(Path, sys.argv[1:3])
deadline = time.monotonic() + float(sys.argv[3])

def text():
    s = serial.read_text(errors='replace') if serial.exists() else ''
    if any(x in s for x in ('USER-MEMORY-FAULT:', 'EXCEPTION:', 'PANIC')):
        raise RuntimeError('native fault')
    return s

def until(predicate, message):
    while not predicate():
        if time.monotonic() >= deadline:
            raise RuntimeError(message)
        time.sleep(.1)

until(lambda: text().count('desktop: ptr title-double ') >= 2,
      'standard desktop input fixture did not finish')
until(lambda: 'desktop: direct pooled rendering active' in text(),
      'direct pooled renderer not active')
paths = list(work.glob('cubit-desktop-display-monitor-*.sock'))
assert len(paths) == 1, paths
monitor = paths[0]
time.sleep(1)

with socket.socket(socket.AF_UNIX) as channel:
    channel.settimeout(5)
    channel.connect(str(monitor))

    def prompt():
        data = b''
        while not data.endswith(b'(qemu) '):
            chunk = channel.recv(4096)
            if not chunk:
                raise RuntimeError('monitor closed')
            data += chunk
        return data

    prompt()

    def command(line):
        channel.sendall((line+'\n').encode())
        prompt()

    def relative(dx,dy):
        command(f'mouse_move {dx} {dy}')
        time.sleep(.12)

    # Confine to known origin using actual PS/2 reports, irrespective of the
    # maximize/restore fixture's final cursor position.
    for _ in range(24):
        relative(-60,-60)
    time.sleep(1)
    pointer = [0,0]

    def move(x,y):
        while pointer != [x,y]:
            dx,dy = [max(-60,min(60,a-b)) for a,b in zip((x,y),pointer)]
            relative(dx,dy)
            pointer[0] += dx; pointer[1] += dy
        time.sleep(.6)

    def capture(name):
        p = serial.with_suffix('.cursor-'+name+'.ppm')
        command(f'screendump "{p}"')
        magic,dims,maximum,data = p.read_bytes().split(b'\n',3)
        w,h = map(int,dims.split())
        assert magic==b'P6' and maximum==b'255' and len(data)==w*h*3
        assert w>=96 and h>660
        # Exclude taskbar clock and current parked cursor; this strip is to
        # the left of the restored Workbench window.
        return b''.join(data[(y*w)*3:(y*w+96)*3] for y in range(300,580))

    move(20,640)
    baseline = capture('baseline')
    for step,(x,y) in enumerate([(20,340),(50,440),(20,520),(50,320)]):
        move(x,y)
        until(lambda: capture(f'visible-{step}') != baseline,
              'cursor did not visibly enter checked region')
        move(20,640)
        # Give outstanding presentation a bounded chance to complete; a
        # matching clean target alone is insufficient, screenshot is scanout.
        phase_deadline = min(deadline,time.monotonic()+5)
        while capture(f'restored-{step}') != baseline:
            if time.monotonic() >= phase_deadline:
                raise RuntimeError(f'cursor left scanout artifacts in phase {step}')
            time.sleep(.1)
    print('CURSOR-NATIVE: PASS visible cursor and exact scanout restoration across four moves',flush=True)
