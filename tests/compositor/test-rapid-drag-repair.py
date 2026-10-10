#!/usr/bin/env python3
"""Rapid window move/resize stale-pixel check on the UEFI live image (QEMU).

Hardware reports (Intel N95 NUC, 1920x1080, software rendering) describe
window moves/resizes that leave stale pixels, with many pointer reports per
frame. This boots the live image like tests/usb-optical/run-live.py
(--uefi --usb-flash --without-ps2 --usb-hub, 8 ports), takes a wallpaper
reference with the auto-started boot viewer (960x646 at 98,82) minimized,
restores it, then sends many small mouse_move steps without waiting for frames:
a move, a resize that grows and a resize that shrinks. After each release has
been processed (Desktop's bounded "ptr drag-up" trace), every pixel outside the
final window footprint (plus shadow margin), the taskbar and the cursor must
equal the reference.

--tcg slows the guest by roughly an order of magnitude, so dozens of motion
reports coalesce into each frame (about 1 frame/s with Mesa softpipe), which
approximates the N95's low software frame rate. --bench instead performs a
sustained drag for the timing build's per-second "desktop: stats" and
"desktop: frames=" records. Run on the host (KVM) or under TCG; this is a
Linux-hosted QEMU check, not hardware evidence.
"""
import argparse
import json
import pathlib
import socket
import subprocess
import sys
import tempfile
import time

from PIL import Image, ImageChops

root = pathlib.Path(__file__).resolve().parents[2]
p = argparse.ArgumentParser()
p.add_argument('--image', type=pathlib.Path, default=root / 'kernel/cubit_live_uefi.img')
p.add_argument('--uefi-firmware', type=pathlib.Path,
               default=pathlib.Path('/usr/share/OVMF/OVMF_CODE_4M.fd'))
p.add_argument('--timeout', type=int, default=300)
p.add_argument('--step', type=int, default=3, help='pixels per motion report')
p.add_argument('--steps', type=int, default=120, help='motion reports per gesture')
p.add_argument('--cpus', type=int, default=4)
p.add_argument('--tcg', action='store_true', help='slow guest: many reports per frame')
p.add_argument('--bench', action='store_true', help='sustained drag for timing records only')
a = p.parse_args()

SCREEN = (1920, 1080)
WINDOW = (98, 82, 960, 646)          # boot viewer x, y, w, h
MINIMIZE, TASK_BUTTON, TITLE = (1004, 94), (180, 1062), (500, 94)
SHADOW_MARGIN = 8                    # window visual margin plus slack
TASKBAR_HEIGHT = 44
CURSOR_BOX = 64                      # excluded around the final pointer
CORNER_INSET = 3                     # resize grip inside the bottom-right edge
SETTLE = 6.0 if a.tcg else 1.0       # seconds after processed input

run = pathlib.Path(tempfile.mkdtemp(prefix='rapid-drag.', dir=None))
serial, monitor = run / 'serial.log', run / 'monitor.sock'
accel = (['-accel', 'tcg,thread=multi', '-machine', 'q35', '-cpu', 'max'] if a.tcg
         else ['-enable-kvm', '-machine', 'q35', '-cpu', 'host'])
command = ['qemu-system-x86_64', *accel, '-smp', str(a.cpus), '-m', '4G', '-nodefaults',
           '-device', 'VGA',
           '-drive', f'if=pflash,format=raw,readonly=on,file={a.uefi_firmware}',
           '-device', 'qemu-xhci,id=xhci',
           '-drive', f'file={a.image.resolve(strict=True)},if=none,id=cd,format=raw,readonly=on',
           '-device', 'usb-bot,id=usbcd,bus=xhci.0,port=1',
           '-device', 'scsi-hd,bus=usbcd.0,lun=0,drive=cd,bootindex=1',
           '-device', 'usb-hub,id=inputhub,bus=xhci.0,port=2,ports=8,port-power=on',
           '-device', 'usb-mouse,bus=xhci.0,port=2.1',
           '-device', 'usb-kbd,bus=xhci.0,port=2.8',
           '-serial', f'file:{serial}', '-qmp', f'unix:{monitor},server,nowait',
           '-display', 'none', '-no-reboot']
(run / 'command.json').write_text(json.dumps(command, indent=2))
print(f'rapid drag logs: {run}', flush=True)
process = subprocess.Popen(command, stdout=(run / 'qemu.log').open('w'), stderr=subprocess.STDOUT)
deadline = time.monotonic() + a.timeout
failed = True
try:
    while not monitor.exists():
        if process.poll() is not None or time.monotonic() > deadline:
            raise RuntimeError('QEMU monitor not created')
        time.sleep(0.05)
    connection = socket.socket(socket.AF_UNIX)
    connection.connect(str(monitor))
    connection.settimeout(5)
    stream = connection.makefile('rb')
    json.loads(stream.readline())
    sequence = 0

    def qmp(operation, arguments=None):
        global sequence
        sequence += 1
        request = {'execute': operation, 'id': sequence}
        if arguments is not None:
            request['arguments'] = arguments
        connection.sendall((json.dumps(request) + '\n').encode())
        while True:
            response = json.loads(stream.readline())
            if response.get('id') == sequence:
                if 'error' in response:
                    raise RuntimeError(response)
                return response['return']

    qmp('qmp_capabilities')

    def hmp(text):
        return qmp('human-monitor-command', {'command-line': text})

    def log_text():
        return serial.read_text(errors='replace') if serial.exists() else ''

    def wait_for(marker, count=1):
        while time.monotonic() < deadline and process.poll() is None:
            if log_text().count(marker) >= count:
                return
            time.sleep(0.2)
        raise RuntimeError(f'timeout waiting for {marker!r} x{count}')

    def traces(label):
        # Desktop's own records only, not the boot viewer's replay of them.
        # Serial output from other services can interleave on the same line.
        text = log_text()
        marker = 'desktop: ptr ' + label
        return text.count(marker) - text.count('boot-logs: ' + marker)

    def wait_trace(label, count):
        while time.monotonic() < deadline and process.poll() is None:
            if traces(label) >= count:
                return
            time.sleep(0.2)
        raise RuntimeError(f'timeout waiting for ptr {label} x{count}')

    def shot(name, settle=SETTLE):
        time.sleep(settle)
        path = run / f'{name}.ppm'
        hmp(f'screendump {path}')
        image = Image.open(path).convert('RGB')
        image.save(run / f'{name}.png')
        return image

    def move(dx, dy, limit=80):
        while dx or dy:
            x, y = max(-limit, min(limit, dx)), max(-limit, min(limit, dy))
            hmp(f'mouse_move {x} {y}')
            dx -= x
            dy -= y
            time.sleep(0.04)

    def goto(x, y):
        move(-2 * SCREEN[0], -2 * SCREEN[1])
        move(x, y)
        time.sleep(0.2)

    def press(x, y):
        downs = traces('hit-down')
        goto(x, y)
        hmp('mouse_button 1')
        wait_trace('hit-down', downs + 1)

    def click(x, y):
        press(x, y)
        hmp('mouse_button 0')

    def release():
        ups = traces('drag-up')
        hmp('mouse_button 0')
        wait_trace('drag-up', ups + 1)

    wait_for('boot-logs: window ready')
    wait_for('desktop: asynchronous frame released')
    initial = shot('initial', 3 * SETTLE)
    if initial.size != SCREEN:
        raise RuntimeError(f'fixture requires {SCREEN} UEFI mode, got {initial.size}')
    click(*MINIMIZE)
    goto(0, 0)
    reference = shot('reference-minimized')
    goto(*TASK_BUTTON)
    hmp('mouse_button 1')
    time.sleep(0.15)
    hmp('mouse_button 0')
    goto(0, 0)
    shot('restored')

    if a.bench:
        press(*TITLE)
        started = time.monotonic()
        for _ in range(3):
            for i in range(400):
                hmp(f'mouse_move {2 if (i // 100) % 2 == 0 else -2} {1 if i < 200 else -1}')
                time.sleep(0.008)
        (run / 'bench-host-seconds.txt').write_text(str(time.monotonic() - started))
        release()
        shot('bench-end')
        for line in log_text().splitlines():
            if line.startswith(('desktop: stats', 'desktop: frames=')):
                print(line, flush=True)
        failed = False
        sys.exit(0)

    def stale(frame, window, cursor, name):
        width, height = SCREEN
        changed = ImageChops.difference(frame, reference).convert('L').point(
            lambda value: 255 if value else 0)
        mask = Image.new('L', SCREEN, 255)
        x, y, w, h = window
        mask.paste(0, (max(0, x - SHADOW_MARGIN), max(0, y - SHADOW_MARGIN),
                       min(width, x + w + SHADOW_MARGIN), min(height, y + h + SHADOW_MARGIN)))
        mask.paste(0, (0, height - TASKBAR_HEIGHT, width, height))
        mask.paste(0, (0, 0, CURSOR_BOX, CURSOR_BOX))  # reference pointer at origin
        cx, cy = cursor
        mask.paste(0, (max(0, cx - CURSOR_BOX), max(0, cy - CURSOR_BOX),
                       min(width, cx + CURSOR_BOX), min(height, cy + CURSOR_BOX)))
        bad = ImageChops.multiply(changed, mask)
        bad.save(run / f'{name}-stale.png')
        count = bad.histogram()[255]
        print(f'{name}: stale pixels={count} bbox={bad.getbbox()}', flush=True)
        return count

    results = {}
    # Move: title press, then many small reports with no frame pacing.
    press(*TITLE)
    dx = dy = 0
    for i in range(a.steps):
        sy = a.step // 2 if i % 2 else a.step - a.step // 2
        hmp(f'mouse_move {a.step} {sy}')
        dx += a.step
        dy += sy
    release()
    window = (WINDOW[0] + dx, WINDOW[1] + dy, WINDOW[2], WINDOW[3])
    results['move'] = stale(shot('move'), window, (TITLE[0] + dx, TITLE[1] + dy), 'move')

    # Resize: grow from the bottom-right grip, then shrink back to the
    # viewer's minimum (its initial size).
    def resize(name, sx, sy):
        global window
        x, y, w, h = window
        start = (x + w - CORNER_INSET, y + h - CORNER_INSET)
        press(*start)
        ty = 0
        for i in range(a.steps):
            step_y = sy if i % 2 == 0 else 0
            ty += step_y
            hmp(f'mouse_move {sx} {step_y}')
        release()
        end = (start[0] + sx * a.steps, start[1] + ty)
        window = (x, y, max(WINDOW[2], end[0] - x), max(WINDOW[3], end[1] - y))
        results[name] = stale(shot(name), window, end, name)

    resize('resize-grow', 3, 1)
    resize('resize-shrink', -3, -1)
    (run / 'results.json').write_text(json.dumps(results, indent=2) + '\n')
    if any(results.values()):
        print(f'RAPID DRAG FAIL: stale pixels {results}; see {run}', flush=True)
    else:
        failed = False
        print('RAPID DRAG PASS: move, grow and shrink leave no stale pixels outside the window',
              flush=True)
finally:
    try:
        process.terminate()
        process.wait(10)
    except Exception:
        process.kill()
sys.exit(1 if failed else 0)
