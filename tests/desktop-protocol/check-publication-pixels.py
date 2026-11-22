#!/usr/bin/env python3
"""Observe a native desktop-check publication fixture through QEMU's monitor.

This checks actual scanout pixels on QEMU's legacy unit-scale output, not
physical photons, GPU execution, or presentation-fence semantics.
"""
import argparse
import pathlib
import re
import socket
import time

parser = argparse.ArgumentParser()
parser.add_argument('--serial', required=True, type=pathlib.Path)
parser.add_argument('--timeout', type=float, default=180)
args = parser.parse_args()
deadline = time.monotonic() + args.timeout
image = args.serial.with_suffix('.publication.ppm').resolve()
marker = re.compile(r'DESKTOP-PUBLICATION-VISIBLE: ready width=\s*(\d+) height=\s*(\d+)')
last_error = 'publication fixture not ready'
while time.monotonic() < deadline:
    serial = args.serial.read_text(errors='replace') if args.serial.exists() else ''
    match = marker.search(serial)
    if not match:
        time.sleep(0.05)
        continue
    width, height = map(int, match.groups())
    monitors = []
    for candidate in pathlib.Path('/tmp').glob('nix-shell.*/cubit-desktop-protocol-monitor-*.sock'):
        pid = candidate.stem.rsplit('-', 1)[-1]
        try:
            command = pathlib.Path('/proc', pid, 'cmdline').read_bytes().split(b'\0')
        except OSError:
            continue
        if str(args.serial).encode() in command:
            monitors.append(candidate)
    if len(monitors) != 1:
        last_error = 'cannot uniquely identify this run\'s monitor'
        time.sleep(0.05)
        continue
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as monitor:
            monitor.settimeout(1)
            monitor.connect(str(monitors[0]))
            monitor.sendall(f'screendump "{image}"\n'.encode())
            # Read command completion, rather than assuming an atomic file write.
            received = b''
            while received.count(b'(qemu)') < 2:
                chunk = monitor.recv(8192)
                if not chunk:
                    break
                received += chunk
        raw = image.read_bytes()
        header = re.match(rb'P6\s+(\d+)\s+(\d+)\s+255\s', raw)
        if not header:
            raise ValueError('unexpected PPM header')
        screen_w, screen_h = map(int, header.groups())
        pixels = raw[header.end():]
        if len(pixels) != screen_w * screen_h * 3:
            raise ValueError('incomplete screenshot')
        upper = bytes((0x20, 0x60, 0xA0))
        lower = bytes((0xE0, 0xA0, 0x40))
        points = [i // 3 for i in range(0, len(pixels), 3) if pixels[i:i+3] == upper]
        if not points:
            raise ValueError('published pixels not scanned out yet')
        left = min(p % screen_w for p in points)
        top = min(p // screen_w for p in points)
        if left + width > screen_w or top + height > screen_h:
            raise ValueError('published extent outside output')
        for y in range(height):
            start = ((top + y) * screen_w + left) * 3
            expected = (upper if y < height // 2 else lower) * width
            if 9 <= y < 20:
                expected = expected[:7*3] + bytes((0x40, 0xD0, 0x80)) * 13 + expected[20*3:]
            if pixels[start:start + width * 3] != expected:
                raise ValueError(f'published image mismatch on row {y}')
        # Both color bands must have exactly the configured logical extent.
        if len(points) != width * (height // 2) - 13 * 11:
            raise ValueError('upper band has an unexpected extent')
        lower_count = sum(pixels[i:i+3] == lower for i in range(0, len(pixels), 3))
        if lower_count != width * (height - height // 2):
            raise ValueError('lower band has an unexpected extent')
        print(f'PUBLICATION-PIXELS: PASS {width}x{height} at {left},{top}: {width*height} exact RGB pixels including 13x11 partial patch', flush=True)
        raise SystemExit(0)
    except (OSError, ValueError) as error:
        last_error = str(error)
        time.sleep(0.05)
raise SystemExit(f'PUBLICATION-PIXELS: FAIL {last_error}')
