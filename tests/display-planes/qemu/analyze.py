#!/usr/bin/env python3
"""Check the virtio-gpu cursor evidence captured by qemu-evidence.sh.

For each guest pause (shown, moved, swapped):
  * the guest scanout (screendump) contains no pixel of either cursor colour,
    so the pointer is not in the guest framebuffer;
  * QEMU's newest virtio_gpu_update_cursor trace places the cursor at the
    expected top-left (pointer minus hotspot) with a live resource;
  * the cursor QEMU publishes to VNC has the expected colour and hotspot;
  * a composite (screendump + published cursor at the traced position) is
    written as <step>-host.png: what a host window shows.
Exit status 0 only if every check holds."""
import json, re, struct, sys, zlib
from pathlib import Path

MAGENTA, GREEN = 0xFF00FF, 0x00FF00
HOT = 4
EXPECT = {
    "shown":   (300 - HOT, 200 - HOT, MAGENTA),
    "moved":   (700 - HOT, 500 - HOT, MAGENTA),
    "swapped": (600 - HOT, 400 - HOT, GREEN),
}

def read_ppm(path):
    data = path.read_bytes()
    parts = data.split(maxsplit=4)
    assert parts[0] == b"P6"
    w, h, _ = int(parts[1]), int(parts[2]), int(parts[3])
    return w, h, bytearray(parts[4][: w * h * 3])

def write_png(path, w, h, rgb):
    raw = b"".join(b"\x00" + bytes(rgb[y * w * 3:(y + 1) * w * 3]) for y in range(h))
    def chunk(tag, body):
        return struct.pack(">I", len(body)) + tag + body + struct.pack(">I", zlib.crc32(tag + body))
    path.write_bytes(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
                     + chunk(b"IDAT", zlib.compress(raw, 6)) + chunk(b"IEND", b""))

def last_cursor(trace):
    events = []
    for line in trace.read_text(errors="replace").splitlines():
        if "virtio_gpu_update_cursor" not in line:
            continue
        nums = re.findall(r"-?\d+", line.split("virtio_gpu_update_cursor", 1)[1])
        kind = "move" if "move" in line else "update"
        scanout, x, y = int(nums[0]), int(nums[1]), int(nums[2])
        res = int(nums[-1])
        events.append((scanout, x, y, kind, res))
    return events

def main(out):
    out = Path(out)
    ok = True
    report = []
    for step, (ex, ey, colour) in EXPECT.items():
        ppm = out / f"{step}.ppm"
        if not ppm.exists():
            report.append(f"{step}: MISSING screendump"); ok = False; continue
        w, h, rgb = read_ppm(ppm)
        hits = sum(1 for i in range(0, len(rgb), 3)
                   if (rgb[i] << 16 | rgb[i + 1] << 8 | rgb[i + 2]) in (MAGENTA, GREEN))
        events = last_cursor(out / f"{step}-trace.log") if (out / f"{step}-trace.log").exists() else []
        live = [e for e in events if e[0] == 0]
        last = live[-1] if live else None
        cur = json.loads((out / f"{step}-cursor.json").read_text()) if (out / f"{step}-cursor.json").exists() else None
        good_trace = last is not None and last[1] == ex and last[2] == ey and last[4] != 0
        good_cursor = cur is not None and cur["hot_x"] == HOT and cur["hot_y"] == HOT and \
            (cur["pixels"][10 * cur["width"] + 10] & 0xFFFFFF) == colour and \
            cur["pixels"][10 * cur["width"] + 10] >> 24 == 0xFF and cur["pixels"][0] >> 24 == 0
        step_ok = hits == 0 and good_trace and good_cursor
        ok &= step_ok
        report.append(f"{step}: {'PASS' if step_ok else 'FAIL'} guest-cursor-pixels={hits} "
                      f"trace={last} cursor={'%dx%d hot %d,%d' % (cur['width'], cur['height'], cur['hot_x'], cur['hot_y']) if cur else None} "
                      f"events={len(events)}")
        if cur and last:
            comp = bytearray(rgb)
            for cy in range(cur["height"]):
                for cx in range(cur["width"]):
                    p = cur["pixels"][cy * cur["width"] + cx]
                    X, Y = last[1] + cx, last[2] + cy
                    if p >> 24 and 0 <= X < w and 0 <= Y < h:
                        i = (Y * w + X) * 3
                        comp[i:i + 3] = bytes(((p >> 16) & 255, (p >> 8) & 255, p & 255))
            write_png(out / f"{step}-host.png", w, h, comp)
            write_png(out / f"{step}-guest.png", w, h, rgb)
    (out / "analysis.txt").write_text("\n".join(report) + "\n")
    print("\n".join(report))
    print("PLANES-EVIDENCE:", "PASS" if ok else "FAIL")
    return 0 if ok else 1

if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
