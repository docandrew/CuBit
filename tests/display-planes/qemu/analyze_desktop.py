#!/usr/bin/env python3
"""Desktop pointer with relative input (desktop-virtio-vga, PS/2 mouse).

QEMU's GTK/SDL frontends show a virtio-gpu cursor only as the host pointer,
and hide the host pointer while grabbing a relative mouse. So with relative
input the desktop must keep compositing its pointer. Checks:
  * QEMU was never given a visible cursor (no UPDATE_CURSOR with a resource);
  * the guest scanout contains the Desktop arrow at the pointer position the
    desktop last reported (every opaque arrow pixel matches the screendump);
  * the injected motion moved it.
Writes desktop-guest.png."""
import re, sys
from pathlib import Path
from analyze import read_ppm, write_png, last_cursor

ROOT = Path(__file__).resolve().parents[3]

def arrow():
    text = (ROOT / "userspace/services/desktop/desktop_cursors.ads").read_text()
    m = re.search(r"Arrow => \(Width => (\d+), Height => (\d+), Hotspot_X => (\d+), Hotspot_Y => (\d+), Offset => (\d+)\)", text)
    w, h, hx, hy, off = map(int, m.groups())
    body = text[text.index("Pixels : constant"):]
    values = [int(v.replace("_", ""), 16) for v in re.findall(r"16#([0-9A-F_]+)#", body)]
    return w, h, hx, hy, values[off:off + w * h]

def main(out):
    out = Path(out)
    serial = (out / "serial.log").read_text(errors="replace")
    positions = [(int(x), int(y)) for x, y in re.findall(r"cursor_x=(\d+)[\s\S]{0,120}?ursor_y=(\d+)", serial)]  # lines interleave
    events = last_cursor(out / "desktop-trace.log") if (out / "desktop-trace.log").exists() else []
    shown = [e for e in events if e[4] != 0]
    w, h, rgb = read_ppm(out / "desktop.ppm")
    aw, ah, hx, hy, pix = arrow()
    px, py = positions[-1]
    opaque = match = 0
    for cy in range(ah):
        for cx in range(aw):
            p = pix[cy * aw + cx]
            X, Y = px - hx + cx, py - hy + cy
            if p >> 24 == 0xFF and 0 <= X < w and 0 <= Y < h:
                opaque += 1
                i = (Y * w + X) * 3
                if (rgb[i] << 16 | rgb[i + 1] << 8 | rgb[i + 2]) == p & 0xFFFFFF:
                    match += 1
    write_png(out / "desktop-guest.png", w, h, rgb)
    moved = len(set(positions)) > 1
    composited_marker = "desktop: pointer composited (host-pointer cursor planes need absolute input)" in serial
    ok = not shown and opaque > 0 and match == opaque and moved and composited_marker
    print(f"host-cursor-events={len(events)} visible-host-cursor={len(shown)} pointer={px},{py} "
          f"arrow-opaque={opaque} matched-in-guest={match} moved={moved} marker={composited_marker}")
    print("DESKTOP-POINTER-EVIDENCE:", "PASS" if ok else "FAIL")
    return 0 if ok else 1

if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
