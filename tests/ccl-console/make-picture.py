#!/usr/bin/env python3
"""Write a QOI test picture for image.load: a reference QOI encoder (every
chunk kind: index, diff, luma, run, RGB) over a generated scene, so the
native decoder meets each operation. Usage: make-picture.py OUT [W H]."""
import math, struct, sys

def scene(w, h):
    for y in range(h):
        for x in range(w):
            # Deep-space gradient, a ringed planet, flat bands for runs.
            r, g, b = 10 + y * 30 // h, 14 + y * 40 // h, 40 + x * 80 // w
            dx, dy = x - w * 0.62, y - h * 0.48
            d = math.hypot(dx, dy)
            if d < h * 0.28:
                shade = max(0.0, 1 - d / (h * 0.28))
                r, g, b = int(92 + 120 * shade), int(200 * shade + 40), int(255 * (0.6 + 0.4 * shade))
            elif abs(dy - dx * 0.25) < 2 and d < h * 0.48:
                r, g, b = 255, 216, 102
            if y > h * 0.86:
                r, g, b = 14, 20, 32
            yield (min(r, 255), min(g, 255), min(b, 255), 255)

def encode(w, h, pixels):
    out = bytearray(b'qoif' + struct.pack('>II', w, h) + bytes([4, 0]))
    index = [(0, 0, 0, 0)] * 64
    prev, run = (0, 0, 0, 255), 0
    pixels = list(pixels)
    for i, px in enumerate(pixels):
        if px == prev:
            run += 1
            if run == 62 or i == len(pixels) - 1:
                out.append(0xC0 | (run - 1)); run = 0
            continue
        if run:
            out.append(0xC0 | (run - 1)); run = 0
        h_ = (px[0] * 3 + px[1] * 5 + px[2] * 7 + px[3] * 11) % 64
        if index[h_] == px:
            out.append(h_)
        else:
            index[h_] = px
            if px[3] == prev[3]:
                vr = (px[0] - prev[0] + 128) % 256 - 128
                vg = (px[1] - prev[1] + 128) % 256 - 128
                vb = (px[2] - prev[2] + 128) % 256 - 128
                vg_r, vg_b = vr - vg, vb - vg
                if -3 < vr < 2 and -3 < vg < 2 and -3 < vb < 2:
                    out.append(0x40 | (vr + 2) << 4 | (vg + 2) << 2 | (vb + 2))
                elif -9 < vg_r < 8 and -33 < vg < 32 and -9 < vg_b < 8:
                    out += bytes([0x80 | (vg + 32), (vg_r + 8) << 4 | (vg_b + 8)])
                else:
                    out += bytes([0xFE, px[0], px[1], px[2]])
            else:
                out += bytes([0xFF, *px])
        prev = px
    return bytes(out + bytes([0] * 7 + [1]))

if __name__ == '__main__':
    w, h = (int(sys.argv[2]), int(sys.argv[3])) if len(sys.argv) > 3 else (160, 100)
    open(sys.argv[1], 'wb').write(encode(w, h, scene(w, h)))
