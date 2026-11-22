"""Interleave reference and CuBit rows (1:1) and a 4x nearest-neighbour zoom.
usage: compare.py REF.png CUBIT.ppm OUT_PREFIX [ZOOM_WIDTH]"""
import sys
from PIL import Image, ImageDraw
sys.path.insert(0, __file__.rsplit("/", 1)[0])
from spec import *
ROW, WIDTH = ROW_PX, WIDTH_PX
ref, cub, prefix = Image.open(sys.argv[1]).convert("RGB"), Image.open(sys.argv[2]).convert("RGB"), sys.argv[3]
zw = int(sys.argv[4]) if len(sys.argv) > 4 else 200
n = len(SAMPLES)
GAP = 2
out = Image.new("RGB", (WIDTH, n * (2 * ROW + GAP)), (255, 0, 255))
for i in range(n):
    box = (0, i * ROW, WIDTH, (i + 1) * ROW)
    y = i * (2 * ROW + GAP)
    out.paste(ref.crop(box), (0, y))
    out.paste(cub.crop(box), (0, y + ROW))
out.save(prefix + "-side.png")
z = out.crop((0, 0, zw, out.height)).resize((zw * 4, out.height * 4), Image.NEAREST)
z.save(prefix + "-zoom4.png")
print(prefix + "-side.png", prefix + "-zoom4.png")
