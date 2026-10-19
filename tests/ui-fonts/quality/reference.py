"""FreeType reference for the shared sample spec (Linux host, not CuBit).

Light hinting (FreeType's auto-hinter, vertical-only), HarfBuzz GPOS kerning,
unhinted fractional advances with true subpixel placement (1/64 px), coverage
blended in linear light (sRGB transfer). Mono uses CuBit's 8 px cell grid so
glyph shapes compare one to one.
usage: reference.py SANS.ttf MONO.ttf SEMIBOLD.ttf OUT.ppm [--darken]
"""
import sys, ctypes
import numpy as np
import freetype, uharfbuzz as hb
from PIL import Image
sys.path.insert(0, __file__.rsplit("/", 1)[0])
from spec import *

sans_path, mono_path, semi_path, out = sys.argv[1:5]
darken = "--darken" in sys.argv
phases = next((int(a.split("=")[1]) for a in sys.argv if a.startswith("--phases=")), 64)
kern = "--nokern" not in sys.argv
EM = 13 * SCALE[0] / SCALE[1]
ASCENT_PX = round(1025 * EM / 1000)  # CuBit: round(ascender * ratio)
MONO_CELL = phys(8)

def srgb_to_lin(v):
    v = v / 255.0
    return np.where(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055) ** 2.4)

def lin_to_srgb(v):
    v = np.clip(v, 0, 1)
    return np.where(v <= 0.0031308, v * 12.92, 1.055 * v ** (1 / 2.4) - 0.055) * 255.0

def rgb(c):
    return np.array([(c >> 16) & 255, (c >> 8) & 255, c & 255], dtype=float)

faces = {}
for name, path in (("sans", sans_path), ("mono", mono_path), ("semi", semi_path)):
    f = freetype.Face(path)
    f.set_char_size(0, round(EM * 64), 72, 72)
    if darken:
        lib = freetype.get_handle()
        on = ctypes.c_int(0)  # no-stem-darkening = false
        freetype.raw.FT_Property_Set(lib, b"autofitter", b"no-stem-darkening", ctypes.byref(on))
    blob = hb.Blob.from_file_path(path)
    hbf = hb.Font(hb.Face(blob))
    hbf.scale = (round(EM * 64), round(EM * 64))
    faces[name] = (f, hbf)

ROW, TOP, LEFT, WIDTH = ROW_PX, TOP_PX, LEFT_PX, WIDTH_PX
H = len(SAMPLES) * ROW
img = np.zeros((H, WIDTH, 3))
for i, (face, fg, bg, text) in enumerate(SAMPLES):
    y0 = i * ROW
    img[y0:y0 + ROW, :] = rgb(bg)
    cov = np.zeros((ROW, WIDTH))
    ft, hbf = faces[face]
    buf = hb.Buffer(); buf.add_str(text); buf.guess_segment_properties()
    hb.shape(hbf, buf, {"kern": kern, "liga": False, "calt": False})
    pen = LEFT * 64
    for info, pos in zip(buf.glyph_infos, buf.glyph_positions):
        x26 = pen + pos.x_offset if face != "mono" else pen
        ft.load_glyph(info.codepoint, freetype.FT_LOAD_TARGET_LIGHT | freetype.FT_LOAD_NO_BITMAP)
        slot = ft.glyph
        step = 64 // phases
        x26 = ((x26 + step // 2) // step) * step   # quantize to the phase grid
        frac = x26 & 63
        if slot.format == freetype.FT_GLYPH_FORMAT_OUTLINE and frac:
            freetype.raw.FT_Outline_Translate(ctypes.byref(slot.outline._FT_Outline), frac, 0)
        slot.render(freetype.FT_RENDER_MODE_NORMAL)
        bm = slot.bitmap
        if bm.width and bm.rows:
            a = np.array(bm.buffer, dtype=float).reshape(bm.rows, bm.pitch)[:, :bm.width] / 255.0
            gx = (x26 >> 6) + slot.bitmap_left
            gy = TOP + ASCENT_PX - slot.bitmap_top
            for r in range(bm.rows):
                yy = gy + r
                if 0 <= yy < ROW:
                    for c in range(bm.width):
                        xx = gx + c
                        if 0 <= xx < WIDTH:
                            cov[yy, xx] = min(1.0, cov[yy, xx] + a[r, c])
        pen += pos.x_advance if face != "mono" else MONO_CELL * 64
    F, B = srgb_to_lin(rgb(fg)), srgb_to_lin(rgb(bg))
    lin = cov[:, :, None] * F + (1 - cov[:, :, None]) * B
    img[y0:y0 + ROW] = lin_to_srgb(lin)
Image.fromarray(np.round(img).astype(np.uint8)).save(out)
print("reference:", out)
