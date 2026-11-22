"""FreeType reference for the shared sample spec (Linux host, not CuBit).

Light hinting (FreeType's auto-hinter, vertical-only), HarfBuzz GPOS kerning,
unhinted fractional advances with true subpixel placement (1/64 px), coverage
blended in linear light (sRGB transfer). Mono uses CuBit's 8 px cell grid so
glyph shapes compare one to one.
usage: reference.py SANS.ttf MONO.ttf SEMIBOLD.ttf OUT.ppm [--darken]
                    [--blend=srgb] [--lcd] [--nohint] [--em=PX]
--blend=srgb blends coverage in sRGB values instead of linear light (what
GTK/Cairo and most Linux toolkits do with grayscale text; dark-on-light text
looks heavier, light-on-dark thinner). --lcd renders RGB-stripe subpixel
coverage (FreeType's default 5-tap LCD filter), one coverage per channel.
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
srgb_blend = "--blend=srgb" in sys.argv
lcd = "--lcd" in sys.argv
# --nohint: unhinted outlines (macOS-style; what swash with hint(false) draws).
load_target = freetype.FT_LOAD_NO_HINTING if "--nohint" in sys.argv else freetype.FT_LOAD_TARGET_LIGHT
if lcd:
    freetype.set_lcd_filter(freetype.FT_LCD_FILTER_DEFAULT)
# --em=PX: render at another logical size (UI size study; default: the
# base em, BASE_EM in spec.py).
EM = next((float(a.split("=")[1]) for a in sys.argv if a.startswith("--em=")), float(BASE_EM)) * SCALE[0] / SCALE[1]
ASCENT_PX = int(1025 * EM / 1000 + 0.5)  # CuBit: round(ascender * ratio), halves up
# The monospace grid's pen step in 1/64 physical pixel (exact at 5/4 too).
MONO_PEN64 = MONO_ADVANCE * 64 * SCALE[0] // SCALE[1]

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
    # PATH@WEIGHT: a variable font's instance at that weight (font study).
    path, _, weight = path.partition("@")
    f = freetype.Face(path)
    f.set_char_size(0, round(EM * 64), 72, 72)
    if weight:
        axes = f.get_variation_info().axes
        f.set_var_design_coords([float(weight) if a.tag == "wght" else a.default for a in axes])
    if darken:
        lib = freetype.get_handle()
        on = ctypes.c_int(0)  # no-stem-darkening = false
        freetype.raw.FT_Property_Set(lib, b"autofitter", b"no-stem-darkening", ctypes.byref(on))
    blob = hb.Blob.from_file_path(path)
    hbf = hb.Font(hb.Face(blob))
    if weight:
        hbf.set_variations({"wght": float(weight)})
    hbf.scale = (round(EM * 64), round(EM * 64))
    faces[name] = (f, hbf)

ROW, TOP, LEFT, WIDTH = ROW_PX, TOP_PX, LEFT_PX, WIDTH_PX
H = len(SAMPLES) * ROW
img = np.zeros((H, WIDTH, 3))
for i, (face, fg, bg, text) in enumerate(SAMPLES):
    y0 = i * ROW
    img[y0:y0 + ROW, :] = rgb(bg)
    cov = np.zeros((ROW, WIDTH, 3))
    ft, hbf = faces[face]
    buf = hb.Buffer(); buf.add_str(text); buf.guess_segment_properties()
    hb.shape(hbf, buf, {"kern": kern, "liga": False, "calt": False})
    pen = LEFT * 64
    for info, pos in zip(buf.glyph_infos, buf.glyph_positions):
        x26 = pen + pos.x_offset if face != "mono" else pen
        ft.load_glyph(info.codepoint, load_target | freetype.FT_LOAD_NO_BITMAP)
        slot = ft.glyph
        step = 64 // phases
        x26 = ((x26 + step // 2) // step) * step   # quantize to the phase grid
        frac = x26 & 63
        if slot.format == freetype.FT_GLYPH_FORMAT_OUTLINE and frac:
            freetype.raw.FT_Outline_Translate(ctypes.byref(slot.outline._FT_Outline), frac, 0)
        slot.render(freetype.FT_RENDER_MODE_LCD if lcd else freetype.FT_RENDER_MODE_NORMAL)
        bm = slot.bitmap
        if bm.width and bm.rows:
            a = np.array(bm.buffer, dtype=float).reshape(bm.rows, bm.pitch)[:, :bm.width] / 255.0
            if lcd:
                a = a[:, :(bm.width // 3) * 3].reshape(bm.rows, bm.width // 3, 3)
            else:
                a = np.repeat(a[:, :, None], 3, axis=2)
            gx = (x26 >> 6) + slot.bitmap_left
            gy = TOP + ASCENT_PX - slot.bitmap_top
            for r in range(bm.rows):
                yy = gy + r
                if 0 <= yy < ROW:
                    for c in range(a.shape[1]):
                        xx = gx + c
                        if 0 <= xx < WIDTH:
                            cov[yy, xx] = np.minimum(1.0, cov[yy, xx] + a[r, c])
        pen += pos.x_advance if face != "mono" else MONO_PEN64
    if srgb_blend:
        img[y0:y0 + ROW] = cov * rgb(fg) + (1 - cov) * rgb(bg)
    else:
        F, B = srgb_to_lin(rgb(fg)), srgb_to_lin(rgb(bg))
        img[y0:y0 + ROW] = lin_to_srgb(cov * F + (1 - cov) * B)
Image.fromarray(np.round(img).astype(np.uint8)).save(out)
print("reference:", out)
