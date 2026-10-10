#!/usr/bin/env python3
"""CuBit desktop icons: the Apps menu, its categories, the Apps button logo
and Power. Haiku/BeOS style: bright, vivid colours, isometric shapes, a
dark outline that holds at 24 pixels, light from the upper left.

Each icon is written as an editable SVG (paths, gradients and circles
only; no filters, text or embedded rasters) into svg/, then exported with
rsvg-convert to png/<name>-<size>.png. Penny keeps her own artwork
(assets/penny). tools/generate_desktop_icons.py builds the desktop atlas
from these exports.

Run from the repository root inside nix develop:
  python3 assets/desktop-icons/make.py
"""
from math import cos, sin, pi, radians
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
SIZES = (16, 24, 32, 48, 64)
VIEW = 48
INK = 2.0          # outline width in the 48-unit view: one pixel at 24
C30, S30 = cos(radians(30)), sin(radians(30))


def iso(x, y, z, ox=24.0, oy=24.0, s=1.0):
    """Isometric point: x to the lower right, y to the lower left, z up."""
    return (ox + (x - y) * C30 * s, oy + (x + y) * S30 * s - z * s)


def pts(points):
    return " ".join(f"{x:.2f},{y:.2f}" for x, y in points)


class Icon:
    def __init__(self, name):
        self.name = name
        self.defs = []
        self.body = []
        self.ids = 0

    def gradient(self, top, bottom, x1=0, y1=0, x2=0, y2=1):
        self.ids += 1
        gid = f"g{self.ids}"
        self.defs.append(
            f'<linearGradient id="{gid}" x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}">'
            f'<stop offset="0" stop-color="{top}"/><stop offset="1" stop-color="{bottom}"/></linearGradient>')
        return f"url(#{gid})"

    def radial(self, inner, outer, cx=0.35, cy=0.3, r=0.8):
        self.ids += 1
        gid = f"g{self.ids}"
        self.defs.append(
            f'<radialGradient id="{gid}" cx="{cx}" cy="{cy}" r="{r}">'
            f'<stop offset="0" stop-color="{inner}"/><stop offset="1" stop-color="{outer}"/></radialGradient>')
        return f"url(#{gid})"

    def poly(self, points, fill, ink="#1d1030", width=INK):
        stroke = f' stroke="{ink}" stroke-width="{width}" stroke-linejoin="round"' if ink else ""
        self.body.append(f'<polygon points="{pts(points)}" fill="{fill}"{stroke}/>')

    def line(self, points, color, width=INK, cap="round"):
        self.body.append(f'<polyline points="{pts(points)}" fill="none" stroke="{color}" '
                         f'stroke-width="{width}" stroke-linecap="{cap}" stroke-linejoin="round"/>')

    def path(self, d, fill, ink="#1d1030", width=INK):
        stroke = f' stroke="{ink}" stroke-width="{width}" stroke-linejoin="round"' if ink else ""
        self.body.append(f'<path d="{d}" fill="{fill}"{stroke}/>')

    def circle(self, cx, cy, r, fill, ink="#1d1030", width=INK):
        stroke = f' stroke="{ink}" stroke-width="{width}"' if ink else ""
        self.body.append(f'<circle cx="{cx:.2f}" cy="{cy:.2f}" r="{r:.2f}" fill="{fill}"{stroke}/>')

    def ellipse(self, cx, cy, rx, ry, fill, ink="#1d1030", width=INK):
        stroke = f' stroke="{ink}" stroke-width="{width}"' if ink else ""
        self.body.append(f'<ellipse cx="{cx:.2f}" cy="{cy:.2f}" rx="{rx:.2f}" ry="{ry:.2f}" fill="{fill}"{stroke}/>')

    def box(self, x0, y0, z0, dx, dy, dz, top, left, right, ink="#1d1030", ox=24.0, oy=24.0):
        """An isometric box: top face, left (front-left, +y) and right
        (front-right, +x) faces. Colours are (light, dark) pairs."""
        P = lambda x, y, z: iso(x, y, z, ox, oy)
        x1, y1, z1 = x0 + dx, y0 + dy, z0 + dz
        self.poly([P(x0, y1, z0), P(x1, y1, z0), P(x1, y1, z1), P(x0, y1, z1)],
                  self.gradient(*left), ink)
        self.poly([P(x1, y0, z0), P(x1, y1, z0), P(x1, y1, z1), P(x1, y0, z1)],
                  self.gradient(*right), ink)
        self.poly([P(x0, y0, z1), P(x1, y0, z1), P(x1, y1, z1), P(x0, y1, z1)],
                  self.gradient(*top, x1=0, y1=0, x2=1, y2=1), ink)

    def svg(self):
        return (f'<svg xmlns="http://www.w3.org/2000/svg" width="{VIEW}" height="{VIEW}" '
                f'viewBox="0 0 {VIEW} {VIEW}">\n<defs>{"".join(self.defs)}</defs>\n'
                + "\n".join(self.body) + "\n</svg>\n")


# Face helpers: points on the front-left (+y) face and front-right (+x)
# face of a box, given face-local (u along the face, v up).
def left_face(x0, y1, z0, u, v, ox=24.0, oy=24.0):
    return iso(x0 + u, y1, z0 + v, ox, oy)


def right_face(x1, y0, z0, u, v, ox=24.0, oy=24.0):
    return iso(x1, y0 + u, z0 + v, ox, oy)


def top_face(x0, y0, z1, u, v, ox=24.0, oy=24.0):
    return iso(x0 + u, y0 + v, z1, ox, oy)


PURPLE = (("#c9a2ff", "#9a5cf2"), ("#7b3fd6", "#4f1fa0"), ("#3ff0d6", "#12a597"))


def cubit_logo():
    """The OS logo: a purple cube with one teal face."""
    i = Icon("cubit")
    i.box(-12, -12, -4, 24, 24, 20, PURPLE[0], PURPLE[1], PURPLE[2], ink="#25094d", oy=28)
    # A highlight along the top's far edges.
    i.line([iso(-10, 10, 16, oy=28), iso(-10, -10, 16, oy=28), iso(10, -10, 16, oy=28)], "#efe2ff", 1.2)
    return i


def workbench():
    """CCL Workbench: a raised slate showing coloured code, on a base."""
    i = Icon("workbench")
    i.box(-13, -10, -8, 26, 20, 5, ("#ffd36b", "#f5a524"), ("#d9821a", "#a55a0c"),
          ("#f0a338", "#c06d10"), oy=26)
    # The slate stands on the back edge, facing front-left.
    x0, y1 = -11, -6
    face = [left_face(x0, y1, -3, 0, 0, oy=26), left_face(x0, y1, -3, 20, 0, oy=26),
            left_face(x0, y1, -3, 20, 21, oy=26), left_face(x0, y1, -3, 0, 21, oy=26)]
    side = [left_face(x0 + 20, y1, -3, 0, 0, oy=26), iso(x0 + 20, y1 - 3, -3, oy=26),
            iso(x0 + 20, y1 - 3, 18, oy=26), left_face(x0 + 20, y1, -3, 0, 21, oy=26)]
    i.poly(side, i.gradient("#4c5d7a", "#27324a"))
    i.poly(face, i.gradient("#2f4f9e", "#13224d"))
    rows = (("#5cf08a", 2, 9), ("#ffd84a", 5, 14), ("#ff7a9c", 5, 11), ("#7fd8ff", 2, 16))
    for n, (color, a, b) in enumerate(rows):
        v = 17 - n * 4
        i.line([left_face(x0, y1, -3, a, v, oy=26), left_face(x0, y1, -3, b, v, oy=26)], color, 2.2)
    return i


def console():
    """Console: a dark terminal block with a green prompt."""
    i = Icon("console")
    i.box(-10, -12, -10, 20, 24, 22, ("#8a96aa", "#5d677a"), ("#2c3240", "#12151c"),
          ("#59627a", "#30364a"), oy=25)
    x0, y1 = -10, 12
    i.line([left_face(x0, y1, -10, 3, 16, oy=25), left_face(x0, y1, -10, 8, 12, oy=25),
            left_face(x0, y1, -10, 3, 8, oy=25)], "#4dff7c", 2.4)
    i.line([left_face(x0, y1, -10, 10, 5, oy=25), left_face(x0, y1, -10, 16, 5, oy=25)], "#4dff7c", 2.4)
    return i


def logs():
    """Logs: a stack of pages, each record marked by its severity colour."""
    i = Icon("logs")
    for k, dz in enumerate((-9, -5, -1)):
        i.box(-12, -12, dz, 24, 24, 2, ("#ffffff", "#dfe6f2"), ("#b8c2d4", "#8e99ad"),
              ("#d2dae8", "#a7b2c6"), oy=24 + k * 0)
    colors = ("#ff4d4d", "#ffb020", "#3aa0ff", "#3ad46a")
    for n, c in enumerate(colors):
        v = -8 + n * 5
        i.line([top_face(-12, -12, 1, 4, 12 + v, oy=24), top_face(-12, -12, 1, 7, 12 + v, oy=24)], c, 2.6)
        i.line([top_face(-12, -12, 1, 10, 12 + v, oy=24), top_face(-12, -12, 1, 20, 12 + v, oy=24)],
               "#6b7890", 1.4)
    return i


def trace():
    """Desktop trace: an oscilloscope plate with a bright trace."""
    i = Icon("trace")
    i.box(-13, -13, -6, 26, 26, 4, ("#2b3550", "#141a2c"), ("#4d5a7a", "#262e44"),
          ("#3a4664", "#1c2338"), oy=24)
    grid = "#3f5a88"
    for g in (-6, 0, 6):
        i.line([top_face(-13, -13, -2, 2, 13 + g, oy=24), top_face(-13, -13, -2, 24, 13 + g, oy=24)], grid, 0.8)
        i.line([top_face(-13, -13, -2, 13 + g, 2, oy=24), top_face(-13, -13, -2, 13 + g, 24, oy=24)], grid, 0.8)
    wave = [(2, 14), (6, 14), (8, 5), (11, 22), (14, 9), (17, 16), (20, 13), (24, 13)]
    i.line([top_face(-13, -13, -2, u, v, oy=24) for u, v in wave], "#44f3ff", 2.6)
    i.line([top_face(-13, -13, -2, u, v, oy=24) for u, v in wave], "#e8ffff", 0.9)
    return i


def doom():
    """DOOM: a red demon block with horns and burning eyes."""
    i = Icon("doom")
    oy = 29
    i.box(-10, -10, -10, 20, 20, 16, ("#ff7a4a", "#e0381c"), ("#b3170c", "#6e0904"),
          ("#e0301a", "#9e1408"), ink="#2a0602", oy=oy)
    bone = i.gradient("#fff6d6", "#c9a35a")
    lx, ly = iso(-10, 4, 6, oy=oy)
    rx, ry = iso(4, -10, 6, oy=oy)
    i.path(f"M {lx-1:.1f} {ly+1:.1f} C {lx-6:.1f} {ly-3:.1f} {lx-7:.1f} {ly-9:.1f} {lx-4:.1f} {ly-14:.1f} "
           f"C {lx-3:.1f} {ly-9:.1f} {lx+1:.1f} {ly-5:.1f} {lx+5:.1f} {ly-3:.1f} Z", bone, ink="#2a0602", width=1.4)
    i.path(f"M {rx+1:.1f} {ry+1:.1f} C {rx+6:.1f} {ry-3:.1f} {rx+7:.1f} {ry-9:.1f} {rx+4:.1f} {ry-14:.1f} "
           f"C {rx+3:.1f} {ry-9:.1f} {rx-1:.1f} {ry-5:.1f} {rx-5:.1f} {ry-3:.1f} Z", bone, ink="#2a0602", width=1.4)
    x0, y1 = -10, 10
    F = lambda u, v: left_face(x0, y1, -10, u, v, oy=oy)
    G = lambda u, v: right_face(10, -10, -10, u, v, oy=oy)
    for face in (F, G):
        a, b = (3, 11) if face is F else (9, 17)
        i.poly([face(a, 10), face(a + 6, 8), face(a + 6, 11), face(a, 12)], "#ffe23d", ink="#2a0602", width=1.0)
    i.poly([F(4, 3), F(16, 3), F(14, 6), F(6, 6)], "#3a0402", ink=None)
    return i

def devices():
    """Devices: a green circuit board carrying a chip with gold pins."""
    i = Icon("devices")
    i.box(-14, -14, -6, 28, 28, 3, ("#4ee07a", "#169a45"), ("#127a35", "#0b4f22"),
          ("#1c9c48", "#0e6a2c"), ink="#06290f", oy=26)
    for u in (4, 9, 14):
        i.line([top_face(-14, -14, -3, u, 2, oy=26), top_face(-14, -14, -3, u, 7, oy=26)], "#c9f7a0", 1.0)
    for k in range(4):
        u = 9 + k * 3.5
        for v0, v1 in ((5, 9), (19, 23)):
            i.line([top_face(-14, -14, -3, u, v0, oy=26), top_face(-14, -14, -3, u, v1, oy=26)], "#ffd447", 1.6)
    i.box(-6, -6, -3, 12, 12, 4, ("#4a4f5c", "#22252e"), ("#15171d", "#08090c"),
          ("#2a2d36", "#121318"), ink="#000000", oy=26)
    i.circle(*iso(-3, -3, 1, oy=26), 1.1, "#9aa3b5", ink=None)
    return i


def files():
    """Files: a yellow folder, Haiku style."""
    i = Icon("files")
    back = [(7, 12), (19, 12), (22, 15), (41, 15), (41, 37), (7, 37)]
    i.poly(back, i.gradient("#f7c64a", "#d18d16"), ink="#3c2400")
    front = [(5, 20), (39, 20), (44, 39), (10, 39)]
    i.poly(front, i.gradient("#ffe991", "#f6b52c"), ink="#3c2400")
    i.line([(9, 23), (37, 23)], "#fff8d0", 1.3)
    return i


def gameboy():
    """SameBoy: a handheld console, slightly turned."""
    i = Icon("gameboy")
    i.path("M 13 7 L 33 4 Q 36 4 36 7 L 37 40 Q 37 43 34 43 L 27 44 Q 15 46 13 42 Z", "#8e8ea0", ink="#26263a")
    i.path("M 11 6 L 31 4 Q 34 4 34 7 L 34 39 Q 34 42 31 42 L 25 43 Q 14 45 11 41 Z",
           i.gradient("#f7f7fa", "#c9c9d4", x2=1, y2=1), ink="#26263a")
    i.path("M 14 9 L 31 8 L 31 23 L 14 24 Z", "#55557a", ink=None)
    i.path("M 16.5 11 L 28.5 10.4 L 28.5 21 L 16.5 21.6 Z", i.gradient("#c4ee6a", "#6fae2a"), ink=None)
    i.line([(14.5, 32), (21.5, 31.6)], "#26263a", 2.6)
    i.line([(18, 28.3), (18, 35.3)], "#26263a", 2.6)
    i.circle(26.5, 32.5, 2.1, "#e8244e", ink="#26263a", width=0.9)
    i.circle(30.8, 29.8, 2.1, "#e8244e", ink="#26263a", width=0.9)
    return i

def settings():
    """Settings: an extruded steel-blue gear lying flat."""
    i = Icon("settings")
    teeth, r_out, r_in, hole = 8, 15.5, 11.5, 5.0
    outline = []
    for k in range(teeth * 4):
        a = 2 * pi * k / (teeth * 4)
        r = r_out if (k % 4) in (1, 2) else r_in
        outline.append((r * cos(a), r * sin(a)))
    depth = 5
    for k in range(len(outline)):
        (ax, ay), (bx, by) = outline[k], outline[(k + 1) % len(outline)]
        mx, my = (ax + bx) / 2, (ay + by) / 2
        # Side faces that face the viewer (toward +x+y).
        if mx + my > -2:
            face = [iso(ax, ay, 0, oy=25), iso(bx, by, 0, oy=25), iso(bx, by, -depth, oy=25),
                    iso(ax, ay, -depth, oy=25)]
            shade = "#3a5f9e" if (bx - ax) * 1 + (by - ay) * -1 > 0 else "#24447a"
            i.poly(face, shade, ink="#0d1a33", width=1.0)
    i.poly([iso(x, y, 0, oy=25) for x, y in outline], i.gradient("#d8e8ff", "#7ea6e6", x2=1), ink="#0d1a33")
    ring = [iso(hole * cos(2 * pi * k / 24), hole * sin(2 * pi * k / 24), 0, oy=25) for k in range(24)]
    i.poly(ring, "#1d3360", ink="#0d1a33")
    return i


def inspector():
    """Config Inspector: a settings page under a magnifying glass."""
    i = Icon("inspector")
    page = [(8, 6), (32, 6), (38, 12), (38, 42), (8, 42)]
    i.poly(page, i.gradient("#ffffff", "#dce4f2"), ink="#1d2440")
    i.poly([(32, 6), (32, 12), (38, 12)], "#b9c6de", ink="#1d2440", width=1.2)
    for n, (k, v) in enumerate(((10, 14), (10, 20), (10, 26), (10, 32))):
        i.line([(12, 13 + n * 6), (18, 13 + n * 6)], "#7b3fd6", 2.2)
        i.line([(21, 13 + n * 6), (31, 13 + n * 6)], "#7a869e", 1.6)
    i.line([(33, 33), (43, 43)], "#2b2b2b", 5.5)
    i.line([(33, 33), (43, 43)], "#e0a030", 3.2)
    i.circle(26, 26, 9, i.radial("#e8fbff", "#7fd0ff"), ink="#11346b", width=2.6)
    i.path("M 21 23 A 6 6 0 0 1 27 20", "none", ink="#ffffff", width=1.6)
    return i


def power():
    """Power: a red button with the power glyph."""
    i = Icon("power")
    i.ellipse(24, 29, 17, 11, "#6e0b0b", ink="#2a0303")
    i.ellipse(24, 25, 17, 11, i.radial("#ff8a7a", "#d4141e", cx=0.4, cy=0.3), ink="#2a0303")
    i.path("M 18.5 21 A 7.5 5 0 1 0 29.5 21", "none", ink="#ffffff", width=2.6)
    i.line([(24, 17), (24, 25)], "#ffffff", 2.6)
    return i


def system():
    """System: a computer tower with a blue status light."""
    i = Icon("system")
    i.box(-6, -12, -12, 12, 24, 26, ("#e2e7f0", "#aeb7c8"), ("#c7cfdd", "#8c96aa"),
          ("#9aa5ba", "#6a7489"), ink="#1a2030", oy=24)
    x0, y1 = -6, 12
    for v in (20, 16):
        i.line([left_face(x0, y1, -12, 4, v), left_face(x0, y1, -12, 20, v)], "#5a6478", 1.4)
    lx, ly = left_face(x0, y1, -12, 17, 5)
    i.circle(lx, ly, 2.0, "#3ab7ff", ink="#0a3e66", width=0.8)
    return i


def development():
    """Development: an orange block carrying </>."""
    i = Icon("development")
    i.box(-12, -9, -9, 24, 18, 16, ("#ffcf7a", "#ff9a1f"), ("#f07f12", "#b4520a"),
          ("#ff9e3a", "#c96510"), ink="#3a1800", oy=26)
    x0, y1 = -12, 9
    L = lambda u, v: left_face(x0, y1, -9, u, v, oy=26)
    i.line([L(8, 12), L(4, 8), L(8, 4)], "#ffffff", 2.4)
    i.line([L(16, 12), L(20, 8), L(16, 4)], "#ffffff", 2.4)
    i.line([L(11, 3), L(13, 13)], "#ffffff", 2.0)
    return i


def web():
    """Web: a blue globe with green continents."""
    i = Icon("web")
    i.circle(24, 24, 18, i.radial("#a8e6ff", "#1667d9", cx=0.35, cy=0.3, r=0.85), ink="#0a2a66")
    land = i.gradient("#7cf08e", "#1fa84a")
    i.path("M 11 15 C 15 10 22 10 24 13 C 25 16 21 17 21 20 C 21 23 25 24 23 28 C 21 32 18 35 16 33 "
           "C 15 29 12 27 10 25 C 8 21 9 18 11 15 Z", land, ink="#0d5a2a", width=1.0)
    i.path("M 29 14 C 33 13 38 17 39 22 C 39 26 36 27 33 26 C 31 28 33 32 31 35 C 28 37 26 33 27 29 "
           "C 27 25 28 22 27 19 C 26 17 27 15 29 14 Z", land, ink="#0d5a2a", width=1.0)
    i.path("M 13 13 A 16 9 0 0 1 35 11", "none", ink="#ffffff", width=1.4)
    i.circle(24, 24, 18, "none", ink="#0a2a66")
    return i

def games():
    """Games: a purple game pad, isometric."""
    i = Icon("games")
    body = "M 9 20 C 9 14 15 13 20 15 L 28 15 C 33 13 39 14 39 20 L 42 32 C 43 37 37 39 34 34 " \
           "L 31 30 L 17 30 L 14 34 C 11 39 5 37 6 32 Z"
    i.path("M 9 23 C 9 17 15 16 20 18 L 28 18 C 33 16 39 17 39 23 L 42 35 C 43 40 37 42 34 37 " \
           "L 31 33 L 17 33 L 14 37 C 11 42 5 40 6 35 Z", "#3a1470", ink="#170530")
    i.path(body, i.gradient("#c79bff", "#7b3fd6"), ink="#170530")
    i.line([(13, 22), (19, 22)], "#ffffff", 2.4)
    i.line([(16, 19), (16, 25)], "#ffffff", 2.4)
    i.circle(31, 23, 1.9, "#3ff0d6", ink="#170530", width=0.8)
    i.circle(35, 20, 1.9, "#ff4d7a", ink="#170530", width=0.8)
    return i


def media():
    """Media: a magenta slab with a play mark and a music note."""
    i = Icon("media")
    i.box(-12, -12, -7, 24, 24, 5, ("#ff8ad8", "#e2289f"), ("#b0127a", "#6e0a4c"),
          ("#d61f92", "#930d63"), ink="#33001f", oy=29)
    tri = [top_face(-12, -12, -2, 5, 8, oy=29), top_face(-12, -12, -2, 5, 20, oy=29),
           top_face(-12, -12, -2, 15, 14, oy=29)]
    i.poly(tri, "#ffffff", ink="#33001f", width=1.2)
    i.path("M 30 20 L 30 5 L 40 3 L 40 8 L 32 9.6 L 32 20 Z", "#1d1030", ink="#1d1030", width=1.0)
    i.ellipse(28.4, 20.2, 3.4, 2.6, "#ffe23d", ink="#1d1030", width=1.2)
    return i

def tools():
    """Tools: a steel wrench crossed with a red screwdriver."""
    i = Icon("tools")
    i.line([(12, 36), (32, 16)], "#1a2030", 7.0)
    i.line([(12, 36), (32, 16)], "#c3cad8", 4.4)
    i.path("M 27 11 A 8 8 0 1 0 37 21 L 33 20 L 30 23 L 25 18 L 28 15 Z",
           i.gradient("#f4f7fb", "#8e98ae", x2=1, y2=1), ink="#1a2030", width=1.8)
    i.circle(12, 36, 3.6, "#c3cad8", ink="#1a2030", width=1.6)
    i.circle(12, 36, 1.4, "#1a2030", ink=None)
    i.line([(16, 15), (25, 24)], "#1a2030", 2.6)
    i.line([(16, 15), (25, 24)], "#e8ecf2", 1.2)
    i.path("M 23 25 L 26 22 L 38 34 Q 41 38 38 41 Q 35 43 32 39 Z", i.gradient("#ff6b5a", "#b80f1c", x2=1, y2=1),
           ink="#2a0303", width=1.6)
    return i

def mesa():
    """Mesa Cube: the classic red-green-blue shaded triangle on a dark tile."""
    i = Icon("mesa")
    i.box(-13, -13, -7, 26, 26, 3, ("#3a3f52", "#1c2030"), ("#4d5470", "#262a3a"),
          ("#3e4560", "#1f2333"), ink="#0b0d14", oy=27)
    top, right, left = (24, 5), (40, 33), (8, 33)
    centre = (24, 24)
    # Each third shades from its corner's colour to white at the centre.
    for corner, nxt, prv, color in ((top, right, left, "#ff2d2d"), (right, left, top, "#2dff4f"),
                                    (left, top, right, "#2d62ff")):
        mid1 = ((corner[0] + nxt[0]) / 2, (corner[1] + nxt[1]) / 2)
        mid2 = ((corner[0] + prv[0]) / 2, (corner[1] + prv[1]) / 2)
        g = i.gradient(color, "#f4f4ff", x1=0, y1=0, x2=1, y2=1) if False else None
        i.ids += 1
        gid = f"g{i.ids}"
        i.defs.append(f'<linearGradient id="{gid}" gradientUnits="userSpaceOnUse" x1="{corner[0]}" y1="{corner[1]}" '
                      f'x2="{centre[0]}" y2="{centre[1]}"><stop offset="0" stop-color="{color}"/>'
                      f'<stop offset="1" stop-color="#ffffff"/></linearGradient>')
        i.poly([corner, mid1, centre, mid2], f"url(#{gid})", ink=None)
    i.poly([top, right, left], "none", ink="#0b0d14")
    return i


def boot():
    """Boot Diagnostics: a clipboard of passed checks."""
    i = Icon("boot")
    i.path("M 9 8 L 37 8 Q 39 8 39 10 L 39 43 Q 39 45 37 45 L 9 45 Q 7 45 7 43 L 7 10 Q 7 8 9 8 Z",
           i.gradient("#c98a4a", "#8a5222"), ink="#2a1606")
    i.poly([(11, 12), (35, 12), (35, 41), (11, 41)], i.gradient("#ffffff", "#e2e8f2"), ink="#2a1606", width=1.2)
    i.path("M 17 5 L 29 5 L 30 11 L 16 11 Z", i.gradient("#e8ecf2", "#9aa4b8"), ink="#2a1606", width=1.4)
    for n in range(3):
        y = 19 + n * 7.5
        i.line([(14, y), (16.5, y + 2.5), (21, y - 2.5)], "#22c55e", 2.4)
        i.line([(24, y), (32, y)], "#7a869e", 1.6)
    return i


ICONS = (cubit_logo, workbench, console, logs, trace, doom, devices, files, gameboy, settings,
         inspector, mesa, boot, power, system, development, web, games, media, tools)


def main():
    (HERE / "svg").mkdir(exist_ok=True)
    (HERE / "png").mkdir(exist_ok=True)
    for make in ICONS:
        icon = make()
        svg = HERE / "svg" / f"{icon.name}.svg"
        svg.write_text(icon.svg())
        for size in SIZES:
            subprocess.run(["rsvg-convert", "-w", str(size), "-h", str(size), "-o",
                            str(HERE / "png" / f"{icon.name}-{size}.png"), str(svg)], check=True)
    print(f"{len(ICONS)} icons -> {HERE / 'svg'} and {HERE / 'png'}")


if __name__ == "__main__":
    main()
