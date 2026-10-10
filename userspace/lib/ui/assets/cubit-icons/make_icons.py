#!/usr/bin/env python3
"""CuBit icon set: the authoring source for the SVGs in this directory.

Every icon is drawn by a small function below with geometry hand-placed for
each size (16, 24, 32, 48). Shared pieces -- the palette, the outline
"underlay", extrusion, isometric boxes, pages -- live here so that every icon
follows the same rules (see README.md). Run with no arguments to rewrite
<size>/<name>.svg for every icon; the SVGs are plain, dependency-free files.

  python3 make_icons.py            # write all SVGs next to this script
  python3 make_icons.py --list     # print icon names

Pure Python 3, no third-party modules.
"""

import math
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
SIZES = (16, 24, 32, 48)

# ---------------------------------------------------------------- palette --
INK = "#1d1530"          # outline: deep violet-black, never pure black
WHITE = "#ffffff"
PAL = {
    #          light      mid        dark       deep
    "violet": ("#c3adff", "#8457ff", "#5a32d6", "#3b1e96"),
    "teal":   ("#8af5e8", "#1fc8c3", "#0e8f99", "#0a5f6b"),
    "amber":  ("#ffdc80", "#ffa41f", "#d9700b", "#9a4a07"),
    "lime":   ("#d8ff85", "#8fdc1f", "#56a00c", "#386b08"),
    "red":    ("#ff9e90", "#f2353f", "#b5172a", "#7c0e1c"),
    "paper":  ("#ffffff", "#ebeef6", "#c6cddd", "#959db3"),
    "steel":  ("#eef1f8", "#b4bccf", "#7d879f", "#4f5870"),
    "slate":  ("#5d5778", "#3a3452", "#2a2540", "#1d1530"),
}
L, M, D, X = 0, 1, 2, 3

# Outline width and extrusion depth per size, in pixels.
OUTLINE = {16: 1, 24: 1, 32: 2, 48: 2}
EXTRUDE = {16: 1, 24: 1, 32: 2, 48: 3}


def col(name, shade):
    return PAL[name][shade]


def fmt(v):
    if isinstance(v, float):
        if abs(v - round(v)) < 1e-6:
            return str(int(round(v)))
        return f"{v:.2f}".rstrip("0").rstrip(".")
    return str(v)


# ----------------------------------------------------------- path helpers --
def poly(pts, close=True):
    d = "M" + " L".join(f"{fmt(x)} {fmt(y)}" for x, y in pts)
    return d + ("Z" if close else "")


def rect(x0, y0, x1, y1):
    return poly([(x0, y0), (x1, y0), (x1, y1), (x0, y1)])


def rrect(x0, y0, x1, y1, r):
    if r <= 0:
        return rect(x0, y0, x1, y1)
    f = fmt
    return (f"M{f(x0 + r)} {f(y0)} H{f(x1 - r)} A{f(r)} {f(r)} 0 0 1 {f(x1)} {f(y0 + r)} "
            f"V{f(y1 - r)} A{f(r)} {f(r)} 0 0 1 {f(x1 - r)} {f(y1)} H{f(x0 + r)} "
            f"A{f(r)} {f(r)} 0 0 1 {f(x0)} {f(y1 - r)} V{f(y0 + r)} A{f(r)} {f(r)} 0 0 1 {f(x0 + r)} {f(y0)}Z")


def circle(cx, cy, r):
    f = fmt
    return (f"M{f(cx - r)} {f(cy)} A{f(r)} {f(r)} 0 1 0 {f(cx + r)} {f(cy)} "
            f"A{f(r)} {f(r)} 0 1 0 {f(cx - r)} {f(cy)}Z")


def ellipse(cx, cy, rx, ry):
    f = fmt
    return (f"M{f(cx - rx)} {f(cy)} A{f(rx)} {f(ry)} 0 1 0 {f(cx + rx)} {f(cy)} "
            f"A{f(rx)} {f(ry)} 0 1 0 {f(cx - rx)} {f(cy)}Z")


def line(*pts):
    return poly(pts, close=False)


def star_pts(cx, cy, ro, ri, n=5, rot=-90):
    pts = []
    for i in range(2 * n):
        r = ro if i % 2 == 0 else ri
        a = math.radians(rot + i * 180 / n)
        pts.append((round(cx + r * math.cos(a), 2), round(cy + r * math.sin(a), 2)))
    return pts


# ----------------------------------------------------------------- canvas --
class Canvas:
    """One icon at one size. Elements go into layers; each layer is drawn as
    an ink underlay (every element stroked 2*outline wide, so the outline
    sits wholly outside the fill edges and stays on the pixel grid) followed
    by the fills. A later layer therefore gets its own outline on top."""

    def __init__(self, s):
        self.s = s
        self.w = OUTLINE[s]
        self.e = EXTRUDE[s]
        self.layers = [[]]
        self.grads = {}

    # Map a 0..48 design coordinate into the drawable box, leaving room for
    # the outline and (unless ext=False) the extrusion. Results are integers.
    def m(self, v, ext=True):
        span = self.s - 2 * self.w - (self.e if ext else 0)
        return self.w + round(v * span / 48)

    def mp(self, pts, ext=True):
        return [(self.m(x, ext), self.m(y, ext)) for x, y in pts]

    def layer(self):
        self.layers.append([])

    def paint(self, fill):
        """'violet' -> family gradient (light to mid), 'violet:2' -> solid
        shade, '#rrggbb' -> solid, 'g:a:b' -> gradient between two hexes."""
        if fill.startswith("#") or fill == "none":
            return fill
        if fill.startswith("g:"):
            _, a, b = fill.split(":")
            key = "g" + a[1:] + b[1:]
            self.grads[key] = (a, b)
            return f"url(#{key})"
        if ":" in fill:
            fam, shade = fill.split(":")
            return col(fam, int(shade))
        a, b = col(fill, L), col(fill, M)
        key = fill
        self.grads[key] = (a, b)
        return f"url(#{key})"

    def shape(self, d, fill, ext=0, side=None, outline=True, rule=None):
        self.layers[-1].append(("shape", d, fill, ext, side, outline, rule))

    def stroke(self, d, color, width, outline=True, cap="round", join="round"):
        self.layers[-1].append(("stroke", d, color, width, outline, cap, join))

    def detail(self, d, fill, opacity=None, rule=None):
        self.layers[-1].append(("detail", d, fill, opacity, rule))

    def hair(self, d, color, width=1, opacity=None, cap="butt"):
        self.layers[-1].append(("hair", d, color, width, opacity, cap))

    # Isometric (2:1) box. (tx, ty) is the top vertex of the top face, a and
    # b the extents along the right-down and left-down axes, h the height,
    # all in pixels. Returns the corner function and the three faces.
    @staticmethod
    def iso(tx, ty, a, b, h):
        def P(x, y, z):
            return (tx + x - y, ty + (x + y) / 2 + (h - z))
        top = [P(0, 0, h), P(a, 0, h), P(a, b, h), P(0, b, h)]
        left = [P(0, b, h), P(a, b, h), P(a, b, 0), P(0, b, 0)]
        right = [P(a, 0, h), P(a, 0, 0), P(a, b, 0), P(a, b, h)]
        sil = [P(0, 0, h), P(a, 0, h), P(a, 0, 0), P(a, b, 0), P(0, b, 0), P(0, b, h)]
        return P, top, left, right, sil

    def iso_box(self, tx, ty, a, b, h, fam_top, fam_left, fam_right, gloss=True):
        P, top, left, right, sil = self.iso(tx, ty, a, b, h)
        self.shape(poly(sil), "none")
        self.detail(poly(top), fam_top)
        self.detail(poly(left), fam_left)
        self.detail(poly(right), fam_right)
        if gloss:
            # specular edge on the front vertical edge and top front edges
            self.hair(line(P(0, b, h), P(a, b, h), P(a, 0, h)), WHITE, 1, 0.55)
        return P

    def svg(self):
        s, w = self.s, self.w
        out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{s}" height="{s}" viewBox="0 0 {s} {s}">']
        body = []
        for lay in self.layers:
            under = []
            fills = []
            for el in lay:
                kind = el[0]
                if kind == "shape":
                    _, d, fill, ext, side, outline, rule = el
                    r = f' fill-rule="{rule}"' if rule else ""
                    if outline:
                        for i in range(ext, -1, -1):
                            t = f' transform="translate({i} {i})"' if i else ""
                            under.append(f'<path d="{d}"{r}{t}/>')
                    for i in range(ext, 0, -1):
                        fills.append(f'<path d="{d}"{r} fill="{self.paint(side)}" transform="translate({i} {i})"/>')
                    if fill != "none":
                        fills.append(f'<path d="{d}"{r} fill="{self.paint(fill)}"/>')
                elif kind == "stroke":
                    _, d, color, width, outline, cap, join = el
                    if outline:
                        under.append(f'<path d="{d}" fill="none" stroke-width="{fmt(width + 2 * w)}" '
                                     f'stroke-linecap="{cap}" stroke-linejoin="{join}"/>')
                    fills.append(f'<path d="{d}" fill="none" stroke="{self.paint(color)}" '
                                 f'stroke-width="{fmt(width)}" stroke-linecap="{cap}" stroke-linejoin="{join}"/>')
                elif kind == "detail":
                    _, d, fill, opacity, rule = el
                    o = f' opacity="{opacity}"' if opacity is not None else ""
                    r = f' fill-rule="{rule}"' if rule else ""
                    fills.append(f'<path d="{d}"{r} fill="{self.paint(fill)}"{o}/>')
                elif kind == "hair":
                    _, d, color, width, opacity, cap = el
                    o = f' opacity="{opacity}"' if opacity is not None else ""
                    fills.append(f'<path d="{d}" fill="none" stroke="{self.paint(color)}" '
                                 f'stroke-width="{fmt(width)}" stroke-linecap="{cap}"{o}/>')
            if under:
                body.append(f'<g fill="{INK}" stroke="{INK}" stroke-width="{2 * w}" stroke-linejoin="round">')
                body += ["  " + u for u in under]
                body.append("</g>")
            body += fills
        if self.grads:
            out.append("<defs>")
            for key, (a, b) in sorted(self.grads.items()):
                out.append(f'<linearGradient id="{key}" x1="0" y1="0" x2=".35" y2="1">'
                           f'<stop offset="0" stop-color="{a}"/><stop offset="1" stop-color="{b}"/></linearGradient>')
            out.append("</defs>")
        out += body
        out.append("</svg>")
        return "\n".join(out) + "\n"


# ============================================================ components ==
def h(v):
    """Pixel centre for a 1-px hairline at integer row/column v."""
    return v + 0.5


def page(c, x0, y0, x1, y1, fold, fill="paper", ext=None):
    """Portrait sheet with a dog-ear at the top right. Returns the fold
    polygon so callers can draw on top."""
    if ext is None:
        ext = c.e
    body = [(x0, y0), (x1 - fold, y0), (x1, y0 + fold), (x1, y1), (x0, y1)]
    c.shape(poly(body), fill, ext=ext, side="paper:3" if fill == "paper" else "slate:3")
    dog = [(x1 - fold, y0), (x1 - fold, y0 + fold), (x1, y0 + fold)]
    c.detail(poly(dog), "paper:2" if fill == "paper" else "slate:0")
    c.hair(line((x1 - fold, y0), (x1 - fold, y0 + fold), (x1, y0 + fold)), INK, 1, 0.45)
    return body


def std_page(c, fill="paper"):
    """The standard page frame for file-type icons, per size."""
    s = c.s
    if s == 16:
        box = (3, 1, 12, 14, 4)
    elif s == 24:
        box = (4, 1, 19, 21, 6)
    elif s == 32:
        box = (5, 2, 25, 27, 8)
    else:
        box = (8, 2, 39, 42, 11)
    page(c, *box, fill=fill)
    return box


def text_lines(c, x0, x1, y0, y1, step, color, short_every=3, width=1):
    rows = []
    y = y0
    i = 0
    while y <= y1:
        xe = x1 - (round((x1 - x0) * 0.35) if i % short_every == short_every - 1 else 0)
        rows.append((y, xe))
        y += step
        i += 1
    for y, xe in rows:
        c.hair(line((x0, y + width / 2), (xe, y + width / 2)), color, width)


def badge_plus(c, cx, cy, r, fam="lime"):
    """A round 'add' badge, its own layer."""
    c.layer()
    c.shape(circle(cx, cy, r), fam)
    t = max(1, round(r * 0.45))
    k = r - max(1, round(r * 0.3))
    c.detail(rect(cx - k, cy - t / 2, cx + k, cy + t / 2), WHITE)
    c.detail(rect(cx - t / 2, cy - k, cx + t / 2, cy + k), WHITE)


def window_frame(c, title="violet"):
    """A small application window (for view-layout actions). Returns the
    client rectangle (x0, y0, x1, y1)."""
    s = c.s
    if s == 16:
        x0, y0, x1, y1, tb = 1, 2, 14, 13, 3
    elif s == 24:
        x0, y0, x1, y1, tb = 1, 3, 22, 20, 4
    elif s == 32:
        x0, y0, x1, y1, tb = 2, 4, 28, 26, 5
    else:
        x0, y0, x1, y1, tb = 2, 6, 43, 40, 7
    c.shape(rect(x0, y0, x1, y1), "paper", ext=c.e, side="steel:2")
    c.detail(rect(x0, y0, x1, y0 + tb), title)
    c.hair(line((x0, y0 + tb + 0.5), (x1, y0 + tb + 0.5)), INK, 1, 0.6)
    return x0, y0 + tb + 1, x1, y1


# ================================================================ places ==
def folder_geom(c):
    s = c.s
    if s == 16:
        return dict(x0=1, x1=14, top=3, tab=6, slope=1, back=4, front=6, y1=13)
    if s == 24:
        return dict(x0=1, x1=22, top=3, tab=9, slope=2, back=5, front=9, y1=20)
    if s == 32:
        return dict(x0=2, x1=28, top=4, tab=12, slope=2, back=7, front=12, y1=27)
    return dict(x0=2, x1=43, top=5, tab=18, slope=3, back=10, front=17, y1=40)


def folder_back(c, g):
    x0, x1 = g["x0"], g["x1"]
    back = [(x0, g["top"]), (g["tab"], g["top"]), (g["tab"] + g["slope"], g["back"]),
            (x1, g["back"]), (x1, g["y1"]), (x0, g["y1"])]
    c.shape(poly(back), "violet:2", ext=c.e, side="violet:3")
    return back


def folder(c, emblem=None):
    g = folder_geom(c)
    folder_back(c, g)
    x0, x1, y1, fy = g["x0"], g["x1"], g["y1"], g["front"]
    c.detail(rect(x0, fy, x1, y1), "g:#b79cff:#7a4cff")
    c.hair(line((x0, fy + 0.5 - 1), (x1, fy + 0.5 - 1)), INK, 1, 0.7)
    c.hair(line((x0 + 1, fy + 0.5), (x1 - 1, fy + 0.5)), WHITE, 1, 0.65)
    if emblem:
        emblem(c, g)


def folder_open(c):
    g = folder_geom(c)
    s = c.s
    x0, x1, y1 = g["x0"], g["x1"], g["y1"]
    inset = {16: 1, 24: 2, 32: 2, 48: 3}[s]
    back = [(x0 + inset, g["top"]), (g["tab"], g["top"]), (g["tab"] + g["slope"], g["back"]),
            (x1 - inset, g["back"]), (x1 - inset, y1), (x0 + inset, y1)]
    c.shape(poly(back), "violet:2", ext=c.e, side="violet:3")
    # a sheet of paper standing in the folder
    px0, px1 = x0 + inset + {16: 2, 24: 3, 32: 4, 48: 6}[s], x1 - inset - {16: 1, 24: 2, 32: 3, 48: 4}[s]
    py = g["back"] - {16: 2, 24: 2, 32: 3, 48: 4}[s]
    c.layer()
    c.shape(rect(px0, py, px1, y1 - 2), "paper")
    if s >= 24:
        n = {24: 2, 32: 3, 48: 4}[s]
        step = {24: 2, 32: 3, 48: 4}[s]
        for i in range(n):
            yy = py + step * (i + 1)
            c.hair(line((px0 + step, yy + 0.5), (px1 - step, yy + 0.5)), "paper:3", 1)
    # front flap, tipped toward the viewer: wider at the top
    c.layer()
    fy = g["front"] + {16: 2, 24: 2, 32: 3, 48: 4}[s]
    front = [(x0, fy), (x1 + (1 if s == 16 else 0), fy), (x1 - inset, y1), (x0 + inset, y1)]
    if s == 16:
        front = [(0.5, fy), (14.5, fy), (14, y1), (1, y1)]
    c.shape(poly(front), "g:#cdb9ff:#8a5eff", ext=c.e, side="violet:3")
    c.hair(line((x0 + 1, fy + 0.5), (x1 - 1, fy + 0.5)), WHITE, 1, 0.7)


def home(c):
    """Isometric house, front gable: teal walls, amber roof, violet door."""
    s = c.s
    # ground origin (back corner), x extent a, depth b, wall height, roof rise
    ox, oy, a, b, hw, rh = {16: (7, 8, 8, 6, 5, 4), 24: (11, 12, 12, 10, 8, 6),
                            32: (14, 16, 16, 12, 10, 8), 48: (22, 24, 24, 20, 16, 12)}[s]

    def P(x, y, z):
        return (ox + x - y, oy + (x + y) / 2 - z)
    top = hw + rh
    apex_f, apex_b = P(a / 2, b, top), P(a / 2, 0, top)
    front = [P(0, b, 0), P(a, b, 0), P(a, b, hw), apex_f, P(0, b, hw)]
    side = [P(a, b, 0), P(a, 0, 0), P(a, 0, hw), P(a, b, hw)]
    roof = [apex_f, apex_b, P(a, 0, hw), P(a, b, hw)]
    c.shape(poly([P(0, b, 0), P(0, b, hw), apex_f, apex_b, P(a, 0, hw), P(a, 0, 0), P(a, b, 0)]), "none")
    c.detail(poly(front), "g:#9ff8ec:#1fc8c3")
    c.detail(poly(side), "teal:2")
    c.detail(poly(roof), "g:#ffe08f:#ff9b16")
    c.hair(line(P(0, b, hw), apex_f, P(a, b, hw)), INK, 1, 0.5)
    if s >= 24:
        c.hair(line(apex_f, apex_b), WHITE, 1, 0.7)
    dw = {16: 2, 24: 4, 32: 4, 48: 6}[s]
    dh = {16: 3, 24: 5, 32: 6, 48: 9}[s]
    x0 = a / 2 - dw / 2
    door = [P(x0, b, 0), P(x0 + dw, b, 0), P(x0 + dw, b, dh), P(x0, b, dh)]
    c.detail(poly(door), "violet:3")
    if s >= 24:
        ww = {24: 4, 32: 4, 48: 6}[s]
        wz = {24: 3, 32: 4, 48: 6}[s]
        wy = b / 2 - ww / 2
        win = [P(a, wy, wz), P(a, wy + ww, wz), P(a, wy + ww, wz + ww * 0.75), P(a, wy, wz + ww * 0.75)]
        c.detail(poly(win), "amber:0")


def computer(c):
    """Monitor with a CuBit cube on screen, on a short stand."""
    s = c.s
    if s == 16:
        mx0, my0, mx1, my1, bez = 1, 1, 13, 10, 1
        st = (6, 10, 8, 12)
        base = (3, 12, 11, 13)
    elif s == 24:
        mx0, my0, mx1, my1, bez = 1, 2, 21, 15, 2
        st = (9, 15, 13, 18)
        base = (5, 18, 17, 20)
    elif s == 32:
        mx0, my0, mx1, my1, bez = 2, 3, 27, 20, 2
        st = (12, 20, 17, 24)
        base = (7, 24, 22, 27)
    else:
        mx0, my0, mx1, my1, bez = 2, 4, 41, 30, 3
        st = (18, 30, 25, 36)
        base = (10, 36, 33, 40)
    c.shape(rect(*base), "steel", ext=c.e, side="steel:3")
    c.shape(rect(*st), "steel:2")
    c.layer()
    c.shape(rect(mx0, my0, mx1, my1), "steel", ext=c.e, side="steel:3")
    sx0, sy0, sx1, sy1 = mx0 + bez, my0 + bez, mx1 - bez, my1 - bez
    c.detail(rect(sx0, sy0, sx1, sy1), "g:#5a2fd0:#0f8f9c")
    c.hair(line((sx0, sy1 + 0.5), (sx1, sy1 + 0.5)), WHITE, 1, 0.4) if s >= 24 else None
    # tiny cube on the screen
    cx = (sx0 + sx1) / 2
    a = {16: 2, 24: 4, 32: 5, 48: 8}[s]
    hh = {16: 2, 24: 4, 32: 5, 48: 8}[s]
    ty = (sy0 + sy1) / 2 - (a + hh) / 2
    if s == 16:
        ty = 3
    P, top, left, right, sil = Canvas.iso(cx, ty, a, a, hh)
    c.detail(poly(top), "violet:0")
    c.detail(poly(left), "teal:0")
    c.detail(poly(right), "teal:1")


def drive(c):
    """Isometric slab: steel case, a dark slot and a lime activity LED."""
    s = c.s
    tx, ty, a, b, hh = {16: (10, 3, 4, 8, 4), 24: (15, 5, 6, 12, 6),
                        32: (19, 7, 8, 16, 7), 48: (29, 10, 12, 24, 11)}[s]
    P = c.iso_box(tx, ty, a, b, hh, "steel", "steel", "steel:2")
    # slot and LED on the long (left-front) face, which spans x along y=b
    sl = {16: 1, 24: 1, 32: 2, 48: 2}[s]
    z = hh / 2 + (0 if s == 16 else 0)
    p0, p1 = P(a * 0.15, b, z), P(a * 0.15, b, z)
    # the long face is the y=b face whose width runs along x: use the other face
    # (x=a face is right). Put details on the left face along its length.
    q0 = P(0 + 1, b, z)
    q1 = P(a - (2 if s == 16 else 3), b, z)
    _ = p0, p1
    # left face runs from P(0,b,*) to P(a,b,*); it's the short one, so we instead
    # place the slot on the right face (x=a), the long side.
    r0 = P(a, b * 0.12, z)
    r1 = P(a, b * 0.62, z)
    c.hair(line(r0, r1), INK, sl, 0.85)
    led = P(a, b * 0.85, z)
    lr = {16: 0.9, 24: 1.2, 32: 1.6, 48: 2.2}[s]
    c.detail(ellipse(led[0], led[1], lr, lr), "lime:0" if s == 16 else "lime")
    _ = q0, q1


def removable(c):
    """A USB stick standing up: steel plug on top, amber body."""
    s = c.s
    if s == 16:
        plug = (5, 1, 10, 5)
        body = (4, 5, 11, 14)
        holes = [(6, 2, 7, 3), (8, 2, 9, 3)]
        r = 1
    elif s == 24:
        plug = (8, 1, 15, 7)
        body = (6, 7, 17, 21)
        holes = [(9, 3, 11, 5), (12, 3, 14, 5)]
        r = 2
    elif s == 32:
        plug = (10, 2, 19, 9)
        body = (8, 9, 21, 27)
        holes = [(12, 4, 14, 6), (15, 4, 17, 6)]
        r = 3
    else:
        plug = (16, 2, 29, 13)
        body = (12, 13, 33, 41)
        holes = [(18, 6, 21, 9), (24, 6, 27, 9)]
        r = 4
    c.shape(rect(*plug), "steel", ext=c.e, side="steel:3")
    for hx in holes:
        c.detail(rect(*hx), INK)
    c.layer()
    c.shape(rrect(*body, r), "amber", ext=c.e, side="amber:3")
    bx0, by0, bx1, by1 = body
    # label stripe with an arrow-ish trident hint: a lime LED
    if s >= 24:
        c.detail(rect(bx0 + 2, by0 + (by1 - by0) // 3, bx1 - 2, by0 + (by1 - by0) // 3 + max(2, s // 12)), "amber:2")
    lr = {16: 1, 24: 1.2, 32: 1.6, 48: 2.2}[s]
    if s == 16:
        c.detail(rect(7, 11, 8, 12), "lime:0")
    else:
        c.detail(circle((bx0 + bx1) / 2, by1 - r - lr, lr), "lime:0")
    c.hair(line((bx0 + 1.5, by0 + 1), (bx0 + 1.5, by1 - r)), WHITE, 1, 0.5)


def network(c):
    """Three linked isometric cubes."""
    s = c.s
    a = {16: 2, 24: 3, 32: 4, 48: 6}[s]
    hh = {16: 3, 24: 4, 32: 5, 48: 8}[s]
    if s == 16:
        nodes = [(8, 1), (3, 8), (13, 8)]
    elif s == 24:
        nodes = [(12, 1), (5, 12), (19, 12)]
    elif s == 32:
        nodes = [(16, 2), (7, 16), (25, 16)]
    else:
        nodes = [(24, 2), (10, 24), (38, 24)]
    centers = [(x, y + (a + hh) / 2) for x, y in nodes]
    lw = {16: 1, 24: 1, 32: 2, 48: 2}[s]
    for i, j in ((0, 1), (0, 2), (1, 2)):
        c.stroke(line(centers[i], centers[j]), "steel:2", lw)
    for x, y in nodes:
        c.layer()
        c.iso_box(x, y, a, a, hh, "teal:0", "teal", "teal:2", gloss=s >= 32)


def trash_geom(c):
    s = c.s
    if s == 16:
        return dict(lid=(2, 3, 13, 5), knob=(6, 1, 9, 3), top=5, bot=14, tx0=3, tx1=12, bx0=4, bx1=11, ribs=[6, 9])
    if s == 24:
        return dict(lid=(3, 4, 20, 7), knob=(9, 2, 14, 4), top=7, bot=21, tx0=4, tx1=19, bx0=6, bx1=17, ribs=[8, 11.5, 15])
    if s == 32:
        return dict(lid=(4, 5, 26, 9), knob=(12, 2, 18, 5), top=9, bot=28, tx0=6, tx1=24, bx0=8, bx1=22, ribs=[11, 15, 19])
    return dict(lid=(6, 8, 40, 13), knob=(18, 3, 28, 8), top=13, bot=42, tx0=9, tx1=37, bx0=12, bx1=34, ribs=[16, 23, 30])


def trash(c, full=False):
    g = trash_geom(c)
    s = c.s
    body = [(g["tx0"], g["top"]), (g["tx1"], g["top"]), (g["bx1"], g["bot"]), (g["bx0"], g["bot"])]
    c.shape(poly(body), "teal", ext=c.e, side="teal:3")
    rw = {16: 1, 24: 1, 32: 2, 48: 2}[s]
    for rx in g["ribs"]:
        frac = (rx - g["tx0"]) / (g["tx1"] - g["tx0"])
        xb = g["bx0"] + frac * (g["bx1"] - g["bx0"])
        c.hair(line((rx, g["top"] + 2), (xb, g["bot"] - 2)), "teal:3", rw, 0.75)
    c.hair(line((g["tx0"] + 1.5, g["top"] + 1), (g["bx0"] + 1.5, g["bot"] - 1)), WHITE, 1, 0.45)
    if full:
        c.layer()
        # crumpled paper and a sheet poking out, lid lifted
        lx0, ly0, lx1, ly1 = g["lid"]
        if s == 16:
            c.shape(poly([(4, 5), (5, 2), (8, 1), (9, 3), (11, 2), (12, 5)]), "paper")
        else:
            k = s / 48
            pts = [(g["tx0"] + 1, g["top"]), (g["tx0"] + 3 * k, g["top"] - 10 * k), (g["tx0"] + 11 * k, g["top"] - 12 * k),
                   (g["tx0"] + 15 * k, g["top"] - 7 * k), (g["tx1"] - 9 * k, g["top"] - 11 * k),
                   (g["tx1"] - 2 * k, g["top"] - 6 * k), (g["tx1"] - 1, g["top"])]
            c.shape(poly([(round(x), round(y)) for x, y in pts]), "paper")
            c.detail(poly([(round(x), round(y)) for x, y in
                           [(g["tx0"] + 12 * k, g["top"]), (g["tx0"] + 15 * k, g["top"] - 7 * k), (g["tx1"] - 9 * k, g["top"])]]), "paper:2")
        c.layer()
        # lid resting tilted on the right
        if s == 16:
            lid = [(9, 6), (14, 2), (15, 3), (10, 7)]
            c.shape(poly([(2, 5), (13, 5), (13, 6), (2, 6)]), "teal:2")
        else:
            lid = None
        if lid:
            pass
        else:
            c.shape(rect(lx0, g["top"] - max(1, round(s / 16)), lx1, g["top"]), "teal:2")
        return
    c.layer()
    c.shape(rect(*g["knob"]), "teal:2")
    c.layer()
    c.shape(rect(*g["lid"]), "teal", ext=0)
    lx0, ly0, lx1, ly1 = g["lid"]
    c.hair(line((lx0 + 1, ly0 + 0.5), (lx1 - 1, ly0 + 0.5)), WHITE, 1, 0.6)


def trash_full(c):
    trash(c, full=True)


def star(c):
    s = c.s
    cx = (s - c.e) / 2
    cy = cx + {16: 0.5, 24: 0.5, 32: 1, 48: 1.5}[s]
    ro = {16: 6.8, 24: 10.5, 32: 13.5, 48: 21}[s]
    ri = ro * 0.47
    c.shape(poly(star_pts(cx, cy, ro, ri)), "g:#ffe58f:#ff9a12", ext=c.e, side="amber:3")
    if s >= 24:
        # facets: light top-left arms
        pts = star_pts(cx, cy, ro, ri)
        for i in (0, 8):
            c.detail(poly([(cx, cy), pts[i], pts[(i + 1) % 10]]), WHITE, 0.35)
        for i in (3, 4, 5):
            c.detail(poly([(cx, cy), pts[i], pts[(i + 1) % 10]]), "amber:2", 0.45)


def clock(c):
    s = c.s
    cx = (s - c.e) / 2
    cy = cx
    r = cx - c.w
    rim = {16: 2, 24: 2, 32: 3, 48: 5}[s]
    c.shape(circle(cx, cy, r), "violet", ext=c.e, side="violet:3")
    c.detail(circle(cx, cy, r - rim), "paper")
    if s >= 24:
        for i in range(12 if s >= 32 else 4):
            a = math.radians(i * (30 if s >= 32 else 90))
            r0 = r - rim - (1.5 if s < 48 else 2.5)
            r1 = r - rim - 0.5
            c.hair(line((cx + r0 * math.cos(a), cy + r0 * math.sin(a)),
                        (cx + r1 * math.cos(a), cy + r1 * math.sin(a))), "paper:3", 1 if s < 48 else 1.5)
    hw = {16: 1, 24: 1.5, 32: 2, 48: 3}[s]
    # hands: 10 past (short) and 12 (long)
    lh = r - rim - {16: 1, 24: 2, 32: 2.5, 48: 4}[s]
    sh = lh * 0.65
    if s == 16:
        c.hair(line((7.5, 3), (7.5, 7.5), (10, 7.5)), INK, 1, cap="square")
        return
    c.hair(line((cx, cy), (cx, cy - lh)), INK, hw, cap="round")
    c.hair(line((cx, cy), (cx + sh * 0.87, cy + sh * 0.5 * -1 * -1)), "violet:3", hw, cap="round")
    c.detail(circle(cx, cy, hw), "amber:2")


# ============================================================ file types ==
def file_generic(c):
    std_page(c)


def document(c):
    x0, y0, x1, y1, fold = std_page(c)
    s = c.s
    if s == 16:
        for y in (5, 7, 9, 11):
            c.hair(line((5, h(y)), ((10 if y != 11 else 8), h(y))), "violet:2", 1)
        c.hair(line((5, h(3)), (8, h(3))), "violet:3", 1)
        return
    pad = {24: 3, 32: 4, 48: 6}[s]
    step = {24: 3, 32: 3, 48: 5}[s]
    lw = {24: 1, 32: 1, 48: 2}[s]
    c.detail(rect(x0 + pad, y0 + pad, x1 - fold - 1, y0 + pad + lw + (1 if s > 24 else 0)), "violet:3")
    text_lines(c, x0 + pad, x1 - pad, y0 + pad + step + 2, y1 - pad - 1, step, "violet:2", 4, lw)


def image(c):
    """Landscape photo: teal sky, amber sun, lime hills."""
    s = c.s
    if s == 16:
        x0, y0, x1, y1, m = 1, 3, 14, 13, 1
    elif s == 24:
        x0, y0, x1, y1, m = 1, 4, 22, 19, 2
    elif s == 32:
        x0, y0, x1, y1, m = 2, 5, 28, 25, 2
    else:
        x0, y0, x1, y1, m = 2, 8, 43, 38, 3
    c.shape(rect(x0, y0, x1, y1), "paper", ext=c.e, side="paper:3")
    ix0, iy0, ix1, iy1 = x0 + m, y0 + m, x1 - m, y1 - m
    c.detail(rect(ix0, iy0, ix1, iy1), "g:#9ff7ff:#1fb6d6")
    w, hgt = ix1 - ix0, iy1 - iy0
    sr = max(1.2, w * 0.12)
    c.detail(circle(ix0 + w * 0.72, iy0 + hgt * 0.3, sr), "amber" if s >= 24 else "amber:1")
    hill = [(ix0, iy1), (ix0, iy0 + hgt * 0.7), (ix0 + w * 0.32, iy0 + hgt * 0.38),
            (ix0 + w * 0.6, iy0 + hgt * 0.75), (ix0 + w * 0.75, iy0 + hgt * 0.6), (ix1, iy0 + hgt * 0.85), (ix1, iy1)]
    c.detail(poly([(round(x), round(y)) for x, y in hill]), "g:#c8ff6a:#4e9a06")


def audio(c):
    """Beamed pair of eighth notes, violet with highlights."""
    s = c.s
    k = (s - c.e) / 48
    if s == 16:
        c.shape(poly([(5, 3), (13, 1), (13, 11), (12, 11), (12, 4), (6, 6), (6, 13), (5, 13)]), "violet:2", ext=1, side="violet:3")
        c.shape(ellipse(4, 12, 2.6, 2), "violet", ext=1, side="violet:3")
        c.shape(ellipse(11, 10.5, 2.6, 2), "violet", ext=1, side="violet:3")
        return
    def P(x, y):
        return (round(c.w + x * k * 1.0), round(c.w + y * k))
    stem = 4 * k
    beam = [P(15, 8), P(42, 1), P(42, 9), P(15, 16)]
    c.shape(poly(beam), "violet:2", ext=c.e, side="violet:3")
    sw = max(2, round(stem))
    lx, rx = P(15, 0)[0], P(42, 0)[0]
    c.shape(rect(lx, P(0, 12)[1], lx + sw, P(0, 37)[1]), "violet:2", ext=c.e, side="violet:3")
    c.shape(rect(rx - sw, P(0, 5)[1], rx, P(0, 31)[1]), "violet:2", ext=c.e, side="violet:3")
    c.layer()
    for (hx, hy) in ((lx + sw - 6.5 * k, P(0, 38)[1]), (rx - 6.5 * k, P(0, 32)[1])):
        d = ellipse(hx, hy, 7.5 * k, 5.5 * k)
        c.shape(d, "violet", ext=c.e, side="violet:3")
        c.detail(ellipse(hx - 2.5 * k, hy - 2 * k, 3 * k, 1.6 * k), WHITE, 0.55)


def video(c):
    """Clapperboard: striped clapper over a slate body with a teal play mark."""
    s = c.s
    if s == 16:
        body = (1, 6, 14, 14)
        clap = [(1, 2), (13, 1), (13.6, 4), (1, 5)]
    elif s == 24:
        body = (1, 9, 22, 21)
        clap = [(1, 3), (20, 1), (21, 6), (1, 8)]
    elif s == 32:
        body = (2, 12, 28, 28)
        clap = [(2, 5), (26, 2), (27, 8), (2, 11)]
    else:
        body = (2, 18, 43, 42)
        clap = [(2, 7), (39, 2), (41, 11), (2, 16)]
    c.shape(rect(*body), "slate", ext=c.e, side="slate:3")
    bx0, by0, bx1, by1 = body
    # stripes on the top band of the body
    sh = {16: 2, 24: 3, 32: 4, 48: 5}[s]
    sw = {16: 3, 24: 4, 32: 5, 48: 8}[s]
    i = 0
    x = bx0
    while x < bx1:
        if i % 2 == 0:
            c.detail(poly([(x, by0 + sh), (min(x + sw, bx1), by0 + sh), (min(x + sw + sh, bx1), by0), (min(x + sh, bx1), by0)]), WHITE)
        x += sw
        i += 1
    # play triangle
    cx, cy = (bx0 + bx1) / 2, (by0 + sh + by1) / 2
    t = {16: 2, 24: 3, 32: 4, 48: 6}[s]
    c.detail(poly([(cx - t * 0.8, cy - t), (cx + t, cy), (cx - t * 0.8, cy + t)]), "teal:0")
    c.layer()
    c.shape(poly(clap), "slate", ext=c.e, side="slate:3")
    # clapper stripes, amber
    (ax, ay), (bx, by), (cx2, cy2), (dx, dy) = clap
    n = {16: 3, 24: 4, 32: 4, 48: 5}[s]
    for j in range(n):
        t0 = (j + 0.25) / n
        t1 = (j + 0.75) / n
        top0 = (ax + (bx - ax) * t0, ay + (by - ay) * t0)
        top1 = (ax + (bx - ax) * t1, ay + (by - ay) * t1)
        bot0 = (dx + (cx2 - dx) * (t0 - 0.12), dy + (cy2 - dy) * (t0 - 0.12))
        bot1 = (dx + (cx2 - dx) * (t1 - 0.12), dy + (cy2 - dy) * (t1 - 0.12))
        c.detail(poly([top0, top1, bot1, bot0]), "amber")


def archive(c):
    """Cardboard box (isometric) with a violet strap."""
    s = c.s
    tx, ty, a, hh = {16: (8, 1, 6, 6), 24: (12, 1, 10, 10), 32: (16, 2, 13, 12), 48: (24, 2, 20, 20)}[s]
    P = c.iso_box(tx, ty, a, a, hh, "amber:0", "amber", "amber:2")
    # strap across top and down both faces at mid-x
    sw = {16: 1, 24: 2, 32: 3, 48: 4}[s]
    m0, m1 = a / 2 - sw / 2, a / 2 + sw / 2
    c.detail(poly([P(m0, 0, hh), P(m1, 0, hh), P(m1, a, hh), P(m0, a, hh)]), "violet")
    c.detail(poly([P(m0, a, hh), P(m1, a, hh), P(m1, a, 0), P(m0, a, 0)]), "violet:1")
    # tape seam on top along y
    if s >= 24:
        n0, n1 = a / 2 - sw / 2, a / 2 + sw / 2
        c.detail(poly([P(0, n0, hh), P(a, n0, hh), P(a, n1, hh), P(0, n1, hh)]), "amber:1", 0.6)
        c.detail(poly([P(a, n0, hh), P(a, n1, hh), P(a, n1, 0), P(a, n0, 0)]), "violet:2")


def executable(c):
    """An app: a CuBit cube in front of the window it opens."""
    s = c.s
    wx0, wy0, wx1, wy1, tb = {16: (6, 1, 14, 10, 2), 24: (8, 1, 22, 15, 3),
                              32: (10, 2, 28, 20, 4), 48: (15, 2, 43, 30, 6)}[s]
    c.shape(rect(wx0, wy0, wx1, wy1), "paper", ext=c.e, side="steel:2")
    c.detail(rect(wx0, wy0, wx1, wy0 + tb), "violet")
    if s >= 24:
        c.hair(line((wx0, wy0 + tb + 0.5), (wx1, wy0 + tb + 0.5)), INK, 1, 0.6)
        bw = {24: 2, 32: 3, 48: 4}[s]
        c.detail(rect(wx1 - 2 - 3 * bw, wy0 + tb + 3, wx1 - 2, wy0 + tb + 3 + bw), "teal:0")
        if s >= 32:
            c.detail(rect(wx1 - 2 - 3 * bw, wy0 + tb + 5 + bw, wx1 - 2 - bw, wy0 + tb + 5 + 2 * bw), "amber:0")
    c.layer()
    # cube in front, lower left; at 16 px flat shades (no gradients, no gloss)
    tx, ty, a, hh = {16: (5, 6, 4, 5), 24: (8, 9, 6, 7), 32: (10, 12, 8, 9), 48: (15, 18, 12, 14)}[s]
    if s == 16:
        c.iso_box(tx, ty, a, a, hh, "violet:0", "teal:1", "violet:1", gloss=False)
    else:
        c.iso_box(tx, ty, a, a, hh, "g:#e6dcff:#b49bff", "g:#7ff3e4:#16b5b3", "g:#8a5cff:#4e28c4", gloss=s >= 32)


def source(c):
    x0, y0, x1, y1, fold = std_page(c)
    s = c.s
    if s == 16:
        c.hair(line((6.5, 6), (4.5, 8.5), (6.5, 11)), "teal:3", 1.4, cap="square")
        c.hair(line((9.5, 6), (11.5, 8.5), (9.5, 11)), "teal:3", 1.4, cap="square")
        return
    cx = (x0 + x1) / 2
    cy = (y0 + fold + y1) / 2
    hgt = {24: 5, 32: 6, 48: 9}[s]
    wdt = {24: 3, 32: 4, 48: 6}[s]
    gap = {24: 2, 32: 2.5, 48: 4}[s]
    lw = {24: 2, 32: 2.5, 48: 3.5}[s]
    c.stroke(line((cx - gap, cy - hgt), (cx - gap - wdt, cy), (cx - gap, cy + hgt)), "teal", lw, outline=False)
    c.stroke(line((cx + gap, cy - hgt), (cx + gap + wdt, cy), (cx + gap, cy + hgt)), "teal", lw, outline=False)
    c.hair(line((cx - gap, cy - hgt), (cx - gap - wdt, cy), (cx - gap, cy + hgt)), "teal:3", lw * 0.4)
    c.hair(line((cx + gap, cy - hgt), (cx + gap + wdt, cy), (cx + gap, cy + hgt)), "teal:3", lw * 0.4)


def ccl_script(c):
    """Dark console card with a CuBit cube and a lime prompt."""
    x0, y0, x1, y1, fold = std_page(c, fill="slate")
    s = c.s
    a = {16: 2, 24: 3, 32: 4, 48: 6}[s]
    hh = a
    cx = x0 + a + {16: 1, 24: 2, 32: 2, 48: 3}[s]
    ty = y0 + {16: 1, 24: 2, 32: 3, 48: 4}[s]
    P, top, left, right, sil = Canvas.iso(cx, ty, a, a, hh)
    c.detail(poly(top), "violet:0")
    c.detail(poly(left), "teal:1")
    c.detail(poly(right), "violet:1")
    if s == 16:
        c.hair(line((5, 8), (7, 9.5), (5, 11)), "lime:0", 1.2, cap="round")
        c.hair(line((8, h(11)), (10, h(11))), "lime:0", 1)
        return
    py = (y0 + y1) / 2 + {24: 1, 32: 2, 48: 3}[s]
    px = x0 + {24: 3, 32: 4, 48: 6}[s]
    t = {24: 3, 32: 4, 48: 6}[s]
    lw = {24: 1.5, 32: 2, 48: 3}[s]
    c.hair(line((px, py - t), (px + t, py), (px, py + t)), "lime:0", lw, cap="round")
    c.hair(line((px + t + 2, py + t - lw / 2), (x1 - {24: 3, 32: 4, 48: 6}[s], py + t - lw / 2)), "lime:0", lw)


def book(c):
    """Closed book in three-quarter view: teal cover, paper block, red ribbon."""
    s = c.s
    if s == 16:
        cover = (2, 1, 12, 13)
        pages_d = 2
        spine = 2
    elif s == 24:
        cover = (3, 1, 18, 19)
        pages_d = 3
        spine = 3
    elif s == 32:
        cover = (4, 2, 24, 25)
        pages_d = 4
        spine = 4
    else:
        cover = (6, 2, 36, 38)
        pages_d = 6
        spine = 6
    x0, y0, x1, y1 = cover
    # page block peeking out bottom-right
    blk = [(x0 + 1, y1), (x1, y1), (x1 + pages_d, y1 - pages_d / 2 + 0 * pages_d), (x1 + pages_d, y0 + pages_d),
           (x1, y0), (x1, y1)]
    blk = [(x0 + pages_d, y1), (x1, y1), (x1 + pages_d - 1, y1 + 0), (x1 + pages_d - 1, y0 + pages_d)]
    c.shape(poly([(x1 - 1, y0 + pages_d), (x1 + pages_d - 1, y0 + pages_d), (x1 + pages_d - 1, y1 + pages_d - 1),
                  (x0 + spine, y1 + pages_d - 1), (x0 + spine, y1)]), "paper")
    for i in range(1, pages_d // 2 + 1) if s >= 32 else []:
        yy = y1 + i * 2 - 0.5
        c.hair(line((x0 + spine + 1, yy), (x1 + pages_d - 2, yy)), "paper:3", 1)
    c.layer()
    c.shape(rect(x0, y0, x1, y1), "teal", ext=0)
    c.detail(rect(x0, y0, x0 + spine, y1), "teal:2")
    c.hair(line((x0 + spine + 0.5, y0), (x0 + spine + 0.5, y1)), INK, 1, 0.4)
    # title plate
    if s >= 24:
        tp = {24: 3, 32: 4, 48: 6}[s]
        c.detail(rect(x0 + spine + tp - 1, y0 + tp, x1 - tp + 1, y0 + tp * 2 + 1), "teal:0", 0.9)
    # ribbon
    rx = x1 - {16: 3, 24: 4, 32: 6, 48: 9}[s]
    rw = {16: 2, 24: 2, 32: 3, 48: 4}[s]
    rl = {16: 3, 24: 4, 32: 5, 48: 8}[s]
    c.layer()
    c.shape(poly([(rx, y0), (rx + rw, y0), (rx + rw, y0 + rl + rw / 2), (rx + rw / 2, y0 + rl), (rx, y0 + rl + rw / 2)]), "red")


def stadium(p, q, r):
    """Closed outline of a pill around segment p-q with radius r."""
    ux, uy = q[0] - p[0], q[1] - p[1]
    n = math.hypot(ux, uy)
    nx, ny = -uy / n * r, ux / n * r
    f = lambda v: fmt(round(v, 2))
    return (f"M{f(p[0] + nx)} {f(p[1] + ny)} L{f(q[0] + nx)} {f(q[1] + ny)} "
            f"A{f(r)} {f(r)} 0 0 0 {f(q[0] - nx)} {f(q[1] - ny)} L{f(p[0] - nx)} {f(p[1] - ny)} "
            f"A{f(r)} {f(r)} 0 0 0 {f(p[0] + nx)} {f(p[1] + ny)}Z")


def link(c):
    """Two interlocked chain links on a 45-degree diagonal."""
    s = c.s
    if s == 16:
        A, B, r, lw = ((3.5, 11.5), (6, 9)), ((9, 6), (11.5, 3.5)), 2.3, 1.6
    else:
        k = (s - c.e - 2 * c.w) / 48
        o = c.w
        P = lambda x, y: (o + x * k, o + y * k)
        A, B = (P(9, 39), P(21, 27)), (P(27, 21), P(39, 9))
        r = 8 * k
        lw = {24: 3, 32: 4, 48: 5}[s]
    c.stroke(stadium(*A, r), "teal:1", lw)
    c.layer()
    c.stroke(stadium(*B, r), "teal", lw)
    if s >= 24:
        c.hair(stadium(*B, r), "teal:0", max(1, lw / 3))


def locked(c):
    """Padlock: steel shackle, amber body, ink keyhole."""
    s = c.s
    if s == 16:
        sh = (4.5, 1.5, 10.5, 9)
        body = (2, 7, 13, 14)
        sw = 2
    elif s == 24:
        sh = (6.5, 2, 15.5, 12)
        body = (3, 10, 19, 21)
        sw = 2.5
    elif s == 32:
        sh = (9, 3, 20, 15)
        body = (4, 13, 25, 28)
        sw = 3
    else:
        sh = (13, 4, 30, 22)
        body = (6, 20, 37, 42)
        sw = 5
    x0, y0, x1, y1 = sh
    r = (x1 - x0) / 2
    d = f"M{fmt(x0)} {fmt(y1)} V{fmt(y0 + r)} A{fmt(r)} {fmt(r)} 0 0 1 {fmt(x1)} {fmt(y0 + r)} V{fmt(y1)}"
    c.stroke(d, "steel:1", sw, cap="butt")
    c.layer()
    c.shape(rrect(*body, 1 if s < 32 else 2), "amber", ext=c.e, side="amber:3")
    bx0, by0, bx1, by1 = body
    cx = (bx0 + bx1) / 2
    kr = {16: 1.2, 24: 1.8, 32: 2.4, 48: 3.6}[s]
    ky = by0 + (by1 - by0) * 0.42
    c.detail(circle(cx, ky, kr), INK)
    c.detail(poly([(cx - kr * 0.5, ky), (cx + kr * 0.5, ky), (cx + kr * 0.6, ky + kr * 2.4), (cx - kr * 0.6, ky + kr * 2.4)]), INK)
    c.hair(line((bx0 + 1, by0 + 1.5), (bx1 - 1, by0 + 1.5)), WHITE, 1, 0.6)


# =============================================================== actions ==
def arrow_pts(c, direction):
    """A bold arrow: head plus shaft, pointing left; rotated for others."""
    s = c.s
    if s == 16:
        pts = [(1, 7), (7, 1), (7, 4.5), (13, 4.5), (13, 9.5), (7, 9.5), (7, 13)]
        cx = cy = 7
    elif s == 24:
        pts = [(1, 11), (11, 1), (11, 6.5), (20, 6.5), (20, 15.5), (11, 15.5), (11, 21)]
        cx = cy = 11
    elif s == 32:
        pts = [(2, 14), (14, 2), (14, 9), (27, 9), (27, 19), (14, 19), (14, 26)]
        cx = cy = 14
    else:
        pts = [(2, 21.5), (21.5, 2), (21.5, 13.5), (42, 13.5), (42, 29.5), (21.5, 29.5), (21.5, 41)]
        cx = cy = 21.5
    rot = {"left": 0, "up": 90, "right": 180, "down": 270}[direction]
    out = []
    for x, y in pts:
        dx, dy = x - cx, y - cy
        if rot == 90:
            dx, dy = -dy, dx
        elif rot == 180:
            dx, dy = -dx, -dy
        elif rot == 270:
            dx, dy = dy, -dx
        out.append((cx + dx, cy + dy))
    return out


def arrow(c, direction, fam="teal"):
    pts = arrow_pts(c, direction)
    c.shape(poly(pts), fam, ext=c.e, side=f"{fam}:3")
    # highlight along the upper-left edges of the head
    if c.s >= 24:
        c.hair(line(pts[0], pts[1]), WHITE, 1, 0.5) if direction in ("left", "up") else None


def back(c):
    arrow(c, "left")


def forward(c):
    arrow(c, "right")


def up(c):
    arrow(c, "up")


def refresh(c):
    """Clockwise circular arrow, lime, head at the top pointing right."""
    s = c.s
    cx = cy = (s - c.e) / 2
    ro = cx - c.w
    t = {16: 2, 24: 3, 32: 4, 48: 6}[s]
    hl = {16: 5, 24: 5.5, 32: 7.5, 48: 11}[s]     # head length
    hb = {16: 3.5, 24: 4.5, 32: 6, 48: 9}[s]        # head half-width
    rm = ro - hb + t / 2
    a_start, a_end = math.radians(5 if s == 16 else -20), math.radians(250)

    def at(a, r=rm):
        return (round(cx + r * math.cos(a), 2), round(cy + r * math.sin(a), 2))
    p0, p1 = at(a_start), at(a_end)
    d = f"M{fmt(p0[0])} {fmt(p0[1])} A{fmt(rm)} {fmt(rm)} 0 1 1 {fmt(p1[0])} {fmt(p1[1])}"
    tang = (-math.sin(a_end), math.cos(a_end))
    tip = (round(p1[0] + tang[0] * hl, 2), round(p1[1] + tang[1] * hl, 2))
    b1, b2 = at(a_end, rm - hb), at(a_end, rm + hb)
    c.stroke(d, "lime:1", t, cap="butt")
    c.shape(poly([b1, tip, b2]), "lime:1")
    c.layer()
    c.stroke(d, "g:#d8ff85:#6cc010", t, outline=False, cap="butt")
    c.detail(poly([b1, tip, b2]), "g:#d8ff85:#8fdc1f")


def new_folder(c):
    folder(c)
    s = c.s
    r = {16: 3.5, 24: 5, 32: 6.5, 48: 9.5}[s]
    cx = s - c.w - r - (0 if s == 16 else 1)
    cy = s - c.w - r - (0 if s == 16 else 1)
    if s == 16:
        cx, cy = 11.5, 11.5
    badge_plus(c, cx, cy, r)


def copy(c):
    s = c.s
    if s == 16:
        a = (1, 1, 9, 11, 3)
        b = (5, 4, 13, 14, 3)
    elif s == 24:
        a = (2, 1, 14, 16, 4)
        b = (8, 6, 20, 21, 4)
    elif s == 32:
        a = (3, 2, 18, 21, 5)
        b = (10, 8, 26, 27, 5)
    else:
        a = (4, 2, 27, 31, 8)
        b = (15, 11, 39, 41, 8)
    page(c, *a)
    c.layer()
    page(c, *b, fill="paper")
    x0, y0, x1, y1, f = b
    if s >= 24:
        st = {24: 3, 32: 3, 48: 5}[s]
        text_lines(c, x0 + st, x1 - st, y0 + f + 1, y1 - st, st, "teal:2", 3, 1 if s < 48 else 2)
    else:
        for y in (8, 10):
            c.hair(line((7, h(y)), (11, h(y))), "teal:2", 1)


def scissors(c):
    """Cut: crossed steel blades, violet finger rings."""
    s = c.s
    k = (s - c.e - 2 * c.w) / 46
    o = c.w
    def P(x, y):
        return (round(o + x * k, 2), round(o + y * k, 2))
    if s == 16:
        c.shape(poly([(4, 1), (6, 1), (9, 9), (7, 10)]), "steel", ext=1, side="steel:3")
        c.shape(poly([(9, 1), (11, 1), (8, 10), (6, 9)]), "steel:0", ext=1, side="steel:3")
        c.layer()
        c.stroke(circle(4.5, 11.5, 2), "violet", 1.6)
        c.stroke(circle(10.5, 11.5, 2), "violet", 1.6)
        return
    bl1 = [P(10, 0), P(16, 0), P(30, 28), P(24, 31)]
    bl2 = [P(30, 0), P(36, 0), P(22, 31), P(16, 28)]
    c.shape(poly(bl1), "steel", ext=c.e, side="steel:3")
    c.shape(poly(bl2), "steel:0", ext=c.e, side="steel:3")
    c.detail(circle(*P(23, 21), max(1, 1.6 * k)), INK)
    c.layer()
    rw = {24: 2, 32: 3, 48: 4}[s]
    c.stroke(circle(*P(12, 37), 7 * k), "violet", rw)
    c.stroke(circle(*P(34, 37), 7 * k), "violet", rw)


def paste(c):
    """Clipboard: amber board, steel clip, a sheet."""
    s = c.s
    if s == 16:
        board = (2, 2, 13, 14)
        sheet = (4, 4, 11, 12)
        clip = (5, 1, 10, 4)
    elif s == 24:
        board = (3, 3, 19, 21)
        sheet = (6, 6, 16, 18)
        clip = (7, 1, 15, 6)
    elif s == 32:
        board = (4, 4, 25, 28)
        sheet = (8, 8, 21, 24)
        clip = (10, 2, 19, 8)
    else:
        board = (6, 6, 38, 42)
        sheet = (11, 12, 33, 37)
        clip = (15, 2, 29, 11)
    c.shape(rrect(*board, 1 if s < 32 else 2), "amber", ext=c.e, side="amber:3")
    c.detail(rect(*sheet), "paper")
    x0, y0, x1, y1 = sheet
    if s >= 24:
        st = {24: 3, 32: 3, 48: 5}[s]
        text_lines(c, x0 + 2, x1 - 2, y0 + st, y1 - 2, st, "violet:2", 3, 1 if s < 48 else 2)
    else:
        for y in (7, 9):
            c.hair(line((6, h(y)), (9, h(y))), "violet:2", 1)
    c.layer()
    c.shape(rrect(*clip, 1), "steel", ext=0)


def delete(c):
    """Bold red X."""
    s = c.s
    n = s - c.e - 2 * c.w
    o = c.w
    t = {16: 2, 24: 3, 32: 4, 48: 6}[s]
    a, b = o, o + n
    if s == 16:
        pts = [(1, 3), (3, 1), (7.5, 5.5), (12, 1), (14, 3), (9.5, 7.5), (14, 12), (12, 14), (7.5, 9.5), (3, 14), (1, 12), (5.5, 7.5)]
    else:
        m = (a + b) / 2
        pts = [(a, a + t), (a + t, a), (m, m - t), (b - t, a), (b, a + t), (m + t, m),
               (b, b - t), (b - t, b), (m, m + t), (a + t, b), (a, b - t), (m - t, m)]
    c.shape(poly(pts), "red", ext=c.e, side="red:3")


def close(c):
    """Close: a small steel x on a round slate button (distinct from delete)."""
    s = c.s
    cx = cy = (s - c.e) / 2
    r = cx - c.w
    c.shape(circle(cx, cy, r), "g:#6b6488:#3a3452", ext=c.e, side="slate:3")
    k = r * 0.42
    t = {16: 1.6, 24: 2.2, 32: 3, 48: 4.5}[s]
    c.hair(line((cx - k, cy - k), (cx + k, cy + k)), WHITE, t, cap="round")
    c.hair(line((cx + k, cy - k), (cx - k, cy + k)), WHITE, t, cap="round")


def rename(c):
    """Pencil over a text baseline with a caret."""
    s = c.s
    k = (s - c.e - 2 * c.w) / 46
    o = c.w
    def P(x, y):
        return (round(o + x * k, 2), round(o + y * k, 2))
    if s == 16:
        c.detail(rect(1, 13, 7, 14), "violet:2")
        c.shape(poly([(4, 9), (11, 2), (14, 5), (7, 12), (3, 13)]), "amber", ext=1, side="amber:3")
        c.detail(poly([(4, 9), (7, 12), (3, 13)]), "g:#ffe9c4:#e8b47a")
        c.detail(poly([(3.5, 12), (4.5, 13), (3, 13)]), INK)
        c.detail(poly([(11, 2), (14, 5), (13, 6), (10, 3)]), "red:1")
        return
    c.shape(rect(*P(0, 41), *P(20, 45)), "violet:2", ext=0)
    c.layer()
    body = [P(10, 30), P(34, 6), P(43, 15), P(19, 39), P(6, 43)]
    c.shape(poly(body), "amber", ext=c.e, side="amber:3")
    c.detail(poly([P(30, 10), P(34, 6), P(43, 15), P(39, 19)]), "red:1")
    c.detail(poly([P(27, 13), P(30, 10), P(39, 19), P(36, 22)]), "steel:1")
    c.detail(poly([P(10, 30), P(19, 39), P(6, 43)]), "g:#ffe9c4:#e8b47a")
    c.detail(poly([P(7.5, 38.5), P(10.5, 41.5), P(6, 43)]), INK)
    c.hair(line(P(12, 30), P(32, 10)), WHITE, 1, 0.55)


def view(c):
    """Eye: white almond, teal iris, ink pupil, glint."""
    s = c.s
    cx = (s - c.e) / 2
    cy = cx
    rx = cx - c.w
    ry = {16: 4.5, 24: 6.5, 32: 8.5, 48: 13}[s]
    d = (f"M{fmt(cx - rx)} {fmt(cy)} Q{fmt(cx)} {fmt(cy - ry * 2)} {fmt(cx + rx)} {fmt(cy)} "
         f"Q{fmt(cx)} {fmt(cy + ry * 2)} {fmt(cx - rx)} {fmt(cy)}Z")
    c.shape(d, "paper", ext=c.e, side="paper:3")
    ir = {16: 3, 24: 4.5, 32: 6, 48: 9}[s]
    c.detail(circle(cx, cy, ir), "teal")
    c.detail(circle(cx, cy, ir * 0.5), INK)
    c.detail(circle(cx - ir * 0.35, cy - ir * 0.35, max(0.7, ir * 0.22)), WHITE)


def columns(c):
    x0, y0, x1, y1 = window_frame(c)
    w = x1 - x0
    for i in (1, 2):
        xx = x0 + round(w * i / 3)
        c.hair(line((xx + 0.5, y0), (xx + 0.5, y1)), INK, 1, 0.7)
    if c.s >= 24:
        xx = x0 + round(w / 3)
        c.detail(rect(x0 + 1, y0 + 1, xx, y0 + 1 + max(2, c.s // 12)), "teal", 0.9)
        st = {24: 3, 32: 3, 48: 5}[c.s]
        for i in range(3):
            cx0 = x0 + round(w * i / 3) + 2
            cx1 = x0 + round(w * (i + 1) / 3) - 1
            yy = y0 + 2 + st
            while yy < y1 - 1:
                c.hair(line((cx0, yy + 0.5), (cx1, yy + 0.5)), "paper:3", 1)
                yy += st


def sidebar(c):
    x0, y0, x1, y1 = window_frame(c)
    sw = round((x1 - x0) * 0.36)
    c.detail(rect(x0, y0, x0 + sw, y1), "teal:0")
    c.hair(line((x0 + sw + 0.5, y0), (x0 + sw + 0.5, y1)), INK, 1, 0.7)
    if c.s >= 24:
        st = {24: 3, 32: 3, 48: 5}[c.s]
        yy = y0 + 2
        while yy < y1 - 1:
            c.hair(line((x0 + 2, yy + 0.5), (x0 + sw - 2, yy + 0.5)), "teal:2", 1)
            yy += st


def split(c):
    x0, y0, x1, y1 = window_frame(c)
    mid = (x0 + x1) // 2
    c.detail(rect(mid + 1, y0, x1, y1), "violet:0", 0.55)
    c.hair(line((mid + 0.5, y0), (mid + 0.5, y1)), INK, 1, 0.85)
    if c.s >= 32:
        c.hair(line((mid - 0.5, y0), (mid - 0.5, y1)), INK, 1, 0.5)
    s = c.s
    r = {16: 3.5, 24: 5, 32: 6.5, 48: 9.5}[s]
    if s == 16:
        badge_plus(c, 11.5, 11.5, 3.5)
    else:
        badge_plus(c, s - c.w - r - 1, s - c.w - r - 1, r)


def tab(c):
    s = c.s
    if s == 16:
        tabr = (2, 2, 8, 5)
        body = (1, 5, 14, 13)
    elif s == 24:
        tabr = (2, 3, 11, 7)
        body = (1, 7, 22, 20)
    elif s == 32:
        tabr = (3, 4, 15, 9)
        body = (2, 9, 28, 27)
    else:
        tabr = (4, 5, 22, 13)
        body = (2, 13, 43, 40)
    t2 = (tabr[2] + 1, tabr[1] + 1, tabr[2] + 1 + (tabr[2] - tabr[0]) - 1, tabr[3])
    c.shape(rect(*t2), "steel", ext=0)
    c.layer()
    c.shape(rect(*body), "paper", ext=c.e, side="steel:2")
    c.shape(rect(*tabr), "violet")
    c.detail(rect(tabr[0], tabr[3] - 1, tabr[2], tabr[3] + 1), "violet:1")
    bx0, by0, bx1, by1 = body
    c.detail(rect(bx0, by0, bx1, by0 + max(1, s // 16)), "violet:1")
    r = {16: 3.5, 24: 5, 32: 6.5, 48: 9.5}[s]
    if s == 16:
        badge_plus(c, 11.5, 11.5, 3.5)
    else:
        badge_plus(c, s - c.w - r - 1, s - c.w - r - 1, r)


def search(c):
    """Magnifier: violet rim, teal glass, slate handle."""
    s = c.s
    if s == 16:
        cx, cy, r, rim = 6, 6, 4.5, 1.6
        h0, h1, hw = (9, 9), (13, 13), 2.6
    elif s == 24:
        cx, cy, r, rim = 9, 9, 7, 2.5
        h0, h1, hw = (14, 14), (20, 20), 3.8
    elif s == 32:
        cx, cy, r, rim = 12, 12, 9, 3
        h0, h1, hw = (18.5, 18.5), (27, 27), 5
    else:
        cx, cy, r, rim = 18, 18, 14, 4.5
        h0, h1, hw = (28, 28), (40, 40), 7
    c.stroke(line(h0, h1), "violet:3", hw, cap="round")
    c.layer()
    c.shape(circle(cx, cy, r), "violet")
    c.detail(circle(cx, cy, r - rim), "g:#c9fff8:#38c9d6")
    c.detail(f"M{fmt(cx - (r - rim) * 0.6)} {fmt(cy)} A{fmt((r - rim) * 0.6)} {fmt((r - rim) * 0.6)} 0 0 1 {fmt(cx)} {fmt(cy - (r - rim) * 0.6)}",
             "none")
    c.hair(f"M{fmt(cx - (r - rim) * 0.55)} {fmt(cy)} A{fmt((r - rim) * 0.55)} {fmt((r - rim) * 0.55)} 0 0 1 {fmt(cx)} {fmt(cy - (r - rim) * 0.55)}",
           WHITE, max(1, rim * 0.5), 0.85, cap="round")


def settings(c):
    """Gear: steel teeth, violet hub."""
    s = c.s
    cx = cy = (s - c.e) / 2
    ro = cx - c.w
    teeth = 6 if s == 16 else 8
    ri = ro * {16: 0.7, 24: 0.76, 32: 0.78, 48: 0.8}[s]
    hub = ro * (0.3 if s == 16 else 0.36)
    pts = []
    tw = {16: 0.42, 24: 0.4, 32: 0.38, 48: 0.36}[s]  # tooth half-width in radians at outer radius
    for i in range(teeth):
        a = math.radians(i * 360 / teeth + (0 if s == 16 else 22.5))
        step = math.pi / teeth
        pts += [(cx + ri * math.cos(a - step + 0.05), cy + ri * math.sin(a - step + 0.05)),
                (cx + ro * math.cos(a - tw * step / 0.7 * 0.7), cy + ro * math.sin(a - tw * step / 0.7 * 0.7)),
                (cx + ro * math.cos(a + tw * step / 0.7 * 0.7), cy + ro * math.sin(a + tw * step / 0.7 * 0.7)),
                (cx + ri * math.cos(a + step - 0.05), cy + ri * math.sin(a + step - 0.05))]
    pts = [(round(x, 2), round(y, 2)) for x, y in pts]
    d = poly(pts) + " " + circle(cx, cy, hub)
    c.shape(d, "steel", ext=c.e, side="steel:3", rule="evenodd")
    c.detail(circle(cx, cy, hub), "none")
    c.layer()
    c.stroke(circle(cx, cy, hub + {16: 0.5, 24: 1, 32: 1.5, 48: 2}[s]), "violet", {16: 1, 24: 1.5, 32: 2, 48: 3}[s], outline=False)


def info(c):
    s = c.s
    cx = cy = (s - c.e) / 2
    r = cx - c.w
    c.shape(circle(cx, cy, r), "teal", ext=c.e, side="teal:3")
    dot = {16: 1.1, 24: 1.6, 32: 2.2, 48: 3.2}[s]
    sw = {16: 2, 24: 3, 32: 4, 48: 6}[s]
    if s == 16:
        c.detail(rect(6.5, 3, 8.5, 5), WHITE)
        c.detail(rect(6.5, 6, 8.5, 12), WHITE)
        return
    c.detail(circle(cx, cy - r * 0.45, dot), WHITE)
    c.detail(rect(cx - sw / 2, cy - r * 0.15, cx + sw / 2, cy + r * 0.6), WHITE)
    c.detail(f"M{fmt(cx - r * 0.25)} {fmt(cy - r * 0.15)} h{fmt(r * 0.25)} v{fmt(sw / 2)} h{fmt(-r * 0.25)}Z", WHITE)
    c.hair(f"M{fmt(cx - r * 0.7)} {fmt(cy - r * 0.2)} A{fmt(r * 0.72)} {fmt(r * 0.72)} 0 0 1 {fmt(cx - r * 0.2)} {fmt(cy - r * 0.7)}",
           WHITE, 1, 0.5, cap="round")


# ================================================================= table ==
ICONS = {
    # places / objects
    "folder": folder,
    "folder-open": folder_open,
    "home": home,
    "computer": computer,
    "drive": drive,
    "removable": removable,
    "network": network,
    "trash-empty": trash,
    "trash-full": trash_full,
    "bookmark": star,
    "recent": clock,
    # file types
    "file": file_generic,
    "file-document": document,
    "file-image": image,
    "file-audio": audio,
    "file-video": video,
    "file-archive": archive,
    "file-executable": executable,
    "file-source": source,
    "file-ccl": ccl_script,
    "file-book": book,
    "link": link,
    "locked": locked,
    # actions
    "go-back": back,
    "go-forward": forward,
    "go-up": up,
    "refresh": refresh,
    "new-folder": new_folder,
    "copy": copy,
    "cut": scissors,
    "paste": paste,
    "delete": delete,
    "rename": rename,
    "view": view,
    "columns": columns,
    "sidebar": sidebar,
    "split": split,
    "new-tab": tab,
    "search": search,
    "settings": settings,
    "info": info,
    "close": close,
}


def render(name, size):
    c = Canvas(size)
    ICONS[name](c)
    return c.svg()


def main(argv):
    if "--list" in argv:
        print("\n".join(ICONS))
        return
    for size in SIZES:
        d = HERE / str(size)
        d.mkdir(exist_ok=True)
        for name in ICONS:
            (d / f"{name}.svg").write_text(render(name, size))


if __name__ == "__main__":
    main(sys.argv[1:])
