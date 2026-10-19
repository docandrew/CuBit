"""Compare CuBit's layout tables (cargo run --example layout_dump) with
HarfBuzz shaping of every printable-ASCII pair (kern feature only), at the
13 px em in 1/64 px. Linux-hosted oracle; exit 1 on any mismatch > 1 unit.
usage: check_kerning.py SANS.ttf MONO.ttf SEMIBOLD.ttf < dump (face numbers 0, 1, 2)"""
import sys
import uharfbuzz as hb
EM64 = 13 * 64
fonts = []
for path in sys.argv[1:4]:
    f = hb.Font(hb.Face(hb.Blob.from_file_path(path)))
    f.scale = (EM64, EM64)
    fonts.append(f)
ours = {}
adv = {}
for line in sys.stdin:
    p = line.split()
    if p[0] == "advance":
        adv[int(p[1])] = [int(x) for x in p[2:]]
    elif p[0] == "kern":
        ours[(int(p[1]), int(p[2]), int(p[3]))] = int(p[4])
bad = 0
pairs = 0
for face, font in enumerate(fonts):
    single = {}
    for c in range(32, 127):
        b = hb.Buffer(); b.add_str(chr(c)); b.guess_segment_properties()
        hb.shape(font, b, {"kern": False, "liga": False, "calt": False})
        single[c] = b.glyph_positions[0].x_advance
        if abs(single[c] - adv[face][c - 32]) > 1:
            print(f"advance mismatch face {face} {chr(c)!r}: {adv[face][c-32]} vs {single[c]}"); bad += 1
    for l in range(32, 127):
        for r in range(32, 127):
            b = hb.Buffer(); b.add_str(chr(l) + chr(r)); b.guess_segment_properties()
            hb.shape(font, b, {"kern": True, "liga": False, "calt": False})
            pos = b.glyph_positions
            if len(pos) != 2:
                continue
            want = pos[0].x_advance - single[l]
            have = ours.get((face, l, r), 0)
            pairs += 1
            if abs(want - have) > 1:
                bad += 1
                if bad < 20:
                    print(f"kern mismatch face {face} {chr(l)}{chr(r)}: ours {have} harfbuzz {want}")
print(f"{pairs} pairs checked, {sum(1 for v in ours.values() if v)} kerned, {bad} mismatches")
sys.exit(1 if bad else 0)
