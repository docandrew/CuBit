"""Distance of CuBit renders from the reference: mean absolute sRGB error over
pixels where either image has ink, per sample face/colour group.
usage: metric.py REF IMG... [--limit=N]
--limit=N: exit 1 when any group's error exceeds N (regression threshold)."""
import sys, numpy as np
from PIL import Image
sys.path.insert(0, __file__.rsplit("/",1)[0])
from spec import *
ROW, WIDTH = ROW_PX, WIDTH_PX
ref = np.asarray(Image.open(sys.argv[1]).convert("RGB"), dtype=float)
def rgb(c): return np.array([(c>>16)&255,(c>>8)&255,c&255],dtype=float)
limit = next((float(a.split("=")[1]) for a in sys.argv if a.startswith("--limit=")), None)
failed = False
for path in [a for a in sys.argv[2:] if not a.startswith("--")]:
    img = np.asarray(Image.open(path).convert("RGB"), dtype=float)
    groups = {}
    for i,(face,fg,bg,text) in enumerate(SAMPLES):
        a, b = ref[i*ROW:(i+1)*ROW], img[i*ROW:(i+1)*ROW]
        ink = (np.abs(a-rgb(bg)).sum(2) > 6) | (np.abs(b-rgb(bg)).sum(2) > 6)
        err = np.abs(a-b).sum(2)[ink].mean() / 3
        key = f"{face}/{'dark' if fg < bg else 'light'}"
        groups.setdefault(key, []).append(err)
    print(f"{path.split('/')[-1]:32s} " + "  ".join(f"{k} {np.mean(v):5.1f}" for k,v in groups.items()))
    worst = max(np.mean(v) for v in groups.values())
    if limit is not None and worst > limit:
        print(f"  FAIL: ink error {worst:.1f} above the limit {limit}")
        failed = True
sys.exit(1 if failed else 0)
