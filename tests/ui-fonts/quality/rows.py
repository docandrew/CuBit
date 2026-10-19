"""Vertical agreement with the reference: per text row, the ink profile
(summed |pixel - background| per pixel row) compared with the reference's;
reported as relative L1 error. Horizontal placement does not affect it.
usage: rows.py REF IMG..."""
import sys, numpy as np
from PIL import Image
sys.path.insert(0, __file__.rsplit("/",1)[0])
from spec import *
ROW, WIDTH = ROW_PX, WIDTH_PX
def prof(img, i, bg):
    r = img[i*ROW:(i+1)*ROW]
    return np.abs(r - np.array([(bg>>16)&255,(bg>>8)&255,bg&255])).sum((1,2))
ref = np.asarray(Image.open(sys.argv[1]).convert("RGB"), dtype=float)
for p in sys.argv[2:]:
    img = np.asarray(Image.open(p).convert("RGB"), dtype=float)
    errs = {}
    for i,(face,fg,bg,t) in enumerate(SAMPLES):
        a, b = prof(ref,i,bg), prof(img,i,bg)
        errs.setdefault(face, []).append(np.abs(a-b).sum()/a.sum())
    print(f"{p:24s} " + "  ".join(f"{k} {100*np.mean(v):5.1f}%" for k,v in errs.items()))
