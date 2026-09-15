from pathlib import Path
import sys
from PIL import Image, ImageChops

root = Path(sys.argv[1])
if len(sys.argv) > 2:
    source = '(let ((answer (+ 20 22))) answer)'
    before = Image.open(root / f"frame-{len(source) + 5:02}.bmp").convert("RGB")
    switched = Image.open(root / f"frame-{len(source) + 6:02}.bmp").convert("RGB")
    formatted = Image.open(root / f"frame-{len(source) + 7:02}.bmp").convert("RGB")
    for area in [(5, 155, 225, 625), (935, 80, 1195, 655)]:
        assert not ImageChops.difference(before.crop(area), switched.crop(area)).getbbox(), "view switch changed paused VM inspection"
        assert not ImageChops.difference(switched.crop(area), formatted.crop(area)).getbbox(), "format changed paused VM inspection"
    switched.save(root / "paused-basic.png")
    print(f"PASS: paused VM inspection survives F8 and Shift+F8; screenshots: {root}")
    sys.exit(0)
length = len('(let ((text (to-string 7))) (if (= (length text) 1) (concat "0" text) text))')
def frame(number):
    return Image.open(root / f"frame-{number:02}.bmp").convert("RGB")

basic = frame(length + 3)  # F8, then F5
lisp = frame(length + 5)   # F8, then F5 again
basic.save(root / "basic.png")
lisp.save(root / "lisp.png")
# Same interpreter result, with different editor presentation.
result_area = (5, 100, 225, 150)
assert ImageChops.difference(basic, lisp).getbbox(), "view did not change"
assert not ImageChops.difference(basic.crop(result_area), lisp.crop(result_area)).getbbox(), "execution result changed"
print(f"PASS: shared Workbench switches views and interprets both; screenshots: {root}")
