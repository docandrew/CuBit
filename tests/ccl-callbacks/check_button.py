from pathlib import Path
import sys
from PIL import Image, ImageChops

root, source = Path(sys.argv[1]), sys.argv[2]
base = len(source) + 2
def frame(action):
    return Image.open(root / f'frame-{base + action:02}.bmp').convert('RGB')
def different(a, b, bounds):
    return ImageChops.difference(a.crop(bounds), b.crop(bounds)).getbbox() is not None
button = (366, 47, 484, 74)
label = (490, 47, 1190, 74)
assert different(frame(0), frame(1), button), 'button did not depress'
assert different(frame(0), frame(2), label), 'click did not invoke the label handler'
assert different(frame(8), frame(10), button), 'Stop did not close the button'
frame(0).save(root / 'registered.png')
frame(2).save(root / 'clicked.png')
frame(8).save(root / 'after-edit-click.png')
frame(10).save(root / 'closed.png')
print(f'PASS: real Workbench button registration, pressed state, click and Stop; screenshots: {root}')
