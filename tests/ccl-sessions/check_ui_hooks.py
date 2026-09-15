from pathlib import Path
import sys
from PIL import Image, ImageChops

root = Path(sys.argv[1])
def frame(n):
    return Image.open(root / f'frame-{n:02}.bmp').convert('RGB')

shown = 2 + len('(ui.label-value 42)')
hidden = shown + 1 + len('(ui.label-visible false)')
clock = hidden + 1 + len('(clock.monotonic-ms)')
text = clock + 1 + len('(ui.label-text (concat "Hello, " "Cubie"))')
label = (366, 47, 1190, 74)
assert ImageChops.difference(frame(1).crop(label), frame(shown).crop(label)).getbbox(), 'CCL did not paint its label'
assert not ImageChops.difference(frame(1).crop(label), frame(hidden).crop(label)).getbbox(), 'hide did not restore the host label area'
frame(shown).save(root / 'label.png')
frame(clock).save(root / 'clock-repl.png')
assert ImageChops.difference(frame(hidden).crop(label), frame(text).crop(label)).getbbox(), 'text label was not painted'
frame(text).save(root / 'text-label.png')
print(f'PASS: REPL-driven numeric/text labels and show/hide; screenshots: {root}')
