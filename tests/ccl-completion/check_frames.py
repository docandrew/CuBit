from pathlib import Path
import sys
from PIL import Image, ImageChops

root = Path(sys.argv[1])
def frame(n):
    return Image.open(root / f"frame-{n:02}.bmp").convert("RGB")

# The final text was entered manually after selecting the completion. Ignore
# the hint row: it intentionally disappears on the first subsequent edit.
before, inspected, completed, manual = frame(11), frame(12), frame(13), frame(33)
area = (245, 610, 790, 660)
assert not ImageChops.difference(before.crop(area), inspected.crop(area)).getbbox(), "inspection modified the proposed input"
assert ImageChops.difference(before.crop(area), completed.crop(area)).getbbox(), "completion did not edit input"
assert not ImageChops.difference(completed.crop(area), manual.crop(area)).getbbox(), "completion differs from manual operation name"
before.save(root / "suggestion.png")
completed.save(root / "completion.png")
print(f"PASS: inline suggestion, non-mutating inspection, Tab matches manual input; screenshots: {root}")
