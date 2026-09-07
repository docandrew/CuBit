from pathlib import Path
import sys
from PIL import Image, ImageChops

root = Path(sys.argv[1])


def frame(number):
    return Image.open(root / f"frame-{number:02}.bmp").convert("RGB")


source = (250, 145, 580, 330)
assert not ImageChops.difference(frame(0).crop(source), frame(14).crop(source)).getbbox(), "REPL mutated the source editor"
transcript = (250, 185, 580, 278)
assert ImageChops.difference(frame(1).crop(transcript), frame(11).crop(transcript)).getbbox(), "REPL did not display a result"
assert not ImageChops.difference(frame(11).crop(transcript), frame(16).crop(transcript)).getbbox(), "switching views or interpreting source changed REPL history"
command = (245, 320, 596, 350)
assert ImageChops.difference(frame(11).crop(command), frame(12).crop(command)).getbbox(), "history recall did not restore input"
assert not ImageChops.difference(frame(11).crop(command), frame(13).crop(command)).getbbox(), "Down did not restore the empty draft"
frame(11).save(root / "repl.png")
frame(12).save(root / "recall.png")
print(f"Workbench REPL view/input isolation PASS; screenshots: {root}")
