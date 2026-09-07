"""Check real Workbench hover damage and modal interaction captures."""
from pathlib import Path
import sys
from PIL import Image, ImageChops

directory = Path(sys.argv[1])


def frame(number):
    return Image.open(directory / f"frame-{number:02}.bmp").convert("RGB")


initial, hover = frame(0), frame(1)
assert initial.size == hover.size == (900, 400)
assert ImageChops.difference(initial.crop((0, 374, 900, 400)),
                            hover.crop((0, 374, 900, 400))).getbbox(), "hover hint did not repaint"
assert not ImageChops.difference(initial.crop((250, 145, 580, 330)),
                                hover.crop((250, 145, 580, 330))).getbbox(), "hover repainted source"
assert ImageChops.difference(hover, frame(2)).getbbox(), "Open dialog did not appear"
assert ImageChops.difference(frame(2), frame(3)).getbbox(), "selection did not move"
assert ImageChops.difference(frame(5), frame(14)).getbbox(), "Save filename did not update"
# Save and subsequent Open restore identical source pixels. Dialog text events
# must not have inserted the chosen filename into the editor behind the modal.
source = (250, 145, 580, 330)
assert not ImageChops.difference(initial.crop(source), frame(15).crop(source)).getbbox(), "Save typed into source"
assert not ImageChops.difference(frame(15).crop(source), frame(19).crop(source)).getbbox(), "loaded source differs"
for number, name in [(1, "hover"), (2, "open"), (14, "save"), (19, "loaded")]:
    frame(number).save(directory / (name + ".png"))
print(f"Workbench hover/modal integration PASS; screenshots: {directory}")
