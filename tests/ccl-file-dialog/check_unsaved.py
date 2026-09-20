"""Real Workbench modal transitions; mock filesystem, no user documents."""
from pathlib import Path
import sys
from PIL import Image, ImageChops, ImageDraw

root, scenario = Path(sys.argv[1]), sys.argv[2]

def frame(n):
    return Image.open(root / f"frame-{n:02}.bmp").convert("RGB")

def same(a, b, box, message):
    diff = ImageChops.difference(frame(a).crop(box), frame(b).crop(box))
    if box == source:
        # Loading resets the caret from after 777 to before it. Ignore only
        # those two one-pixel caret columns, not any source glyph region.
        pen = ImageDraw.Draw(diff)
        for x in (0, 24):
            pen.line((x, 0, x, 7), fill=(0, 0, 0))
    assert diff.getbbox() is None, message

def different(a, b, box, message):
    assert ImageChops.difference(frame(a).crop(box), frame(b).crop(box)).getbbox(), message

# Exclude caret pixels and the document-status strip. Include the source text.
source = (250, 147, 570, 300)
prompt_title = (220, 157, 675, 179)
picker_title = (160, 72, 735, 92)
different(4, 5, prompt_title, "dirty open did not show confirmation")
if scenario == "cancel":
    same(4, 6, source, "Cancel lost edits")
    same(5, 7, prompt_title, "Cancel incorrectly marked document saved")
elif scenario == "discard":
    same(4, 8, source, "canceling Open after Discard lost edits")
    same(5, 9, prompt_title, "Discard prematurely cleared dirty state")
    different(4, 12, source, "Discard did not allow loading another document")
    same(7, 13, picker_title, "successfully loaded document is not clean")
elif scenario == "save":
    different(6, 7, picker_title, "successful save did not continue to Open")
    same(4, 8, source, "saving changed source")
    same(7, 9, picker_title, "saved source incorrectly prompts again")
    same(4, 12, source, "saved document did not roundtrip")
elif scenario == "save-cancel":
    same(4, 7, source, "canceling Save lost edits")
    same(5, 8, prompt_title, "canceling Save incorrectly marked source clean")
    same(4, 11, source, "canceled deferred Open leaked into a later Save")
elif scenario == "save-fail":
    same(6, 17, picker_title, "failed save continued to Open")
    different(16, 17, (160, 350, 735, 384), "save failure not displayed")
    same(4, 18, source, "save conflict lost edits")
    same(5, 19, prompt_title, "save failure incorrectly marked source clean")
elif scenario == "open-fail":
    same(7, 21, picker_title, "failed load closed the picker")
    different(20, 21, (160, 350, 735, 384), "load failure not displayed")
    same(4, 22, source, "failed load lost edits")
    same(5, 23, prompt_title, "failed load incorrectly marked source clean")
else:
    raise AssertionError(scenario)
frame(5).save(root / "unsaved-prompt.png")
print(f"PASS: unsaved {scenario}; frames: {root}")
