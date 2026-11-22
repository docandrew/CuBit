from pathlib import Path
import sys
from PIL import Image, ImageChops

root = Path(sys.argv[1])
def frame(n):
    return Image.open(root / f'frame-{n:02}.bmp').convert('RGB')

shown = 2 + len('(ui.label-value 42)')
hidden = shown + 1 + len('(ui.label-visible false)')
clock = hidden + 1 + len('(clock.monotonic-ms)')
text = clock + 1 + len('(define (greet (name String)) String (concat "Hello, " name)) '
                       '(ui.label-text (greet "Cubie"))')
label = (366, 47, 1190, 74)
assert ImageChops.difference(frame(1).crop(label), frame(shown).crop(label)).getbbox(), 'CCL did not paint its label'
assert not ImageChops.difference(frame(1).crop(label), frame(hidden).crop(label)).getbbox(), 'hide did not restore the host label area'
frame(shown).save(root / 'label.png')
frame(clock).save(root / 'clock-repl.png')
assert ImageChops.difference(frame(hidden).crop(label), frame(text).crop(label)).getbbox(), 'text label was not painted'
frame(text).save(root / 'text-label.png')
output = text + 1 + len('(ui.output-append "Hello from CCL!")')
second = output + 1 + len('(ui.output-append (to-string (clock.monotonic-ms)))')
cleared = second + 1 + len('(ui.output-clear)')
pane = (240, 480, 920, 660)
assert ImageChops.difference(frame(text).crop(pane), frame(output).crop(pane)).getbbox(), 'output did not append'
assert ImageChops.difference(frame(output).crop(pane), frame(second).crop(pane)).getbbox(), 'second output line missing'
assert not ImageChops.difference(frame(text).crop(pane), frame(cleared).crop(pane)).getbbox(), 'clear did not restore empty output'
scroll_command = '(ui.output-append "' + r'\n'.join(f'line {n:02}' for n in range(1, 11)) + '")'
before_click = cleared + 1 + len(scroll_command)
assert ImageChops.difference(frame(cleared).crop(pane), frame(before_click).crop(pane)).getbbox(), 'clear button test has no text'
# Keep the hoverable button itself out of this comparison.
field = (244, 518, 897, 635)
assert ImageChops.difference(frame(before_click).crop(field), frame(before_click + 3).crop(field)).getbbox(), 'Output wheel did not scroll'
assert ImageChops.difference(frame(before_click + 4).crop(field), frame(before_click + 5).crop(field)).getbbox(), 'Output thumb did not drag'
assert not ImageChops.difference(frame(cleared).crop(field), frame(before_click + 8).crop(field)).getbbox(), 'Clear button did not erase output'
assert not ImageChops.difference(frame(cleared).crop(field), frame(before_click + 11).crop(field)).getbbox(), 'read-only output accepted typed text'
source = (244, 145, 899, 420)
assert not ImageChops.difference(frame(0).crop(source), frame(before_click + 12).crop(source)).getbbox(), 'typing in output mutated the source'
frame(second).save(root / 'output-pane.png')
print(f'PASS: REPL-driven numeric/text labels and show/hide; screenshots: {root}')
