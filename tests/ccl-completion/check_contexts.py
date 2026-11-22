"""Drive rejected suggestion contexts through the real hosted REPL."""
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from PIL import Image, ImageChops

cases = [
    ('"(clock.mon', ''),
    ('"escaped \\" (clock.mon', ''),
    ('# (clock.mon', ''),
    ('(unknown.mon', ''),
    ('clock.mon', ''),
    ('(clock.mon', 'select'),
    ('(clock.mon', 'left'),
]
for source, move in cases:
    root = Path(tempfile.mkdtemp(prefix='cubit-completion-context.'))
    env = dict(os.environ, SDL_VIDEODRIVER='dummy', SDL_RENDER_DRIVER='software',
               CCL_UI_WINDOW_WIDTH='1200', CCL_UI_WINDOW_HEIGHT='700',
               CCL_TEST_CAPTURE=str(root), CCL_TEST_COMPLETION_PROBE=source,
               CCL_TEST_COMPLETION_MOVE=move, LD_PRELOAD=sys.argv[2])
    subprocess.run([sys.argv[1]], env=env, check=True, timeout=20)
    def input_frame(index):
        return Image.open(root / f'frame-{index:02}.bmp').convert('RGB').crop((245, 610, 920, 660))
    before = input_frame(len(source) + 2)
    after = input_frame(len(source) + 3)
    assert not ImageChops.difference(before, after).getbbox(), (source, move, root)
print('PASS: Tab leaves strings, comments, unknown names, non-call heads, selections and mid-token carets unchanged')
