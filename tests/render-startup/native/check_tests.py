"""Negative controls for the native admission oracle, without running CuBit."""
import contextlib
import importlib.util
import io
import os
import subprocess
import tempfile
from pathlib import Path
spec = importlib.util.spec_from_file_location('native_check', Path(__file__).with_name('check.py'))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
ids = [2**32+1, 2**32+2, 2**32+3, 2**32+4, 2**33+4, 2**32+5]
lines = []
for n, identity in enumerate(ids):
    software = n in (2,4,5)
    lines.append(f'procmgr: render attempt incarnation= {identity} software={str(software).upper()}')
    if n in (1,3):
        lines.append('procmgr: render admission submitted; child suspended')
    if n in (2,4):
        lines.append('procmgr: render software-only admitted')
        lines.append(f'RENDER-STARTUP: software child incarnation= {identity}')
    else:
        lines.extend(['procmgr: render admission denied; child not resumed',
                      'procmgr: failed launch child stop requested'])
    if n == 3:
        lines.append('procmgr: render retry software with fresh child')
lines.extend(['procmgr: failed launch child stop requested', 'devices: native window ready'])
lines.extend('procmgr: init spawn failed: '+n+'.app' for n in
             ('render-denied','render-unavailable','render-occupied','render-invalid'))
base = '\n'.join(lines)
with contextlib.redirect_stdout(io.StringIO()):
    module.check(base)
    bad = [base.replace('devices: native window ready',''),
           base+'\nTEST: FAIL sentinel', base+'\nKILL: denied',
           base+'\nprocmgr: render admission complete',
           base+'\nprocmgr: failed launch process cleanup rejected',
           base.replace('procmgr: render retry software with fresh child',''),
           base+'\nprocmgr: render retry software with fresh child',
           base.replace(str(ids[4]), str(ids[3])),
           base.replace(str(ids[4]), '4'),
           base.replace(f'software child incarnation= {ids[4]}', f'software child incarnation= {ids[3]}'),
           base.replace(f'software child incarnation= {ids[4]}', f'software child incarnation= {ids[5]}')]
    # The last case exposes a rejected software child's accidental execution.
    for index, text in enumerate(bad):
        try:
            module.check(text)
        except AssertionError:
            continue
        raise AssertionError(f'negative control {index} accepted')
print(f'RENDER-STARTUP: native oracle PASS with {len(bad)} negative controls')

# Exercise the actual shell guard too: the first native run exposed a missing
# status check in a runner which intentionally does not use `set -e`.
root = Path(__file__).resolve().parents[3]
runner = (root / 'tests/headless/run.sh').read_text()
start = runner.index('        if ! python3 "$ROOT_DIR/tests/render-startup/native/check.py"')
end = runner.index('        fi\n', start) + len('        fi\n')
guard = runner[start:end]
with tempfile.TemporaryDirectory(prefix='render-startup-oracle-') as directory:
    serial = Path(directory) / 'serial.log'
    for text, expected in ((base, 0), (bad[0], 1)):
        serial.write_text(text)
        result = subprocess.run(['bash', '-c', guard], capture_output=True,
                                env={**os.environ, 'ROOT_DIR': str(root),
                                     'SERIAL_LOG': str(serial)})
        assert result.returncode == expected, (expected, result.stderr)
print('RENDER-STARTUP: actual headless guard PASS success and rejection propagation')
