#!/usr/bin/env python3
"""Generated shader mutants must fail the real Vulkan image oracle (host only)."""
from pathlib import Path
import os
import re
import shlex
import struct
import subprocess
import sys

root = Path(__file__).resolve().parent
artifacts = Path(sys.argv[1]).resolve()
header = (artifacts / 'teapot-assets.h').read_text()
shader = (root / 'gallery.vert').read_text()
mutants = {
    'frozen-all': (shader.replace('animation.frame.x *', '0.0 *'), 'frozen-cell=0'),
    'frozen-one': (shader.replace('animation.frame.x *',
                     '(id == 7 ? 0.0 : animation.frame.x) *'), 'frozen-cell=7'),
    'missing-one': (shader.replace('world_normal =',
                     'if (id == 7) gl_Position = vec4(5,5,0,1);\n    world_normal ='), 'missing-cell=7'),
}
flags = shlex.split(subprocess.check_output(
    ['pkg-config', '--cflags', '--libs', 'vulkan'], text=True))
for name, (source, expected) in mutants.items():
    assert source != shader
    out = artifacts / name
    out.mkdir()
    vertex = out / 'gallery.vert'
    vertex.write_text(source)
    binary = out / 'gallery.spv'
    subprocess.run(['glslangValidator', '-V', '--target-env', 'vulkan1.0',
                    str(vertex), '-o', str(binary)], check=True, stdout=subprocess.DEVNULL)
    subprocess.run(['spirv-val', '--target-env', 'vulkan1.0', str(binary)], check=True)
    data = binary.read_bytes()
    words = struct.unpack('<' + 'I' * (len(data)//4), data)
    replacement = 'static const uint32_t teapot_vertex[] = {' + ','.join(hex(w) for w in words) + '};'
    changed, count = re.subn(r'static const uint32_t teapot_vertex\[\] = \{.*?\};',
                             replacement, header, flags=re.S)
    assert count == 1
    (out / 'teapot-assets.h').write_text(changed)
    executable = out / 'test'
    subprocess.run(['cc', '-std=c11', '-O2', '-Wall', '-Wextra', '-Werror',
                    '-DCUBIT_TEAPOT_GALLERY=1', '-DCUBIT_TEAPOT_DETERMINISTIC=1',
                    '-DCUBIT_TEAPOT_FRAME_COUNT=16', '-I'+str(out),
                    str(root / 'host-test.c'), *flags, '-o', str(executable)], check=True)
    result = subprocess.run([str(executable)], capture_output=True, text=True,
                            timeout=60, env={**os.environ, 'TEAPOT_PPM': str(out/'image.ppm')})
    log = result.stdout + result.stderr
    (out / 'run.log').write_text(log)
    if result.returncode != 7 or ('GALLERY CHECK ' + expected) not in log:
        raise SystemExit(f'{name}: oracle failed to reject mutant; see {out}/run.log')
    if 'VULKAN VALIDATION errors=0 warnings=0' not in log:
        raise SystemExit(f'{name}: failure must be image-oracle based, not invalid Vulkan usage')
    print(f'Gallery negative control PASS: {name} ({expected})')
