#!/usr/bin/env python3
"""Hosted native-type consumer regression with mocked Desktop/transport only."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
build = Path(sys.argv[1]).resolve()
spec = importlib.util.spec_from_file_location(
    'policy', root / 'tests/mesa-anv/test-native-memory-policy.py')
policy = importlib.util.module_from_spec(spec)
spec.loader.exec_module(policy)
entries = json.loads((build / 'compile_commands.json').read_text())
entry, = [e for e in entries if e['file'].endswith('/vulkan/anv_kmd_backend.c')]
out = Path(tempfile.mkdtemp(prefix='cubit-triangle-consumer.'))
for size in (64, 256):
    obj = out / f'test-{size}.o'
    exe = out / f'test-{size}'
    subprocess.run(policy.compiler_command(entry) + [
        '-UNDEBUG', f'-DCUBIT_TEST_FRAME_SIZE={size}', '-I' + str(root / 'userspace/mesa/anv'),
        '-c', str(root / 'tests/mesa-anv/triangle-present-test.c'), '-o', str(obj)],
        cwd=entry['directory'], check=True)
    subprocess.run(['cc', '-Wl,--gc-sections', str(obj), '-o', str(exe)], check=True)
    subprocess.run([str(exe)], check=True, timeout=20)
print('Hosted consumer evidence:', out, '(NOT native execution)')
