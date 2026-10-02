#!/usr/bin/env python3
"""Compile native factory against Mesa types; run hosted lifecycle mocks.

SOURCE is a fresh prepared tree, BUILD an existing configured native build.
This does not run Vulkan, CuBit IPC or the GPU.
"""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
source, build = [Path(p).resolve() for p in sys.argv[1:]]
spec = importlib.util.spec_from_file_location('policy', root / 'tests/mesa-anv/test-native-memory-policy.py')
policy = importlib.util.module_from_spec(spec)
spec.loader.exec_module(policy)
entries = json.loads((build / 'compile_commands.json').read_text())
entry, = [e for e in entries if e['file'].endswith('/vulkan/anv_kmd_backend.c')]
cwd = Path(entry['directory'])
original = (cwd / entry['file']).resolve().parents[3]
command = policy.compiler_command(entry)
for i, arg in enumerate(command):
    if arg.startswith('-I'):
        path = (cwd / arg[2:]).resolve()
        if path.is_relative_to(original):
            command[i] = '-I' + str(source / path.relative_to(original))
out = Path(tempfile.mkdtemp(prefix='cubit-physical.'))
native = root / 'userspace/mesa/anv'
objects = []
for path in [native / 'anv_cubit_physical.c', root / 'tests/mesa-anv/native-physical-test.c']:
    obj = out / (path.stem + '.o')
    subprocess.run(command + ['-UNDEBUG', '-iquote', str(native), '-I' + str(native),
                             '-c', str(path), '-o', str(obj)], cwd=cwd, check=True)
    objects.append(str(obj))
subprocess.run(['cc', '-Wl,--gc-sections', *objects, '-o', str(out / 'test')], check=True)
subprocess.run([str(out / 'test')], check=True, timeout=60)
print('Native factory compile + hosted lifecycle PASS (not GPU execution):', out)
objects = []
for path in [source / 'src/intel/vulkan/anv_physical_device.c',
             root / 'tests/mesa-anv/budget-dispatch-test.c']:
    obj = out / (path.stem + '.o')
    subprocess.run(command + ['-UNDEBUG', '-c', str(path), '-o', str(obj)],
                   cwd=cwd, check=True)
    objects.append(str(obj))
subprocess.run(['cc', '-Wl,--gc-sections', *objects,
                str(build / 'src/vulkan/util/libvulkan_util.a'),
                '-o', str(out / 'dispatch')], check=True)
subprocess.run([str(out / 'dispatch')], check=True, timeout=60)
print('Actual ANV budget entrypoint dispatch PASS (host only):', out)
handoff = out / 'handoff.o'
subprocess.run(command + ['-UNDEBUG', '-I' + str(native), '-c',
                         str(root / 'tests/mesa-anv/native-device-handoff-test.c'),
                         '-o', str(handoff)], cwd=cwd, check=True)
subprocess.run(['cc', '-Wl,--gc-sections', str(handoff),
                '-o', str(out / 'handoff')], check=True)
subprocess.run([str(out / 'handoff')], check=True, timeout=60)
