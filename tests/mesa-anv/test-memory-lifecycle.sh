#!/usr/bin/env bash
# Nix + shared build lock; actual prepared ANV types, hosted execution only.
set -euo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
python3 - "$root" "${1:?current prepared Mesa build required}" "${2:-memory-lifecycle-test.c}" "${3:-}" <<'PY'
import json, pathlib, shlex, subprocess, sys, tempfile
root = pathlib.Path(sys.argv[1])
build = pathlib.Path(sys.argv[2]).resolve()
entries = json.loads((build / 'compile_commands.json').read_text())
entry, = [e for e in entries if e['file'].endswith('/vulkan/anv_kmd_backend.c')]
args = shlex.split(entry['command'])
# Optional fresh prepared source, retaining generated headers in the build.
if sys.argv[4]:
    old_source = (build / entry['file']).resolve().parents[3]
    source = pathlib.Path(sys.argv[4]).resolve()
    for index, arg in enumerate(args):
        prefix = '-I' if arg.startswith('-I') else ''
        path = arg[2:] if prefix else arg
        if path.startswith('-'):
            continue
        absolute = (build / path).resolve()
        if absolute.is_relative_to(old_source):
            args[index] = prefix + str(source / absolute.relative_to(old_source))
clean = []
i = 0
while i < len(args):
    arg = args[i]
    if arg in ('-o', '-MF', '-MQ'):
        i += 2
        continue
    if arg not in ('-MD', '-c') and not arg.endswith('/anv_kmd_backend.c'):
        clean.append(arg)
    i += 1
out = pathlib.Path(tempfile.mkdtemp(prefix='memory-lifecycle.', dir=root / 'tests/mesa-anv/target'))
objects = []
sources = [('adapter', root / 'userspace/mesa/anv/anv_cubit_memory.c'),
           ('test', root / 'tests/mesa-anv' / sys.argv[3])]
if sys.argv[3] == 'mapping-drain-lifecycle-test.c':
    sources.append(('mapping', root / 'userspace/mesa/anv/native_gpu_mapping.c'))
for name, source in sources:
    obj = out / (name + '.o')
    subprocess.run(clean + ['-UNDEBUG', '-I' + str(root / 'userspace/mesa/anv'),
                            '-c', str(source), '-o', str(obj)], cwd=build, check=True)
    objects.append(str(obj))
link = ['cc', '-Wl,--gc-sections']
if sys.argv[3] in ('memory-lifecycle-test.c', 'session-attach-test.c'):
    # The allocation-failure fixture supplies __wrap_* hooks; without linker
    # wrapping its OOM assertions exercise real allocations instead.
    link += ['-Wl,--wrap=realloc', '-Wl,--wrap=calloc']
subprocess.run(link + [*objects, '-o', str(out / 'test')], check=True)
subprocess.run([str(out / 'test')], check=True)
print('Hosted lifecycle evidence:', out)
PY
