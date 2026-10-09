#!/usr/bin/env python3
"""Build fresh combined ANV/softpipe archives and a bundle in a new directory.

Run under Nix, holding the shared build lock unless the root is an isolated
workspace. Requires an already built CuBit runtime/libc. Does not stage or run
CuBit executables. The host tools are generators, never target payloads.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys
from native_mesa_targets import archives as select_archives


def sha(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path, help='new build directory; never overwritten')
    parser.add_argument('--jobs', type=int, default=4)
    parser.add_argument('--inside-host-shell', action='store_true', help=argparse.SUPPRESS)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    out = args.output.resolve()
    if not os.environ.get('IN_NIX_SHELL') or not 1 <= args.jobs <= 64:
        parser.error('use Nix and --jobs between 1 and 64')
    if not args.inside_host_shell:
        if out.exists():
            parser.error('output already exists; preserve it and choose a new directory')
        command = [sys.executable, str(Path(__file__).resolve()), str(out),
                   '--jobs', str(args.jobs), '--inside-host-shell']
        subprocess.run(['nix-shell', str(root / 'tests/mesa-anv/host-shell.nix'),
                        '--run', shlex.join(command)], cwd=root, check=True)
        return

    # Directory creation is exclusive even if another caller raced the outer check.
    out.mkdir(parents=False, exist_ok=False)
    inputs = {}
    for directory in ('tests/mesa-anv', 'userspace/mesa/anv'):
        for path in sorted((root / directory).iterdir()):
            if path.is_file() and path.suffix in ('.py', '.sh', '.nix', '.ini', '.patch', '.c', '.h', '.ads', '.adb', '.ld'):
                inputs[str(path)] = sha(path)
    for name in ('tests/mesa-software/prepare-cubit-source.sh',
                 'tests/mesa-software/cubit-platform.patch',
                 'tools/build_native_mesa.py', 'tools/build_mesa_service_bundle.py',
                 'tools/native_mesa_targets.py',
                 'tools/verify_mesa_service_bundle.py',
                 'userspace/runtime/adalib/libgnat-user.a',
                 'userspace/libc/build/sysroot/lib/libc.a'):
        path = root / name
        inputs[str(path)] = sha(path)
    record = {'status': 'BUILDING', 'inputs_sha256': inputs, 'commands': [],
              'scope': 'Fresh combined native ANV/softpipe archives and link check; no GPU execution'}
    manifest = out / 'build.json'

    def save():
        manifest.write_text(json.dumps(record, indent=2) + '\n')

    def run(command):
        command = list(map(str, command))
        record['commands'].append(command)
        save()
        subprocess.run(command, cwd=root, check=True)

    try:
        pristine = Path(subprocess.check_output(
            ['nix', 'eval', '--impure', '--raw', '--expr',
             'toString (import ./tests/mesa-anv/source.nix)'], cwd=root, text=True).strip())
        record['pristine_source'] = str(pristine)
        source, host, native = out / 'source', out / 'host', out / 'native'
        run(['bash', root / 'tests/mesa-anv/configure-host.sh', pristine, host])
        run(['ninja', '-C', host, '-j', args.jobs,
             'src/compiler/clc/mesa_clc', 'src/compiler/spirv/vtn_bindgen2'])
        run(['bash', root / 'tests/mesa-anv/prepare-cubit-source.sh', pristine, source])
        run(['bash', root / 'tests/mesa-anv/configure-cubit.sh', source, native, host, 'softpipe'])
        commands = json.loads((native / 'compile_commands.json').read_text())
        if sum(item['file'].endswith('/st_manager.c') for item in commands) != 1:
            raise RuntimeError('missing unique Gallium frontend ABI compile recipe')
        targets = json.loads((native / 'meson-info/intro-targets.json').read_text())
        archives = select_archives(targets, native)
        record['selected_archives'] = [str(p.relative_to(native)) for p in archives]
        if not archives or any(not p.is_relative_to(native) or p.suffix != '.a' for p in archives):
            raise RuntimeError('invalid native static archive targets')
        run(['ninja', '-C', native, '-j', args.jobs,
             *(str(p.relative_to(native)) for p in archives)])
        run([sys.executable, root / 'tools/build_mesa_service_bundle.py', native, out / 'bundle', '--root', root])
        run([sys.executable, root / 'tools/verify_mesa_service_bundle.py', out / 'bundle'])
        for name, expected in inputs.items():
            if sha(Path(name)) != expected:
                raise RuntimeError('build input changed: ' + name)
        record['status'] = 'LINK_PASS'
        record['bundle'] = str(out / 'bundle')
    except BaseException as error:
        record['status'] = 'FAILED'
        record['error'] = repr(error)
        save()
        raise
    save()
    print(out / 'bundle')


if __name__ == '__main__':
    main()
