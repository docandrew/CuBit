"""Build native trace observers/clients from a recorded private source snapshot.

Use Nix and hold the source checkout's build lock while snapshotting/building.
Runtime, font archive and startup object are explicit prebuilt inputs.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--collector', choices=('raw', 'archive', 'interrupted'), default='raw',
                   help='archive saves a bounded file; interrupted exits after its first successful write')
    p.add_argument('--root', type=Path, required=True)
    p.add_argument('--toolchain-root', type=Path, required=True)
    p.add_argument('--manifest-compiler', type=Path, required=True)
    p.add_argument('--catalog', type=Path, required=True)
    p.add_argument('--schema', type=Path, required=True)
    p.add_argument('output', type=Path)
    args = p.parse_args()
    if not os.environ.get('IN_NIX_SHELL'):
        p.error('Use Nix')
    root, out = args.root.resolve(), args.output.resolve()
    if out == root or root in out.parents:
        p.error('Output must be outside the source checkout')
    out.mkdir(parents=True, exist_ok=False)
    tree, inputs, commands = out / 'tree', {}, []
    def digest(path):
        return hashlib.sha256(path.read_bytes()).hexdigest()
    def copy(source, target):
        data = source.read_bytes()
        inputs[str(source)] = hashlib.sha256(data).hexdigest()
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(data)
    for relative in ('userspace/runtime/gnat', 'userspace/runtime/adalib',
                     'userspace/lib/ui', 'userspace/lib/theme',
                     'userspace/lib/compositor', 'userspace/lib/display',
                     'userspace/allocator/src'):
        for source in sorted((root / relative).rglob('*')):
            if source.is_file() and not any(part.startswith('build') for part in source.relative_to(root / relative).parts[:-1]):
                copy(source, tree / source.relative_to(root))
    for relative in ('userspace/runtime/ada_source_path', 'userspace/runtime/ada_object_path',
                     'userspace/runtime/runtime.xml', 'userspace/runtime/target_properties',
                     'userspace/c/link.ld', 'userspace/c/build/crt0.o',
                     'userspace/rust/build/font-native/libcubit_fonts.a'):
        copy(root / relative, tree / relative)
    app = tree / 'tests/compositor/trace-native'
    for source in Path(__file__).parent.rglob('*'):
        if source.suffix in ('.adb', '.ads', '.gpr', '.ccl'):
            copy(source, app / source.relative_to(Path(__file__).parent))
    inputs[str(Path(__file__).resolve())] = digest(Path(__file__).resolve())
    if args.collector != 'raw':
        for name in ('main.adb', 'manifest.ccl'):
            copy(Path(__file__).parent / 'archive' / name, app / name)
        (app / 'archive_test_control.ads').write_text(
            'package Archive_Test_Control with Pure is\n'
            '   Exit_After_First_Write : constant Boolean := ' +
            ('True' if args.collector == 'interrupted' else 'False') + ';\n'
            'end Archive_Test_Control;\n')
    for source in (args.manifest_compiler, args.catalog, args.schema):
        inputs[str(source.resolve())] = digest(source)
    (out / 'inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')
    def run(command, **kw):
        command = list(map(str, command))
        commands.append(command)
        (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        subprocess.run(command, cwd=args.toolchain_root / 'kernel', check=True, **kw)
    for manifest, build, project in (('manifest.ccl', 'build', 'observer.gpr'),
                                     ('client-manifest.ccl', 'build/client', 'native_dpi_client.gpr'),
                                     ('summary/manifest.ccl', 'summary/build', 'summary/observer.gpr')):
        directory = app / build
        (directory / 'generated').mkdir(parents=True, exist_ok=True)
        with (directory / 'manifest.S').open('w') as stream:
            run([args.manifest_compiler, args.catalog, app / manifest,
                 '--schema', args.schema, '--ada-output', directory / 'generated/ccl_manifest_bindings.ads'], stdout=stream)
        run(['alr', 'exec', '--', 'gcc', '-c', directory / 'manifest.S', '-o', directory / 'manifest.o'])
        run(['alr', 'exec', '--', 'gprbuild', '-p', '-P', app / project])
    binaries = (app / 'build/desktop-trace-observer.app', app / 'build/client/trace-publication.app', app / 'summary/build/desktop-metrics-observer.app')
    for path, expected in inputs.items():
        if digest(Path(path)) != expected:
            raise RuntimeError('Input changed: ' + path)
    (out / 'result.json').write_text(json.dumps({
        'status': 'BUILD_PASS', 'executed': False, 'collector': args.collector,
        'scope': 'Native link only with recorded runtime/font/startup seeds',
        'binaries': {str(path): digest(path) for path in binaries}}, indent=2) + '\n')

if __name__ == '__main__':
    main()
