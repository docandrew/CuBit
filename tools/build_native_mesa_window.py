"""Build an inventoried native non-cube Mesa window fixture; never stage or boot."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', required=True, type=Path,
                        help='CuBit tree providing fixture sources and frozen native prerequisites')
    parser.add_argument('--mesa-build', required=True, type=Path,
                        help='verified build_native_mesa.py combined ANV+softpipe output')
    parser.add_argument('--continued', action='store_true',
                        help='require verified continuation.json chained to retained failed build.json')
    parser.add_argument('output', type=Path, help='new exclusive output directory')
    args = parser.parse_args()
    if not os.environ.get('IN_NIX_SHELL'):
        parser.error('Nix environment required')
    root, build, output = args.root.resolve(), args.mesa_build.resolve(), args.output.resolve()
    # Verification belongs to this builder version, not to frozen application
    # prerequisites whose older verifier may not understand continuation data.
    verifier_tools = Path(__file__).resolve().parent
    sys.path.insert(0, str(verifier_tools))
    from verify_native_mesa_build import check
    from verify_mesa_service_bundle import verify
    bundle = build / 'bundle'
    check(build, bundle, True, continued=args.continued)
    prefix, flags = verify(bundle)
    # Never reuse an output, including one containing a failed prior build.
    output.mkdir(exist_ok=False)
    tracked, commands = {}, []
    record = {'status': 'FAILED', 'variant': 'non-cube', 'executed': False,
              'hardware_validated': False, 'root': str(root), 'mesa_build': str(build),
              'continued': args.continued,
              'inputs_sha256': tracked, 'commands': commands}

    def track(path):
        path = Path(path).resolve()
        tracked[str(path)] = digest(path)
        return path

    def run(command, stdout=None):
        command = list(map(str, command))
        commands.append(command)
        subprocess.run(command, cwd=output, check=True, stdout=stdout)

    try:
        track(__file__)
        for path in (bundle / 'inputs.json', bundle / 'link-args.json', build / 'build.json',
                     build / 'native/compile_commands.json',
                     verifier_tools / 'verify_native_mesa_build.py',
                     verifier_tools / 'verify_mesa_service_bundle.py',
                     verifier_tools / 'native_mesa_targets.py'):
            track(path)
        if args.continued:
            track(build / 'continuation.json')
        helper = track(root / 'tests/mesa-software/compile-native-probe.py')
        manifest_tool = track(root / 'userspace/ccl/build/manifest/ccl-manifest')
        catalog = track(root / 'userspace/ccl/catalogs/native-runtime-services.ccl')
        manifest = track(root / 'tests/mesa-software/window-manifest.ccl')
        for folder in ('tests/mesa-software', 'userspace/c'):
            for path in sorted((root / folder).rglob('*.h')):
                track(path)
        objects = []
        for name in ('native-mesa-window', 'buffer-winsys'):
            source = track(root / ('tests/mesa-software/' + name + '.c'))
            obj = output / (name + '.o')
            run([sys.executable, helper, build / 'native', source, obj])
            objects.append(obj)
        with (output / 'window-manifest.S').open('w') as stream:
            run([manifest_tool, catalog, manifest], stream)
        run(['as', '--64', output / 'window-manifest.S', '-o', output / 'window-manifest.o'])
        elf = output / 'native-mesa-window.app'
        run([*prefix, *objects, *flags, '--manifest', output / 'window-manifest.o', '-o', elf])
        if subprocess.check_output(['nm', '-u', str(elf)], text=True).strip():
            raise ValueError('unresolved fixture symbols')
        symbols = {line.split()[-1] for line in subprocess.check_output(
            ['nm', '--defined-only', str(elf)], text=True).splitlines() if line.split()}
        if not {'main', 'softpipe_create_screen', 'cubit_mesa_service_start'} <= symbols:
            raise ValueError('missing fixture/engine symbols')
        check(build, bundle, True, continued=args.continued)
        for name, expected in tracked.items():
            if digest(name) != expected:
                raise ValueError('fixture input changed: ' + name)
        record['output_sha256'] = {str(path): digest(path) for path in
            [*objects, output / 'window-manifest.S', output / 'window-manifest.o', elf]}
        record['status'] = 'LINK_PASS'
    except Exception as error:
        record['error'] = str(error)
        raise
    finally:
        (output / 'fixture.json').write_text(json.dumps(record, indent=2) + '\n')
    print(json.dumps({'fixture': str(elf), 'sha256': digest(elf), 'executed': False}))


if __name__ == '__main__':
    main()
