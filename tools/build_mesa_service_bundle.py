#!/usr/bin/env python3
"""Build a native Mesa service link bundle; run in Nix under build.lock.

No executable is staged or run. The link-check ELF has no GPU authority.
Consumers use link-args.json after checking inputs.json hashes and retain their
own binder, manifest, assets and compositor adapters. Paths are absolute.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def compiler_command(entry):
    args = entry.get('arguments') or shlex.split(entry['command'])
    result, skip = [], False
    for arg in args:
        if skip:
            skip = False
        elif arg in ('-o', '-MF', '-MQ', '-MT'):
            skip = True
        elif arg not in ('-MD', '-MMD', '-MP', '-c') and not arg.endswith('/anv_kmd_backend.c'):
            result.append(arg)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build', type=Path)
    parser.add_argument('output', type=Path, help='new, nonexistent directory')
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[1])
    args = parser.parse_args()
    if not os.environ.get('IN_NIX_SHELL'):
        raise SystemExit('Run inside the pinned Nix development shell')
    root, build, out = args.root.resolve(), args.build.resolve(), args.output.resolve()
    metadata_path = build / 'meson-info/meson-info.json'
    metadata = json.loads(metadata_path.read_text())
    if Path(metadata['directories']['build']).resolve() != build:
        raise SystemExit('Configured build path does not match')
    source = Path(metadata['directories']['source']).resolve()
    if 'cubit_mesa_build_id_for_address(addr)' not in (source / 'src/util/build_id.c').read_text():
        raise SystemExit('Missing native static build-ID adaptation')
    tracked = {}

    def track(path):
        path = Path(path).resolve()
        value = digest(path)
        if str(path) in tracked and tracked[str(path)] != value:
            raise SystemExit('Input changed during inventory: ' + str(path))
        tracked[str(path)] = value
        return path

    track(metadata_path)
    targets_file = track(build / 'meson-info/intro-targets.json')
    commands_file = track(build / 'compile_commands.json')
    targets = json.loads(targets_file.read_text())
    libraries = sorted({Path(name).resolve() for target in targets
                        if target['type'] == 'static library' for name in target['filename']})
    aggregate = build / 'src/intel/vulkan/libvulkan_intel.a'
    if aggregate not in libraries or any(not p.is_relative_to(build) or p.suffix != '.a'
                                         for p in libraries):
        raise SystemExit('Invalid native static archive inventory')
    for path in libraries:
        track(path)
    native = root / 'userspace/mesa/anv'
    prepared = source / 'src/intel/vulkan'
    for name in ('anv_cubit_memory.c', 'anv_cubit_memory.h', 'anv_cubit_physical.c',
                 'anv_cubit_physical.h', 'native_gpu_mapping.c', 'native_gpu_mapping.h'):
        track(prepared / name)
    for owned in sorted(native.iterdir()):
        if owned.suffix not in ('.c', '.h', '.ads', '.adb', '.ld'):
            continue
        track(owned)
        copied = prepared / owned.name
        if copied.is_file() and owned.suffix in ('.c', '.h'):
            track(copied)
            if tracked[str(owned.resolve())] != tracked[str(copied.resolve())]:
                raise SystemExit('Stale prepared transport: ' + owned.name)
    service = root / 'userspace/mesa'
    for name in ('service-device.c', 'service-device.h', 'device-bootstrap.h', 'launch-session.h'):
        track(service / name)
    runtime = track(root / 'userspace/runtime/adalib/libgnat-user.a')
    wrapper = track(root / 'tests/mesa-anv/native-compiler.sh')
    sysroot = root / 'userspace/libc/build/sysroot/lib'
    for name in ('libc.a', 'cubit-crt1.o', 'crti.o', 'crtn.o', 'cubit.ld'):
        track(sysroot / name)
    track(root / 'userspace/libc/build/cross-gcc')
    # Capture header state before compilation, not just headers visible after
    # a successful compile. Archive hashes establish binary identity, not that
    # these libraries were rebuilt from every current upstream source file.
    for directory, suffixes in (
        (source, ('.h',)), (build, ('.h',)),
        (root / 'userspace/runtime', ('.ads', '.adb', '.ali')),
        (root / 'userspace/libc/build/sysroot/include', ('.h',))):
        for path in sorted(directory.rglob('*')):
            if path.is_file() and path.suffix in suffixes:
                track(path)
    entries = [e for e in json.loads(commands_file.read_text())
               if e['file'].endswith('/vulkan/anv_kmd_backend.c')]
    if len(entries) != 1:
        raise SystemExit('Expected one native ANV backend compiler entry')
    entry = entries[0]
    out.mkdir(parents=False, exist_ok=False)
    commands = []

    def run(command, cwd=out):
        command = list(map(str, command))
        commands.append({'command': command, 'cwd': str(cwd)})
        with (out / 'build.log').open('a') as log:
            subprocess.run(command, cwd=cwd, stdout=log, stderr=subprocess.STDOUT, check=True)

    objects = []
    obj = out / 'service-device.o'
    run(compiler_command(entry) + ['-I' + str(native), '-c', service / 'service-device.c', '-o', obj],
        Path(entry['directory']))
    objects.append(obj)
    for name in ('native_build_id', 'native_build_id_link'):
        obj = out / (name + '.o')
        run(['bash', wrapper, 'c', '-c', native / (name + '.c'), '-o', obj])
        objects.append(obj)
    for name in ('native_gpu_buffers', 'native_gpu_memory', 'native_gpu_query'):
        run(['gnatmake', '-q', '-c', '-gnatA', '-gnat2022', '-O2', '-mno-red-zone', '-fno-pic',
             '--RTS=' + str(root / 'userspace/runtime'), '-I' + str(native), native / (name + '.adb')])
        objects.append(out / (name + '.o'))
    symbols = ['cubit_mesa_service_' + name for name in ('start', 'device', 'status', 'close')]
    dispatch = ['vk_common_' + name for name in ('GetPhysicalDeviceProperties2', 'CreateFramebuffer',
                'DestroyFramebuffer', 'CreatePipelineLayout', 'DestroyPipelineLayout',
                'QueueSubmit', 'CmdCopyImageToBuffer')]
    link_args = [*map(str, objects), *('-Wl,--undefined=' + name for name in symbols),
                 '-Wl,--build-id=sha1', '-Wl,--start-group', '-Wl,--whole-archive', str(aggregate),
                 '-Wl,--no-whole-archive', *(str(p) for p in libraries if p != aggregate),
                 str(runtime), '-Wl,--end-group']
    # This main does not invoke Vulkan, synthesize replies or acquire authority.
    check_source = out / 'link-check.c'
    check_source.write_text('int main(void) { return 0; }\n')
    check_obj, elf = out / 'link-check.o', out / 'link-check.app'
    run(['bash', wrapper, 'c', '-c', check_source, '-o', check_obj])
    run(['bash', wrapper, 'cpp', check_obj, *link_args, '-o', elf])
    defined = {line.split()[-1] for line in subprocess.check_output(
        ['nm', '--defined-only', str(elf)], text=True).splitlines() if line.split()}
    missing = set(symbols + dispatch) - defined
    undefined = subprocess.check_output(['nm', '-u', str(elf)], text=True).strip()
    if missing or undefined:
        raise SystemExit('Incomplete final ELF: ' + repr((sorted(missing), undefined)))
    for path, expected in tracked.items():
        if digest(path) != expected:
            raise SystemExit('Input changed during build: ' + path)
    (out / 'link-args.json').write_text(json.dumps(link_args, indent=2) + '\n')
    (out / 'inputs.json').write_text(json.dumps({
        'inputs_sha256': tracked, 'objects_sha256': {str(p): digest(p) for p in objects},
        'commands': commands, 'link_prefix': ['bash', str(wrapper), 'cpp'],
        'required_symbols': symbols + dispatch, 'executed': False,
        'link_check_sha256': digest(elf), 'link_args_sha256': digest(out / 'link-args.json'),
        'status': 'LINK_PASS',
    }, indent=2) + '\n')
    print(out, flush=True)


if __name__ == '__main__':
    main()

