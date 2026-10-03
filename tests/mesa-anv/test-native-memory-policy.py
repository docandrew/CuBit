#!/usr/bin/env python3
"""Run hosted memory-policy regressions with configured native Mesa types.

Uses compile_commands.json for headers and configuration, but links a hosted
executable with mocked IPC. This does not execute on CuBit or validate GPU
cache coherence. Run inside the pinned Nix development environment.
"""
import argparse
import json
from pathlib import Path
import shlex
import subprocess
import tempfile


def compiler_command(entry):
    args = entry.get('arguments') or shlex.split(entry['command'])
    result = []
    skip = False
    for arg in args:
        if skip:
            skip = False
            continue
        if arg in ('-o', '-MF', '-MQ', '-MT'):
            skip = True
        elif arg not in ('-MD', '-MMD', '-MP', '-c') and not arg.endswith('/anv_kmd_backend.c'):
            result.append(arg)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build', type=Path)
    parser.add_argument('--root', type=Path)
    args = parser.parse_args()
    root = args.root.resolve() if args.root else Path(__file__).resolve().parents[2]
    build = args.build.resolve()
    entries = [entry for entry in json.loads((build / 'compile_commands.json').read_text())
               if entry['file'].endswith('/vulkan/anv_kmd_backend.c')]
    if len(entries) != 1:
        parser.error('Expected exactly one configured ANV backend compilation')
    entry = entries[0]
    cwd = Path(entry['directory'])
    command = compiler_command(entry)
    out = Path(tempfile.mkdtemp(prefix='cubit-memory-policy.'))
    tests = root / 'tests/mesa-anv'
    native = root / 'userspace/mesa/anv'
    fixtures = {
        'transport-failure-capture': [tests / 'transport-failure-capture-test.c'],
        'cleanup-status': [native / 'anv_cubit_memory.c', tests / 'cleanup-status-test.c'],
        'service-probe-progress': [tests / 'service-probe-progress-test.c'],
        'service-device': [root / 'userspace/mesa/service-device.c',
                           tests / 'service-device-test.c'],
        'launch-session': [tests / 'launch-session-test.c'],
        'mapping-lifetime': [native / 'native_gpu_mapping.c',
                             tests / 'mapping-lifetime-test.c'],
        'memory-info': [native / 'cubit-device-query.c', native / 'cubit-memory-info.c',
                        tests / 'memory-info-test.c'],
        'session-attach': [native / 'anv_cubit_memory.c', tests / 'session-attach-test.c'],
        'session-status': [native / 'anv_cubit_memory.c', tests / 'session-status-test.c'],
        'memory-lifecycle': [native / 'anv_cubit_memory.c', tests / 'memory-lifecycle-test.c'],
        'submission-lifecycle': [native / 'anv_cubit_memory.c', tests / 'submission-lifecycle-test.c'],
        'concurrent-submission': [native / 'anv_cubit_memory.c', tests / 'concurrent-submission-test.c'],
        'slab-submission': [native / 'anv_cubit_memory.c', tests / 'slab-submission-test.c'],
        'state-table-backing': [native / 'anv_cubit_state_table.c',
                                tests / 'state-table-backing-test.c'],
    }
    for name in ('device', 'timestamp', 'budget', 'memory', 'vm'):
        fixtures[name + '-query'] = [native / 'cubit-device-query.c',
                                    tests / ('cubit-' + name + '-query-test.c')]
    fixtures['native-query-adapter'] = [native / 'cubit-device-native.c',
                                        native / 'cubit-device-query.c',
                                        tests / 'cubit-device-native-test.c']
    print('Hosted mock-IPC regression artifacts:', out, flush=True)
    # Device-info/topology need the full Mesa device-info library to link.
    # Compile every production discovery adapter with the configured target
    # command here; separate finalizer/topology fixtures exercise semantics.
    adapters = out / 'adapters'
    adapters.mkdir()
    for name in ('cubit-device-query', 'cubit-device-info', 'cubit-device-native',
                 'cubit-topology', 'cubit-memory-info'):
        subprocess.run(command + ['-iquote', str(native), '-I' + str(native), '-c', str(native / (name + '.c')),
                       '-o', str(adapters / (name + '.o'))], cwd=cwd, check=True)
    for name, sources in fixtures.items():
        directory = out / name
        directory.mkdir()
        objects = []
        for source in sources:
            obj = directory / (source.stem + '.o')
            subprocess.run(command + ['-UNDEBUG', '-iquote', str(native),
                           '-I' + str(tests), '-I' + str(native),
                           '-c', str(source), '-o', str(obj)], cwd=cwd, check=True)
            objects.append(str(obj))
        binary = directory / 'test'
        wrappers = (['-Wl,--wrap=calloc', '-Wl,--wrap=realloc']
                    if name in ('memory-lifecycle', 'session-attach') else [])
        subprocess.run(['cc', '-Wl,--gc-sections', *wrappers, *objects, '-o', str(binary)], check=True)
        if name == 'service-device':
            # Static production owner is deliberately never reset/reused.
            for scenario in range(19):
                subprocess.run([str(binary), str(scenario)], check=True, timeout=60)
        else:
            subprocess.run([str(binary)], check=True, timeout=60)
    print(f'PASS: five adapter compiles, {len(fixtures)} hosted fixtures; '
          'NOT native or hardware execution')


if __name__ == '__main__':
    main()
