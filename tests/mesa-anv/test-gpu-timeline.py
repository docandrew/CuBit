#!/usr/bin/env python3
"""GPU-001 step 3, Linux-hosted: Mesa's session-queue submission and GPU
timeline sync with actual Mesa types (configured native ANV build), Mesa's
real vk_sync dispatcher, the proved Ada timeline logic compiled for the host,
host pthreads and a mock of the queue's C ABI (gpu-queue-mock.c). Also runs
the no-wait source check. Not native CuBit, not the driver, not a GPU.

Usage, in the Nix shell: test-gpu-timeline.py <native Mesa build> [fixture ...]
"""
import argparse
import json
from pathlib import Path
import shlex
import shutil
import subprocess
import tempfile

# Runtime specs the timeline unit's closure needs (CuBit.GPU_Queues).
ADA_RUNTIME = ('cubit.ads', 'cubit-protocols.ads', 'cubit-channel_contracts.ads',
               'cubit-channel_contracts.adb', 'cubit-channel_rings.ads', 'cubit-channel_rings.adb',
               'cubit-slot_rings.ads', 'cubit-slot_rings.adb', 'cubit-submission_queues.ads',
               'cubit-submission_queues.adb', 'cubit-gpu_queues.ads')
ADA_MESA = ('native_gpu_timeline.ads', 'native_gpu_timeline.adb')


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


def ada_objects(root, out):
    """The proved timeline unit, compiled for the host (no runtime calls: -gnatp)."""
    ada = out / 'ada'
    ada.mkdir()
    for name in ADA_RUNTIME:
        shutil.copy(root / 'userspace/runtime/gnat' / name, ada / name)
    for name in ADA_MESA:
        shutil.copy(root / 'userspace/mesa/anv' / name, ada / name)
    subprocess.run(['gnatmake', '-q', '-c', '-gnat2022', '-gnatp', '-O2', 'native_gpu_timeline.adb'],
                   cwd=ada, check=True)
    return [str(p) for p in sorted(ada.glob('*.o'))]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build', type=Path)
    parser.add_argument('fixtures', nargs='*')
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    build = args.build.resolve()
    entry, = [e for e in json.loads((build / 'compile_commands.json').read_text())
              if e['file'].endswith('/vulkan/anv_kmd_backend.c')]
    command, cwd = compiler_command(entry), Path(entry['directory'])
    tests, native = root / 'tests/mesa-anv', root / 'userspace/mesa/anv'
    # Fixtures: sources compiled with the configured Mesa command, linked
    # with the host Ada timeline objects.
    adapter = [native / 'anv_cubit_memory.c', native / 'anv_cubit_sync.c',
               tests / 'sync-support.c', tests / 'gpu-queue-mock.c']
    fixtures = {
        'gpu-timeline-sync': [tests / 'gpu-timeline-sync-test.c', tests / 'gpu-queue-mock.c'],
        'queue-real-sync': adapter + [tests / 'queue-real-sync-test.c'],
        'concurrent-submission': adapter + [tests / 'concurrent-submission-test.c'],
        'slab-submission': adapter + [tests / 'slab-submission-test.c'],
        'submission-lifecycle': adapter + [tests / 'submission-lifecycle-test.c'],
    }
    selected = args.fixtures or list(fixtures)
    out = Path(tempfile.mkdtemp(prefix='gpu-timeline.', dir=tests / 'target'))
    print('Hosted GPU timeline artifacts:', out, flush=True)
    ada = ada_objects(root, out)
    for name in selected:
        directory = out / name
        directory.mkdir()
        objects = []
        for source in fixtures[name]:
            obj = directory / (source.stem + '.o')
            subprocess.run(command + ['-UNDEBUG', '-iquote', str(native), '-I' + str(tests),
                                      '-I' + str(native), '-c', str(source), '-o', str(obj)],
                           cwd=cwd, check=True)
            objects.append(str(obj))
        binary = directory / 'test'
        subprocess.run(['cc', '-Wl,--gc-sections', *objects, *ada, '-lpthread', '-o', str(binary)],
                       check=True)
        subprocess.run([str(binary)], check=True, timeout=300)
    subprocess.run(['python3', str(tests / 'test-submit-no-wait.py')], check=True)
    print(f'PASS: {len(selected)} hosted GPU timeline fixtures; NOT native or hardware execution')


if __name__ == '__main__':
    main()
