"""Build the Desktop GPU scene archive against native CuBit Ada and musl.

Run inside Nix through kernel/alr. All outputs use a private, hashed snapshot.
This checks native compilation and elaboration, not final linking or GPU use.
"""
from pathlib import Path
import argparse
import hashlib
import json
import os
import re
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--mesa-source', type=Path, required=True, help='Existing Mesa source tree containing include/vulkan')
parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[2])
parser.add_argument('--project', type=Path, default=Path(__file__).with_name('desktop_gpu_scene_native.gpr'))
args = parser.parse_args()
assert os.environ.get('IN_NIX_SHELL'), 'Use Nix and kernel/alr'
root = args.root.resolve()
output = root / 'tests/compositor/build'
output.mkdir(exist_ok=True)
snapshot = Path(tempfile.mkdtemp(prefix='desktop-gpu-native-', dir=output))
inputs = {}

def digest(data):
    return hashlib.sha256(data).hexdigest()

def copy(source, relative):
    data = source.read_bytes()
    target = snapshot / relative
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(data)
    assert source.read_bytes() == data, f'Changed while copying: {source}'
    inputs[str(source)] = {'sha256': digest(data), 'copy': str(relative)}

def verify():
    for source, entry in inputs.items():
        assert digest(Path(source).read_bytes()) == entry['sha256'], source
        assert digest((snapshot / entry['copy']).read_bytes()) == entry['sha256'], entry['copy']

def run(command, cwd=snapshot):
    shown = command if command[0] != 'ar' else [*command[:3], f'{len(command) - 3} objects']
    print('RUN', *map(str, shown), flush=True)
    subprocess.run(list(map(str, command)), cwd=cwd, check=True,
                   env={**os.environ, 'NIX_HARDENING_ENABLE': ''})

print(snapshot, flush=True)
result = {'status': 'INCOMPLETE', 'scope': 'native Ada elaboration and musl C bridge archive; no final link, boot or GPU execution'}
try:
    directories = ['userspace/lib/compositor', 'userspace/lib/display',
                   'userspace/services/desktop', 'userspace/lib/theme', 'userspace/mesa']
    names = re.findall(r'"([^"/]+\.ad[bs])"', args.project.read_text())
    assert len(names) == len(set(names)) and 'desktop_gpu_scene.adb' in names
    for name in names:
        sources = [root / directory / name for directory in directories if (root / directory / name).is_file()]
        assert len(sources) == 1, (name, sources)
        copy(sources[0], sources[0].relative_to(root))
    for directory in ['gnat', 'gnarl', 'adalib']:
        for source in sorted((root / 'userspace/runtime' / directory).rglob('*')):
            if source.is_file():
                copy(source, source.relative_to(root))
    for name in ['ada_source_path', 'ada_object_path', 'runtime.xml', 'target_properties']:
        source = root / 'userspace/runtime' / name
        copy(source, source.relative_to(root))
    project = Path('tests/compositor/desktop_gpu_scene_native.gpr')
    copy(args.project.resolve(), project)
    c_names = ['vulkan_checker', 'vulkan_affine', 'vulkan_backdrop', 'vulkan_context', 'vulkan_copy',
               'vulkan_device_storage', 'vulkan_owned_image', 'vulkan_owned_target_binding',
               'vulkan_sources', 'vulkan_submission_native', 'vulkan_targets',
               'vulkan_upload_buffer', 'vulkan_upload_record']
    for name in c_names:
        source = root / 'userspace/lib/compositor' / (name + '.c')
        copy(source, source.relative_to(root))
    for source in sorted((root / 'userspace/lib/compositor').glob('*.h')):
        copy(source, source.relative_to(root))
    for name in ['vulkan_affine.vert', 'vulkan_affine.frag', 'vulkan_checker.frag']:
        source = root / 'userspace/lib/compositor' / name
        copy(source, source.relative_to(root))
    for name in ['userspace/mesa/service-device.h', 'tests/compositor/build-vulkan-affine-shaders.py',
                 'tests/mesa-anv/native-compiler.sh', 'userspace/libc/build/cross-gcc']:
        copy(root / name, Path(name))
    for source in sorted((root / 'userspace/libc/build/sysroot/include').rglob('*')):
        if source.is_file(): copy(source, source.relative_to(root))
    mesa_include = args.mesa_source.resolve() / 'include'
    assert (mesa_include / 'vulkan/vulkan.h').is_file(), mesa_include
    for directory in ['vulkan', 'vk_video']:
        for source in sorted((mesa_include / directory).rglob('*')):
            if source.is_file(): copy(source, Path('headers/mesa') / source.relative_to(mesa_include))
    verify()
    (snapshot / 'inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')
    run(['gprbuild', '--version'])
    run(['gprbuild', '-q', '-p', '-c', '-P', project])
    objects = snapshot / 'tests/compositor/build/desktop-gpu-scene-native'
    runtime = snapshot / 'userspace/runtime'
    run(['gnatbind', '-n', '-Ldesktop_gpu_scene', '--RTS=' + str(runtime),
         'desktop_gpu_scene-backdrop.ali'], cwd=objects)
    run(['gnatmake', '-q', '-c', '-gnatA', '-gnat2022', '-O2', '-mno-red-zone',
         '-fno-pic', '-mno-sse', '-mno-sse2', '--RTS=' + str(runtime),
         'b~desktop_gpu_scene-backdrop.adb'], cwd=objects)
    run(['python3', 'tests/compositor/build-vulkan-affine-shaders.py', 'generated'])
    for name in c_names:
        run(['bash', 'tests/mesa-anv/native-compiler.sh', 'c', '-std=c11', '-O2',
             '-Wall', '-Wextra', '-Werror', '-Iheaders/mesa', '-Igenerated',
             '-Iuserspace/lib/compositor', '-c', 'userspace/lib/compositor/' + name + '.c',
             '-o', objects / (name + '_native.o')])
    archive = snapshot / 'libcubit-desktop-gpu-scene.a'
    run(['ar', 'rcs', archive, *sorted(objects.glob('*.o'))])
    with (snapshot / 'undefined-symbols.txt').open('w') as output:
        subprocess.run(['nm', '-u', archive], check=True, stdout=output)
    def symbols(options):
        lines = subprocess.check_output(['nm', '-g', *options, str(archive)], text=True).splitlines()
        return {line.split()[-1] for line in lines if line.split() and not line.endswith(':')}
    external = symbols(['-u']) - symbols(['--defined-only'])
    (snapshot / 'external-symbols.json').write_text(json.dumps(sorted(external), indent=2) + '\n')
    assert not any(name.startswith('cubit_vulkan_') for name in external), sorted(external)
    verify()
    result.update(status='PASS', inputs=len(inputs), archive=str(archive),
                  archive_sha256=digest(archive.read_bytes()))
    print('PASS native Desktop GPU scene archive:', archive, flush=True)
except Exception as error:
    result.update(status='FAIL', error=str(error))
    raise
finally:
    (snapshot / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
