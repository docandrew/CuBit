#!/usr/bin/env python3
"""Real Mesa producer -> frozen Ada scene bridge; Linux only, not CuBit execution.

Run in tests/compositor/vulkan-affine-shell.nix. All outputs are private.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('snapshot', type=Path)
p.add_argument('--teapot', action='store_true')
a = p.parse_args()
root = Path(__file__).resolve().parents[2]
snapshot = a.snapshot.resolve()
manifest = json.loads((snapshot / 'inputs.json').read_text())
if json.loads((snapshot / 'result.json').read_text()).get('status') != 'PASS':
    raise SystemExit('Incomplete bridge snapshot')
for name, info in manifest.items():
    copied = (snapshot / info['copy']).resolve()
    if not copied.is_relative_to(snapshot) or hashlib.sha256(copied.read_bytes()).hexdigest() != info['sha256']:
        raise SystemExit('Changed snapshot input: ' + name)
for name in ('vulkan_affine.h', 'vulkan_targets.h', 'vulkan_submission.h', 'vulkan_owned_image.h', 'compositor.h'):
    relative = 'userspace/lib/compositor/' + name
    if hashlib.sha256((root / relative).read_bytes()).hexdigest() != manifest[relative]['sha256']:
        raise SystemExit('Current scene adapter ABI differs from snapshot: ' + name)
out = Path(tempfile.mkdtemp(prefix='scene-host.', dir=root / 'tests/mesa-anv/target'))
generated = out / 'generated'
producer = 'scene-teapot-host.c' if a.teapot else 'scene-host.c'
inputs = [root / 'tests/mesa-anv' / name for name in (
    producer, 'scene_host.adb', 'native-scene-consumer.h', 'completed-image.h',
    'completed-image-host.h', 'native-triangle-probe.h', 'triangle-host-test.c')]
inputs += [root / 'tests/mesa-teapot' / name for name in ('render.h', 'host-test.c')]
inputs += [root / 'tests/compositor/native_scene_transfer.h']
inputs += [root / 'userspace/lib/compositor' / name for name in
           ('vulkan_affine.h', 'vulkan_targets.h', 'vulkan_submission.h', 'vulkan_owned_image.h', 'compositor.h')]
def hashes():
    return {str(path): hashlib.sha256(path.read_bytes()).hexdigest() for path in inputs}
before = hashes()
subprocess.run(['python3', str(root / 'tests/mesa-anv/build-triangle-shaders.py'), str(generated)], check=True)
subprocess.run(['python3', str(root / 'tests/compositor/build-vulkan-affine-shaders.py'), str(generated)], check=True)
if a.teapot:
    subprocess.run(['python3', str(root / 'tests/mesa-teapot/build-assets.py'), str(generated)], check=True)
project = out / 'scene_host.gpr'
fault_compile = '' if a.teapot else ', "-DCUBIT_SCENE_FAULTS=1"'
fault_link = '' if a.teapot else ', "-Wl,--wrap=cubit_vulkan_record_affine"'
project.write_text(f'''project Scene_Host extends "{snapshot}/tests/compositor/native_scene_bridge_native.gpr" is
   for Languages use ("Ada", "C");
   for Runtime ("Ada") use "";
   for Source_Dirs use ("{snapshot}/tests/compositor", "{snapshot}/userspace/lib/compositor",
     "{snapshot}/userspace/lib/display", "{snapshot}/userspace/runtime/gnat", "{root}/tests/mesa-anv");
   for Source_Files use Native_Scene_Bridge_Native'Source_Files &
     ("cubit.ads", "scene_host.adb", "{producer}", "vulkan_submission_native.c",
      "vulkan_targets.c", "vulkan_owned_image.c", "vulkan_owned_target_binding.c", "vulkan_affine.c", "vulkan_sources.c");
   for Object_Dir use "{out}/obj";
   for Exec_Dir use "{out}";
   for Main use ("scene_host.adb");
   package Compiler is
      for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
      for Default_Switches ("C") use ("-std=c11", "-O2", "-Wall", "-Wextra", "-Werror",
        "-DCUBIT_TEST_SCENE=1", "-I{generated}"{fault_compile});
   end Compiler;
   package Linker is
      for Default_Switches ("Ada") use ("-lvulkan"{fault_link});
   end Linker;
end Scene_Host;
''')
subprocess.run(['alr', 'exec', '--', 'gprbuild', '-q', '-p', '-P', str(project)], cwd=root / 'kernel', check=True)
env = dict(os.environ, VK_DRIVER_FILES=os.environ['MESA_DRIVER_ROOT'] + '/share/vulkan/icd.d/lvp_icd.x86_64.json',
           XDG_DATA_DIRS=os.environ['MESA_DRIVER_ROOT'] + '/share', TEAPOT_PPM=str(out / 'composed-teapot.ppm'))
with (out / 'positive.log').open('w') as log:
    subprocess.run([str(out / 'scene_host')], env=env, stdout=log, stderr=subprocess.STDOUT, timeout=60, check=True)
positive = (out / 'positive.log').read_text()
pixels = 65536 if a.teapot else 4096
if positive.count(f'MESA-SCENE composed pixels={pixels} mismatches=0') != 8:
    raise SystemExit('Missing eight exact composed frames: ' + str(out))
if 'VULKAN VALIDATION errors=0 warnings=0' not in positive:
    raise SystemExit('Missing clean validation verdict: ' + str(out))
if not a.teapot:
    for label, settings, marker in (
        ('cancel', {'CUBIT_SCENE_FAIL_RECORD': '1'}, 'scene record cancelled cleanly'),
        ('output-map', {'CUBIT_SCENE_FAIL_MAP': '1'}, 'scene mapping failure retired cleanly'),
        ('baseline-map', {'CUBIT_SCENE_FAIL_MAP': '2'}, 'scene mapping failure retired cleanly')):
        with (out / (label + '.log')).open('w') as log:
            subprocess.run([str(out / 'scene_host')], env=dict(env, **settings),
                           stdout=log, stderr=subprocess.STDOUT, timeout=60, check=True)
        recovery = (out / (label + '.log')).read_text()
        if (marker not in recovery or recovery.count('MESA-SCENE composed pixels=4096 mismatches=0') != 8 or
                'VULKAN VALIDATION errors=0 warnings=0' not in recovery):
            raise SystemExit('Missing fault retirement/reopen evidence: ' + label)
    with (out / 'negative.log').open('w') as log:
        negative = subprocess.run([str(out / 'scene_host')], env=dict(env, CUBIT_TEST_STRIP_SAMPLED='1'),
                                  stdout=log, stderr=subprocess.STDOUT, timeout=60)
    if negative.returncode != 1 or 'VUID-VkImageMemoryBarrier-oldLayout-01211' not in (out / 'negative.log').read_text():
        raise SystemExit('Missing sampled-usage negative control: ' + str(out))
if hashes() != before:
    raise SystemExit('Producer/adapter sources changed during test')
(out / 'inputs.json').write_text(json.dumps(before, indent=2) + '\n')
(out / 'result.json').write_text(json.dumps({'status': 'PASS', 'hosted_only': True,
    'teapot': a.teapot, 'cycles': 8, 'exact_pixels': pixels * 8,
    'fault_recovery_runs': 0 if a.teapot else 3, 'snapshot': str(snapshot)}, indent=2) + '\n')
print('HOST ONLY Mesa source/Ada scene PASS:', out)
