"""Compile the real Mesa/device/context bridge in a private native snapshot.

Invoke inside Nix through kernel/alr. Does not link, boot or obtain authority.
"""
from pathlib import Path
import argparse, hashlib, json, os, subprocess, tempfile
root = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('mesa_source', type=Path)
args = parser.parse_args()
assert os.environ.get('IN_NIX_SHELL')
include = (args.mesa_source.resolve() / 'include').relative_to(root)
assert (root / include / 'vulkan/vulkan.h').is_file()
out = Path(tempfile.mkdtemp(prefix='vulkan-device-native-', dir=root / 'tests/compositor/build'))
inputs = {}
def copy(path):
    data = path.read_bytes()
    target = out / path.relative_to(root)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(data)
    inputs[str(path.relative_to(root))] = hashlib.sha256(data).hexdigest()
for rel in ('userspace/lib/compositor', 'userspace/lib/display', 'userspace/mesa'):
    for path in (root / rel).iterdir():
        if path.is_file() and path.suffix in ('.ads', '.adb', '.h', '.c'):
            copy(path)
for rel in ('userspace/runtime/gnat', 'userspace/runtime/adalib', str(include)):
    for path in (root / rel).rglob('*'):
        if path.is_file(): copy(path)
for rel in ('userspace/runtime/ada_source_path', 'userspace/runtime/ada_object_path',
            'userspace/runtime/runtime.xml', 'userspace/runtime/target_properties',
            'tests/compositor/vulkan_submission_native.gpr',
            'tests/compositor/vulkan_context_native.gpr', 'tests/compositor/vulkan_device_native.gpr'):
    copy(root / rel)
(out / 'inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')
print(out, flush=True)
result = {'status': 'INCOMPLETE', 'scope': 'native Ada/C component compilation only'}
try:
    subprocess.run(['gprbuild', '-q', '-c', '-p', '-P', 'tests/compositor/vulkan_device_native.gpr'],
                   cwd=out, check=True, env={**os.environ, 'NIX_HARDENING_ENABLE': ''})
    subprocess.run(['bash', str(root / 'tests/mesa-anv/native-compiler.sh'), 'c',
                    '-std=c11', '-O2', '-Wall', '-Wextra', '-Werror', '-I' + str(out / include),
                    '-c', str(out / 'userspace/lib/compositor/vulkan_device_storage.c'),
                    '-o', str(out / 'vulkan_device_storage.o')], check=True)
    drift = [p for p, h in inputs.items() if hashlib.sha256((root / p).read_bytes()).hexdigest() != h]
    assert not any(Path(p).name.startswith(('vulkan_device', 'mesa_service')) for p in drift), drift
    result.update(status='COMPILED', root_drift=drift)
finally:
    (out / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
