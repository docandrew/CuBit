#!/usr/bin/env python3
"""Hosted ABI/owner adapter test; native compile only, no GPU execution."""
from pathlib import Path
import argparse
import json
import hashlib
import importlib.util
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('mesa_source', type=Path)
parser.add_argument('--bundle', type=Path, required=True)
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
inputs = [root / 'userspace/mesa' / name for name in
          ('mesa_service.ads', 'mesa_service.adb', 'service-device.h')]
inputs += [root / 'tests/mesa-anv' / name for name in
           ('service-ada-mock.c', 'service_ada_tests.adb', 'test-service-ada.py')]
hashes = {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs}
out = Path(tempfile.mkdtemp(prefix='service-ada.', dir=root / 'tests/mesa-anv/target'))
def run(command, cwd=out):
    subprocess.run(list(map(str, command)), cwd=cwd, check=True)
run(['cc', '-std=c11', '-UNDEBUG', '-I' + str(args.mesa_source.resolve() / 'include'),
     '-c', root / 'tests/mesa-anv/service-ada-mock.c', '-o', out / 'mock.o'])
run(['gnatmake', '-q', '-gnat2022', '-gnatp', '-O2', '-I' + str(root / 'userspace/mesa'),
     root / 'tests/mesa-anv/service_ada_tests.adb', '-o', out / 'test', '-largs', out / 'mock.o'])
run([out / 'test'])
native = out / 'native'
native.mkdir()
run(['gnatmake', '-q', '-c', '-gnatA', '-gnat2022', '-O2', '-mno-red-zone', '-fno-pic',
     '--RTS=' + str(root / 'userspace/runtime'), root / 'userspace/mesa/mesa_service.adb'], native)
spec = importlib.util.spec_from_file_location('bundle', root / 'tools/verify_mesa_service_bundle.py')
bundle = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bundle)
prefix, flags = bundle.verify(args.bundle)
main = native / 'main.c'
main.write_text('int main(void) { return 0; }\n')
run([*prefix[:-1], 'c', '-c', main, '-o', native / 'main.o'])
elf = native / 'service-ada-link.app'
run([*prefix, native / 'main.o', native / 'mesa_service.o', *flags, '-o', elf])
defined = {line.split()[-1] for line in subprocess.check_output(
    ['nm', '--defined-only', str(elf)], text=True).splitlines() if line.split()}
assert all('mesa_service__' + name in defined for name in
           ('start', 'accepted', 'borrow', 'health', 'close'))
assert not subprocess.check_output(['nm', '-u', str(elf)], text=True).strip()
bundle.verify(args.bundle)
assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest() == value for p, value in hashes.items())
(out / 'result.json').write_text(json.dumps({'hosted_abi_lifecycle': 'PASS',
    'native_compile': 'PASS', 'native_link': 'PASS', 'gpu_executed': False,
    'inputs_sha256': hashes, 'native_elf_sha256': hashlib.sha256(elf.read_bytes()).hexdigest()}, indent=2) + '\n')
print(out)
