"""Nix-only isolated archive stream, plot, and foreign-adapter regressions."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('output', type=Path)
p.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[3])
p.add_argument('--toolchain-root', type=Path)
p.add_argument('--prove', action='store_true')
a = p.parse_args()
if not os.environ.get('IN_NIX_SHELL'):
    p.error('Use nix develop -c python3 ...')
root = a.root.resolve()
toolchain = (a.toolchain_root or root).resolve()
out = a.output.resolve()
if out == root or root in out.parents:
    p.error('Use a fresh private output directory outside the checkout')
out.mkdir(parents=True, exist_ok=False)
src = out / 'source'
src.mkdir()
inputs = {}
def copy(source, target):
    data = source.read_bytes()
    inputs[str(source)] = hashlib.sha256(data).hexdigest()
    target.write_bytes(data)
files = list((root / 'userspace/ccl/src').glob('*.ad?'))
files += list((root / 'userspace/lib/compositor').glob('compositor_*trace*.ad?'))
files += [root / 'userspace/lib/compositor' / name for name in
          ('compositor_elapsed.ads', 'compositor_requests.ads', 'compositor_requests.adb')]
for pattern in ('observatory_trace_*.ad?', 'observatory_archive_*.ad?',
                'observatory_history.ad?', 'observatory_query_lifetime.ad?'):
    files += list((root / 'userspace/lib/observatory').glob(pattern))
files += [root / 'userspace/runtime/gnat' / name for name in
          ('cubit.ads', 'cubit-metric_records.ads', 'cubit-metric_records.adb',
           'cubit-metric_protocol.ads', 'cubit-failures.ads', 'cubit-failures.adb')]
for source in files:
    assert not (src / source.name).exists(), source
    copy(source, src / source.name)
fixture = out / 'native.cubittrace'
copy(root / 'tests/compositor/trace-archive/fixtures/native201.cubittrace', fixture)
package = Path(__file__).resolve().parent
for source in [*package.glob('*_tests.adb'), * (package / 'mocks').glob('*.ad?')]:
    assert not (src / source.name).exists(), source
    copy(source, src / source.name)
copy(Path(__file__).resolve(), out / 'runner.py')
project = out / 'archive_io.gpr'
project.write_text('''project Archive_IO is
 for Source_Dirs use ("source");
 for Object_Dir use "obj";
 for Exec_Dir use ".";
 for Main use ("stream_tests.adb", "reader_tests.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
 end Compiler;
end Archive_IO;
''')
(out / 'inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')
commands = []
def run(command, name):
    commands.append(list(map(str, command)))
    (out / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
    with (out / name).open('w') as log:
        subprocess.run(command, cwd=toolchain / 'kernel', stdout=log,
                       stderr=subprocess.STDOUT, check=True)
run(['alr', 'exec', '--', 'gprbuild', '-p', '-P', str(project), '-j2'], 'build.log')
for name in ('stream', 'reader'):
    run([str(out / (name + '_tests')), str(fixture)], name + '.log')
if a.prove:
    run(['alr', 'exec', '--', 'gnatprove', '-P', str(project), '-u',
         'observatory_archive_stream.adb', 'observatory_trace_plot.adb',
         'observatory_query_lifetime.adb', '--level=2', '--timeout=30', '-j2',
         '--report=all', '--checks-as-errors=on'], 'proof.log')
for source, digest in inputs.items():
    assert hashlib.sha256(Path(source).read_bytes()).hexdigest() == digest, source
(out / 'result.json').write_text(json.dumps({
    'status': 'PASS', 'proof_requested': a.prove,
    'scope': 'Hosted policy and mocked foreign filesystem adapter; native ABI verified separately',
    'stream': (out / 'stream.log').read_text().strip(),
    'reader': (out / 'reader.log').read_text().strip()}, indent=2) + '\n')
print((out / 'result.json').read_text())
