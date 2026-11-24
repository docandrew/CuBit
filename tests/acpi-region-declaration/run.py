#!/usr/bin/env python3
"""Guarded hosted declaration-matching regression; no native build."""
from pathlib import Path
import argparse, hashlib, json, os, subprocess, time

here = Path(__file__).resolve().parent
repo = here.parents[1]
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
parser = argparse.ArgumentParser()
parser.add_argument('--output', type=Path)
parser.add_argument('--validate-only', action='store_true')
args = parser.parse_args()
catalog = json.loads((here / 'sources.json').read_text())
pins = {str(repo / x['path']): x['sha256'] for x in catalog['inputs']}
pins.update({str(here / x['path']): x['sha256'] for x in catalog['tests']})
def verify():
    for p, h in pins.items():
        if sha(Path(p)) != h:
            raise RuntimeError('Input drift: ' + p)
verify()
if args.validate_only:
    print(json.dumps({'validated': True, 'production': len(catalog['inputs']),
                      'tests': len(catalog['tests']), 'executed': False}))
    raise SystemExit(0)
if not args.output or not args.output.is_absolute():
    parser.error('--output requires an absolute, new private directory')
out = args.output
out.mkdir(exist_ok=False, parents=True)
src = out / 'source'
src.mkdir()
names = {}
for p, h in list(pins.items()):
    path = Path(p)
    if path.suffix not in ('.ads', '.adb'):
        continue
    if path.name in names and names[path.name] != h:
        raise RuntimeError('Conflicting Ada unit: ' + path.name)
    names[path.name] = h
    copy = src / path.name
    copy.write_bytes(path.read_bytes())
    pins[str(copy)] = h
project = out / 'test.gpr'
project.write_text('project Test is for Source_Dirs use ("source"); '
                  'for Main use ("route_tests.adb"); for Object_Dir use "obj"; '
                  'for Exec_Dir use "bin"; package Compiler is '
                  'for Default_Switches ("Ada") use ("-O1","-gnat2022","-gnata","-gnato"); '
                  'end Compiler; end Test;\n')
pins[str(project)] = sha(project)
runs = []
def save(accepted):
    (out / 'RESULT.json').write_text(json.dumps({
        'accepted': accepted, 'pins': pins, 'runs': runs,
        'scope': 'Hosted descriptor predicate; runtime and assertion checks enabled. '
                 'No native/hardware execution or whole-unit proof claim.'}, indent=2) + '\n')
def run(label, argv, seconds):
    verify()
    start = time.monotonic()
    proc = subprocess.run(['timeout', '--kill-after=2s', str(seconds),
                           'prlimit', '--as=25769803776', '--stack=134217728', *argv],
                          cwd=out, capture_output=True, text=True, timeout=seconds + 5)
    output = proc.stdout + proc.stderr
    (out / (label + '.log')).write_text(output)
    runs.append({'label': label, 'argv': argv, 'status': proc.returncode,
                 'seconds': time.monotonic() - start})
    save(False)
    verify()
    if proc.returncode:
        raise RuntimeError(label + ' failed; see ' + str(out / (label + '.log')))
    return output
run('build', ['gprbuild', '-q', '-p', '-j2', '-P', str(project)], 300)
exe = out / 'bin/route_tests'
pins[str(exe)] = sha(exe)
output = run('tests', [str(exe)], 30)
if output != 'Descriptor checks 270\n':
    raise RuntimeError('Unexpected check count: ' + repr(output))
save(True)
print(json.dumps({'accepted': True, 'passed': 270, 'total': 270, 'percent': 100,
                  'result': str(out / 'RESULT.json')}))
