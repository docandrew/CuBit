#!/usr/bin/env python3
"""Guarded hosted direct-store regressions; no native hardware execution."""
from pathlib import Path
import argparse, hashlib, json, os, re, shutil, subprocess, tempfile

suite = Path(__file__).resolve().parent
root = suite.parents[1]
ap = argparse.ArgumentParser()
ap.add_argument('--mode', choices=['checked', 'release'], default='release')
ap.add_argument('--validate-only', action='store_true')
args = ap.parse_args()
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()
catalog = json.loads((suite/'sources.json').read_text())
pins = {str(suite/'sources.json'): sha(suite/'sources.json')}
files = {}
for row in catalog['inputs']:
    rel = Path(row['path'])
    assert not rel.is_absolute() and '..' not in rel.parts
    p = root/rel
    assert p.is_file() and not p.is_symlink() and sha(p) == row['sha256'], p
    pins[str(p)] = sha(p)
    if p.suffix in ('.ads', '.adb'):
        assert p.name not in files, 'duplicate Ada basename: '+p.name
        files[p.name] = p
compiler = Path(catalog['gprbuild']['path'])
assert str(compiler).startswith('/nix/store/') and sha(compiler) == catalog['gprbuild']['sha256']
pins[str(compiler)] = sha(compiler)
if args.validate_only:
    print(json.dumps(dict(validated=True, source_inputs=len(catalog['inputs']), executed=False)))
    raise SystemExit
assert os.environ.get('TMPDIR') == '/home/doc/cubit-build-tmp', 'Export TMPDIR before Nix'
out = Path(tempfile.mkdtemp(prefix='acpi-direct-stores-', dir=os.environ['TMPDIR']))
(out/'source').mkdir()
for name, p in files.items():
    shutil.copy2(p, out/'source'/name)
    pins[str(out/'source'/name)] = sha(p)
flags = ['-O1', '-gnat2022', '-gnatwa', '-gnatwe']
flags += ['-gnata', '-gnato'] if args.mode == 'checked' else ['-gnatp']
project = out/'hosted.gpr'
project.write_text('project Hosted is\nfor Source_Dirs use ("source");for Object_Dir use "obj";for Exec_Dir use "bin";for Main use ("direct_store_tests.adb");\npackage Compiler is for Default_Switches ("Ada") use ('+','.join(json.dumps(x) for x in flags)+');end Compiler;\nend Hosted;\n')
pins[str(project)] = sha(project)
runs = []
def verify():
    for p, h in pins.items():
        assert sha(p) == h, 'input drift: '+p
def save(done):
    (out/'RESULT.json').write_text(json.dumps(dict(completed=done, profile=args.mode,
        runs=runs, pins=pins, checks=180 if done else 0, passed=180 if done else 0,
        percent=100 if done else None, native_executed=False, new_proof=False,
        scope='Actual namespace owner/objects/VM bodies; byte/value/origin, owner validity and capacity failure regressions. Hosted only, no whole-service proof.'), indent=2)+'\n')
def run(label, argv, seconds):
    verify()
    result = subprocess.run(['timeout', '--kill-after=2s', str(seconds)+'s',
        'prlimit', '--stack=134217728', '--as=25769803776', *map(str,argv)],
        cwd=out, capture_output=True, text=True, timeout=seconds+3)
    log = out/(label+'.log')
    log.write_text(result.stdout+result.stderr)
    runs.append(dict(label=label, argv=list(map(str,argv)), status=result.returncode,
                     log=str(log), log_sha256=sha(log)))
    save(False)
    assert result.returncode == 0, log
    verify()
    return log.read_text()
run('build', [compiler, '-p', '-j2', '-P', project], 180)
for p in (out/'obj').glob('*.ali'):
    if p.name.startswith('b__'):
        continue
    lines = p.read_text().splitlines()
    for flag in flags[1:]:
        assert 'A '+flag in lines, (p,flag)
    if args.mode == 'release':
        assert 'A -gnata' not in lines and 'A -gnato' not in lines
    pins[str(p)] = sha(p)
binary = out/'bin/direct_store_tests'
pins[str(binary)] = sha(binary)
text = run('direct-store-tests', [binary], 30)
assert re.search(r'Owned namespace explicit checks:\s*180 PASS', text), text
verify()
save(True)
print(json.dumps(dict(accepted=True, mode=args.mode, checks=180, percent=100,
                     receipt=str(out/'RESULT.json'))))
