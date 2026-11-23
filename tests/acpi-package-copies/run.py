#!/usr/bin/env python3
"""Bounded hosted copy and invariant regressions; both profiles retain checks."""
from pathlib import Path
import argparse
import hashlib
import json
import resource
import shutil
import subprocess

here = Path(__file__).resolve().parent
ap = argparse.ArgumentParser()
ap.add_argument('--root', type=Path, default=here.parents[1])
ap.add_argument('--output', type=Path, default=here / 'build')
args = ap.parse_args()
root, output = args.root.resolve(), args.output.resolve()
output.mkdir(parents=True, exist_ok=True)
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
inputs = json.loads((here / 'sources.json').read_text())['inputs']

def check_inputs():
    for rel, expected in inputs.items():
        assert sha(root / rel) == expected, 'source drift: ' + rel

check_inputs()
compiler = Path(shutil.which('gprbuild') or '/missing').resolve()
assert str(compiler).startswith('/nix/store/'), 'Run in the pinned Nix development shell'
resource.setrlimit(resource.RLIMIT_STACK, (134217728, resource.getrlimit(resource.RLIMIT_STACK)[1]))
resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
runs = []
for profile in ('checked', 'release'):
    work = output / profile
    work.mkdir(exist_ok=True)
    switches = ['-O2', '-gnat2022', '-gnatwa', '-gnatwe']
    if profile == 'checked':
        switches += ['-gnata', '-gnato']
    assert '-gnatp' not in switches
    directories = [here, here / 'integer-shape', root / 'userspace/services/acpi/lib',
                   root / 'shared/firmware', root / 'shared/acpi']
    quote = lambda text: '"' + str(text).replace('"', '""') + '"'
    project = work / 'copy_tests.gpr'
    project.write_text('project Copy_Tests is\nfor Source_Dirs use (' + ','.join(map(quote, directories)) + ');\n'
        'for Object_Dir use "obj"; for Exec_Dir use "bin";\n'
        'for Main use ("copy_value_tests.adb", "integer_shape_test.adb");\n'
        'package Compiler is for Default_Switches ("Ada") use (' + ','.join(map(quote, switches)) + '); end Compiler;\n'
        'end Copy_Tests;\n')
    argv = [str(compiler), '-p', '-P', str(project), '-j2']
    log = work / 'build.log'
    with log.open('w') as stream:
        result = subprocess.run(argv, stdout=stream, stderr=subprocess.STDOUT, timeout=300)
    assert result.returncode == 0, log
    for name, marker, count in (
            ('copy_value_tests', 'package copy checks 337', 337),
            ('integer_shape_test', 'STRENGTHENED MODEL REJECTS INTEGER SIZE=1: PASS', 1)):
        executable = work / 'bin' / name
        log = work / (name + '.log')
        with log.open('w') as stream:
            result = subprocess.run([str(executable)], stdout=stream, stderr=subprocess.STDOUT, timeout=60)
        assert result.returncode == 0 and marker in log.read_text(), log
        check_inputs()
        runs.append(dict(profile=profile, argv=[str(executable)], checks=count, status=0,
                         switches=switches, executable_sha256=sha(executable),
                         log=str(log), log_sha256=sha(log)))
        print(profile, name, str(count) + '/' + str(count), '(100%)', flush=True)
check_inputs()
(output / 'RESULT.json').write_text(json.dumps(dict(completed=True, checks_passed=676, checks=676,
    percent=100, runs=runs, source_guards=inputs, compiler=str(compiler), compiler_sha256=sha(compiler),
    runtime_checks=True, proofs_run=False, native=False), indent=2) + '\n')
