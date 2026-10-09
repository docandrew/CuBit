#!/usr/bin/env python3
"""Test/prove raw metric history using a frozen private source copy and fresh output."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--source-root', type=Path, default=Path(__file__).resolve().parents[3])
parser.add_argument('--toolchain-root', type=Path)
parser.add_argument('--output', type=Path, required=True)
parser.add_argument('--no-prove', action='store_true')
args = parser.parse_args()
if not os.environ.get('IN_NIX_SHELL'):
    parser.error('Run inside nix develop')
root = args.source_root.resolve()
toolchain = (args.toolchain_root or root).resolve()
output = args.output.resolve()
output.mkdir(parents=True, exist_ok=False)
inputs, commands = {}, []

def copy(source, target):
    data = source.read_bytes()
    inputs[str(source)] = hashlib.sha256(data).hexdigest()
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_bytes(data)

def run(argv, expected_failure=False):
    argv = list(map(str, argv))
    commands.append(argv)
    with (output / 'commands.log').open('a') as log:
        result = subprocess.run(argv, cwd=toolchain / 'kernel', stdout=log, stderr=log)
    if (result.returncode == 0) == expected_failure:
        raise RuntimeError('Unexpected command result: ' + repr(argv))

def project(directory, mains):
    names = ', '.join('"' + name + '"' for name in mains)
    (directory / 'suite.gpr').write_text('project Suite is\n'
        ' for Source_Dirs use ("source");\n for Object_Dir use "obj";\n'
        ' for Exec_Dir use ".";\n for Main use (' + names + ');\n'
        ' package Compiler is\n for Default_Switches ("Ada") use '
        '("-gnat2022", "-gnata", "-gnato");\n end Compiler;\nend Suite;\n')

runtime = root / 'userspace/runtime/gnat'
fixtures = root / 'tests/metrics/raw-history'
common = ['cubit.ads', 'cubit-protocols.ads', 'cubit-log_records.ads',
          'cubit-log_records.adb', 'cubit-log_protocol.ads',
          'cubit-metric_records.ads', 'cubit-metric_records.adb',
          'cubit-metric_protocol.ads', 'cubit-metric_raw_validation.ads',
          'cubit-metric_raw_validation.adb']
core, observer = output / 'core', output / 'observer'
for directory in (core, observer):
    for name in common:
        copy(runtime / name, directory / 'source' / name)
for name in ('cubit-metric_batches.ads', 'cubit-metric_batches.adb'):
    copy(runtime / name, core / 'source' / name)
for name in ('metric_store', 'metric_histograms', 'metric_history', 'metric_raw_query'):
    for suffix in ('.ads', '.adb'):
        copy(root / 'userspace/services/metricsvc' / (name + suffix),
             core / 'source' / (name + suffix))
copy(root / 'tests/metrics/main.adb', core / 'source/main.adb')
for name in ('raw_check.adb', 'query_check.adb', 'history_check.adb', 'trace_group_check.adb', 'trace_assembly_check.adb', 'trace_stream_check.adb'):
    copy(fixtures / name, core / 'source' / name)
for name in ('cubit-metric_raw_observer.ads', 'cubit-metric_raw_observer.adb'):
    copy(runtime / name, observer / 'source' / name)
for path in sorted((fixtures / 'observer-mocks').glob('*.ad?')):
    copy(path, observer / 'source' / path.name)
copy(fixtures / 'observer_check.adb', observer / 'source/check.adb')
copy(fixtures / 'validation_check.adb', observer / 'source/validation_check.adb')
for stem in ('compositor_trace_wire', 'compositor_trace_metrics', 'compositor_trace_stream', 'compositor_input_trace',
             'compositor_source_trace', 'compositor_render_trace', 'compositor_frame_trace'):
    for suffix in ('.ads', '.adb'):
        copy(root / 'userspace/lib/compositor' / (stem + suffix), core / 'source' / (stem + suffix))
copy(root / 'userspace/lib/compositor/compositor_elapsed.ads', core / 'source/compositor_elapsed.ads')
project(core, ('main.adb', 'raw_check.adb', 'query_check.adb', 'history_check.adb', 'trace_group_check.adb', 'trace_assembly_check.adb', 'trace_stream_check.adb'))
project(observer, ('check.adb', 'validation_check.adb'))
(output / 'inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')
(output / 'result.json').write_text('{"status":"INCOMPLETE"}\n')
try:
    for directory, programs in ((core, ('main', 'raw_check', 'query_check', 'history_check', 'trace_group_check', 'trace_assembly_check', 'trace_stream_check')),
                                (observer, ('check', 'validation_check'))):
        run(['alr', 'exec', '--', 'gprbuild', '-q', '-p', '-P', directory / 'suite.gpr'])
        for program in programs:
            run([directory / program])
    if not args.no_prove:
        run(['alr', 'exec', '--', 'gnatprove', '-P', core / 'suite.gpr', '-u',
             'metric_store.adb', 'metric_raw_query.adb', 'cubit-metric_raw_validation.adb',
             'cubit-metric_records.adb', 'cubit-metric_batches.adb',
             'compositor_trace_wire.adb', 'compositor_trace_metrics.adb', 'compositor_trace_stream.adb',
             '--level=2', '--timeout=30', '--counterexamples=off', '--report=all',
             '--checks-as-errors=on', '-j2'])
    # Compile before expecting a failure: a compiler error is never a killed mutant.
    negative = output / 'negative'
    shutil.copytree(observer, negative, ignore=shutil.ignore_patterns('obj', 'check', 'validation_check'))
    path = negative / 'source/cubit-metric_raw_validation.adb'
    text = path.read_text()
    marker = 'not P.Is_Publisher (Page (I) (2))'
    assert marker in text
    path.write_text(text.replace(marker, 'False'))
    run(['alr', 'exec', '--', 'gprbuild', '-q', '-p', '-P', negative / 'suite.gpr'])
    run([negative / 'validation_check'], expected_failure=True)
    for path, expected in inputs.items():
        assert hashlib.sha256(Path(path).read_bytes()).hexdigest() == expected, path
    result = {'status': 'PASS', 'scope': 'Hosted policy/adapter tests; kernel IPC and grants mocked',
              'proof_run': not args.no_prove, 'negative': 'forged publisher acceptance rejected'}
except Exception as error:
    (output / 'result.json').write_text(json.dumps({'status': 'FAIL', 'reason': str(error)}, indent=2) + '\n')
    raise
else:
    (output / 'result.json').write_text(json.dumps(result, indent=2) + '\n')
finally:
    (output / 'commands.json').write_text(json.dumps(commands, indent=2) + '\n')
