#!/usr/bin/env python3
"""Explicit hosted buffer-sizing milestone; requires the repository Nix shell."""
import argparse
import hashlib
import json
import math
import re
import shlex
import resource
import signal
import sys
import tempfile
import time
from pathlib import Path
from hosted_capture import capture
ROOT=Path(__file__).resolve().parents[2]
HERE=Path(__file__).resolve().parent
REF=ROOT/'tests/aml-buffer-reference/reference'

def interrupted(signum,frame):raise InterruptedError('buffer-sizing suite interrupted')
signal.signal(signal.SIGTERM,interrupted)
p=argparse.ArgumentParser()
p.add_argument('--mode',choices=['release','checked'],default='release')
p.add_argument('--test-timeout',type=float,default=30)
a=p.parse_args()
if not math.isfinite(a.test_timeout) or a.test_timeout<=0:p.error('--test-timeout must be finite and positive')
resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,64*1024*1024))
print('HOSTED_STACK_LIMIT_BYTES',resource.getrlimit(resource.RLIMIT_STACK),flush=True)
result_root=HERE/'results';result_root.mkdir(exist_ok=True)
logs=Path(tempfile.mkdtemp(prefix=a.mode+'-',dir=result_root))
print('buffer-sizing logs:',logs,flush=True)
sha=lambda path:hashlib.sha256(path.read_bytes()).hexdigest()
inputs={}
def pin(path,expected=None):
    path=path.resolve(strict=True);h=sha(path)
    if expected is not None and h!=expected:raise ValueError('initial source mismatch: '+str(path))
    if str(path) in inputs and inputs[str(path)]!=h:raise ValueError('source hash collision')
    inputs[str(path)]=h
# Derive every source directory from each checked-in project, including adapter
# dependencies. Projects and source files are pinned before any child starts.
projects=[ROOT/'tests'/folder/(a.mode+'.gpr') for folder in
          ['aml-buffer-layouts','aml-buffer-data','aml-buffer-integration',
           'aml-buffer-execute','aml-buffer-reference']]
adapters=json.loads((HERE/'adapter-paths.json').read_text())
if len(adapters)!=13 or len(set(adapters))!=13:raise ValueError('expected thirteen distinct adapters')
projects += [ROOT/'tests/aml-buffer-adapters'/f'adapter_{i}_{a.mode}.gpr' for i in range(13)]
directories={HERE,REF}
for project in projects:
    pin(project)
    match=re.search(r'for Source_Dirs use \((.*?)\);',project.read_text(),re.S)
    if match is None:raise ValueError('project source directories missing')
    for name in re.findall(r'"([^"\n]+)"',match.group(1)):
        directory=(project.parent/name).resolve(strict=True)
        if not directory.is_relative_to(ROOT):raise ValueError('source directory escape')
        directories.add(directory)
for directory in directories:
    for path in directory.iterdir():
        if path.is_file() and path.suffix in ('.adb','.ads','.gpr','.py','.json'):pin(path)
pin(ROOT/'tests/acpi-hosted/run.py')
manifest=REF/'reference-manifest.json';pin(manifest)
for name,h in json.loads(manifest.read_bytes()).items():
    rel=Path(name);path=REF/rel
    if rel.is_absolute() or '..' in rel.parts or any(part.is_symlink() for part in [path,*path.parents]):raise ValueError('unsafe reference path')
    if not path.resolve(strict=True).is_relative_to(REF):raise ValueError('reference escape')
    pin(path,h)
(logs/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
report={'mode':a.mode,'state':'running','outcomes':[],'scope':'Hosted buffer sizing: five mains, thirteen compile-only adapters, eighteen cached ACPICA comparisons. No full proof or native deployment claim.'}
def check():
    for path,h in inputs.items():
        if sha(Path(path))!=h:raise RuntimeError('input drift: '+path)
def save():(logs/'results.json').write_text(json.dumps(report,indent=2)+'\n')
def run(label,command,timeout):
    check();row={'label':label,'command':command,'state':'running','timeout_seconds':timeout};report['outcomes'].append(row);save();start=time.monotonic()
    try:
        code=capture(command,ROOT,logs/(label+'.log'),timeout)
        row.update(state='passed' if code==0 else 'failed',returncode=code)
        if code!=0:raise RuntimeError(label+': '+str(code))
        check()
    except BaseException as error:
        row.update(state='failed',error=repr(error));raise
    finally:row['elapsed_seconds']=time.monotonic()-start;save()
    return row
try:
    run('reference-provenance',[sys.executable,'-B',str(REF/'verify.py')],30)
    run('versions',['gprbuild','--version'],30)
    for folder,mains in [('aml-buffer-layouts',['buffer_layout_tests','buffer_allocation_tests']),('aml-buffer-data',['buffer_data_tests']),('aml-buffer-integration',['runtime_buffer_tests']),('aml-buffer-execute',['buffer_execute_tests'])]:
        base=ROOT/'tests'/folder
        run(folder+'-build',['gprbuild','-j1','-p','-P',str(base/(a.mode+'.gpr'))],180)
        for main in mains:run(main,[str(base/('build-'+a.mode)/main)],min(a.test_timeout,30))
    for index,source in enumerate(adapters):
        project=ROOT/'tests/aml-buffer-adapters'/f'adapter_{index}_{a.mode}.gpr'
        label=f'adapter-{index}'
        row=run(label,['gprbuild','-j1','-p','-c','-u','-f','-v','-P',str(project),Path(source).name],180)
        selected=set()
        for line in (logs/(label+'.log')).read_text().splitlines():
            tokens=shlex.split(line)
            if '-c' in tokens:selected.update(token for token in tokens if token.endswith('.adb'))
        row['selected_source_paths']=sorted(selected)
        if str(ROOT/source) not in selected:
            row['state']='failed';raise RuntimeError('adapter compiler source selection mismatch')
        save()
    replay=ROOT/'tests/aml-buffer-reference'
    run('replay-build',['gprbuild','-j1','-p','-P',str(replay/(a.mode+'.gpr'))],180)
    run('reference-guard-tests',[sys.executable,'-B',str(REF/'test_guards.py')],60)
    run('reference-protocol-tests',[sys.executable,'-B',str(REF/'test_compare.py')],180)
    # Replay bounds each child; avoid a competing supervisor deadline.
    run('cached-acpica-18',[sys.executable,'-B',str(REF/'run.py'),'--runner',str(replay/('build-'+a.mode)/'buffer_reference_runner'),'--output',str(logs/'reference-results'),'--source-manifest',str(logs/'inputs.json')],None)
    report['state']='passed'
except BaseException as error:report.update(state='failed',error=repr(error));raise
finally:save()
print('Hosted buffer sizing passed:',logs,flush=True)
