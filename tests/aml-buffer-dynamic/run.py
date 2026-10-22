#!/usr/bin/env python3
"""Explicit hosted dynamic Buffer milestone; requires the repository Nix shell."""
import argparse
import hashlib
import json
import math
import resource
import signal
import sys
import tempfile
import time
from pathlib import Path
from hosted_capture import capture
ROOT=Path(__file__).resolve().parents[2]
HERE=Path(__file__).resolve().parent
REF=ROOT/'tests/aml-buffer-dynamic-reference/reference'

def interrupted(signum,frame):raise InterruptedError('dynamic Buffer suite interrupted')
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
print('dynamic Buffer logs:',logs,flush=True)
sha=lambda path:hashlib.sha256(path.read_bytes()).hexdigest()
inputs={}
def pin(path,expected=None):
    path=path.resolve(strict=True);h=sha(path)
    if expected is not None and h!=expected:raise ValueError('initial source mismatch: '+str(path))
    if str(path) in inputs and inputs[str(path)]!=h:raise ValueError('source hash collision')
    inputs[str(path)]=h
for directory in ['userspace/lib/acpi','userspace/services/acpi','shared/firmware','tests/aml-buffer-dynamic','tests/aml-buffer-dynamic-reference']:
    for path in (ROOT/directory).rglob('*'):
        relative=path.relative_to(ROOT)
        if any(part.startswith('build-') or part in ('results','__pycache__','evidence') for part in relative.parts):continue
        if path.is_file() and path.suffix in ('.adb','.ads','.gpr','.py','.json'):pin(path)
pin(ROOT/'tests/acpi-hosted/run.py')
manifest=REF/'reference.json';pin(manifest)
for name,h in json.loads(manifest.read_bytes())['inputs'].items():
    rel=Path(name);path=REF/rel
    if rel.is_absolute() or '..' in rel.parts or any(part.is_symlink() for part in [path,*path.parents]):raise ValueError('unsafe reference path')
    if not path.resolve(strict=True).is_relative_to(REF):raise ValueError('reference escape')
    pin(path,h)
(logs/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
report={'mode':a.mode,'state':'running','outcomes':[],'scope':'Hosted dynamic Buffer: owner boundary/GC fixtures plus 20 cached ACPICA comparisons (16 integer flows, 2 divide errors, 2 coarse target-only errors). Malformed/depth/budget policies are local tests. No full proof or native deployment claim.'}
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
try:
    run('reference-provenance',[sys.executable,'-B',str(REF/'verify.py')],30)
    run('versions',['gprbuild','--version'],30)
    run('dynamic-build',['gprbuild','-j1','-p','-P',str(HERE/(a.mode+'.gpr'))],180)
    run('buffer_dynamic_tests',[str(HERE/('build-'+a.mode)/'buffer_dynamic_tests')],min(a.test_timeout,30))
    replay=REF.parent
    run('replay-build',['gprbuild','-j1','-p','-P',str(replay/(a.mode+'.gpr'))],180)
    run('reference-guard-tests',[sys.executable,'-B',str(REF/'test_guards.py')],30)
    run('reference-protocol-tests',[sys.executable,'-B',str(REF/'test_compare.py')],30)
    run('cached-acpica-20',[sys.executable,'-B',str(REF/'run.py'),'--runner',str(replay/('build-'+a.mode)/'buffer_dynamic_reference_runner'),'--output',str(logs/'reference-results'),'--source-manifest',str(logs/'inputs.json')],None)
    report['state']='passed'
except BaseException as error:report.update(state='failed',error=repr(error));raise
finally:save()
print('Hosted dynamic Buffer passed:',logs,flush=True)
