#!/usr/bin/env python3
"""Explicit hosted metadata milestone; requires the repository Nix shell."""
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
REF=HERE/'reference'

def interrupted(signum,frame):raise InterruptedError('metadata suite interrupted')
signal.signal(signal.SIGTERM,interrupted)
p=argparse.ArgumentParser()
p.add_argument('--mode',choices=['release','checked'],default='release')
p.add_argument('--test-timeout',type=float,default=180)
a=p.parse_args()
if not math.isfinite(a.test_timeout) or a.test_timeout<=0:p.error('--test-timeout must be finite and positive')
resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,64*1024*1024))
print('HOSTED_STACK_LIMIT_BYTES',resource.getrlimit(resource.RLIMIT_STACK),flush=True)
result_root=HERE/'results';result_root.mkdir(exist_ok=True)
logs=Path(tempfile.mkdtemp(prefix=a.mode+'-',dir=result_root))
print('metadata logs:',logs,flush=True)
sha=lambda path:hashlib.sha256(path.read_bytes()).hexdigest()
inputs={}
def pin(path,expected=None):
    path=path.resolve(strict=True);h=sha(path)
    if expected is not None and h!=expected:raise ValueError('initial source mismatch: '+str(path))
    if str(path) in inputs and inputs[str(path)]!=h:raise ValueError('source hash collision')
    inputs[str(path)]=h
for directory in ['userspace/lib/acpi','userspace/services/acpi','shared/firmware','tests/aml-alias','tests/aml-metadata','tests/aml-metadata-normalization','tests/aml-metadata-owner','tests/aml-metadata-root','tests/aml-metadata-execute']:
    for path in (ROOT/directory).rglob('*'):
        relative=path.relative_to(ROOT)
        if any(part.startswith('build-') or part in ('results','__pycache__','evidence') for part in relative.parts):continue
        if path.is_file() and path.suffix in ('.adb','.ads','.gpr','.py','.json'):pin(path)
pin(ROOT/'tests/acpi-hosted/run.py')
manifest=REF/'reference-manifest.json';pin(manifest)
for name,h in json.loads(manifest.read_bytes()).items():
    rel=Path(name);path=REF/rel
    if rel.is_absolute() or '..' in rel.parts or any(part.is_symlink() for part in [path,*path.parents]):raise ValueError('unsafe reference path')
    if not path.resolve(strict=True).is_relative_to(REF):raise ValueError('reference escape')
    pin(path,h)
(logs/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
report={'mode':a.mode,'state':'running','outcomes':[],'scope':'Hosted metadata tests; 70 integer flows and 12 explicitly coarse error projections, 2 compiler rejections preserved. No full proof or native deployment claim.'}
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
    for folder,mains in [('aml-metadata-normalization',['metadata_normalization_tests']),('aml-metadata-owner',['metadata_namespace_auth_tests','metadata_retained_gc_tests','metadata_service_auth_tests']),('aml-metadata-root',['metadata_root_owner_tests','metadata_root_gc_tests']),('aml-metadata-execute',['metadata_executor_tests']),('aml-alias',['alias_namespace_tests','alias_runtime_tests','root_reference_tests'])]:
        base=ROOT/'tests'/folder
        run(folder+'-build',['gprbuild','-j1','-p','-P',str(base/(a.mode+'.gpr'))],180)
        for main in mains:run(main,[str(base/('build-'+a.mode)/main)],a.test_timeout)
    run('replay-build',['gprbuild','-j1','-p','-P',str(HERE/(a.mode+'.gpr'))],180)
    run('reference-guard-tests',[sys.executable,'-B',str(REF/'test_guards.py')],60)
    run('reference-protocol-tests',[sys.executable,'-B',str(REF/'test_compare.py')],180)
    # Replay has bounded individual children; no competing supervisor deadline.
    run('cached-acpica-82',[sys.executable,'-B',str(REF/'run.py'),'--runner',str(HERE/('build-'+a.mode)/'metadata_runner'),'--output',str(logs/'reference-results'),'--source-manifest',str(logs/'inputs.json')],None)
    report['state']='passed'
except BaseException as error:report.update(state='failed',error=repr(error));raise
finally:save()
print('Hosted metadata passed:',logs,flush=True)
