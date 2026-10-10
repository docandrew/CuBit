"""Portable corrected-DSDT explicit-format differential gate; no raw String decoding."""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import subprocess
import resource
from byte_protocol import matches
ROOT = Path(__file__).resolve().parent

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def verify():
    manifest = json.loads((ROOT/'manifest.json').read_text())
    for name, expected in manifest.items():
        path = ROOT/name
        if not path.resolve().is_relative_to(ROOT) or digest(path) != expected:
            raise ValueError('Reference integrity: '+name)
    cases = json.loads((ROOT/'cases.json').read_text())
    assert len(cases) == 70 and len({(c['group'],c['key']) for c in cases}) == 70
    assert Counter(c['classification'] for c in cases) == {'returned':64,'runtime_error':6}
    assert Counter(c['expected']['status'] for c in cases if c['classification']=='runtime_error') == {'UNSUPPORTED_VALUE':6}
    for c in cases:
        prefix=ROOT/f"reference/group{c['group']}"
        if c['classification']=='compile_rejected':
            assert '6058' in (prefix/'results'/f"{c['key']}-compile.log").read_text()
            continue
        b=(ROOT/c['aml']).read_bytes()
        assert b[:4]==b'DSDT' and len(b)>=36 and int.from_bytes(b[4:8],'little')==len(b)
        assert sum(b)%256==0 and b[8]==c['revision']
        assert c['bits']==(32 if b[8]<2 else 64)
        assert c['mode'] in ('ToDecimalString','ToHexString')
        method=(prefix/'results'/f"{c['key']}-TEST.dsl").read_text()
        assert c['mode']+' (' in method
        observations=json.loads((prefix/'results/observations.json').read_text())
        row=next(r for r in observations if r['key']==c['key'])
        assert row['width_returncode']==0 and row['method_opcode_verified']
        assert row['width_witness']==[('0000000000000000' if c['bits']==32 else '00000000FFFFFFFF')]
        assert digest(ROOT/c['aml'])==row['aml_sha256']
    return cases

def main():
    ap=argparse.ArgumentParser();ap.add_argument('--mode',choices=['release','checked'],required=True)
    ap.add_argument('--runner',type=Path,required=True);ap.add_argument('--output',type=Path,required=True)
    args=ap.parse_args(); cases=verify(); runner=args.runner.resolve(); before=digest(runner)
    manifest_before=digest(ROOT/'manifest.json')
    args.output.mkdir(parents=True,exist_ok=False)
    cases_before=digest(ROOT/'cases.json')
    outcomes=[]
    def limits():
        resource.setrlimit(resource.RLIMIT_AS,(1024**3,1024**3))
        resource.setrlimit(resource.RLIMIT_STACK,(64*1024**2,64*1024**2))
    def save():
        (args.output/'outcomes.json').write_text(json.dumps({'mode':args.mode,'runner_sha256':before,'manifest_sha256':manifest_before,'cases_sha256':cases_before,'cases':outcomes},indent=2)+'\n')
    try:
        for c in cases:
            if c['classification']=='compile_rejected': continue
            key=f"{c['group']}-{c['key']}"
            stdout=b''; stderr=b''; rc=None; error=None
            try:
                result=subprocess.run([str(runner),str(ROOT/c['aml']),'TEST'],capture_output=True,timeout=30,preexec_fn=limits)
                stdout=result.stdout; stderr=result.stderr; rc=result.returncode
            except subprocess.TimeoutExpired as exc:
                stdout=exc.stdout or b''; stderr=exc.stderr or b''; error='timeout'
            except Exception as exc:
                error=type(exc).__name__+': '+str(exc)
            (args.output/(key+'.stdout')).write_bytes(stdout)
            (args.output/(key+'.stderr')).write_bytes(stderr)
            ok=error is None and not stderr and matches(c['expected'],rc,stdout)
            outcomes.append({'key':key,'passed':ok,'returncode':rc,'error':error})
            save()
    finally:
        save()
        verify(); assert digest(runner)==before
        assert digest(ROOT/'manifest.json')==manifest_before
        assert digest(ROOT/'cases.json')==cases_before
    if not all(x['passed'] for x in outcomes): raise SystemExit('explicit-format comparison mismatch')
    print('70 exact explicit-format comparisons; 0 compiler rejections')
if __name__=='__main__': main()
