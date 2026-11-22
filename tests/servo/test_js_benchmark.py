"""Check the shared benchmark's answers with independent Python calculations.

Runs in Nix with Node for fixture validation; not native SpiderMonkey evidence.
"""
from pathlib import Path
import hashlib
import json
import subprocess

source=Path(__file__).with_name('js-benchmark.js').resolve()
expected={
    'integer':sum((i*17)^(i>>3) for i in range(100000)) & 0xffffffff,
    'typed-array':32*sum((i*2654435761)&0xffffffff for i in range(16384)) & 0xffffffff,
    'objects':20*sum(range(2000)),
    'json':8*sum(range(1000)),
    'regexp':2*sum(range(2000)),
    'sort':4*sum((i+1)*i for i in range(4096)) & 0xffffffff,
}
script='require('+json.dumps(str(source))+'); PennyJSBenchmark.run('+json.dumps(expected)+').then(r=>console.log(JSON.stringify(r)));'
report=json.loads(subprocess.check_output(['node','-e',script],text=True,timeout=60))
assert report['version']=='penny-js-v1'
assert {r['id']:r['checksum'] for r in report['results']}==expected
assert all(len(r['samples_ms'])==5 for r in report['results'])
print('PASS JavaScript benchmark: 6 workloads, 30 measured samples, independent checksums')
print('Input SHA256:',hashlib.sha256(source.read_bytes()).hexdigest())
print('Expected:',json.dumps(expected,sort_keys=True))
