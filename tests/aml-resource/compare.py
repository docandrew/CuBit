#!/usr/bin/env python3
"""Exact retained resource-template oracle replay; no recompilation."""
import argparse, hashlib, json, resource, subprocess, tempfile
import byte_protocol
from pathlib import Path
ERRORS = {'AE_AML_NO_RESOURCE_END_TAG':'NO_RESOURCE_END_TAG',
 'AE_AML_INVALID_RESOURCE_TYPE':'INVALID_RESOURCE_TYPE',
 'AE_AML_BAD_RESOURCE_LENGTH':'BAD_RESOURCE_LENGTH',
 'AE_AML_BUFFER_LENGTH':'RESOURCE_BUFFER_LENGTH',
 'AE_AML_OPERAND_TYPE':'UNSUPPORTED_VALUE'}
def expected(row):
 marker=int(row['marker_values'][0],16)
 if row['classification']=='runtime_error':
  return {'status':ERRORS[row['exact_error']], 'result':None, 'marker':marker}
 if row['kind']=='integer':
  return {'status':'RETURNED','result':{'kind':'integer','value':row['integer']},'marker':marker}
 return {'status':'OBJECT_RETURNED','result':{'kind':'buffer','hex':row['hex']},'marker':marker}
def classify(row, code, output):
 return byte_protocol.matches(expected(row),code,output)
def capture(command):
 # Disk-backed output with an OS-enforced size ceiling; never an unbounded PIPE.
 with tempfile.TemporaryFile() as output:
  try:
   result=subprocess.run(command,stdout=output,stderr=subprocess.STDOUT,
     timeout=30,preexec_fn=cap)
   code=result.returncode
  except subprocess.TimeoutExpired:
   code='timeout'
  output.seek(0)
  data=output.read(byte_protocol.MAX_OUTPUT_BYTES+1)
 return code,data
def digest(path): return hashlib.sha256(path.read_bytes()).hexdigest()
def cap():
 resource.setrlimit(resource.RLIMIT_AS,(1024*1024*1024,)*2)
 resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2)
 resource.setrlimit(resource.RLIMIT_FSIZE,(byte_protocol.MAX_OUTPUT_BYTES,)*2)
def main():
 ap=argparse.ArgumentParser();ap.add_argument('--runner',type=Path,required=True);ap.add_argument('--output',type=Path,required=True);args=ap.parse_args()
 base=Path(__file__).resolve().parent;manifest=json.loads((base/'reference-manifest.json').read_text())
 def verify(): return all(digest(base/k)==v for k,v in manifest.items())
 assert verify();runner=args.runner.resolve();initial=digest(runner);args.output.mkdir(parents=True,exist_ok=False)
 results=[]
 for row in json.loads((base/'cases.json').read_text()):
  if row['classification']=='compile_rejected': raise RuntimeError('Rejected case is not executable')
  aml=base/row['aml'];raw=aml.read_bytes();assert raw[:4]==b'DSDT' and raw[8]==row['revision']
  code,out=capture([str(runner),str(aml),'TEST'])
  (args.output/(row['key']+'.log')).write_bytes(out);results.append({'key':row['key'],'exit':code,'passed':classify(row,code,out)})
 unchanged=verify() and digest(runner)==initial
 (args.output/'report.json').write_text(json.dumps({'runner_sha256':initial,'inputs_unchanged':unchanged,'results':results},indent=2)+'\n')
 return 0 if unchanged and all(x['passed'] for x in results) else 1
if __name__=='__main__': raise SystemExit(main())
