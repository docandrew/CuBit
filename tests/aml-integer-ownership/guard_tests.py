#!/usr/bin/env python3
"""Bounded negative-input and capture-cleanup checks; no Ada/native compilation."""
from pathlib import Path
import json,os,shutil,subprocess,sys,tempfile
from hosted_capture import capture
if not __debug__:raise SystemExit('Use Python without -O')
HERE=Path(__file__).resolve().parent
checks=0
def check(value):
 global checks
 checks+=1
 if not value:raise AssertionError(checks)
with tempfile.TemporaryDirectory(prefix='integer-ownership-guards-') as tmp:
 root=Path(tmp);script=root/'tests/aml-integer-ownership';script.mkdir(parents=True)
 for name in ['run.py','hosted_capture.py','selections.json']:shutil.copy2(HERE/name,script/name)
 (root/'changed.txt').write_text('actual content')
 (script/'source-manifest.json').write_text(json.dumps({'changed.txt':'0'*64}))
 env=dict(os.environ,TMPDIR='/home/doc/cubit-build-tmp',PYTHONDONTWRITEBYTECODE='1')
 result=subprocess.run([sys.executable,str(script/'run.py')],env=env,capture_output=True,text=True,timeout=5)
 check(result.returncode!=0 and 'AssertionError' in result.stderr)
 check(not (script/'results').exists())
 result=subprocess.run([sys.executable,'-O',str(script/'run.py')],env=env,capture_output=True,text=True,timeout=5)
 check(result.returncode!=0 and 'optimization disables required guards' in result.stderr)
 check(not (script/'results').exists())
 # A child and grandchild remain active until the helper enforces each limit.
 for mode in ['timeout','log_limit']:
  pidfile=root/(mode+'.pids');log=root/(mode+'.log')
  code=r"""import os,sys,time
p=os.fork()
with open(sys.argv[1], 'a') as f:f.write(str(os.getpid())+'\n');f.flush()
time.sleep(0.05)
if sys.argv[2]=='log_limit':
 while True:os.write(1,b'x'*4096)
else:
 while True:time.sleep(0.02)
"""
  status=capture([sys.executable,'-c',code,str(pidfile),mode],root,log,2.0,1024)
  check(status==mode)
  check(log.stat().st_size<=1024)
  pids=[int(x) for x in pidfile.read_text().splitlines()]
  check(len(pids)==2)
  for pid in pids:
   state=Path('/proc')/str(pid)/'stat'
   check(not state.exists() or state.read_text().split()[2]=='Z')
print('Integer ownership guard checks',checks)
