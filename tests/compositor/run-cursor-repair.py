"""Run native cursor repair screenshots plus the standard desktop regression.

After building the desired Desktop, run from Nix under coordination/build.lock:
  python3 tests/compositor/run-cursor-repair.py TAG [ABSOLUTE_DESKTOP_IMAGE] [PRIVATE_BASE_DISK]
Logs and captures are /tmp/cubit-cursor-TAG*. No shared runner changes.
The observer checks actual displayed pixels; the existing runner supplies the
input regression and final fault scan. Both must succeed.
"""
import os,subprocess,tempfile,sys
from pathlib import Path
root=Path(__file__).resolve().parents[2]
tag=sys.argv[1]
assert tag and all(c.isalnum() or c in '-_' for c in tag), 'use a simple unique run tag'
serial=f'/tmp/cubit-cursor-{tag}.serial'
with tempfile.TemporaryDirectory(prefix=f'cubit-cursor-{tag}-') as tmp:
 env=os.environ.copy(); env['TMPDIR']=tmp
 if len(sys.argv)>2: env['CUBIT_DESKTOP_IMAGE']=sys.argv[2]
 with open(f'/tmp/cubit-cursor-{tag}-native.log','w') as log:
  p=subprocess.Popen(['bash','tests/headless/run.sh',*(['--disk',sys.argv[3]] if len(sys.argv)>3 else []),'--test','desktop-display','--accel','tcg,thread=multi','--cpus','4','--timeout','100','--serial',serial,'--keep-logs'],cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT)
  observer=subprocess.run(['python3','tests/compositor/check-cursor-repair.py',tmp,serial,'90'],cwd=root)
  status=p.wait()
  print(f'CURSOR-RUN: observer={observer.returncode} runner={status}',flush=True)
  sys.exit(observer.returncode or status)
