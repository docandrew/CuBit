#!/usr/bin/env python3
"""Prove pixel-work records and bounded metadata admission in a private snapshot.

Run with: nix-shell tests/compositor/vulkan-affine-shell.nix --run
          'python3 tests/compositor/test-work-metrics.py'
The result records source hashes; no shared build outputs or staging are changed.
"""
import os
if not os.environ.get("IN_NIX_SHELL"):
    raise SystemExit("Run this proof in the CuBit Nix development environment")
from pathlib import Path
import tempfile,subprocess,json,hashlib
r=Path(__file__).resolve().parents[2];(r/'tests/compositor/build').mkdir(exist_ok=True);w=Path(tempfile.mkdtemp(prefix='work-metrics-',dir=r/'tests/compositor/build'));print(w,flush=True);inputs={}
for folder,names in [('userspace/lib/compositor',['compositor_work_metrics','compositor_metric_batch_policy','compositor_elapsed']),('userspace/runtime/gnat',['cubit','cubit-metric_records'])]:
 for name in names:
  for ext in ['ads','adb']:
   p=r/folder/(name+'.'+ext)
   if p.exists():data=p.read_bytes();(w/p.name).write_bytes(data);inputs[str(p.relative_to(r))]=hashlib.sha256(data).hexdigest()
(w/'test.gpr').write_text('project Test is for Source_Dirs use ("."); for Object_Dir use "obj"; package Compiler is for Default_Switches ("Ada") use ("-gnat2022"); end Compiler; end Test;')
subprocess.run(['alr','exec','--','gnatprove','-P',str(w/'test.gpr'),'-u','compositor_work_metrics.adb','compositor_metric_batch_policy.adb','--level=2','--timeout=30','-j2'],cwd=r/'kernel',check=True)
total=next(l for l in (w/'obj/gnatprove/gnatprove.out').read_text().splitlines() if l.startswith('Total '));assert total.split()[-2:]==['.','.'],total
for p,h in inputs.items():assert hashlib.sha256((r/p).read_bytes()).hexdigest()==h,p
(w/'result.json').write_text(json.dumps({'status':'PASS','proof':total,'inputs':inputs},indent=2));print(total)
