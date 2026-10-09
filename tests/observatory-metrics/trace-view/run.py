"""Snapshot and check bounded archive views through the real CCL interpreter."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[3])
p.add_argument('--toolchain-root', type=Path)
p.add_argument('--prove', action='store_true')
p.add_argument('output', type=Path)
a=p.parse_args()
if not os.environ.get('IN_NIX_SHELL'):p.error('Use Nix')
root=a.root.resolve();toolchain=(a.toolchain_root or root).resolve();out=a.output.resolve()
if out == root or root in out.parents:p.error('Use a private output directory outside the source checkout')
out.mkdir(parents=True,exist_ok=False);src=out/'source';src.mkdir();inputs={}
def copy(source,target):
 data=source.read_bytes();inputs[str(source)]=hashlib.sha256(data).hexdigest();target.write_bytes(data)
files=list((root/'userspace/ccl/src').glob('*.ad?'))
files += [root/relative for relative in ['userspace/lib/compositor/compositor_elapsed.ads', 'userspace/lib/compositor/compositor_frame_trace.ads', 'userspace/lib/compositor/compositor_frame_trace.adb', 'userspace/lib/compositor/compositor_input_trace.ads', 'userspace/lib/compositor/compositor_input_trace.adb', 'userspace/lib/compositor/compositor_source_trace.ads', 'userspace/lib/compositor/compositor_source_trace.adb', 'userspace/lib/compositor/compositor_render_trace.ads', 'userspace/lib/compositor/compositor_render_trace.adb', 'userspace/lib/compositor/compositor_trace_wire.ads', 'userspace/lib/compositor/compositor_trace_wire.adb', 'userspace/lib/compositor/compositor_trace_metrics.ads', 'userspace/lib/compositor/compositor_trace_metrics.adb', 'userspace/lib/compositor/compositor_trace_stream.ads', 'userspace/lib/compositor/compositor_trace_stream.adb', 'userspace/runtime/gnat/cubit.ads', 'userspace/runtime/gnat/cubit-metric_records.ads', 'userspace/runtime/gnat/cubit-metric_records.adb', 'userspace/runtime/gnat/cubit-metric_protocol.ads', 'userspace/lib/compositor/compositor_trace_archive.ads', 'userspace/lib/compositor/compositor_trace_archive.adb', 'userspace/lib/compositor/compositor_trace_framing.ads', 'userspace/lib/compositor/compositor_trace_framing.adb', 'userspace/runtime/gnat/cubit-failures.adb', 'userspace/runtime/gnat/cubit-failures.ads']]
files += list((root/'userspace/lib/observatory').glob('observatory_trace_*.ad?'))
for source in files:
 assert not (src/source.name).exists(),source
 copy(source,src/source.name)
copy(Path(__file__).with_name('view_tests.adb'),src/'view_tests.adb')
copy(Path(__file__).with_name('trace_view.gpr'),out/'trace_view.gpr')
copy(root/'tests/compositor/trace-archive/fixtures/native201.cubittrace',out/'native.cubittrace')
inputs[str(Path(__file__).resolve())]=hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
(out/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
commands=[]
def run(command,name):
 commands.append(list(map(str,command)));(out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n')
 with (out/name).open('w') as log:
  subprocess.run(command,cwd=toolchain/'kernel',stdout=log,stderr=subprocess.STDOUT,check=True)
run(['alr','exec','--','gprbuild','-p','-P',str(out/'trace_view.gpr')],'build.log')
run([str(out/'bin/view_tests'),str(out/'native.cubittrace')],'tests.log')
if a.prove:
 run(['alr','exec','--','gnatprove','-P',str(out/'trace_view.gpr'),'-u','observatory_trace_view.adb','observatory_trace_ccl.adb','--level=2','--timeout=30','-j2','--report=all','--checks-as-errors=on'],'proof.log')
for source,h in inputs.items():assert hashlib.sha256(Path(source).read_bytes()).hexdigest()==h,source
(out/'result.json').write_text(json.dumps({'status':'PASS','scope':'Hosted CCL interpreter with real native archive fixture; not a native viewer','proof_requested':a.prove,'tests':(out/'tests.log').read_text().strip()},indent=2)+'\n')
print((out/'result.json').read_text())
