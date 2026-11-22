"""Verify all exported event timestamps/durations with an installed Perfetto TP."""
import argparse
import csv
import io
import json
from pathlib import Path
import subprocess

p=argparse.ArgumentParser(description=__doc__)
p.add_argument('trace',type=Path)
p.add_argument('--processor',required=True,type=Path)
a=p.parse_args()
data=json.loads(a.trace.read_text())
expected=sorted((e['name'],e['ts']*1000,e.get('dur',0)*1000)
                for e in data['traceEvents'] if e['ph'] in ('X','I'))
query='SELECT name, ts, dur FROM slice ORDER BY name, ts, dur'
r=subprocess.run([str(a.processor),'query',str(a.trace),query],check=True,capture_output=True,text=True)
actual=[(x['name'],int(x['ts']),int(x['dur'])) for x in csv.DictReader(io.StringIO(r.stdout))]
if actual!=expected:raise SystemExit('Perfetto import changed or omitted observed event times/durations')
r=subprocess.run([str(a.processor),'query',str(a.trace),
    "SELECT name, value FROM stats WHERE severity IN ('error','data_loss') AND value > 0"],
    check=True,capture_output=True,text=True)
errors=list(csv.DictReader(io.StringIO(r.stdout)))
if errors:raise SystemExit('Perfetto reported import errors: '+repr(errors))
print(json.dumps({'pass':True,'events_verified':len(expected),'import_errors':errors},indent=2))
