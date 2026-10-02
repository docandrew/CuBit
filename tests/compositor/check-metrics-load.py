"""Require four workers, desktop interaction during their overlap, later metrics."""
import json
import re
import sys
from pathlib import Path

def check(text):
    active=set(); begun=set(); ended=set(); saves=0; seen=False; result=None
    for line in text.splitlines():
        if 'TEST: FAIL' in line or 'desktop: metrics quarantined' in line:
            raise ValueError('fault or quarantined metrics')
        m=re.fullmatch(r'TEST: metrics-load begin pid=\s*(\d+)',line)
        if m:
            pid=int(m[1]); assert pid not in begun
            active.add(pid); begun.add(pid)
        m=re.fullmatch(r'TEST: metrics-load done pid=\s*(\d+) chunks=\s*(\d+)',line)
        if m:
            pid=int(m[1]); assert pid in active and int(m[2])>0
            active.remove(pid); ended.add(pid)
        if len(active)==4:
            seen=True
            saves+=line.startswith('ccl-workbench: workspace saved ')
        m=re.fullmatch(r'TEST: PASS desktop-metrics-load frames=\s*(\d+) batches=\s*(\d+) dropped=\s*(\d+)',line)
        if m:
            assert not active and len(ended)==4 and int(m[1])>=3
            result=dict(frames=int(m[1]),batches=int(m[2]),dropped=int(m[3]))
    assert seen and len(begun)==4 and begun==ended and saves>0 and result
    return dict(workers=4,saves_during_worker_overlap=saves,**result)

if __name__=='__main__':
    print(json.dumps(check(Path(sys.argv[1]).read_text()),indent=2))
