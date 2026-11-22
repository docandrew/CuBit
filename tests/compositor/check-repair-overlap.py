"""Compare repair work in two native Observatory fixed-client workloads.

Pixel-work evidence only: intervals have different scheduling and this does not
compare wall time, frame rate, percentile latency, or hardware performance.
"""
import json
from pathlib import Path
import re
import sys

def counters(path):
    text=Path(path).read_text(errors='replace')
    if 'observatory: closed' not in text or re.search(r'panic|EXCEPTION:|TEST: FAIL|quarantined',text,re.I):
        raise ValueError('incomplete or faulted native workload')
    rows=[]
    for line in text.splitlines():
        if 'desktop: stats ' not in line:continue
        row={k:int(n) for k,n in re.findall(r'\b([a-z_]+)=(\d+)',line)}
        if row.get('frames',0)>0 and row.get('fast')==row['frames'] and row.get('px',0)>100000:
            rows.append(row)
    result={key:sum(row[key] for row in rows) for key in ('frames','px','repair_px')}
    if result['frames']<20:raise ValueError('insufficient repeated fast redraws')
    if result['px']!=result['frames']*425600:raise ValueError('different client redraw workload')
    result['intervals']=len(rows)
    return result

def check(before,after):
    old,new=counters(before),counters(after)
    if new['repair_px']*old['frames']*10>=old['repair_px']*new['frames']:
        raise ValueError('repair pixels per frame did not fall by at least 90%')
    return {'before':old,'after':new}
if __name__=='__main__':
    try:print('REPAIR-OVERLAP: PASS '+json.dumps(check(*sys.argv[1:3]),sort_keys=True))
    except (ValueError,OSError) as error:raise SystemExit(str(error))
