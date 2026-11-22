"""Native copy-counter regression for the fixed Observatory interaction workload.

Compares work volume, not clock time or physical rendering performance.
"""
import json
from pathlib import Path
import re
import sys

def counters(path):
    text=Path(path).read_text(errors='replace')
    if 'observatory: closed' not in text or re.search(r'panic|EXCEPTION:|TEST: FAIL|quarantined',text,re.I):
        raise ValueError('incomplete or faulted native workload')
    result={}
    for stage in ('display_backend','display_repair','gpu_upload_request'):
        records=[tuple(map(int,r)) for r in re.findall(
            r'GRAPHICS: stage='+stage+r' bytes=\s*(\d+) regions=\s*(\d+) overflow=(\d+)',text)]
        if not records or any(r[2] for r in records):raise ValueError('missing or saturated copy counters')
        if any(a[0]>b[0] or a[1]>b[1] for a,b in zip(records,records[1:])):raise ValueError('counter reset')
        result[stage]={'bytes':records[-1][0],'regions':records[-1][1]}
    if result['display_backend']['regions']<60 or result['display_backend']['bytes']<64000000:
        raise ValueError('insufficient native presentation work')
    return result

def check(before,after):
    old,new=counters(before),counters(after)
    for stage in ('display_backend','gpu_upload_request'):
        if not 9*old[stage]['bytes']<=10*new[stage]['bytes']<=11*old[stage]['bytes']:
            raise ValueError('source/upload workload changed by more than 10%')
    if new['display_repair']['bytes']*old['display_backend']['bytes']*10 >= old['display_repair']['bytes']*new['display_backend']['bytes']:
        raise ValueError('normalized repair traffic did not fall by at least 90%')
    return {'before':old,'after':new}
if __name__=='__main__':
    try:print('DISPLAY-REPAIR-COUNTS: PASS '+json.dumps(check(*sys.argv[1:3]),sort_keys=True))
    except (ValueError,OSError) as error:raise SystemExit(str(error))
