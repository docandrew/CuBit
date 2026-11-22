#!/usr/bin/env python3
"""Loss/early-close controls for the stability evidence checker."""
import copy
from check_stability import check

markers = ['url /browser-a', 'loaded-path /browser-a', 'title CuBitBrowserPointerFocus',
           'title CuBitBrowserTyped:abc', 'url /browser-b', 'loaded-path /browser-b',
           'history traversal complete', 'title CuBitBrowserRestoredA',
           'history traversal complete', 'title CuBitBrowserRestoredB',
           'loaded-path /browser-b', 'title CuBitBrowserWheel:1',
           'title CuBitBrowserScrolled', 'title CuBitBrowserResize:854x504',
           'title CuBitBrowserResize:800x494', 'tab new 2', 'tab select 1', 'tab select 2',
        'title CuBitBrowserRetained:abc', 'title CuBitBrowserRetained:abcd', 'title CuBitBrowserTyped:abcd', 'title CuBitBrowserResize:608x524', 'tab close 2', 'tab parked 2']
events = [{'seconds': 0.0, 'event': 'browser-alive-start', 'required_seconds': 180}]
for cycle in range(1, 4):
    base = (cycle - 1) * 60.0
    events.append({'seconds': base + 0.1, 'event': 'cycle-start', 'cycle': cycle})
    for n, marker in enumerate(markers):
        events.append({'seconds': base + n + 1, 'event': 'callback',
                       'marker': 'CUBITSHELL-BROWSER: ' + marker})
    events += [{'seconds': base + 55, 'event': 'idle-start', 'cycle': cycle},
               {'seconds': base + 59, 'event': 'callback',
                'marker': 'CUBITSHELL-BROWSER: title CuBitBrowserRestoredB'},
               {'seconds': base + 59.5, 'event': 'resize-pixels-pass', 'cycle': cycle, 'checked': 31930},
               {'seconds': base + 60, 'event': 'cycle-complete', 'cycle': cycle}]
events += [{'seconds': 180.1, 'event': 'browser-alive-complete', 'seconds_observed': 180.1, 'cycles': 3},
           {'seconds': 180.5, 'event': 'closed', 'cycles': 3},
           {'seconds': 190, 'event': 'preference-reopen-pass'},
           {'seconds': 191, 'event': 'reopened-browser-closed'}]
assert check(events)['complete_interaction_cycles'] == 3
controls = []
for removed in ['browser-alive-start', 'browser-alive-complete', 'closed', 'idle-start', 'preference-reopen-pass', 'reopened-browser-closed', 'resize-pixels-pass']:
    controls.append([e for e in events if e['event'] != removed])
for marker in markers:
    controls.append([e for e in events if e.get('marker') != 'CUBITSHELL-BROWSER: ' + marker])
bad = copy.deepcopy(events); bad[-3]['seconds'] = 170; controls.append(bad)
bad = copy.deepcopy(events); bad[-4]['seconds_observed'] = 360; controls.append(bad)
bad = copy.deepcopy(events); bad[0]['required_seconds'] = 0; controls.append(bad)
bad = copy.deepcopy(events); bad.append({'seconds': 192, 'event': 'failure'}); controls.append(bad)
bad = copy.deepcopy(events)
next(e for e in bad if e['event'] == 'resize-pixels-pass')['checked'] = 0
controls.append(bad)
for bad in controls:
    try:
        check(bad)
    except AssertionError:
        continue
    raise AssertionError('negative control accepted')
print(f'SERVO-STABILITY-CHECKER: PASS complete timeline and {len(controls)} loss/short/close/failure controls')
