#!/usr/bin/env python3
"""Validate browser-open duration and interaction coverage, never VM uptime."""
import json
from pathlib import Path
import sys


def check(events, minimum=180.0):
    assert events and all(isinstance(e.get('seconds'), (int, float)) for e in events)
    times = [e['seconds'] for e in events]
    assert times[0] >= 0 and all(a <= b for a, b in zip(times, times[1:])), 'time order'
    assert not any(e['event'] == 'failure' for e in events), 'failed fixture'
    starts = [e for e in events if e['event'] == 'browser-alive-start']
    ends = [e for e in events if e['event'] == 'browser-alive-complete']
    closes = [e for e in events if e['event'] == 'closed']
    assert len(starts) == len(ends) == len(closes) == 1, 'missing/duplicate lifetime event'
    begin, end, closed = starts[0], ends[0], closes[0]
    duration = end['seconds'] - begin['seconds']
    assert begin['required_seconds'] >= minimum and duration >= minimum, 'short browser lifetime'
    assert abs(duration - end['seconds_observed']) < 0.01, 'duration mismatch'
    assert closed['seconds'] >= end['seconds'], 'early close'
    reopened = [e for e in events if e['event'] == 'preference-reopen-pass']
    reclosed = [e for e in events if e['event'] == 'reopened-browser-closed']
    assert len(reopened) == len(reclosed) == 1, 'missing preference reopen validation'
    assert closed['seconds'] < reopened[0]['seconds'] < reclosed[0]['seconds'], 'reopen order'
    current = None
    cycles = 0
    required = [
        'url /browser-a', 'loaded-path /browser-a', 'title CuBitBrowserPointerFocus',
        'title CuBitBrowserTyped:abc', 'url /browser-b', 'loaded-path /browser-b',
        'history traversal complete', 'title CuBitBrowserRestoredA',
        'title CuBitBrowserRestoredB', 'title CuBitBrowserWheel:1',
        'title CuBitBrowserScrolled', 'title CuBitBrowserResize:854x504',
        'title CuBitBrowserResize:800x494', 'tab new 2', 'tab select 1', 'tab select 2',
        'title CuBitBrowserRetained:abc', 'title CuBitBrowserRetained:abcd', 'title CuBitBrowserTyped:abcd', 'title CuBitBrowserResize:608x524', 'tab close 2', 'tab parked 2',
    ]
    for event in events:
        if event['event'] == 'cycle-start':
            assert current is None and event['cycle'] == cycles + 1, 'cycle order'
            assert begin['seconds'] <= event['seconds'] < end['seconds']
            current = {'number': event['cycle'], 'tab_id': event.get('tab_id', 2), 'markers': [], 'idle': None, 'renewed': False, 'pixels': False}
        elif event['event'] == 'callback' and current is not None:
            current['markers'].append(event['marker'])
            if current['idle'] is not None:
                assert event['seconds'] - current['idle'] >= 3.0, 'idle interval too short'
                if event['marker'] == 'CUBITSHELL-BROWSER: title CuBitBrowserRestoredB':
                    current['renewed'] = True
        elif event['event'] == 'idle-start':
            assert current is not None and current['idle'] is None
            assert event['cycle'] == current['number']
            current['idle'] = event['seconds']
        elif event['event'] == 'resize-pixels-pass':
            assert current is not None and event['cycle'] == current['number']
            assert event['checked'] == 31930 and not current['pixels']
            current['pixels'] = True
        elif event['event'] == 'cycle-complete':
            assert current is not None and current['renewed'], 'missing renewed interaction'
            assert current['pixels'], 'missing resize pixel comparison'
            assert event['cycle'] == current['number'] and event['seconds'] <= end['seconds']
            for suffix in required:
                if suffix.startswith('tab ') and suffix.endswith(' 2'):
                    suffix = suffix[:-1] + str(current['tab_id'])
                assert 'CUBITSHELL-BROWSER: ' + suffix in current['markers'], 'missing ' + suffix
            assert current['markers'].count('CUBITSHELL-BROWSER: history traversal complete') >= 2
            assert current['markers'].count('CUBITSHELL-BROWSER: loaded-path /browser-b') >= 2
            cycles += 1
            current = None
    assert current is None and cycles >= 2 and end['cycles'] == closed['cycles'] == cycles
    return {'browser_alive_seconds': round(duration, 3), 'complete_interaction_cycles': cycles,
            'exact_wallpaper_pixels_per_cycle': 31930,
            'callbacks': sum(e['event'] == 'callback' for e in events),
            'evidence': 'host-monotonic native-QEMU functional responsiveness; no hardware latency claim'}


if __name__ == '__main__':
    events = [json.loads(line) for line in Path(sys.argv[1]).read_text().splitlines()]
    report = check(events)
    if len(sys.argv) > 2:
        run_log = Path(sys.argv[2]).read_text(errors='replace')
        assert 'headless: PASS servo' in run_log, 'native final gate missing'
        report['native_final_gate'] = 'PASS'
    print(json.dumps(report, indent=2))
