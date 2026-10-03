#!/usr/bin/env python3
"""Require native tab-overflow and multi-window completion, not just startup."""
import json
from pathlib import Path
import sys


def check(events):
    assert not any(e['event'] == 'failure' for e in events), 'failed fixture'
    starts = [i for i, e in enumerate(events) if e['event'] == 'features-start']
    assert len(starts) == 1, 'missing/duplicate feature phase'
    phase = events[starts[0]:]
    markers = [e['marker'] for e in phase if e['event'] == 'callback']
    first = phase[0].get('first_tab_id', 2)
    count = phase[0].get('live_tabs', 20)
    assert isinstance(first, int) and first >= 2
    assert isinstance(count, int) and count >= 20
    last = first + count - 2
    for index in range(first, last + 1):
        assert f'CUBITSHELL-BROWSER: tab new {index}' in markers
        assert f'CUBITSHELL-BROWSER: tab close {index}' in markers
    for index in (last - 1, last):
        assert markers.count(f'CUBITSHELL-BROWSER: tab select {index}') >= 2
    assert markers.count('CUBITSHELL-BROWSER: window opened') == 4
    assert markers.count('CUBITSHELL-BROWSER: window limit') == 1
    # Four sibling closures are awaited individually. The final original and
    # reopened windows are observed through whole-process completion instead.
    assert markers.count('CUBITSHELL-BROWSER: window closed') == 4
    assert markers.count('CUBITSHELL: closed') == 2
    tabs = [e for e in phase if e['event'] == 'tabs-overflow-pass']
    windows = [e for e in phase if e['event'] == 'windows-isolation-reuse-pass']
    assert len(tabs) == len(windows) == 1
    assert tabs[0]['live_tabs'] == count and windows[0]['windows'] == 4
    assert tabs[0]['seconds'] < windows[0]['seconds']
    assert any(e['event'] == 'reopened-browser-closed' for e in phase)
    return {'live_tabs': count, 'overflow_orientations': 2, 'simultaneous_windows': 4,
            'capacity_rejection': 'PASS', 'isolated_close_and_reuse': 'PASS'}


if __name__ == '__main__':
    events = [json.loads(line) for line in Path(sys.argv[1]).read_text().splitlines()]
    print(json.dumps(check(events), indent=2))
