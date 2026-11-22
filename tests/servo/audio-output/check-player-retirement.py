#!/usr/bin/env python3
"""Check diagnostic native-player teardown order; DOM completion is insufficient.

The private race fixture logs each player's creator, bus cleanup and player
weak notification. This checker requires every native bus to be cleaned up
before its player, and all players to be disposed by one separate worker.
It does not prove absence of arbitrary native references, leaks or AV glitches.
"""
import argparse
import json
from pathlib import Path
import re


def check(text, cycles):
    events = {'BUS': {}, 'PLAYER': {}}
    errors = []
    pattern = re.compile(r'PENNY-NATIVE-(BUS-DROP|DROP): player=(\d+) owner=([0-9a-f]+) current=([0-9a-f]+)')
    for match in pattern.finditer(text):
        kind = 'BUS' if match[1] == 'BUS-DROP' else 'PLAYER'
        player = int(match[2])
        if player in events[kind]:
            errors.append(f'duplicate {kind} notification for player {player}')
        events[kind][player] = (match.start(), match[3], match[4])
    expected = set(range(cycles))
    for kind, values in events.items():
        if set(values) != expected:
            errors.append(f'{kind} IDs differ: expected {sorted(expected)}, observed {sorted(values)}')
    workers = {event[2] for event in events['PLAYER'].values()}
    if len(workers) != 1:
        errors.append(f'expected one retirement worker, observed {len(workers)}')
    for player in sorted(expected & events['BUS'].keys() & events['PLAYER'].keys()):
        bus, native = events['BUS'][player], events['PLAYER'][player]
        if bus[0] >= native[0]:
            errors.append(f'player {player} finalized before bus cleanup')
        if bus[1] != native[1]:
            errors.append(f'player {player} creator differs between notifications')
        if native[2] in (native[1], bus[2]):
            errors.append(f'player {player} disposed on creator or native playback thread')
    for marker in ('USER-MEMORY-FAULT', 'CUBIT KERNEL PANIC', 'PENNY-ABORT:', 'CUBITSHELL: panic'):
        if marker in text:
            errors.append(f'failure marker: {marker}')
    return {'result': 'FAIL' if errors else 'PASS', 'cycles': cycles,
            'players': len(events['PLAYER']), 'errors': errors}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('serial', type=Path)
    parser.add_argument('--cycles', type=int, default=8)
    args = parser.parse_args()
    if args.cycles <= 0:
        parser.error('--cycles must be positive')
    report = check(args.serial.read_text(errors='replace'), args.cycles)
    print(json.dumps(report, indent=2))
    raise SystemExit(report['result'] != 'PASS')
