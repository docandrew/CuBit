"""Validate complete bounded publication trace batches from one Desktop run.

Input watermarks are client assertions, not trusted proof of pixel causality.
This does not join a publication to output presentation or physical photons.
"""
import json
from pathlib import Path
import sys


def fields(line, marker, expected):
    parts = line.split(marker, 1)[1].split()
    pairs = [p.split('=') for p in parts]
    if any(len(p) != 2 or not p[1].isascii() or not p[1].isdigit() for p in pairs):
        raise ValueError('malformed source trace')
    values = {k: int(v) for k, v in pairs}
    if len(values) != len(parts) or values.keys() != expected:
        raise ValueError('duplicate, missing or unknown source field')
    if any(v >= 2**64 for v in values.values()):
        raise ValueError('source field exceeds uint64')
    return values


def check(text):
    rows, identities, pending, batches = [], set(), 0, 0
    for line in text.splitlines():
        if 'COMPOSITOR-SOURCE:' in line:
            row = fields(line, 'COMPOSITOR-SOURCE:',
                         {'surface', 'epoch', 'ticket', 'input_after', 'accepted_us'})
            if not row['surface'] or not 1 <= row['epoch'] < 2**31 or not 1 <= row['ticket'] < 2**31:
                raise ValueError('invalid source identity')
            if row['accepted_us'] == 2**64-1:
                raise ValueError('unavailable acceptance clock')
            identity = row['surface'], row['epoch'], row['ticket']
            if identity in identities:
                raise ValueError('repeated publication identity')
            if rows and row['accepted_us'] < rows[-1]['accepted_us']:
                raise ValueError('reversed acceptance clock')
            identities.add(identity)
            rows.append(row)
            pending += 1
            if pending > 64:
                raise ValueError('source batch exceeds storage bound')
        elif 'COMPOSITOR-SOURCE-STATS:' in line:
            row = fields(line, 'COMPOSITOR-SOURCE-STATS:', {'count', 'invalid', 'dropped'})
            if row['count'] != pending or row['invalid'] or row['dropped']:
                raise ValueError('incomplete, invalid or saturated source batch')
            pending = 0
            batches += 1
    if not rows or pending:
        raise ValueError('missing or unterminated source batch')
    return {'scope': 'Accepted publications and untrusted client input watermarks; not presentation or photon latency',
            'units': 'microseconds', 'batches': batches, 'records': rows,
            'unknown_input_records': sum(r['input_after'] == 0 for r in rows)}


if __name__ == '__main__':
    try:
        print(json.dumps(check(Path(sys.argv[1]).read_text()), indent=2))
    except (ValueError, OSError) as error:
        raise SystemExit(f'source trace rejected: {error}')
