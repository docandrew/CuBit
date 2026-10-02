"""Join observed Desktop dequeues to client-declared publication watermarks.

A matched watermark is metadata evidence, not proof that the pixels respond to
that input. No output visibility, scanout, or physical-key timing is inferred.
Reported loss or incomplete printed batches are rejected. Events still in
memory when capture ends are outside this evidence.
"""
import json
from pathlib import Path
import runpy
import sys
source = runpy.run_path(str(Path(__file__).with_name('check-source-trace.py')))
fields = source['fields']


def check(text, *, require_match=True):
    publications = source['check'](text)
    events, pending, batches, previous = {}, 0, 0, None
    retries = set()
    deliveries = 0
    for line in text.splitlines():
        if 'COMPOSITOR-INPUT:' in line:
            row = fields(line, 'COMPOSITOR-INPUT:',
                         {'surface', 'serial', 'kind', 'dequeued_us'})
            identity = row['surface'], row['serial']
            if not all(identity) or not 1 <= row['kind'] <= 10:
                raise ValueError('invalid dequeued input identity or kind')
            tick = row['dequeued_us']
            if tick == 2**64-1 or (previous is not None and tick < previous):
                raise ValueError('unavailable or reversed dequeue clock')
            if identity in events:
                # Retained close requests may be redelivered until After_Serial
                # acknowledges them. Do not choose an arbitrary attempt for a
                # publication watermark whose identity cannot distinguish it.
                if row['kind'] != 10 or events[identity]['kind'] != 10:
                    raise ValueError('repeated dequeue identity')
                retries.add(identity)
            else:
                events[identity] = row
            deliveries += 1
            previous = tick
            pending += 1
            if pending > 64:
                raise ValueError('input batch exceeds storage bound')
        elif 'COMPOSITOR-INPUT-STATS:' in line:
            row = fields(line, 'COMPOSITOR-INPUT-STATS:', {'count','invalid','dropped'})
            if row['count'] != pending or row['invalid'] or row['dropped']:
                raise ValueError('incomplete, invalid or saturated input batch')
            pending = 0
            batches += 1
    if pending or (require_match and not events):
        raise ValueError('missing or unterminated input batch')
    joins, missing, unknown, ambiguous = [], 0, 0, 0
    for publication in publications['records']:
        serial = publication['input_after']
        if not serial:
            unknown += 1
            continue
        identity = (publication['surface'], serial)
        event = events.get(identity)
        if event is None:
            missing += 1
            continue
        elapsed = publication['accepted_us'] - event['dequeued_us']
        if elapsed < 0:
            raise ValueError('publication watermark refers to a future dequeue')
        if identity in retries:
            ambiguous += 1
            continue
        joins.append(dict(publication, kind=event['kind'],
                          dequeued_us=event['dequeued_us'],
                          dequeue_to_accept_us=elapsed))
    if require_match and not joins:
        raise ValueError('no observed dequeue matches a publication watermark')
    return {'scope': 'Desktop dequeue to accepted publication carrying the same client-declared input watermark; no pixel causality or presentation claim',
            'units': 'microseconds', 'input_records': len(events),
            'input_delivery_records': deliveries,
            'retried_close_identities': len(retries), 'ambiguous_watermarks': ambiguous,
            'input_batches': batches, 'publication_records': len(publications['records']),
            'unknown_watermarks': unknown, 'unmatched_watermarks': missing,
            'matched_publications': len(joins), 'records': joins}


if __name__ == '__main__':
    try:
        print(json.dumps(check(Path(sys.argv[1]).read_text()), indent=2))
    except (ValueError, OSError) as error:
        raise SystemExit(f'input/publication evidence rejected: {error}')
