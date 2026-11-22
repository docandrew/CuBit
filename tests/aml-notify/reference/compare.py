"""Strict Notify observation protocol; no upstream scheduling or absence claims."""
import collections
import re

U64_MAX = (1 << 64) - 1
EVENT_MAX = (1 << 16) - 1
QUEUE_MAX = 32
OUTPUT_MAX = 1024 * 1024
STATUSES = {'RETURNED', 'UNSUPPORTED_VALUE', 'EMPTY_BUFFER', 'DIVISION_BY_ZERO'}
PATH = r'\\[A-Z_][A-Z_0-9]{3}(?:\.[A-Z_][A-Z_0-9]{3})*'
EVENT = re.compile(r'EVENT (SYSTEM|DEVICE) (DEVICE|PROCESSOR|THERMAL) (' + PATH + r') (0|[1-9][0-9]*) (CURRENT|STALE)')


def number(text, limit):
    if not re.fullmatch(r'0|[1-9][0-9]*', text) or len(text) > 20:
        raise ValueError('noncanonical integer')
    value = int(text)
    if value > limit:
        raise ValueError('integer out of range')
    return value


def parse(raw):
    if not isinstance(raw, bytes) or not raw or len(raw) > OUTPUT_MAX:
        raise ValueError('invalid output size/type')
    if any(byte not in range(32, 127) and byte != 10 for byte in raw):
        raise ValueError('control or non-ASCII byte')
    if not raw.endswith(b'\n'):
        raise ValueError('missing final newline')
    lines = raw.decode('ascii').splitlines()
    if len(lines) < 6 or lines[0] != 'NOTIFY_REPLAY 1' or lines[-1] != 'END':
        raise ValueError('bad envelope')
    def field(index, prefix):
        if not lines[index].startswith(prefix):
            raise ValueError('missing ordered field')
        return lines[index][len(prefix):]
    status = field(1, 'STATUS ')
    if status not in STATUSES:
        raise ValueError('unexpected execution status')
    result_text = field(2, 'RESULT ')
    result = number(result_text, U64_MAX) if status == 'RETURNED' else None
    if status != 'RETURNED' and result_text != 'NONE':
        raise ValueError('non-return has result')
    marker = number(field(3, 'MARKER '), U64_MAX)
    count = number(field(4, 'EVENTS '), QUEUE_MAX)
    if len(lines) != 6 + count:
        raise ValueError('wrong event count or extra output')
    events = []
    for line in lines[5:-1]:
        match = EVENT.fullmatch(line)
        if not match:
            raise ValueError('malformed event')
        cls, kind, path, value, freshness = match.groups()
        if len(path) > 1 + 32 * 5:
            raise ValueError('path bound')
        events.append(dict(cls=cls, kind=kind, path=path,
                           value=number(value, EVENT_MAX), freshness=freshness))
    return dict(status=status, result=result, marker=marker, events=events)


def compare(actual, expected):
    for field in ('status', 'result', 'marker'):
        if actual[field] != expected[field]:
            raise ValueError('mismatch: ' + field)
    # All oracle tables declare these as direct root children. ACPICA logs only
    # leaf names; this does not establish arbitrary path or identity provenance.
    kinds = {'\\DEV0': 'DEVICE', '\\DEV1': 'DEVICE', '\\THM0': 'THERMAL', '\\CPU0': 'PROCESSOR'}
    for event in actual['events']:
        if kinds.get(event['path']) != event['kind'] or event['freshness'] != 'CURRENT':
            raise ValueError('local target-description invariant')
    if expected['event_comparison'] == 'positive_complete_multiset':
        got = collections.Counter((e['cls'], e['path'], e['value']) for e in actual['events'])
        want = collections.Counter((e['class'], e['path'], e['value']) for e in expected['events'])
        if got != want:
            raise ValueError('positive event multiset mismatch')
    elif expected['event_comparison'] != 'not_observed_no_absence_claim':
        raise ValueError('unknown comparison policy')
    policy = expected.get('local_event_policy')
    if policy is not None:
        if policy['name'] != 'pre_admission_failure_empty_queue':
            raise ValueError('unknown local event policy')
        if actual['events']:
            raise ValueError('CuBit policy: pre-admission failure queued an event')
    return True


def encode(expected):
    kinds = {'\\DEV0': 'DEVICE', '\\DEV1': 'DEVICE', '\\THM0': 'THERMAL', '\\CPU0': 'PROCESSOR'}
    lines = ['NOTIFY_REPLAY 1', 'STATUS ' + expected['status'],
             'RESULT ' + (str(expected['result']) if expected['result'] is not None else 'NONE'),
             'MARKER ' + str(expected['marker']), 'EVENTS ' + str(len(expected['events']))]
    lines += [f"EVENT {e['class']} {kinds[e['path']]} {e['path']} {e['value']} CURRENT" for e in expected['events']]
    return ('\n'.join(lines + ['END']) + '\n').encode('ascii')
