"""Cumulative observed copy accounting, deliberately not a bandwidth/FPS claim."""
import re

STAGES = ('desktop_staging', 'display_backend', 'display_repair',
          'gpu_upload_request', 'gpu_legacy_copy')
LINE = re.compile(r'^GRAPHICS: stage=(\w+) bytes=\s*(\d+) regions=\s*(\d+) overflow=([01])$')
LIMIT = 2**64 - 1


def graphics_report(text):
    snapshots = {}
    errors = []
    for line in text.splitlines():
        if 'GRAPHICS:' not in line:
            continue
        match = LINE.fullmatch(line)
        if not match or match[1] not in STAGES:
            errors.append('Malformed or interleaved graphics record')
            continue
        stage, byte_count, regions, overflow = match.groups()
        record = dict(bytes=int(byte_count), regions=int(regions))
        if (overflow != '0' or max(record.values()) > LIMIT or
                (record['regions'] == 0) != (record['bytes'] == 0)):
            errors.append(f'Invalid or overflowed counter: {stage}')
        previous = snapshots.get(stage)
        if previous and any(record[k] < previous[k] for k in record):
            errors.append(f'Counter reset/wrap: {stage}')
        snapshots[stage] = record
    if not snapshots and not errors:
        return None
    required = set(STAGES[:3])
    if 'virtio-gpu: ready' in text or any(k.startswith('gpu_') for k in snapshots):
        required.update(STAGES[3:])
    missing = required - snapshots.keys()
    if missing:
        errors.append('Missing counters: ' + ', '.join(sorted(missing)))
    valid = not errors
    return dict(valid=valid, stages=snapshots, errors=errors,
                observed_cpu_copy_bytes=(sum(v['bytes'] for k, v in snapshots.items()
                                             if k != 'gpu_upload_request') if valid else None),
                scope='Latest cumulative snapshot per service, including startup; '
                      'asynchronous sampling may miss the tail. Not phase-aligned, '
                      'CPU time, memory-bus traffic, frame rate or key-to-photon latency. '
                      'GPU upload requests are not measured host copies. '
                      'Rendering/composition writes and unrelated copies are excluded.')
