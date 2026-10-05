"""Summarize opt-in native memory samples without treating them as RSS or leaks."""
import json
from pathlib import Path
import re


ORACLE = 'CUBITSHELL-MEMORY: PASS charge/release 2097152 bytes'
SAMPLE = re.compile(r'CUBITSHELL-MEMORY: ms=(\d+) owned_bytes=(\d+) sampled_peak_bytes=(\d+) windows=(\d+)')


def memory_report(serial, destination):
    text = Path(serial).read_text(errors='replace')
    assert 'CUBITSHELL-MEMORY: unavailable' not in text
    samples, intervals = [], []
    current = None
    for line in text.splitlines():
        if ORACLE in line:
            # The self-check runs at browser startup. Never count a restart's
            # smaller sample as reclamation by the preceding browser instance.
            current = []
            intervals.append(current)
        match = SAMPLE.search(line)
        if match:
            assert current is not None, 'memory sample without startup self-check'
            sample = dict(zip(('ms', 'owned_bytes', 'sampled_peak_bytes', 'windows'),
                              map(int, match.groups())))
            assert sample['sampled_peak_bytes'] >= sample['owned_bytes'], 'invalid peak'
            if current:
                assert sample['ms'] >= current[-1]['ms'], 'clock reset without startup self-check'
                assert sample['sampled_peak_bytes'] >= current[-1]['sampled_peak_bytes'], 'peak moved backwards'
            samples.append(sample)
            current.append(sample)
    assert samples and all(intervals), 'missing owned-memory samples or empty startup interval'
    runs = [dict(sample_count=len(run), first_ms=run[0]['ms'], last_ms=run[-1]['ms'],
                 first_owned_bytes=run[0]['owned_bytes'], last_owned_bytes=run[-1]['owned_bytes'],
                 maximum_owned_bytes=max(s['owned_bytes'] for s in run)) for run in intervals]
    report = {
        'measurement': 'caller-owned physical frames; excludes borrowed mappings, page tables and service allocations',
        'peak_kind': 'five-second sampled maximum, not exact high-water mark',
        'run_boundary': 'each successful startup charge/release self-check; times are local to that run',
        'first_last_same_startup_run': len(runs) == 1,
        'run_count': len(runs),
        'runs': runs,
        'first_owned_bytes': samples[0]['owned_bytes'],
        'last_owned_bytes': samples[-1]['owned_bytes'],
        'maximum_owned_bytes': max(s['owned_bytes'] for s in samples),
        'samples': samples,
    }
    Path(destination).write_text(json.dumps(report, indent=2))
    print('MEMORY:', json.dumps({k:v for k,v in report.items() if k!='samples'}), flush=True)
    return report
