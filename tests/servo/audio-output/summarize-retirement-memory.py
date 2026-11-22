#!/usr/bin/env python3
"""Summarize a completed video-retirement/idle guest trace; not a leak verdict."""
import argparse
import json
from pathlib import Path
import re

SAMPLE = re.compile(r'CUBITSHELL-MEMORY: ms=(\d+) owned_bytes=(\d+) sampled_peak_bytes=(\d+) windows=(\d+)')
CYCLE = re.compile(r'CUBITSHELL-PERF: CuBitBrowserPerfVideoCycle(\d+)\b')
IDLE = 'CUBITSHELL-PERF: CuBitBrowserPerfIdleBaseline'
END = 'CUBITSHELL-PERF: CuBitBrowserPerfAudioEnded'

def summarize(text, cycles=8, minimum_idle_ms=50000):
    for error in ('USER-MEMORY-FAULT', 'PENNY-ABORT:', 'CUBITSHELL: panic',
                  'PANIC', 'Error evaluating script', 'CuBitBrowserPerfAudioError',
                  'CuBitBrowserPerfAudioRejected'):
        if error in text:
            raise ValueError('failure marker: ' + error)
    samples, idle_samples, completed = [], [], []
    phase = 'playback'
    idle_count = end_count = 0
    for line in text.splitlines():
        if match := CYCLE.search(line):
            if phase != 'playback':
                raise ValueError('video cycle after navigation to idle page')
            completed.append(int(match[1]))
        if IDLE in line:
            idle_count += 1
            if completed != list(range(1, cycles + 1)):
                raise ValueError('missing, repeated or out-of-order video cycles')
            phase = 'idle'
        if END in line:
            end_count += 1
            if phase != 'idle':
                raise ValueError('completion outside idle phase')
            phase = 'done'
        if match := SAMPLE.search(line):
            sample = dict(zip(('ms', 'owned_bytes', 'sampled_peak_bytes', 'windows'), map(int, match.groups())))
            if samples and sample['ms'] <= samples[-1]['ms']:
                raise ValueError('non-increasing guest sample time')
            if sample['windows'] != 1:
                raise ValueError('expected one browser window')
            samples.append(sample)
            if phase == 'idle':
                idle_samples.append(sample)
    if idle_count != 1 or end_count != 1 or len(idle_samples) < 2:
        raise ValueError('incomplete idle observation')
    duration = idle_samples[-1]['ms'] - idle_samples[0]['ms']
    if duration < minimum_idle_ms:
        raise ValueError('insufficient observed idle duration')
    values = [s['owned_bytes'] for s in idle_samples]
    return {
        'result': 'OBSERVED',
        'metric': 'kernel process-owned mapped bytes; not RSS or live heap bytes',
        'cycles': completed,
        'sample_count': len(samples),
        'first_sample_bytes': samples[0]['owned_bytes'],
        'sampled_peak_bytes': max(s['owned_bytes'] for s in samples),
        'idle': {'observed_ms': duration, 'sample_count': len(values),
                 'first_bytes': values[0], 'last_bytes': values[-1],
                 'minimum_bytes': min(values), 'maximum_bytes': max(values),
                 'delta_bytes': values[-1] - values[0],
                 'samples': idle_samples},
        'scope': 'Natural cleanup after navigation; no forced GC. Flat samples do not prove absence of leaks; retained mappings do not prove a leak.',
    }

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('serial', type=Path)
    parser.add_argument('--cycles', type=int, default=8)
    args = parser.parse_args()
    if args.cycles < 1:
        parser.error('--cycles must be positive')
    try:
        result = summarize(args.serial.read_text(errors='replace'), args.cycles)
    except ValueError as error:
        parser.exit(1, str(error) + '\n')
    print(json.dumps(result, indent=2))

if __name__ == '__main__':
    main()
