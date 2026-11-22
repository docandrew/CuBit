#!/usr/bin/env python3
"""Validate native collector stall ordering and interaction during the hold."""
import argparse
import re
from pathlib import Path


def check(text: str) -> dict[str, int]:
    lines = text.splitlines()
    held = [i for i, s in enumerate(lines) if s == 'TEST: metrics-stall grant held']
    stable = [i for i, s in enumerate(lines)
              if s == 'TEST: metrics-stall held page unchanged checks=600']
    if len(held) != 1 or len(stable) != 1 or held[0] >= stable[0]:
        raise ValueError('missing or ambiguous acquired-grant hold interval')
    interactive = sum(s == 'ccl-workbench: live label SAMPLED'
                      for s in lines[held[0] + 1:stable[0]])
    saved = sum(s.startswith('ccl-workbench: workspace saved ')
                for s in lines[held[0] + 1:stable[0]])
    if interactive < 2 or saved < 1:
        raise ValueError('no complete live-label/save interaction during grant hold')
    recovered = [re.fullmatch(r'TEST: PASS metrics-stall resumed batches=\s*(\d+) dropped=\s*(\d+)', s)
                 for s in lines[stable[0] + 1:]]
    recovered = [m for m in recovered if m and int(m[1]) > 2 and int(m[2]) > 0]
    if not recovered:
        raise ValueError('no post-hold recovery with producer loss')
    if any('TEST: FAIL' in s or 'desktop: metrics quarantined' in s for s in lines):
        raise ValueError('fault or telemetry quarantine')
    return {'held_page_checks': 600, 'live_samples_during_hold': interactive,
            'saves_during_hold': saved, 'recovered_batch': int(recovered[0][1]),
            'producer_dropped': int(recovered[0][2])}


if __name__ == '__main__':
    import json
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('serial', type=Path)
    args = parser.parse_args()
    print(json.dumps(check(args.serial.read_text()), indent=2))
