#!/usr/bin/env python3
"""Record compiler stack estimates, never infer a whole-service stack bound."""
import json
from pathlib import Path

root = Path(__file__).resolve().parent / 'build' / 'native'
entries = []
for source in sorted((root / 'obj').glob('*.su')):
    for line in source.read_text().splitlines():
        fields = line.split('\t')
        if len(fields) != 3:
            raise RuntimeError(f'Malformed stack record: {line}')
        entries.append(dict(symbol=fields[0], bytes=int(fields[1]), classification=fields[2]))
if not entries or not (root / 'lib' / 'libacpi_core.a').is_file():
    raise RuntimeError('Native compile evidence missing')
entries.sort(key=lambda item: item['bytes'], reverse=True)
report = dict(scope='CuBit-runtime static library compilation; no native execution',
              whole_call_chain_verified=False,
              dynamic_frames=sum('dynamic' in item['classification'] for item in entries),
              entries=entries)
(root / 'stack-report.json').write_text(json.dumps(report, indent=2) + '\n')
print(f"ACPI-NATIVE-COMPILE: PASS; largest reported frame {entries[0]['bytes']} bytes; "
      f"{report['dynamic_frames']} dynamic frame records; whole-call stack bound unverified")
