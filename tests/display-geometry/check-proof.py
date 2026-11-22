"""Reject vacuous/skipped geometry proof runs, not just failed tool exits."""
import json
from pathlib import Path

result = json.loads((Path(__file__).parent / 'build/gnatprove/cubit-display_geometry.spark').read_text())
if not __debug__:
    raise SystemExit('Proof audit requires Python assertions')
assert result['spark'] and all(value == 'all' for value in result['spark'].values())
assert result['progress'] == 'PROGRESS_PROOF'
assert result['stop_reason'] == 'STOP_REASON_NONE'
for field in ('pragma_assume', 'skip_flow_proof', 'skip_proof'):
    assert not result[field], field
assert result['proof'], 'No geometry proof obligations'
assert all(item['severity'] == 'info' for item in result['proof'])
print(f"PASS geometry proof: {len(result['proof'])} proof diagnostics; no skips or assumptions")
