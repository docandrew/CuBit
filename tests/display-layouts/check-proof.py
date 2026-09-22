"""Require non-vacuous SPARK coverage for the shared layout admission core."""
import json
from pathlib import Path

if not __debug__:
    raise SystemExit('Proof audit requires Python assertions')
result = json.loads((Path(__file__).parent / 'build/gnatprove/cubit-display_layouts.spark').read_text())
assert result['spark'] and all(value == 'all' for value in result['spark'].values())
assert result['progress'] == 'PROGRESS_PROOF'
assert result['stop_reason'] == 'STOP_REASON_NONE'
for field in ('pragma_assume', 'skip_flow_proof', 'skip_proof'):
    assert not result[field], field
assert result['proof'], 'No layout admission proof obligations'
assert all(item['severity'] == 'info' for item in result['proof'])
assert any(item['rule'] == 'VC_POSTCONDITION' for item in result['proof'])
print(f"PASS layout proof: {len(result['proof'])} proof diagnostics; no skips or assumptions")
