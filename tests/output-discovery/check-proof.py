"""Require non-vacuous proof with no assumptions or skipped obligations."""
import json
from pathlib import Path

if not __debug__:
    raise SystemExit('Proof audit requires Python assertions')
result = json.loads((Path(__file__).parent /
                     'build/gnatprove/cubit-output_discovery.spark').read_text())
assert result['spark'] and all(v == 'all' for v in result['spark'].values())
assert result['progress'] == 'PROGRESS_PROOF'
assert result['stop_reason'] == 'STOP_REASON_NONE'
for field in ('pragma_assume', 'skip_flow_proof', 'skip_proof'):
    assert not result[field], field
assert result['proof']
assert all(item['severity'] == 'info' for item in result['proof'])
assert any(item['rule'] == 'VC_POSTCONDITION' for item in result['proof'])
print(f"PASS output discovery: {len(result['proof'])} proof diagnostics; "
      'no skips or assumptions')
