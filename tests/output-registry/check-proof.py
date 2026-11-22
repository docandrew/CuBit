"""Require complete non-vacuous proof of registry and ticket entry points."""
import json
from pathlib import Path

if not __debug__:
    raise SystemExit('Proof audit requires Python assertions')
total = 0
for unit in ('cubit-display_outputs', 'registry_proof', 'cubit-placement_tickets'):
    result = json.loads((Path(__file__).parent /
                        f'build/gnatprove/{unit}.spark').read_text())
    assert result['spark'] and all(v == 'all' for v in result['spark'].values())
    assert result['progress'] == 'PROGRESS_PROOF'
    assert result['stop_reason'] == 'STOP_REASON_NONE'
    for field in ('pragma_assume', 'skip_flow_proof', 'skip_proof'):
        assert not result[field], field
    assert result['proof'], f'No proof obligations for {unit}'
    assert all(item['severity'] == 'info' for item in result['proof'])
    assert any(item['rule'] == 'VC_POSTCONDITION' for item in result['proof'])
    total += len(result['proof'])
print(f'PASS output registry/tickets: {total} proof diagnostics; '
      'no skips or assumptions')
