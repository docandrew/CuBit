"""Reject skipped or incomplete loan proofs, including vacuous instantiations."""
import json
from pathlib import Path

if not __debug__:
    raise SystemExit('Proof audit requires Python assertions')
root = Path(__file__).parent / 'build' / 'gnatprove'
total = 0
for unit in ('loan_proof', 'memory_grants'):
    result = json.loads((root / f'{unit}.spark').read_text())
    assert result['spark'] and all(v == 'all' for v in result['spark'].values())
    assert result['progress'] == 'PROGRESS_PROOF'
    assert result['stop_reason'] == 'STOP_REASON_NONE'
    for field in ('pragma_assume', 'skip_flow_proof', 'skip_proof'):
        assert not result[field], (unit, field)
    assert result['proof'], unit
    assert all(p['severity'] == 'info' for p in result['proof']), unit
    posts = [p for p in result['proof'] if p['rule'] == 'VC_POSTCONDITION']
    assert posts, unit
    if unit == 'memory_grants':
        entities = {int(k): v['name'] for k, v in result['entities'].items()}
        proved = {entities[p['entity']] for p in posts}
        for operation in ('Retain_Forwarding_Hold', 'Release_Forwarding_Hold',
                          'Record_Acquire', 'Record_Return',
                          'Request_Revocation', 'Close_Receiver'):
            assert f'Memory_Grants.{operation}' in proved, operation
    if unit == 'loan_proof':
        entities = {int(k): v['name'] for k, v in result['entities'].items()}
        proved = {entities[p['entity']] for p in posts}
        for instance in ('Production', 'Bounded'):
            for operation in ('Configure', 'Reserve', 'Publish', 'Acquire',
                              'Return_Reader', 'Revoke', 'Finish_Retirement',
                              'Close', 'Release_Parent'):
                name = f'Loan_Proof.{instance}.{operation}'
                assert name in proved, name
        for operation in ('Submit', 'Apply', 'Close'):
            assert f'Loan_Proof.Presentation.{operation}' in proved, operation
    total += len(result['proof'])
print(f'PASS grant loans: {total} proof diagnostics; no skips or assumptions')
