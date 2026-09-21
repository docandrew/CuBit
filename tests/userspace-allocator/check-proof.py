#!/usr/bin/env python3
"""A successful command is not proof evidence if the units were skipped."""
import json
import pathlib

if not __debug__:
    raise SystemExit('Proof audit must not run with Python assertions disabled')
root = pathlib.Path(__file__).resolve().parent / 'build/slabs/gnatprove'
units = ['heap_classes', 'heap_bitmap', 'heap_extents', 'slab_model', 'heap_slab_instance']
total = 0
for unit in units:
    result = json.loads((root / f'{unit}.spark').read_text())
    assert result['spark'] and all(v == 'all' for v in result['spark'].values()), unit
    for field in ['pragma_assume', 'skip_flow_proof', 'skip_proof']:
        assert not result[field], (unit, field, result[field])
    assert result['progress'] == 'PROGRESS_PROOF', unit
    assert result['stop_reason'] == 'STOP_REASON_NONE', unit
    assert result['proof'], f'{unit}: no obligations found'
    for obligation in result['proof']:
        assert obligation['severity'] == 'info', (unit, obligation)
    total += len(result['proof'])
print(f'PASS proof coverage: all {len(units)} units analyzed, {total} proof diagnostics, no skipped units or pragma Assume')
