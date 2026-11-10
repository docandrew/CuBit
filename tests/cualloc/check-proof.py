#!/usr/bin/env python3
"""A successful gnatprove run is not proof evidence if units were skipped or
assumptions crept in. CuAlloc may leave exactly these outside SPARK (spec
analysed, body not): the provider calls, the address conversion, payload
zeroing and copying, the atomic steps on shared words (map, registry,
inboxes, lock words), node words, the thread token and the thread-local
heap slot (whose package, SPARK_Mode Off, holds no subprogram). Its stated assumptions are all at the memory boundary
(userspace/allocator/process/cualloc.adb), per heap instance:

  7  "a heap's private part is as its holder last left it": the overlay of
     the calling thread's heap at each entry (Allocate_Fast, Free_Fast,
     Allocate_Here, Free_Here, Exit_Here), which the holder discipline
     gives; and two read-only overlays of other heaps (Find_Arena, for
     Usable_Size: the record read is fixed while the block is live;
     Statistics: diagnostics, approximate while threads run).
  3  "a small arena's metadata is as its holder last left it" (Slab_Allocate,
     Slab_Take, the fast path's allocation from a cached slab, and
     Slab_Release), and
  2  the same for medium arenas (Extent_Allocate, Extent_Release): only the
     arena's heap's holder calls them.
  4  "the directory is as the last locked operation left it": the huge
     directory's overlay in each locked huge-block operation.
"""
import collections
import json
import pathlib

if not __debug__:
    raise SystemExit('Proof audit must not run with Python assertions disabled')
root = pathlib.Path(__file__).resolve().parent / 'build/proof/gnatprove'
HEAPS = ['cualloc_proof_heap', 'cualloc_proof_odd_heap']
UNITS = HEAPS + ['cualloc_requests']
BOUNDARY = {
    'Provider_Reserve', 'Provider_Commit', 'Provider_Release', 'To_Address', 'Zero', 'Copy',
    'Load_64', 'Load_32', 'Store_64', 'Store_32', 'Take_All', 'Push_Node',
    'Node_Read', 'Node_Entry', 'Node_Put', 'Node_Seal',
    'Get_Token', 'Get_Thread_Heap', 'Set_Thread_Heap',
    'Try_Take', 'Give_Back', 'Try_Take_Fallback', 'Give_Back_Fallback', 'Pause',
}
ASSUMES = {
    "a heap's private part is as its holder last left it": 7,
    "a small arena's metadata is as its holder last left it": 3,
    "a medium arena's metadata is as its holder last left it": 2,
    'the directory is as the last locked operation left it': 4,
}
source = (pathlib.Path(__file__).resolve().parents[2] / 'userspace/allocator/process/cualloc.adb').read_text()
for claim, count in ASSUMES.items():
    found = source.count(f'"{claim}"')
    assert found == count, (claim, found, count)
total = 0
for unit in UNITS:
    result = json.loads((root / f'{unit}.spark').read_text())
    entities = result['entities']
    outside = {entities[k]['name'].split('.')[-1] for k, v in result['spark'].items() if v != 'all'}
    assert all(v in ('all', 'spec', 'no') for v in result['spark'].values()), unit
    # Atomic intrinsics (GCC builtins) are imported with SPARK_Mode Off too.
    outside = {name for name in outside if not name.startswith(('Atomic_', 'Exchange_8', 'Store_8'))}
    assert outside == (BOUNDARY if unit in HEAPS else set()), (unit, sorted(outside ^ BOUNDARY))
    for field in ['skip_flow_proof', 'skip_proof']:
        assert not result[field], (unit, field, result[field])
    assert result['progress'] == 'PROGRESS_PROOF', unit
    assert result['stop_reason'] == 'STOP_REASON_NONE', unit
    assert result['proof'], f'{unit}: no obligations found'
    for obligation in result['proof'] + result['flow']:
        assert obligation['severity'] in ('info', 'warning'), (unit, obligation)
    assumes = result['pragma_assume']
    if unit in HEAPS:
        assert all(a['file'] == 'cualloc.adb' for a in assumes), (unit, assumes)
        seen = collections.Counter(a.get('claim', a.get('message', '')) for a in assumes)
        assert len(assumes) == sum(ASSUMES.values()), (unit, len(assumes), seen)
    else:
        assert not assumes, (unit, assumes)
    total += len(result['proof'])
print(f'PASS CuAlloc proof coverage: {len(UNITS)} units, {total} proof diagnostics, no skipped units; '
      f'outside SPARK only {sorted(BOUNDARY)} (and GCC atomic intrinsics); '
      f'{sum(ASSUMES.values())} pragma Assume per heap, all in cualloc.adb')
