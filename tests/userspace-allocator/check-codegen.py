#!/usr/bin/env python3
"""Release archive must not contain Ghost predicates or assertion machinery."""
import pathlib
import re
import subprocess

if not __debug__:
    raise SystemExit('Codegen audit must not run with Python assertions disabled')
root = pathlib.Path(__file__).resolve().parents[2]
archive = root / 'userspace/allocator/build/lib/libcubit_heap.a'
symbols = subprocess.check_output(['nm', '-a', str(archive)], text=True)
for forbidden in [r'__valid\b', r'__membership_preserved\b', r'__preserved\b', r'__population\b',
                  r'__can_allocate\b', r'__prove_',
                  r'__ghost_', r'__diagnostics', r'system__bit_ops__', r'system__bitfields__',
                  r'__gnat_rcheck', r'__gnat_raise', r'assert_failure']:
    assert not re.search(forbidden, symbols, re.IGNORECASE), forbidden
assert 'ca_malloc' in symbols and 'ca_free' in symbols
bridge = root / 'userspace/allocator/build/objects/heap_bridge.o'
undefined = subprocess.check_output(['nm', '-u', str(bridge)], text=True).splitlines()
allowed = {'heap_slab_instance__initialize', 'heap_slab_instance__scan_pages',
           'heap_bitmap__reconfigure', 'heap_bitmap__refill',
           'heap_slab_instance__live', 'heap_classes__classes'}
assert all(line.split()[-1] in allowed for line in undefined), undefined
print('PASS release codegen: no Ghost/assertion symbols; bridge dependencies limited to slab core and class table')
