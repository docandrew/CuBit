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
           'heap_slab_instance__live', 'heap_classes__classes',
           # The class tables (data): strides, slots per slab, reciprocals.
           'heap_classes__strides', 'heap_classes__slot_counts', 'heap_classes__magics'}
assert all(line.split()[-1] in allowed for line in undefined), undefined


def body(archive, symbol):
    """The instructions of symbol's function in archive (objdump)."""
    out = subprocess.check_output(['objdump', '-d', '--no-show-raw-insn', f'--disassemble={symbol}', str(archive)],
                                  text=True)
    return [line.split('\t', 1)[1] for line in out.splitlines() if re.match(r'^\s+[0-9a-f]+:\t', line)]


# The allocation entry points are CuAlloc's fast path with no frame: no
# call (a miss is a tail jump to the out-of-line rest, where errno or the
# Ada heap's stop lives) and no register saved (ALLOC-001 step 15).
def frameless(archive, symbol):
    code = body(archive, symbol)
    assert code, (archive, symbol)
    bad = [i for i in code if i.startswith(('call', 'push'))]
    assert not bad, (archive.name, symbol, bad)


frameless(archive, 'cualloc_allocate')
checked = ['cualloc_allocate']
# Native archives, when built (make -C kernel world): the libc's malloc and
# GNAT's __gnat_malloc over the same fast path.
for native, symbol in [(root / 'userspace/libc/build/sysroot/lib/libc.a', 'malloc'),
                       (root / 'userspace/runtime/adalib/libgnat-user.a', '__gnat_malloc')]:
    if native.exists():
        frameless(native, symbol)
        checked.append(f'{native.name}:{symbol}')
print('PASS release codegen: no Ghost/assertion symbols; bridge dependencies limited to slab core and class tables;'
      ' frameless: ' + ', '.join(checked))
