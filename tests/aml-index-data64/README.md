# IndexField through a QWord data field

Run in the pinned development shell with TMPDIR=/home/doc/cubit-build-tmp:

    python3 tests/aml-index-data64/run.py --validate-only
    bash tests/aml-index-data64/check_reference.sh
    python3 tests/aml-index-data64/run.py --mode release
    python3 tests/aml-index-data64/run.py --mode checked

Both profiles pass 529 checks through the actual service. The two normally
compiled 438-byte DSDTs have identical AML bodies and supply 28 ordered typed
ACPICA observations each. Revision 1 requires eight eight-byte Buffer results;
all bytes, exact lengths and retained-result releases are checked.

The fixture uses Byte/Word/DWord/QWord indexed access through an independently
named QWord data field, unaligned QWord reads/stores, Preserve/Ones/Zeros and
Buffer store sources. Selector byte-offset sequences are checked separately from
result values. Ones/Zeros stores succeed without data-read permission.

The initial implementation returned all 64 data bits from narrow revision-1
reads, whereas ACPICA returns the low 32 bits. The production fix normalizes
only the final scalar to the caller's AML integer width. Internal field transfers
remain 64 bits; wider-than-integer fields still allocate and return Buffers.
The unaligned QWord Preserve store retains the exact upper/lower partial bits
through both transfers, including when its source is a Buffer under revision 1.

The reference checker verifies ordered query names, types, scalar values and
Buffer bytes, exact embedded inputs, identical AML bodies, table extent and
checksums. Both normal compiles report zero errors, warnings and remarks. Original
ACPICA allocation diagnostics remain in the logs; no clean-teardown claim.
Actual runtime origins and retained failed predecessor are recorded in HANDOFF.

A direct QWord DAT0 read with read permission removed must return Unsupported_Value
without any attempted provider dispatch, selector write, data read, register
change or retained result. This also exercises release of the preallocated
eight-byte result Buffer for AML revision 1. A fresh read after explicit permission
restoration must succeed with exactly one dispatch/read. The regression fails
against the predecessor lacking the final planned-read authorization check.

This is hosted functionality evidence, not a new SPARK proof or native execution.
The provider stores one fixed data-register value, matching ACPICA's simulated
region; it does not emulate a physical indexed bank, varying per-index values,
transport completion or concurrency. Larger-than-64-bit Fields, remaining width
combinations, protocols/GlobalLock, dynamic table lifetimes, whole-interpreter
proof and native integration remain unfinished. Earlier recovery evidence keeps
its separate source/profile scope; it was not broadly rerun for this checkpoint.

Limits: build180s, invocation30s, ACPICA5s, j1/nice19, stack64MiB, captured log1MiB.
