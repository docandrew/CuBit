# Larger IndexField payload and result-copy checks

Use the pinned development shell with TMPDIR=/home/doc/cubit-build-tmp:

    python3 tests/aml-index-large/run.py --validate-only
    bash tests/aml-index-large/check_reference.sh
    python3 tests/aml-index-large/run.py --mode release
    python3 tests/aml-index-large/run.py --mode checked

Each size is an independent bounded process: small512bytes, medium4096bytes,
large16384bytes. Both profiles pass all three. Exact alternating QWord selector
writes and Byte data reads are checked, including selectors beyond255; every
result byte, exact Buffer length and retained-handle release is required.
The caller explicitly selects existing maximum provider/work budgets before the
first measurement. Production defaults and the30-second runtime limit are unchanged.

Normal ACPICA returned complete512/4096byte Buffers, all bytes EF. Its debugger
could not return16384bytes plus the24-byte result header in its fixed0x4000 output
buffer. The original AE_BUFFER_OVERFLOW is retained and is NOT counted as a
successful returned-value observation. The later selector query confirms0x3FFF.
The16KiB CuBit case is a hosted stress test with independently modeled data,
not a full ACPICA return match. The reference checker distinguishes these outcomes
and checks exact embedding, extent/checksum and normal compiler output. Original
allocation diagnostics remain; no clean-teardown claim or tool-limit change.

The production change replaces repeated Set_Stored_Byte calls during result commit
with one Store_Buffer update. The result is already allocated and nonempty, so
that API overwrites the same storage without allocation or changing object identity.
Allocation still precedes I/O and publication still follows successful transfers.

Actual checked times:512B0.294s,4096B1.824s,16384B7.112s. Release times were0.017s,
0.029s,0.071s. These include setup, full transfer/byte checks and cleanup. A separate
serial, alternating three-pair512B comparison measured medians0.345s before and
0.304s after (about12% locally). Both binaries/tools were hashed before/after and
every run required2577 checks. This is a small local sample, not a universal speedup
claim. Per-byte assertion postconditions compared the entire object store repeatedly;
the bulk update checks the resulting store once.

Case counts are2577/20497/81937 (many are individual byte/request checks), not AML
coverage percentages. New wide922 and QWord-data496 regressions also pass both
profiles on the exact changed production. The checkpoint HANDOFF identifies origins,
raw reports, audits and comparison files.

After the later Store_Buffer proof refactor, a quadratic executable occupancy-map
invariant caused checked512B to take9.45s and4096B to exceed30s. The corrected
array aggregate restores checked times to roughly0.347/2.118/8.041s on the actual
occupancy-aggregate-UiVZj0MJ source; all three cases pass under the same limits.
These are local smoke timings with possible host contention, not a controlled
speedup claim. The aggregate's small correspondence proof is separate from the
still-partial Store_Buffer proof. The checkpoint HANDOFF records exact run paths.

No native hardware execution, complete payload-size/width/fault coverage or
whole-interpreter correctness claim. Large writes, larger-payload failure prefixes,
table lifetimes, direct/banked wide fields and native integration remain open.
Build180s, invocation30s, ACPICA5s, j1/nice19, stack64MiB, captured log1MiB.
