# Hosted banked field widths

Run `python3 tests/aml-bank-widths/run.py --mode release` or `--mode checked` in
the pinned Nix environment, with TMPDIR set before entry as coordination requires.
The actual Core suite covers AnyAcc and unaligned Word/DWord/QWord banked fields
through eight independently named register targets. Each wider field spans two
datums. Tests check the constant bank selector before every datum operation,
exact data target order/direction, preserved adjacent bits and result lifetime.
WriteAsOnes/Zeros require no data-read permission or data reads.

The normal 485-byte ACPICA DSDT compiles without errors, warnings or remarks.
There are 24 successful typed observations per integer width. The 32-bit variant
changes the revision/checksum header bytes only. Wide reads return eight-byte
Buffers under 32-bit AML and Integers under 64-bit AML; stores use raw Buffer
bytes so they do not depend on integer-literal truncation. Root result bytes,
length, release and retained counts are checked. The raw oracle retains its
original allocation diagnostics; this is not a claim of clean ACPICA teardown.

`bash tests/aml-bank-widths/check_reference.sh` validates all ordered names,
Integer/Buffer values, embedded bytes, header revision, extent and checksum.
It does not invoke ACPICA or rebuild anything. The model uses nonoverlapping
register pairs, so byte values correspond to the independently simulated memory.
It does not model physical hardware serialization, Global Lock, nested selectors,
all widths/geometries or wide-operation failure prefixes. Existing focused
partial-write recovery tests retain their separate scope. No production change
or new namespace/executor/banked-I/O proof accompanies this width test checkpoint.

The strengthened matrix contains 734 checks across the two AML integer widths.
One combined provider request log verifies an alternating bank-write/data-access
sequence, exact selection values and named target directions. Separate selector
and data logs alone were insufficient to establish that interleaving; the initial
590-check matrix is retained as earlier value/result evidence. No production fix
was needed when the interleaving assertions were added.
