# Wide Field selectors

Private hosted suite using normally compiled ACPICA reference tables in AML32
and AML64 modes. Checks a73-bit unaligned direct QWord selector and80-bit indexed
selector feeding BankFields. All request order, widths, selector padding,
Preserve edges and overlapping memory effects are checked against an independent
memory model. Buffer writes ensure a child selector uses its own scalar input,
not the root Buffer source. ACPICA supplies14 typed observations/revision,
including three complete ten-byte Buffer returns after wider DataName stores.

Release and assertion-enabled profiles each pass6434 checks across both AML
integer widths. Each revision additionally denies the next request at all5
direct-bank read prefixes and all51 nested-bank read prefixes, including initial
denial. This gives108 mid-operation revocations and4 initial denials/profile.
The exact dispatch prefix and full memory state must match; no retained result
or unknown completion may escape. Fresh invocations after explicit restoration
must succeed. These model checks do not establish atomic concurrent revocation.

The expanded cases write through an80-bit DataName with full-byte Preserve,
partial WriteAsOnes and partial WriteAsZeros. Each insertion uses the full64-bit
intermediate scalar then zero padding; the DataName's own QWord Preserve edge
requires separate read permission. All nine request prefixes per store/result
sequence are checked for exact denial effects and Buffer cleanup, and missing
tail-read permission must prevent all I/O. Wider datum reads and partial outer
Preserve stores requiring them remain rejected before any selector effects.
Across both revisions this adds48 mid-operation revocations, six initial next-
request denials and two initial tail-read denials/profile to the selector cases.

The original interpreter builds but rejects BRED as Unsupported_Value; its
failed result is retained separately. The new code permits bounded wide child
writes and zero-extends each child scalar. DataName writes now accept wider
fields when no old datum is needed; child datum reads remain scalar. Capacity
derives from the existing object byte bound.
The selected fixture covers73/80-bit selectors, not every possible size or graph.

Run saved run.py --mode release / --mode checked in the pinned development shell.
Each revision has its own30s runtime bound; compile180s,j1,nice19,stack64MiB,
log1MiB. Reference capture uses the existing pinned20260408 tools with5s limits.
No native or SPARK proof claim. Remaining wide data-read semantics, Field graphs,
protocols, GlobalLock, concurrency and dynamic table lifetime are separate work.
Actual runtime/audit origins and remaining proof debt are recorded in HANDOFF.
