# Wider direct and banked region Fields

Private hosted functionality suite; not a whole-interpreter/native proof.
Normal ACPICA1074-byte DSDTs yield52 typed observations/revision (26 complete
10-byte Buffer returns and26 INIT Integers). Exact inputs/logs/alloc diagnostics
are retained. The independent provider models overlapping Byte/Word/Qword named
registers over a32-byte memory region, with exact geometry/permissions checks.
Tests cover73/80-bit direct/BankField reads, aligned/unaligned Byte, Word/Qword,
Preserve/Ones/Zeros stores, long/short Buffer and Integer sources, full memory
state, exact request sequences, retained-result release and permission denial.
Run saved run.py in the pinned shell with --mode release / --mode checked.
Each AML revision is an independent30-second case; compile180s,j1,nice19.
The extended fixture includes Word/Qword stores, both partial Qword Preserve
edges and exact overlapping selector/data memory effects. The normal baseline
cases exercise all26 reference methods, now with an explicit clean-state check
before each reset. Additional cases inject576 failures/profile at every request
position in direct Qword reads, banked Byte reads, direct Qword Preserve stores
and unaligned banked Byte Preserve stores, in both AML integer widths.

Each method/revision/failure-kind combination is an independent bounded case.
Denied-before-effect, failed-after-effect and unknown-before/after-effect cases
check exact dispatched prefixes and all completed overlapping-register effects.
No partial returned Buffer may escape. Unknown completion blocks re-entry;
unconfirmed or wrong receipts are rejected, exact confirmed reconciliation does
no I/O, and an explicit later invocation must recover. Provider effect knowledge
and authoritative confirmation are trusted hosted assumptions.

Separate one-work-unit cases require exhaustion before I/O. One-request cases
require exactly one completed direct read or bank-selector write, no retained
partial result/unknown receipt, and successful later normal-budget invocation.
No production defaults or runtime limits change. The checkpoint HANDOFF names
actual results and independent source/reference/tool/runtime/ALI/artifact audits.
Both release and assertion-enabled runs pass all48 cases and34062 checks/profile.
Twelve additional cases exercise158 mid-operation permission revocations and
four scalar pre-denials per profile. The model records attempted dispatches
separately from admitted requests: each denial must stop before the next dispatch,
preserve exactly the completed prefix and modeled memory, and publish no partial
Buffer. Explicit permission restoration and a new invocation must succeed.
Setup resets model memory only after checking the failed operation's full effects.
These are selected check counts and local smoke timings, not coverage percentages
or a controlled performance measurement. Production now rechecks direct scalar
read/write and planned-read permission immediately before dispatch. The provider
still independently admits every actual request. The predecessor attempted a
dispatch after a known denial; the provider rejected it without hardware effect.
Test success does not establish native register admission or
hardware bank switching, GlobalLock/protocol/concurrency support, all widths/
sizes/failure prefixes, table lifetimes or full SPARK correctness.
