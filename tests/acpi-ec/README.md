# Hosted EC engine, named register owner and deferred endpoint

Run in the Nix development shell, with TMPDIR set before entering it:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-ec/run.sh
```

`--validate-only` checks source/test hashes without compiling or executing.
`--source-root DIR` selects an explicitly supplied matching source tree.
All builds are private, nice19/j1, 180s compile/30s runtime, stack64MiB/core0,
logs at most1MiB. Checked uses assertions/overflow checks; release uses -gnatp.
Both reject warnings. Input hashes and actual ALI switches are checked, and
command statuses and COMPLETE.json are retained in the printed output folder.

The controller model exercises exact command/address/data order, delayed IBF
assertion/consumption, output buffering, all256 cell offsets, read/write/query,
stop/drain, deadlines/budgets, lost/reversed clocks, lost ownership, refused and
unknown accesses, unexpected output, burst refusal, inherited busy input, no
pending query event and clock loss after consuming read output. Every Step is
checked for at most one register callback, and terminal calls cannot replay I/O.

The actual named owner and deferred endpoint use the existing Field wire
protocol. Tests cover full-width owners differing above their low32 bits, stale
and advancing epochs, read/write/mask admission, exact-write safety, active
revocation refusal, retained ID history, monotonic tokens, one completion,
malformed packets, deferred successful read, and unknown completion mapping
with retained ownership and zero result data. Admissions are trusted fixture
facts, not facts discovered from firmware.

This is model-based hosted testing, not ACPICA EC validation or native IPC,
Global Lock, hardware or SPARK proof. Native integration and the full verified
userspace ACPI/AML objective remain incomplete. Counts are regression evidence,
not a coverage percentage.

Query dispatch joins the actual serialized query owner, AML service core,
_Qxx method owner, notification queue and typed lid reader. Tests validate
nonzero name mapping, zero suppression, no ancestor fallback, legitimate
aliases, wrong object/arguments, fatal/error reports, zero budgets, foreign
Core refusal and retained-result cleanup after a failed release. A modeled
query triggers actual _Q01 AML, queues Notify(LID0,0x80), and changes a typed
_LID sample from Closed to Open. No physical lid event is claimed.

The pinned ACPICA20260408 compiler compiles the complete ASL fixture normally,
without force or diagnostic suppression. Repair-disabled ACPICA verifies the
successful handler effects and lid value. Two unused-local warnings and four
shutdown cache allocations remain in the raw logs. Neither exit0 nor these
fixture observations prove clean ACPICA teardown or driver conformance.

The separate tests/acpi-ec-proof gate proves selected transaction-owner state,
timing, work and callback-count contracts at levels1/2. Hosted tests here do
not establish those proofs or extend them to catalog/method/native behavior.


## Catalog progress scheduling

Registers.Next_Progress(Now) is a serialized,read-only advisory observation of
the catalog's single accepted operation. No_Work means no catalog advancement
or uncollected completion,Run_Step means call the owning adapter's bounded
Advance,Wait_Until carries a monotonic microsecond timestamp no later than the
accepted request's original deadline. It exposes no hardware or clock callback.
Nonwait recommendations have At_Time=0. Repeated observation does not move wake
time,consume work budget or perform IO. Newly accepted work replaces the cached
deadline only after Engine.Start succeeds.

After writing,Run_Step allows the engine's fresh postwrite timestamp sample.
The next observation exposes its earliest safe poll. At/after deadline,Run_Step
allows timeout/collection rather than sleeping past it. Terminal completions
also need the owning route's Advance/Collect; calling Step alone cannot deliver
them. Field saved-reply and query/event owners must remain serialized and use
their own completion routes. Engine checks its own fresh clock,ownership and
budget even if an advisory observation was stale or ignored.

No_Work is catalog-specific,not global quiescence. Delivered hardware recovery
can remain occupied and retained while Next_Progress says No_Work. Saved replies,
pending publication/receipts,AML roots and Field uncertainty have independent
lifetimes. Pending publication work must run even when catalog progression is
empty. Recompute advice after incoming work; OS wait/deadline conversion,fairness
and polling cadence remain the native owner's job. This adds no native loop,
launcher,capability or hardware access.

Hosted startup/Field/native-reply/event integration now drives virtual time from
these recommendations:1868joined+1063geometry checks per checked/release profile.
Original EC suite passes24375engine+1043catalog+645query checks per profile.
Deadline/wake tests at ordinary and maximum clock values verify no IO on repeated
observations,immediate terminal collection,and No_Work beside retained recovery.
No new proof or native/hardware execution claimed; existing engine source and
its proof inputs are unchanged.
