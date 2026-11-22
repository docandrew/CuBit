# Bounded EC backend progress dispatcher

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-backend-progress/run.sh

Use the pinned project Nix shell. --source-root DIR and --validate-only supported.
270 actual sources,three test inputs,normal iASL fixture. Both checked/release
profiles pass1954joined+1063geometry checks. This adds a trusted backend
dispatcher and hosted composition,not a launched native receive loop or new ABI.

ACPI_EC_Backend_Progress binds the concrete catalog,saved Field reply owner and
event publisher by matching generic package identities. Next is read-only.
Advance chooses at most one Field adapter step,query publisher step or bounded
nonblocking publication attempt. It never directly steps the engine,executes
AML,waits internally or calls a peer synchronously. Report.Field applies only
when Action=Advance_Field; other actions do not attest a Field completion.

Runnable Field/publication work alternates. Publication remains runnable while
Field waits for its safe poll time,and when the hardware catalog has no work.
Finite publication episodes are not reset by the dispatcher. Stalled or unknown
notice delivery never causes retry,query replay or automatic receipt acceptance.
Fields can still progress alongside unknown notice retention for handler readback.

Owner fault and inconsistent saved-reply/query/catalog ownership stop advancement
without stepping hardware,answering an unrelated caller or clearing obligations.
After each selected action the report refreshes retained reply/notice/recovery
observations; Action names the work actually selected. No_Work is not global
quiescence. Catalog recovery,AML roots/Field uncertainty,notice/receipt retention
and supervisor obligations remain independent. Serialize all callbacks/entry
points; callback truthfulness,normal return and nonreentry remain assumptions.

The outer native owner must receive/authenticate calls/events/receipts,start only
admitted queries,perform bounded OS waits,convert clock/deadline units and retain
reservation/GlobalLock. Recompute Next after incoming work. No source/register
authority,launcher,component or kernel/runtime change is added. Existing AML
native service remains snapshot-only/hardware-pending.

Hosted actual startup/Field/native-reply/query/receiver/AML/lid flow uses the
dispatcher. A simultaneous saved Field read and two backpressured publication
attempts alternate; the Field completes while notice is stalled,then explicit
resume publishes the same token without a new query. The suite checks wait
recommendations,per-step bounded IO,unknown delivery,independent recovery and
no replay. A deliberately unowned query is refused without IO,then its initiating
test owner cancels it before writing. An injected inconsistent Save result
quarantines its separate modeled owner without IO or answers. That negative
instance intentionally retains its modeled invalid saved flag; no real reply
capability is created or released,and no global quiescence is claimed.

ASL fixture bytes are identical to the earlier independent repair-disabled
ACPICA startup/lid reference. That reference still checks those method semantics,
not this dispatcher or physical EC/IPC/timing behavior; it was not rerun here.
Hardware,clock,receive metadata and queues are modeled. No new SPARK proof,
native execution or hardware validation. Full verified AML/service remains open.


## Actual-runtime semantic gate

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-backend-progress/semantic.sh

Use the pinned project shell. --source-root DIR and --unit-root DIR permit
isolated source snapshots. This runner is separate from the behavioral hosted
suite: it copies222actual runtime/capability specification files with the real
EC/publisher/native-call/dispatcher declarations and bodies,238sources total.
No IPC fixture declaration is selected. Default Save_Reply/Answer/reply callbacks
are instantiated,with actual opaque Process_ID sender/consumer values above32bits.

Checked/release semantic profiles compile with -gnatc and strict warnings,inspect
the emitted ALI switches,and require zero object/binder output. No executable is
linked or run. native-semantics.json retains the original commands,input hashes
and source origins. This is declaration/type compatibility,not runtime behavior,
whole native compilation,launch permission,actual message delivery,clock-unit
conversion,admitted hardware,stack fit or a SPARK proof. Hardware callbacks in
the semantic-only main are refusing placeholders; that main must never execute.

The first private attempt found a useless test-main assignment; corrected while
strict warnings remained enabled,then both profiles passed. Production code
unchanged. Native service remains snapshot-only/hardware-pending; provider
admission,backend launch,receive/wait/event routing and hardware acceptance remain.


## Successor bounded wait supervision

ACPI_EC_Backend_Waits combines one bounded pause and one owning adapter check,
so a stopped clock cannot defer engine budget forever. Pause failure/exhaustion
requests stop without claiming cancellation or clearing recovery. New2021joined+
1063geometry/profile cases cover advancing/frozen clocks and exactly-once saved
reply completion. See tests/acpi-ec-backend-waits for assumptions/integration gaps.
Its246-source actual-runtime semantic gate covers the changed dispatcher and
real clock callback. Earlier native-semantics records remain historical snapshots
for that dispatcher; no old hash record is relabeled as current. No new proof,
native launch,hardware execution or recovery reset is claimed.
